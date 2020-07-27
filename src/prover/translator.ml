open ProverLib
module TezlaCfg = ProverLib.TezlaCfg

(*****************************************************************************)
(*****************************************************************************)
(* Exceptions                                                                *)
(*****************************************************************************)
(*****************************************************************************)

exception Exn_Translator of string

let fail s = raise (Exn_Translator s)


(*****************************************************************************)
(*****************************************************************************)
(* Convert Batteries Datatype to Core Datatype                               *)
(*****************************************************************************)
(*****************************************************************************)

let c_list_of_batset : 'a BatSet.t -> 'a Core.List.t
=fun s -> BatSet.elements s

let c_hashtbl_of_b_hashtbl : ('a, 'b) Batteries.Hashtbl.t -> ('a, 'b) Core.Hashtbl.t
=fun bht ->
  let cht = Core.Hashtbl.Poly.create () in
  Batteries.Hashtbl.iter (fun k v -> Core.Hashtbl.Poly.add cht ~key:k ~data:v |> Stdlib.ignore) bht;
  cht


(*****************************************************************************)
(*****************************************************************************)
(* Tezla_cfg to Cfg                                                          *)
(*****************************************************************************)
(*****************************************************************************)

let tcfg_cast_stmt x = x
let tcfg_cast_edge_label : TezlaCfg.edge_label -> Cfg.edge_label = function
  | Normal -> Normal
  | If_true -> If_true
  | If_false -> If_false
let tcfg_get_id n = n.Tezla_cfg.Cfg_node.id
let tcfg_get_stmt n = n.Tezla_cfg.Cfg_node.stmt

let of_tezlaCfg tcfg =
  let open Cfg in
  let open Core in
  let vertices : int List.t = TezlaCfg.labels tcfg |> c_list_of_batset in
  (* Get entry and exit nodes *)
  let t_vertices = List.filter vertices ~f:(fun n -> TezlaCfg.is_extremal tcfg n) in (* terminal vertices in Cfg *)
  let inflow_len g n = TezlaCfg.inflow g n |> List.length in   (* # of inflow edges *)
  let outflow_len g n = TezlaCfg.outflow g n |> List.length in  (* # of outflow edges *)
  let entry_vertices = List.filter t_vertices ~f:(fun n -> (inflow_len tcfg n = 0) && (outflow_len tcfg n > 0)) in
  let exit_vertices  = List.filter t_vertices ~f:(fun n -> (inflow_len tcfg n > 0) && (outflow_len tcfg n = 0)) in
  let enlen = List.length entry_vertices in
  let exlen = List.length exit_vertices in
  let main_entry, main_exit = 
    if (enlen <> 1 || exlen <> 1)
    then fail ("of_tezlaCfg : size(entry_vertices) =" ^ (string_of_int enlen) ^ ", size(exit_vertices) =" ^ (string_of_int exlen))
    else (List.hd_exn entry_vertices, List.hd_exn exit_vertices)
  in
  (* Get flow (edges) *)
  let graph_v : G.t = List.fold vertices ~init:G.empty ~f:(fun acc v -> G.add_vertex acc v) in
  let label_wrap : TezlaCfg.G.E.t -> G.E.t =
    let open TezlaCfg.G.E in
    fun e -> G.E.create (src e |> tcfg_get_id) (label e |> tcfg_cast_edge_label) (dst e |> tcfg_get_id) in
  let flow = TezlaCfg.G.fold_edges_e (fun e acc -> G.add_edge_e acc (label_wrap e)) tcfg.flow graph_v in
  (* Get node_info *)
  let node_info : (vertex, stmt) Core.Hashtbl.t = Core.Hashtbl.Poly.create () in
  let t_tbl : (int, TezlaCfg.Node.t) Core.Hashtbl.t = TezlaCfg.get_blocks tcfg |> c_hashtbl_of_b_hashtbl in (* tezlaCfg table *)
  let _ = Core.Hashtbl.iter t_tbl ~f:(fun x -> Core.Hashtbl.add node_info ~key:x.id ~data:(tcfg_cast_stmt x.stmt) |> Stdlib.ignore) in
  let gen_t : 'a -> 'a Michelson.Adt.t = fun x -> {pos = Michelson.Location.Unknown; d = x;} in (* sugar function *)
  { flow = flow;
    (*node_info = node_info;*)
    vertex_info = Core.Hashtbl.fold node_info ~init:ProverLib.Cfg.CPMap.empty ~f:(fun ~key ~data acc -> ProverLib.Cfg.CPMap.add_exn acc ~key:key ~data:data);
    type_info = ProverLib.Cfg.CPMap.empty;
    main_entry = main_entry;
    main_exit = main_exit;
    (* added for forward compatibility. They have no meaning *)
    adt = {param=(gen_t Michelson.Adt.T_bool); storage=(gen_t Michelson.Adt.T_bool); code={pos=Michelson.Location.Unknown; d=I_noop};};
    lambda_id_map = Cfg.CPMap.empty;
    fail_vertices = Core.Set.Poly.empty;
  }


(*****************************************************************************)
(*****************************************************************************)
(* Stack information (internal datatype)                                     *)
(*****************************************************************************)
(*****************************************************************************)

type stack_info_t =
| NS of string list (* Normal Stack info *)
| ES of string      (* Error  Stack info *)

let ns_hd = begin function | NS sl -> Core.List.hd_exn sl       | ES _ -> fail "ns_hd : error-stack-info comes in argument" end
let ns_tl = begin function | NS sl -> NS (Core.List.tl_exn sl)  | ES _ -> fail "ns_tl : error-stack-info comes in argument" end
let ns_nth s n = begin match s with | NS sl -> Core.List.nth_exn sl n | ES _ -> fail "ns_nth : error-stack-info comes in argument" end
let ns_rev = begin function | NS sl -> NS (Core.List.rev sl)    | ES _ ->  fail "ns_rev : error-stack-info comes in argument" end

let ns_cons s = begin function | NS sl -> NS (s :: sl) | ES _ -> fail "ns_cons : error-stack-info comes in argument" end
let ns_append = fun s1 s2 -> begin match s1, s2 with | NS sl1, NS sl2 -> NS (sl1 @ sl2) | _ -> fail "ns_append : error-stack-info comes in one of the arguments" end
let ns_unlift = begin function | NS sl -> sl | ES _ -> fail "ns_unlift : error-stack-info comes in argument" end
let ns_split_n sl n = begin match sl with | NS s -> let (h, t) = Core.List.split_n s n in (NS h, NS t) | _ -> fail "ns_split_n : error-stack-info comes in argument" end

let ns_rev_append s1 s2 = begin match s1, s2 with | NS sl1, NS sl2 -> NS (Core.List.rev_append sl1 sl2) | _ -> fail "ns_rev_append : error-stack-info comes in one of the arguments" end

let is_es = begin function | ES _ -> true | _ -> false end
let get_es_str = begin function | ES s -> s | _ -> fail "get_es_str : NS detected" end

(* for debugging purpose. *)
(*let ns_print = begin function | NS sl -> print_string (Core.String.concat ~sep:(", ") sl) | _ -> print_string "[Error-Stack]" end*)


(*****************************************************************************)
(*****************************************************************************)
(* Adt to Cfg                                                                *)
(*****************************************************************************)
(*****************************************************************************)

type cfgcon_ctr = Cfg.cfgcon_ctr
(*let new_vtx : cfgcon_ctr -> Core.Int.t = fun c -> (incr c.vertex_counter; !(c.vertex_counter))*)
(*let new_var : cfgcon_ctr -> Core.String.t = fun c -> (incr c.var_counter; "v" ^ (string_of_int (!(c.var_counter))))*)


let gen_t : 'a -> 'a Michelson.Adt.t = fun x -> {pos = Michelson.Location.Unknown; d = x;}

let get_d : 'a Michelson.Adt.t -> 'a = fun x -> x.Michelson.Adt.d


let map_add : string -> ('k, 'v) Core.Map.Poly.t -> 'k -> 'v -> ('k, 'v) Cfg.CPMap.t
=fun caller_name m k v -> Cfg.t_map_add ~errtrace:caller_name m k v
let map_find : string -> ('k, 'v) Core.Map.Poly.t -> 'k -> 'v
=fun caller_name m k -> Cfg.t_map_find ~errtrace:caller_name m k

(* typical flow manipulation procedure *)
(*
let add_typical_vertex : cfgcon_ctr -> (Cfg.vertex * Cfg.vertex) -> Cfg.t -> (Cfg.G.t * Cfg.vertex)
= let open Cfg in
  fun counter (in_v, out_v) cfg -> begin
  let mid_v = new_vtx counter in
  let flow_1 = G.add_vertex cfg.flow mid_v in
  let flow_2 = G.add_edge (G.add_edge flow_1 in_v mid_v) mid_v out_v in
  (flow_2, mid_v)
end
*)
let add_typical_vertex : cfgcon_ctr -> (Cfg.vertex * Cfg.vertex) -> Cfg.t -> (Cfg.G.t * Cfg.vertex)
= let open Cfg in 
  fun counter (in_v, out_v) cfg -> begin
  let (c, mid_v) = begin
    t_add_vtx counter (cfg, ())
    |> t_con_vtx_front in_v
    |> t_con_vtx_backr out_v
  end in
  (c.flow, mid_v)
end
(* t_add_typical vertex constructs (updated-cfg, new-vertex) *)
let t_add_typical_vertex : string -> cfgcon_ctr -> (Cfg.vertex * Cfg.vertex) -> Cfg.stmt -> Cfg.t -> (Cfg.t * Cfg.vertex)
= let open Cfg in
  fun errtrace counter (in_v, out_v) stm cfg -> begin
  (cfg, ())
  |> t_add_vinfo ~errtrace:(errtrace ^ " : t_add_typical_vertex : in_v") (in_v, Cfg_skip)
  |> t_add_vtx counter
  |> t_add_vinfo_now ~errtrace:(errtrace ^ " : t_add_typical_vertex : mid_v") stm
  |> t_con_vtx_front in_v
  |> t_con_vtx_backr out_v
end

(* Merge two stacks (variable-renaming process). It reconstructs flow, type-info too. *)
(* Precondition : two stacks have same length and type scheme. *)
let merge_two_stack_infos : cfgcon_ctr -> string -> Cfg.t -> (stack_info_t * stack_info_t) -> (Cfg.vertex * Cfg.vertex * Cfg.vertex * Cfg.vertex) -> (Cfg.t * stack_info_t)
= let open Cfg in
  (* sugar functions *)
  let addvtx vtx flw : G.t = G.add_vertex flw vtx in
  let addedg v1 v2 flw : G.t = G.add_edge flw v1 v2 in
  (* FUNCTION BEGIN *)
  fun counter errmsg_trace cfg (stack_info_1, stack_info_2) (in_1_v, in_2_v, out_1_v, out_2_v) -> begin
  let errmsg_gen s : string = (errmsg_trace ^ " : merge_two_stack_infos : " ^ s) in
  begin (* check whether two stack_infos has error state *)
    match (stack_info_1, stack_info_2) with
    | ES v1, ES v2 -> begin
        (* Two Failed edges will be included *)
        let (cfg_end, _) =
          (cfg, ())
          |> t_add_fail_edg (in_1_v, out_1_v)
          |> t_add_fail_edg (in_2_v, out_2_v)
          |> t_add_vinfos ~errtrace:(errmsg_gen "ES-ES") [(in_1_v, Cfg_failwith v1); (in_2_v, Cfg_failwith v2);]
        in
        (cfg_end, ES v1)
      end
    | ES v, NS _ -> begin
      (* No merge needed and stop error stack propagation *)
        let (cfg_end, _) =
          (cfg, ())
          |> t_add_edg (in_2_v, out_2_v)
          |> t_add_fail_edg (in_1_v, out_1_v)
          |> t_add_vinfos ~errtrace: (errmsg_gen "ES-NS") [(in_2_v, Cfg_skip); (in_1_v, Cfg_failwith v);]
        in
        (cfg_end, stack_info_2)
      end
    | NS _, ES v -> begin
        (* No merge needed and stop error stack propagation *)
        let (cfg_end, _) =
          (cfg, ())
          |> t_add_edg (in_1_v, out_1_v)
          |> t_add_fail_edg (in_2_v, out_2_v)
          |> t_add_vinfos ~errtrace: (errmsg_gen "NS-ES") [(in_1_v, Cfg_skip); (in_2_v, Cfg_failwith v);]
        in
        (cfg_end, stack_info_1)
      end
    | NS stack_info_1, NS stack_info_2 -> (* rename two stack_info_t values *)
      (* before folding, we need to update vertex_info of in_1_v and in_2_v. Their stmt will become Cfg_skip. *)
      let vertex_info_1 = map_add (errmsg_gen "vertex_info_1") cfg.vertex_info in_1_v Cfg_skip in
      let vertex_info_2 = if (in_1_v = in_2_v) then (vertex_info_1) else (map_add (errmsg_gen "vertex_info_2") vertex_info_1 in_2_v Cfg_skip) in
      let cfg_1 = {cfg with vertex_info=vertex_info_2;} in
      let fold_func : (Cfg.t * string list * Cfg.vertex * Cfg.vertex) -> string -> string -> (Cfg.t * string list * Cfg.vertex * Cfg.vertex)
      =fun (acc_cfg, acc_stack_info, acc_in_1_v, acc_in_2_v) var_1 var_2 -> begin
        if (var_1 = var_2)
        then (acc_cfg, (var_1 :: acc_stack_info), acc_in_1_v, acc_in_2_v)
        else begin
          (*  flow        : add two new vertices, fold_vtx_1 and fold_vtx_2
                            acc_in_1_v -> fold_vtx_1
                            acc_in_2_v -> fold_vtx_2
              vertex_info : fold_vtx_1 -> (Cfg_assgin (fold_newvar, E_itself var_1))
                            fold_vtx_2 -> (Cfg_assign (fold_newvar, E_itself var_2))
              type_info   : fold_newvar -> (typ of var_1)  (* We consider that the type of var_1 and var_2 are same. *)
              acc_stack_info  : fold_newvar :: acc_stack_info  (* It'll be appended as reverse-order by fold2 function. *)
          *)
          let fold_newvar         = new_var counter in
          let fold_vartyp         = map_find (errmsg_gen "fold_vartyp") acc_cfg.type_info var_1 in
          let fold_vartyp_2       = map_find (errmsg_gen "fold_vartyp_2") acc_cfg.type_info var_2 in
          (* check if two types are equal *)
          let _ : unit = if (Adt.is_typ_equal fold_vartyp fold_vartyp_2) then () else fail (errmsg_gen "is_typ_equal : not-equal") in
          (* if two types are equal, continue *)
          let fold_type_info_1    = map_add (errmsg_gen "fold_type_info_1") acc_cfg.type_info fold_newvar fold_vartyp in
          let (fold_vtx_1, fold_vtx_2) = (new_vtx counter, new_vtx counter) in
          let fold_flow_1         = begin acc_cfg.flow |> addvtx fold_vtx_1 |> addvtx fold_vtx_2 end in
          let fold_flow_2         = begin fold_flow_1 |> addedg acc_in_1_v fold_vtx_1 |> addedg acc_in_2_v fold_vtx_2 end in
          let fold_vertex_info_1  = map_add (errmsg_gen "fold_vertex_info_1") acc_cfg.vertex_info fold_vtx_1 (Cfg_assign (fold_newvar, E_itself var_1)) in
          let fold_vertex_info_2  = map_add (errmsg_gen "fold_vertex_info_2") fold_vertex_info_1 fold_vtx_2 (Cfg_assign (fold_newvar, E_itself var_2)) in
          let fold_cfg            = {acc_cfg with flow=fold_flow_2; vertex_info=fold_vertex_info_2; type_info=fold_type_info_1;} in
          (fold_cfg, (fold_newvar :: acc_stack_info), fold_vtx_1, fold_vtx_2)
        end
      end in
      let fold_result = Core.List.fold2 stack_info_1 stack_info_2 ~init:(cfg_1, [], in_1_v, in_2_v) ~f:fold_func in
      let (cfg_3, stack_info_3_rev, last_in_1_v, last_in_2_v) = 
      begin
        match fold_result with
        | Core.List.Or_unequal_lengths.Ok a -> a
        | _ -> fail (errmsg_trace ^ "merge_two_stack_infos : Core.List.Or_unequal_lengths : case unequal_lengths")
      end in
      (* connect in and out vertices *)
      let flow_4 = begin cfg_3.flow |> addedg last_in_1_v out_1_v |> addedg last_in_2_v out_2_v end in
      ({cfg_3 with flow=flow_4;}, NS (Core.List.rev stack_info_3_rev))
  end
end


(* construct control flow graph from instruction *)
(* inst_to_cfg Argument interpretation 
  counter     : simple context to easily make (new-vertex-id, new-variable-name, new-lambda-identifier)
  in_v        : start vertex-id for current scope
  out_v       : end   vertex-id for current scope
  func_in_v   : current-function-start vertex-id
  func_out_v  : current-function-end   vertex-id
  ist         : instruction which inst_to_cfg will translate
  cfg         : control-flow-graph which this function should complete to write
  stack_info  : current stack status to trace which variable located in stack
*)
let rec inst_to_cfg : cfgcon_ctr -> (Cfg.vertex * Cfg.vertex) -> (Cfg.vertex * Cfg.vertex) -> Adt.inst -> (Cfg.t * stack_info_t) -> (Cfg.t * stack_info_t)
= let open Cfg in

  (* sugar functions *)
  let add_skip_vinfo : string -> (int, Cfg.stmt) Cfg.CPMap.t -> Cfg.vertex -> (int, Cfg.stmt) Cfg.CPMap.t
  =fun errmsg_trace vinfo in_v -> begin 
    map_add ("inst_to_cfg : " ^ errmsg_trace) vinfo in_v Cfg_skip
  end in
  let template_of_push_value : string -> cfgcon_ctr -> (Cfg.vertex * Cfg.vertex) -> (Tezla.Adt.expr * Michelson.Adt.typ) -> (Cfg.t * stack_info_t) -> (Cfg.t * stack_info_t)
  =fun errmsg_trace counter (in_v, out_v) (expr, typ) (cfg, stack_info) -> begin
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_assign (new-var, <Expression>)
        type_info   : new-var -> <Type>
        stack_info  : push new-var on top of the stack
    *)
    let gen_emsg s : string = ("inst_to_cfg : " ^ errmsg_trace ^ " : template_of_push_value : " ^ s) in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter (gen_t typ) (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, expr)) cfg_vr_added in
    (cfg_ended, ns_cons v_r stack_info)
  end in
  (*let stack_hdtl : 'a list -> ('a * 'a list) = fun li -> (Core.List.hd_exn li, Core.List.tl_exn li) in*)
  let stack_hdtl : stack_info_t -> (string * stack_info_t) = begin function
    | NS li -> (Core.List.hd_exn li, NS (Core.List.tl_exn li))
    | ES _ -> fail "inst_to_cfg : stack_hdtl : ES"
  end in

  (* FUNCTION BEGIN *)
  (* COMMENT TEMPLATE *)
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_skip
        type_info   : no change
        stack_info  : no change
    *)
  fun counter (in_v, out_v) (func_in_v, func_out_v) ist (cfg, stack_info) -> begin
  match get_d ist with 
  | I_seq (i1, i2) ->
    (*  flow        : add two vertices between in-and-out
                      in_v -> mid_v_1 -> (i1) -> mid_2 -> (i2) -> out_v
        vertex_info : no change
        type_info   : no change
        stack_info  : no change
    *)
    let (cfg_2v, (mid_v_1, mid_v_2)) = t_add_vtx_2 counter (cfg, ()) in
    let (cfg_b, _) = begin
      (cfg_2v, ())
      |> t_add_vinfo ~errtrace:("inst_to_cfg : I_seq : in_v") (in_v, Cfg_skip)
      |> t_add_edg (in_v, mid_v_1)
    end in
    let (cfg_1, stack_info_1) = inst_to_cfg_handle_es counter (mid_v_1, mid_v_2) (func_in_v, func_out_v) i1 (cfg_b, stack_info) in
    let (cfg_2, stack_info_2) = inst_to_cfg_handle_es counter (mid_v_2, out_v) (func_in_v, func_out_v) i2 (cfg_1, stack_info_1) in
    (cfg_2, stack_info_2)

  | I_drop ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_drop [...]
        type_info   : no change
        stack_info  : drop one element from the top of the stack.
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let vertex_info_1 = add_skip_vinfo "I_drop : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_drop : vertex_info_2" vertex_info_1 mid_v (Cfg_drop [ns_hd stack_info]) in
    let stack_info_1 = ns_tl stack_info in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2;}, stack_info_1)

  | I_drop_n zn ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_drop [...] (* string list, which is flattened string lists. *)
        type_info   : no change
        stack_info  : drop "zn" elements from the top of the stack.
    *)
    let nn = Z.to_int zn in
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let (slist, stack_info_1) = ns_split_n stack_info nn in
    let vertex_info_1 = add_skip_vinfo "I_drop_n : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_drop_n : vertex_info_2" vertex_info_1 mid_v (Cfg_drop (ns_unlift slist)) in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2;}, stack_info_1)

  | I_dup ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_assign (new-var, E_dup var)
        type_info   : no change
        stack_info  : add new variable on top
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let topvar_name : string = ns_hd stack_info in
    let topvar_typ    = map_find "inst_to_cfg : I_dup : topvar_typ" cfg.type_info topvar_name in
    let vertex_info_1 = add_skip_vinfo "I_dup : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_dup : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, E_dup (topvar_name))) in
    let type_info_1   = map_add "inst_to_cfg : I_dup : type_info_1" cfg.type_info nv_name topvar_typ in
    let stack_info_1  = ns_cons (ns_hd stack_info) stack_info in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2; type_info=type_info_1;}, stack_info_1)

  | I_swap ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_swap
        type_info   : no change
        stack_info  : swap top two elements
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let vertex_info_1 = add_skip_vinfo "I_swap : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_swap : vertex_info_2" vertex_info_1 mid_v Cfg_swap in
    let stack_info_1 = let (h, t) = ns_split_n stack_info 2 in ns_rev_append h t in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2;}, stack_info_1)

  | I_dig zn ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_dig
        type_info   : no change
        stack_info  : dig-n-change
    *)
    let nn = Z.to_int zn in
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let vertex_info_1 = add_skip_vinfo "I_dig : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_dig : vertex_info_2" vertex_info_1 mid_v Cfg_dig in
    let stack_info_1 = let (h, t) = ns_split_n stack_info nn in (ns_append (ns_cons (ns_hd t) h) (ns_tl t)) in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2;}, stack_info_1)

  | I_dug zn ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_dug
        type_info   : no change
        stack_info  : dug-n-change
    *)
    let nn = Z.to_int zn in
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let vertex_info_1 = add_skip_vinfo "I_dug : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_dug : vertex_info_2" vertex_info_1 mid_v Cfg_dug in
    let stack_info_1 = let (th, tt) = ns_split_n (ns_tl stack_info) nn in ns_append th (ns_cons (ns_hd stack_info) tt) in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2;}, stack_info_1)

  | I_push (ty, d) ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_assign (new-var, (E_push (d, ty)))
        type_info   : new-var -> ty
        stack_info  : new data on stack top
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let vertex_info_1 = add_skip_vinfo "I_push : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_push : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, E_push (d ,ty))) in
    let type_info_1 = map_add "inst_to_cfg : I_push : type_info_1" cfg.type_info nv_name ty in
    let stack_info_1 = ns_cons nv_name stack_info in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2; type_info=type_info_1;}, stack_info_1)

  | I_some ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_assign (new-var, (E_some var))
        type_info   : new-var -> T_option (ty)
        stack_info  : exchange var to new-var
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let (v_name, si_tail) = stack_hdtl stack_info in
    let vertex_info_1 = add_skip_vinfo "I_some : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_some : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, E_some v_name)) in
    let v_type = map_find "inst_to_cfg : I_some : v_type" cfg.type_info v_name in
    let type_info_1 = map_add "inst_to_cfg : I_some : type_info_1" cfg.type_info nv_name (gen_t (Michelson.Adt.T_option v_type)) in
    let stack_info_1 = ns_cons nv_name si_tail in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2; type_info=type_info_1;}, stack_info_1)

  | I_none ty ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_assign (new-var, (E_none ty))
        type_info   : new-var -> T_option (ty)
        stack_info  : push new-var to the top of the stack
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let vertex_info_1 = add_skip_vinfo "I_none : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_none : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, (Tezla.Adt.E_none ty))) in
    let type_info_1 = map_add "inst_to_cfg : I_none : type_info_1" cfg.type_info nv_name (gen_t (Michelson.Adt.T_option ty)) in
    let stack_info_1 = ns_cons nv_name stack_info in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2; type_info=type_info_1;}, stack_info_1)

  | I_unit ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_assign (new-var, E_unit)
        type_info   : new-var -> T_unit
        stack_info  : push new-var to the top of the stack
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let vertex_info_1 = add_skip_vinfo "I_unit : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_unit : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, Tezla.Adt.E_unit)) in
    let type_info_1 = map_add "inst_to_cfg : I_unit : type_info_1" cfg.type_info nv_name (gen_t (Michelson.Adt.T_unit)) in
    let stack_info_1 = ns_cons nv_name stack_info in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2; type_info=type_info_1;}, stack_info_1)

  | I_if_none (i1, i2) ->
    (*  flow        : (in_v [If_true]-> i1_begin) & (in_v [If_false]-> i2_begin)
                      & (i1_begin -> (i1 ...) -> i1_end) & (i2_begin -> (i2 ...) -> i2_end)
                      & (i1_end -> (renaming symbols ...) -> out_v) & (i2_end -> (renaming symbols ...) -> out_v)
        vertex_info : in_v : Cfg_if_none (top-var)
                      i1_begin, i2_begin : decided by THEN, ELSE branches.
                      i1_end, i2_end : Cfg_skip
        type_info   : no change
        stack_info  : dramatically renamed at the end.
    *)
    (* set new flow (front) *)
    let nvtx () : vertex = new_vtx counter in
    let addvtx vtx flw : G.t = G.add_vertex flw vtx in
    let addedg v1 v2 flw : G.t = G.add_edge flw v1 v2 in
    let addedg_e e flw : G.t = G.add_edge_e flw e in
    let (i1_begin, i1_end, i2_begin, i2_end) = (nvtx (), nvtx (), nvtx (), nvtx ()) in
    let flow_vtx_added = begin
      cfg.flow |> addvtx i1_begin |> addvtx i1_end |> addvtx i2_begin |> addvtx i2_end
    end in
    let flow_edg_added = begin
      let true_edg = G.E.create in_v If_true i1_begin in
      let false_edg = G.E.create in_v If_false i2_begin in
      flow_vtx_added |> addedg_e true_edg |> addedg_e false_edg
    end in
    (* set vertex infos of in_v, i1_end, i2_end *)
    let topvar_name : string = ns_hd stack_info in
    let vertex_info_1 = map_add "inst_to_cfg : I_if_none : vertex_info_1" cfg.vertex_info in_v (Cfg_if_none topvar_name) in
    (*
    let vertex_info_2 = add_skip_vinfo "I_if_none : vertex_info_2" vertex_info_1 i1_end in
    let vertex_info_3 = add_skip_vinfo "I_if_none : vertex_info_3" vertex_info_2 i2_end in
    let cfg_1 = {cfg with flow=flow_edg_added; vertex_info=vertex_info_3;} in
    *)
    let cfg_1 = {cfg with flow=flow_edg_added; vertex_info=vertex_info_1;} in
    
    (* complete THEN branch (i1_begin ~ i1_end) *)
    let (cfg_2, stack_info_1) = inst_to_cfg_handle_es counter (i1_begin, i1_end) (func_in_v, func_out_v) i1 (cfg_1, stack_info) in
    (* complete ELSE branch (i2_begin ~ i2_end) *)
      (*  flow        : (i2_begin -> i2_ready) & (i2_ready -> (i2 ...) -> i2_end)
          vertex_info : i2_begin : Cfg_assgin (new-var, (E_unlift_option top-var))
                        i2_ready : decided by i2
          type_info   : new-var -> (Unwrapped T_option)
          stack_info  : top-var is replaced with new-var.
      *)
      (* For implementation, in ELSE branch Cfg generation, 
          flow keeps construction from cfg_2,
          but stack_info will be constructed independently with stack_info_1.
      *)
    let i2_ready = nvtx () in
    let flow_i2_ready = begin cfg_2.flow |> addvtx i2_ready |> addedg i2_begin i2_ready end in
    let i2_unwrap_var = new_var counter in
    let vertex_info_4 = map_add "inst_to_cfg : I_if_none : vertex_info_4" cfg_2.vertex_info i2_begin (Cfg_assign (i2_unwrap_var, Tezla.Adt.E_unlift_option topvar_name)) in
    let topvar_typ    = map_find "inst_to_cfg : I_if_none : topvar_typ" cfg_2.type_info topvar_name in
    let topvar_unwrap_typ = begin
      match topvar_typ with | {pos = _; d = Michelson.Adt.T_option tt} -> tt | _ -> fail "inst_to_cfg : I_if_none : topvar_unwrap_typ"
    end in
    let type_info_1   = map_add "inst_to_cfg : I_if_none : type_info_1" cfg_2.type_info i2_unwrap_var topvar_unwrap_typ in
    let stack_info_2  = ns_cons i2_unwrap_var (ns_tl stack_info) in
    let cfg_3 = {cfg_2 with flow=flow_i2_ready; vertex_info=vertex_info_4; type_info=type_info_1;} in
    let (cfg_4, stack_info_3) = inst_to_cfg_handle_es counter (i2_ready, i2_end) (func_in_v, func_out_v) i2 (cfg_3, stack_info_2) in
    (* Renaming variables to merge names from stack_info_1 and stack_info_3 *)
    let (cfg_5, stack_info_4) = begin
      merge_two_stack_infos 
        counter 
        "inst_to-cfg : I_if_none" 
        cfg_4 
        (stack_info_1, stack_info_3)
        (i1_end, i2_end, out_v, out_v)
    end in
    cfg_5, stack_info_4

  | I_if_some (i2, i1) ->
    (* Beware of the sequence of i2 and i1. *)
    (* Tezla_cfg.Cfg_node.stmt has Cfg_if_none only, so I named i1 as None-case-THEN-branch, and i2 as Some-case-ELSE branch. *)
    (*  flow        : (in_v [If_true]-> i1_begin) & (in_v [If_false]-> i2_begin)
                      & (i1_begin -> (i1 ...) -> i1_end) & (i2_begin -> (i2 ...) -> i2_end)
                      & (i1_end -> (renaming symbols ...) -> out_v) & (i2_end -> (renaming symbols ...) -> out_v)
        vertex_info : in_v : Cfg_if_none (top-var)
                      i1_begin, i2_begin : decided by THEN, ELSE branches.
                      i1_end, i2_end : Cfg_skip
        type_info   : no change
        stack_info  : dramatically renamed at the end.
    *)
    (* set new flow (front) *)
    let nvtx () : vertex = new_vtx counter in
    let addvtx vtx flw : G.t = G.add_vertex flw vtx in
    let addedg v1 v2 flw : G.t = G.add_edge flw v1 v2 in
    let addedg_e e flw : G.t = G.add_edge_e flw e in
    let (i1_begin, i1_end, i2_begin, i2_end) = (nvtx (), nvtx (), nvtx (), nvtx ()) in
    let flow_vtx_added = begin
      cfg.flow |> addvtx i1_begin |> addvtx i1_end |> addvtx i2_begin |> addvtx i2_end
    end in
    let flow_edg_added = begin
      let true_edg = G.E.create in_v If_true i1_begin in
      let false_edg = G.E.create in_v If_false i2_begin in
      flow_vtx_added |> addedg_e true_edg |> addedg_e false_edg
    end in
    (* set vertex infos of in_v, i1_end, i2_end *)
    let topvar_name : string = ns_hd stack_info in
    let vertex_info_1 = map_add "inst_to_cfg : I_if_some : vertex_info_1" cfg.vertex_info in_v (Cfg_if_none topvar_name) in
    (*
    let vertex_info_2 = add_skip_vinfo "I_if_some : vertex_info_2" vertex_info_1 i1_end in
    let vertex_info_3 = add_skip_vinfo "I_if_some : vertex_info_3" vertex_info_2 i2_end in
    let cfg_1 = {cfg with flow=flow_edg_added; vertex_info=vertex_info_3;} in
    *)
    let cfg_1 = {cfg with flow=flow_edg_added; vertex_info=vertex_info_1;} in
    (* complete THEN branch (i1_begin ~ i1_end) *)
    let (cfg_2, stack_info_1) = inst_to_cfg_handle_es counter (i1_begin, i1_end) (func_in_v, func_out_v) i1 (cfg_1, stack_info) in
    (* complete ELSE branch (i2_begin ~ i2_end) *)
      (*  flow        : (i2_begin -> i2_ready) & (i2_ready -> (i2 ...) -> i2_end)
          vertex_info : i2_begin : Cfg_assgin (new-var, (E_unlift_option top-var))
                        i2_ready : decided by i2
          type_info   : new-var -> (Unwrapped T_option)
          stack_info  : top-var is replaced with new-var.
      *)
      (* For implementation, in ELSE branch Cfg generation, 
          flow keeps construction from cfg_2,
          but stack_info will be constructed independently with stack_info_1.
      *)
    let i2_ready = nvtx () in
    let flow_i2_ready = begin cfg_2.flow |> addvtx i2_ready |> addedg i2_begin i2_ready end in
    let i2_unwrap_var = new_var counter in
    let vertex_info_4 = map_add "inst_to_cfg : I_if_some : vertex_info_4" cfg_2.vertex_info i2_begin (Cfg_assign (i2_unwrap_var, Tezla.Adt.E_unlift_option topvar_name)) in
    let topvar_typ    = map_find "inst_to_cfg : I_if_some : topvar_typ" cfg_2.type_info topvar_name in
    let topvar_unwrap_typ = begin
      match topvar_typ with | {pos = _; d = Michelson.Adt.T_option tt} -> tt | _ -> fail "inst_to_cfg : I_if_some : topvar_unwrap_typ"
    end in
    let type_info_1   = map_add "inst_to_cfg : I_if_some : type_info_1" cfg_2.type_info i2_unwrap_var topvar_unwrap_typ in
    let stack_info_2  = ns_cons i2_unwrap_var (ns_tl stack_info) in
    let cfg_3 = {cfg_2 with flow=flow_i2_ready; vertex_info=vertex_info_4; type_info=type_info_1;} in
    let (cfg_4, stack_info_3) = inst_to_cfg_handle_es counter (func_in_v, func_out_v) (i2_ready, i2_end) i2 (cfg_3, stack_info_2) in
    (* Renaming variables to merge names from stack_info_1 and stack_info_3 *)
    let (cfg_5, stack_info_4) = begin
      merge_two_stack_infos 
        counter 
        "inst_to-cfg : I_if_some" 
        cfg_4 
        (stack_info_1, stack_info_3)
        (i1_end, i2_end, out_v, out_v)
    end in
    cfg_5, stack_info_4

  | I_pair ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : new vertex -> Cfg_assign (new-var, (E_pair (var-1, var-2)))
        type_info   : new-var -> T_pair (t1, t2)
        stack_info  : pop two, push new-var.
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let var_1   = ns_hd stack_info in
    let var_2   = begin ns_tl stack_info |> ns_hd end in
    let typ_1   = map_find "inst_to_cfg : I_pair : typ_1" cfg.type_info var_1 in
    let typ_2   = map_find "inst_to_cfg : I_pair : typ_2" cfg.type_info var_2 in
    let vertex_info_1 = add_skip_vinfo "I_pair : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_pair : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, E_pair (var_1, var_2))) in
    let type_info_1 = map_add "inst_to_cfg : I_pair : type_info_1" cfg.type_info nv_name (gen_t (Michelson.Adt.T_pair (typ_1, typ_2))) in
    let stack_info_1 = ns_cons nv_name (ns_split_n stack_info 2 |> Stdlib.snd) in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2; type_info=type_info_1;}, stack_info_1)

  | I_car ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : new vertex -> Cfg_assign (new-var, (E_car var-1))
        type_info   : new-var -> (match t1 with | T_pair (t1, _) -> t1 | _ -> error case )
        stack_info  : replace top one into new variable.
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let var_1   = ns_hd stack_info in
    let typ_1   = map_find "inst_to_cfg : I_car : typ_1" cfg.type_info var_1 in
    let vertex_info_1 = add_skip_vinfo "I_car : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_car : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, E_car var_1)) in
    let type_info_1 = begin
      match get_d typ_1 with
      | T_pair (t1, _) -> map_add "inst_to_cfg : I_car : type_info_1" cfg.type_info nv_name t1
      | _ -> fail "inst_to_cfg : I_car : type_info_1 : match failed"
    end in
    let stack_info_1 = ns_cons nv_name (ns_tl stack_info) in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2; type_info=type_info_1}, stack_info_1)

  | I_cdr ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : new vertex -> Cfg_assign (new-var, (E_cdr var-1))
        type_info   : new-var -> (match t1 with | T_pair (_, t2) -> t2 | _ -> error case )
        stack_info  : replace top one into new variable.
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let var_1   = ns_hd stack_info in
    let typ_1   = map_find "inst_to_cfg : I_cdr : typ_1" cfg.type_info var_1 in
    let vertex_info_1 = add_skip_vinfo "I_cdr : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_cdr : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, E_cdr var_1)) in
    let type_info_1 = begin
      match get_d typ_1 with
      | T_pair (_, t2) -> map_add "inst_to_cfg : I_cdr : type_info_1" cfg.type_info nv_name t2
      | _ -> fail "inst_to_cfg : I_cdr : type_info_1 : match failed"
    end in
    let stack_info_1 = ns_cons nv_name (ns_tl stack_info) in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2; type_info=type_info_1}, stack_info_1)

  | I_left ty ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : new vertex -> Cfg_assign (new-var, (E_left (var-1, T_or(t1, ty))))
        type_info   : new-var -> T_or (t1, ty)
        stack_info  : replace top one into new variable.
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let var_1   = ns_hd stack_info in
    let typ_1   = map_find "inst_to_cfg : I_left : typ_1" cfg.type_info var_1 in
    let typ_2   = gen_t (Michelson.Adt.T_or (typ_1, ty)) in
    let vertex_info_1 = add_skip_vinfo "I_left : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_left : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, E_left (var_1, typ_2))) in
    let type_info_1 = map_add "inst_to_cfg : I_left : type_info_1" cfg.type_info nv_name typ_2 in
    let stack_info_1 = ns_cons nv_name (ns_tl stack_info) in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2; type_info=type_info_1}, stack_info_1)

  | I_right ty ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : new vertex -> Cfg_assign (new-var, (E_right (var-1, T_or(ty, t1))))
        type_info   : new-var -> T_or (ty, t1)
        stack_info  : replace top one into new variable.
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let var_1   = ns_hd stack_info in
    let typ_1   = map_find "inst_to_cfg : I_right : typ_1" cfg.type_info var_1 in
    let typ_2   = gen_t (Michelson.Adt.T_or (ty, typ_1)) in
    let vertex_info_1 = add_skip_vinfo "I_right : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_right : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, E_right (var_1, typ_2))) in
    let type_info_1 = map_add "inst_to_cfg : I_right : type_info_1" cfg.type_info nv_name typ_2 in
    let stack_info_1 = ns_cons nv_name (ns_tl stack_info) in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2; type_info=type_info_1}, stack_info_1)

  | I_if_left (i1, i2) ->
    (*  flow        : (in_v [If_true]-> i1_begin) & (in_v [If_false]-> i2_begin)
                      & (i1_begin -> (i1 ...) -> i1_end) & (i2_begin -> (i2 ...) -> i2_end)
                      & (i1_end -> (renaming symbols ...) -> out_v) & (i2_end -> (renaming symbols ...) -> out_v)
        vertex_info : in_v : Cfg_if_left (top-var)
                      i1_begin, i2_begin : decided by THEN, ELSE branches.
                      i1_end, i2_end : Cfg_skip
        type_info   : no change
        stack_info  : dramatically renamed at the end.
    *)
    (* set new flow (front) *)
    let nvtx () : vertex = new_vtx counter in
    let addvtx vtx flw : G.t = G.add_vertex flw vtx in
    let addedg v1 v2 flw : G.t = G.add_edge flw v1 v2 in
    let addedg_e e flw : G.t = G.add_edge_e flw e in
    let (i1_begin, i1_end, i2_begin, i2_end) = (nvtx (), nvtx (), nvtx (), nvtx ()) in
    let flow_vtx_added = begin
      cfg.flow |> addvtx i1_begin |> addvtx i1_end |> addvtx i2_begin |> addvtx i2_end
    end in
    let flow_edg_added = begin
      let true_edg = G.E.create in_v If_true i1_begin in
      let false_edg = G.E.create in_v If_false i2_begin in
      flow_vtx_added |> addedg_e true_edg |> addedg_e false_edg
    end in
    (* set vertex infos of in_v, i1_end, i2_end *)
    let topvar_name : string = ns_hd stack_info in
    let vertex_info_1 = map_add "inst_to_cfg : I_if_left : vertex_info_1" cfg.vertex_info in_v (Cfg_if_left topvar_name) in
    let vertex_info_2 = add_skip_vinfo "I_if_left : vertex_info_2" vertex_info_1 i1_end in
    let vertex_info_3 = add_skip_vinfo "I_if_left : vertex_info_3" vertex_info_2 i2_end in
    let cfg_1 = {cfg with flow=flow_edg_added; vertex_info=vertex_info_3;} in
    (* complete THEN branch (i1_begin ~ i1_end) *)
      (*  flow        : (i1_begin -> i1_ready) & (i1_ready -> (i1 ...) -> i1_end)
          vertex_info : i1_begin : Cfg_assign (new-var-left, (E_unlift_or top-var))
                        i1_ready : decided by i1
          type_info   : new-var-left -> (match t1 with | T_or (tt, _) -> tt | _ -> error case)
          stack_info  : top-var is replaced with new-var-left.
      *)
    let i1_ready = nvtx () in
    let flow_i1_ready = begin cfg_1.flow |> addvtx i1_ready |> addedg i1_begin i1_ready end in
    let i1_unwrap_var = new_var counter in
    let vertex_info_tb_1 = map_add "inst_to_cfg : I_if_left : vertex_info_tb_1" cfg_1.vertex_info i1_begin (Cfg_assign (i1_unwrap_var, (Tezla.Adt.E_unlift_or topvar_name))) in
    let topvar_typ = map_find "inst_to_cfg : I_if_left : topvar_typ" cfg_1.type_info topvar_name in
    let (topvar_unwrap_typ_left, topvar_unwrap_typ_right) = begin
      match topvar_typ with | {pos = _; d = Michelson.Adt.T_or (tl, tr)} -> (tl, tr) | _ -> fail "inst_to_cfg : I_if_left : topvar_unwrap_typ_leftright"
    end in
    let type_info_tb_1 = map_add "inst_to_cfg : I_if_left : type_info_tb_1" cfg_1.type_info i1_unwrap_var topvar_unwrap_typ_left in
    let stack_info_tb_1 = ns_cons i1_unwrap_var (ns_tl stack_info) in
    let cfg_tb = {cfg_1 with flow=flow_i1_ready; vertex_info=vertex_info_tb_1; type_info=type_info_tb_1;} in
    let (cfg_tb_fin, stack_info_tb_fin) = inst_to_cfg_handle_es counter (i1_ready, i1_end) (func_in_v, func_out_v) i1 (cfg_tb, stack_info_tb_1) in
    (* complete ELSE branch (i2_begin ~ i2_end) *)
      (*  flow        : (i2_begin -> i2_ready) & (i2_ready -> (i2 ...) -> i2_end)
          vertex_info : i2_begin : Cfg_assgin (new-var-right, (E_unlift_or top-var))
                        i2_ready : decided by i2
          type_info   : new-var-right -> (match t1 with | T_or (_, tt) -> tt | _ -> error case)
          stack_info  : top-var is replaced with new-var-right.
      *)
      (* For implementation, in ELSE branch Cfg generation, 
          flow keeps construction from cfg_tb_fin,
          but stack_info does not use stack_info_tb_fin.
      *)
    let i2_ready = nvtx () in
    let flow_i2_ready = begin cfg_tb_fin.flow |> addvtx i2_ready |> addedg i2_begin i2_ready end in
    let i2_unwrap_var = new_var counter in
    let vertex_info_eb_1 = map_add "inst_to_cfg : I_if_left : vertex_info_eb_1" cfg_tb_fin.vertex_info i2_begin (Cfg_assign (i2_unwrap_var, Tezla.Adt.E_unlift_or topvar_name)) in
    let type_info_eb_1   = map_add "inst_to_cfg : I_if_left : type_info_eb_1" cfg_tb_fin.type_info i2_unwrap_var topvar_unwrap_typ_right in
    let stack_info_eb_1  = ns_cons i2_unwrap_var (ns_tl stack_info) in
    let cfg_eb = {cfg_tb_fin with flow=flow_i2_ready; vertex_info=vertex_info_eb_1; type_info=type_info_eb_1;} in
    let (cfg_eb_fin, stack_info_eb_fin) = inst_to_cfg_handle_es counter (i2_ready, i2_end) (func_in_v, func_out_v) i2 (cfg_eb, stack_info_eb_1) in
    (* Renaming variables to merge names from stack_info_tb_fin and stack_info_eb_fin *)
    let (cfg_collect, stack_info_collect) = begin
      merge_two_stack_infos 
        counter 
        "inst_to-cfg : I_if_left" 
        cfg_eb_fin 
        (stack_info_tb_fin, stack_info_eb_fin)
        (i1_end, i2_end, out_v, out_v)
    end in
    (cfg_collect, stack_info_collect)

  | I_if_right (i2, i1) ->
    (* Beware of the sequence of i2 and i1. *)
    (* Tezla_cfg.Cfg_node.stmt has Cfg_if_left only, so I named i1 as Left-case-THEN-branch, and i2 as Right-case-ELSE branch. *)
    (*  flow        : (in_v [If_true]-> i1_begin) & (in_v [If_false]-> i2_begin)
                      & (i1_begin -> (i1 ...) -> i1_end) & (i2_begin -> (i2 ...) -> i2_end)
                      & (i1_end -> (renaming symbols ...) -> out_v) & (i2_end -> (renaming symbols ...) -> out_v)
        vertex_info : in_v : Cfg_if_left (top-var)
                      i1_begin, i2_begin : decided by THEN, ELSE branches.
                      i1_end, i2_end : Cfg_skip
        type_info   : no change
        stack_info  : dramatically renamed at the end.
    *)
    (* set new flow (front) *)
    let nvtx () : vertex = new_vtx counter in
    let addvtx vtx flw : G.t = G.add_vertex flw vtx in
    let addedg v1 v2 flw : G.t = G.add_edge flw v1 v2 in
    let addedg_e e flw : G.t = G.add_edge_e flw e in
    let (i1_begin, i1_end, i2_begin, i2_end) = (nvtx (), nvtx (), nvtx (), nvtx ()) in
    let flow_vtx_added = begin
      cfg.flow |> addvtx i1_begin |> addvtx i1_end |> addvtx i2_begin |> addvtx i2_end
    end in
    let flow_edg_added = begin
      let true_edg = G.E.create in_v If_true i1_begin in
      let false_edg = G.E.create in_v If_false i2_begin in
      flow_vtx_added |> addedg_e true_edg |> addedg_e false_edg
    end in
    (* set vertex infos of in_v, i1_end, i2_end *)
    let topvar_name : string = ns_hd stack_info in
    let vertex_info_1 = map_add "inst_to_cfg : I_if_right : vertex_info_1" cfg.vertex_info in_v (Cfg_if_left topvar_name) in
    (*
    let vertex_info_2 = add_skip_vinfo "I_if_right : vertex_info_2" vertex_info_1 i1_end in
    let vertex_info_3 = add_skip_vinfo "I_if_right : vertex_info_3" vertex_info_2 i2_end in
    let cfg_1 = {cfg with flow=flow_edg_added; vertex_info=vertex_info_3;} in
    *)
    let cfg_1 = {cfg with flow=flow_edg_added; vertex_info=vertex_info_1;} in
    (* complete THEN branch (i1_begin ~ i1_end) *)
      (*  flow        : (i1_begin -> i1_ready) & (i1_ready -> (i1 ...) -> i1_end)
          vertex_info : i1_begin : Cfg_assign (new-var-left, (E_unlift_or top-var))
                        i1_ready : decided by i1
          type_info   : new-var-left -> (match t1 with | T_or (tt, _) -> tt | _ -> error case)
          stack_info  : top-var is replaced with new-var-left.
      *)
    let i1_ready = nvtx () in
    let flow_i1_ready = begin cfg_1.flow |> addvtx i1_ready |> addedg i1_begin i1_ready end in
    let i1_unwrap_var = new_var counter in
    let vertex_info_tb_1 = map_add "inst_to_cfg : I_if_right : vertex_info_tb_1" cfg_1.vertex_info i1_begin (Cfg_assign (i1_unwrap_var, (Tezla.Adt.E_unlift_or topvar_name))) in
    let topvar_typ = map_find "inst_to_cfg : I_if_right : topvar_typ" cfg_1.type_info topvar_name in
    let (topvar_unwrap_typ_left, topvar_unwrap_typ_right) = begin
      match topvar_typ with | {pos = _; d = Michelson.Adt.T_or (tl, tr)} -> (tl, tr) | _ -> fail "inst_to_cfg : I_if_right : topvar_unwrap_typ_leftright"
    end in
    let type_info_tb_1 = map_add "inst_to_cfg : I_if_right : type_info_tb_1" cfg_1.type_info i1_unwrap_var topvar_unwrap_typ_left in
    let stack_info_tb_1 = ns_cons i1_unwrap_var (ns_tl stack_info) in
    let cfg_tb = {cfg_1 with flow=flow_i1_ready; vertex_info=vertex_info_tb_1; type_info=type_info_tb_1;} in
    let (cfg_tb_fin, stack_info_tb_fin) = inst_to_cfg_handle_es counter (i1_ready, i1_end) (func_in_v, func_out_v) i1 (cfg_tb, stack_info_tb_1) in
    (* complete ELSE branch (i2_begin ~ i2_end) *)
      (*  flow        : (i2_begin -> i2_ready) & (i2_ready -> (i2 ...) -> i2_end)
          vertex_info : i2_begin : Cfg_assgin (new-var-right, (E_unlift_or top-var))
                        i2_ready : decided by i2
          type_info   : new-var-right -> (match t1 with | T_or (_, tt) -> tt | _ -> error case)
          stack_info  : top-var is replaced with new-var-right.
      *)
      (* For implementation, in ELSE branch Cfg generation, 
          flow keeps construction from cfg_tb_fin,
          but stack_info does not use stack_info_tb_fin.
      *)
    let i2_ready = nvtx () in
    let flow_i2_ready = begin cfg_tb_fin.flow |> addvtx i2_ready |> addedg i2_begin i2_ready end in
    let i2_unwrap_var = new_var counter in
    let vertex_info_eb_1 = map_add "inst_to_cfg : I_if_right : vertex_info_eb_1" cfg_tb_fin.vertex_info i2_begin (Cfg_assign (i2_unwrap_var, Tezla.Adt.E_unlift_or topvar_name)) in
    let type_info_eb_1   = map_add "inst_to_cfg : I_if_right : type_info_eb_1" cfg_tb_fin.type_info i2_unwrap_var topvar_unwrap_typ_right in
    let stack_info_eb_1  = ns_cons i2_unwrap_var (ns_tl stack_info) in
    let cfg_eb = {cfg_tb_fin with flow=flow_i2_ready; vertex_info=vertex_info_eb_1; type_info=type_info_eb_1;} in
    let (cfg_eb_fin, stack_info_eb_fin) = inst_to_cfg_handle_es counter (i2_ready, i2_end) (func_in_v, func_out_v) i2 (cfg_eb, stack_info_eb_1) in
    (* Renaming variables to merge names from stack_info_tb_fin and stack_info_eb_fin *)
    let (cfg_collect, stack_info_collect) = begin
      merge_two_stack_infos 
        counter 
        "inst_to-cfg : I_if_right" 
        cfg_eb_fin 
        (stack_info_tb_fin, stack_info_eb_fin)
        (i1_end, i2_end, out_v, out_v)
    end in
    (cfg_collect, stack_info_collect)

  | I_nil ty ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : new vertex -> Cfg_assign (new-var, E_nil ty)
        type_info   : new-var -> T_list ty
        stack_info  : push new variable on the top of the stack
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let vertex_info_1 = add_skip_vinfo "I_nil : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_nil : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, E_nil ty)) in
    let type_info_1   = map_add "inst_to_cfg : I_nil : type_info_1" cfg.type_info nv_name (gen_t (Michelson.Adt.T_list ty)) in
    let stack_info_1  = ns_cons nv_name stack_info in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2; type_info=type_info_1;}, stack_info_1)
    
  | I_cons ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : new vertex -> Cfg_assign (new-var, E_cons (var-1, var-2))
        type_info   : new-var -> t2
        stack_info  : pop two and push new variable.
        other       : T_list (t1) should be equal to t2
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let var_1   = ns_hd stack_info in
    let var_2   = begin ns_tl stack_info |> ns_hd end in
    let typ_1   = map_find "inst_to_cfg : I_cons : typ_1" cfg.type_info var_1 in
    let typ_2   = map_find "inst_to_cfg : I_cons : typ_2" cfg.type_info var_2 in
    let _ : unit = if (Adt.is_typ_equal (gen_t (Michelson.Adt.T_list typ_1)) typ_2) then () else fail "inst_to_cfg : I_cons : is_typ_equal : not-equal" in
    let vertex_info_1 = add_skip_vinfo "I_cons : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_cons : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, E_cons (var_1, var_2))) in
    let type_info_1 = map_add "inst_to_cfg : I_cons : type_info_1" cfg.type_info nv_name (typ_2) in
    let stack_info_1 = ns_cons nv_name (ns_split_n stack_info 2 |> Stdlib.snd) in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2; type_info=type_info_1}, stack_info_1)

  | I_if_cons (i1, i2) ->
    (*  flow        : (in_v [If_true]-> i1_begin) & (in_v [If_false]-> i2_begin)
                      & (i1_begin -> (i1 ...) -> i1_end) & (i2_begin -> (i2 ...) -> i2_end)
                      & (i1_end -> (renaming symbols ...) -> out_v) & (i2_end -> (renaming symbols ...) -> out_v)
        vertex_info : in_v : Cfg_if_cons (top-var)
                      i1_begin, i2_begin : decided by THEN, ELSE branches.
                      i1_end, i2_end : Cfg_skip
        type_info   : no change
        stack_info  : dramatically renamed at the end.
    *)
    (* set new flow (front) *)
    let nvtx () : vertex = new_vtx counter in
    let addvtx vtx flw : G.t = G.add_vertex flw vtx in
    let addedg v1 v2 flw : G.t = G.add_edge flw v1 v2 in
    let addedg_e e flw : G.t = G.add_edge_e flw e in
    let (i1_begin, i1_end, i2_begin, i2_end) = (nvtx (), nvtx (), nvtx (), nvtx ()) in
    let flow_vtx_added = begin
      cfg.flow |> addvtx i1_begin |> addvtx i1_end |> addvtx i2_begin |> addvtx i2_end
    end in
    let flow_edg_added = begin
      let true_edg = G.E.create in_v If_true i1_begin in
      let false_edg = G.E.create in_v If_false i2_begin in
      flow_vtx_added |> addedg_e true_edg |> addedg_e false_edg
    end in
    (* set vertex infos of in_v, i1_end, i2_end *)
    let topvar_name : string = ns_hd stack_info in
    let vertex_info_1 = map_add "inst_to_cfg : I_if_cons : vertex_info_1" cfg.vertex_info in_v (Cfg_if_cons topvar_name) in
    (*
    let vertex_info_2 = add_skip_vinfo "I_if_cons : vertex_info_2" vertex_info_1 i1_end in
    let vertex_info_3 = add_skip_vinfo "I_if_cons : vertex_info_3" vertex_info_2 i2_end in
    let cfg_1 = {cfg with flow=flow_edg_added; vertex_info=vertex_info_3;} in
    *)
    let cfg_1 = {cfg with flow=flow_edg_added; vertex_info=vertex_info_1;} in
    (* complete THEN branch (i1_begin ~ i1_end) *)
      (*  flow        : (i1_begin -> i1_begin_2) & (i1_begin_2 -> i1_ready) & (i1_ready -> (i1 ...) -> i1_end)
          vertex_info : i1_begin   : Cfg_assign (new-var-hd, (E_hd top-var))
                        i1_begin_2 : Cfg_assign (new-var-tl, (E_tl top-var))
                        i1_ready : decided by i1
          type_info   : new-var-hd -> (match t1 with | T_list tt -> tt | _ -> error case)
                        new-var-tl -> t1
          stack_info  : new-var-hd :: new-var-tl :: (List.tl existing-stack-info)
      *)
    let i1_begin_2 = nvtx () in
    let i1_ready = nvtx () in
    let flow_i1_begin_2 = begin cfg_1.flow |> addvtx i1_begin_2 |> addedg i1_begin i1_begin_2 end in
    let flow_i1_ready = begin flow_i1_begin_2 |> addvtx i1_ready |> addedg i1_begin_2 i1_ready end in
    let (i1_newvar_hd, i1_newvar_tl) = (new_var counter, new_var counter) in
    let vertex_info_tb_1 = map_add "inst_to_cfg : I_if_cons : vertex_info_tb_1" cfg_1.vertex_info i1_begin (Cfg_assign (i1_newvar_hd, (Tezla.Adt.E_hd topvar_name))) in
    let vertex_info_tb_2 = map_add "inst_to_cfg : I_if_cons : vertex_info_tb_2" vertex_info_tb_1 i1_begin_2 (Cfg_assign (i1_newvar_tl, (Tezla.Adt.E_tl topvar_name))) in
    let topvar_typ = map_find "inst_to_cfg : I_if_cons : topvar_typ" cfg_1.type_info topvar_name in
    let topvar_elem_typ = begin
      match topvar_typ with | {pos = _; d = Michelson.Adt.T_list tt} -> tt | _ -> fail "inst_to_cfg : I_if_cons : topvar_elem_typ"
    end in
    let type_info_tb_1 = map_add "inst_to_cfg : I_if_cons : type_info_tb_1" cfg_1.type_info i1_newvar_hd topvar_elem_typ in
    let type_info_tb_2 = map_add "inst_to_cfg : I_if_cons : type_info_tb_2" type_info_tb_1 i1_newvar_tl topvar_typ in
    let stack_info_tb_1 = (ns_cons i1_newvar_hd (ns_cons i1_newvar_tl (ns_tl stack_info))) in
    let cfg_tb = {cfg_1 with flow=flow_i1_ready; vertex_info=vertex_info_tb_2; type_info=type_info_tb_2;} in
    let (cfg_tb_fin, stack_info_tb_fin) = inst_to_cfg_handle_es counter (i1_ready, i1_end) (func_in_v, func_out_v) i1 (cfg_tb, stack_info_tb_1) in
    (* complete ELSE branch (i2_begin ~ i2_end) *)
      (*  flow        : (i2_begin -> (i2 ...) -> i2_end)
          vertex_info : i2_begin : Cfg_skip
          type_info   : no change
          stack_info  : pop the top element of the stack.
      *)
      (* For implementation, in ELSE branch Cfg generation, 
          flow keeps construction from cfg_tb_fin,
          but stack_info does not use stack_info_tb_fin.
      *)
    let vertex_info_eb_1 = map_add "inst_to_cfg : I_if_cons : vertex_info_eb_1" cfg_tb_fin.vertex_info i2_begin (Cfg_skip) in
    let stack_info_eb_1  = ns_tl stack_info in
    let cfg_eb = {cfg_tb with vertex_info=vertex_info_eb_1;} in
    let (cfg_eb_fin, stack_info_eb_fin) = inst_to_cfg_handle_es counter (i2_begin, i2_end) (func_in_v, func_out_v) i2 (cfg_eb, stack_info_eb_1) in
    (* Renaming variables to merge names from stack_info_tb_fin and stack_info_eb_fin *)
    let (cfg_collect, stack_info_collect) = begin
      merge_two_stack_infos 
        counter 
        "inst_to-cfg : I_if_cons" 
        cfg_eb_fin 
        (stack_info_tb_fin, stack_info_eb_fin)
        (i1_end, i2_end, out_v, out_v)
    end in
    (cfg_collect, stack_info_collect)

  | I_size ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : new vertex -> Cfg_assign (new-var, E_size top-var)
        type_info   : new-var -> T_nat
        stack_info  : replace top one into new variable.
        other       : top-var should have one of the following types:
                      - T_set _
                      - T_map (_, _)
                      - T_list _
                      - T_string
                      - T_bytes
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let var_1   = ns_hd stack_info in
    let typ_1   = map_find "inst_to_cfg : I_size : typ_1" cfg.type_info var_1 in
    let _ : unit = begin
      let open Michelson.Adt in
      match get_d typ_1 with
      | T_set _ | T_map _ | T_list _ | T_string | T_bytes -> ()
      | _ -> fail "inst_to_cfg : I_size : type constraint : unsatisfy"
    end in
    let vertex_info_1 = add_skip_vinfo "I_size : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_size : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, E_size var_1)) in
    let type_info_1 = map_add "inst_to_cfg : I_size : type_info_1" cfg.type_info nv_name (gen_t Michelson.Adt.T_nat) in
    let stack_info_1 = ns_cons nv_name (ns_tl stack_info) in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2; type_info=type_info_1;}, stack_info_1)

  | I_empty_set ty -> template_of_push_value "I_empty_set" counter (in_v, out_v) (E_empty_set ty, Michelson.Adt.T_set ty) (cfg, stack_info)
    
  | I_empty_map (t1, t2) -> template_of_push_value "I_empty_map" counter (in_v, out_v) (E_empty_map (t1, t2), Michelson.Adt.T_map (t1, t2)) (cfg, stack_info)

  | I_empty_big_map (t1, t2) -> template_of_push_value "I_empty_big_map" counter (in_v, out_v) (E_empty_big_map (t1, t2), Michelson.Adt.T_big_map (t1, t2)) (cfg, stack_info)

  | I_map i ->
    (*  flow        : (in_v -> result_init -> map_v) & (map_v [If_false]-> out_v)
                      & (map_v [If_true]-> map_setup_1 -> map_setup_2 -> map_body_begin)
                      & (map_body_begin -> (i ...) -> map_body_end)
                          <CASE-LIST> & (map_body_end -> map_update_result -> map_sync_begin)
                          <CASE-MAP>  & (map_body_end -> map_get_key -> map_update_somev -> map_update_result -> map_sync_begin)
                      & (map_sync_begin -> (assigns ...) -> map_v)
        variables   : var-1     : list or map which placed on top of the stack at the beginning of this process.
                      newvar-r  : list or map which will contain the result of the map instruction.
                      newvar    : head of list or head of map. In this context, the head of map meaning the <key, value> pair.
                      newvar-k  : <CASE-MAP> equal to "car newvar"
                      apply-r   : result value made from each mapping-loop
                      apply-sr  : E_some (apply-r)
        vertex_info : in_v                -> Cfg_skip
                      result_init         -> "Cfg_assign (newvar-r, E_nil t1)" or "Cfg_assign (newvar-r, E_empty_map (t1_k, t1_v)"
                      map_v               -> Cfg_map var-1
                      map_setup_1         -> Cfg_assign (newvar, (E_hd var-1))
                      map_setup_2         -> Cfg_assign (var-1, (E_tl var-1))
                      map_body_begin      -> decided by i
                      map_body_end        -> Cfg_skip
                      map_get_key         -> <CASE-MAP>  Cfg_assign (newvar-k, E_car newvar)
                      map_update_somev    -> <CASE-MAP>  Cfg_assign (apply-sr, E_some apply-r)
                      map_update_result   -> <CASE-LIST> Cfg_assign (newvar-r, (E_cons (apply-r, newvar-r)))
                      map_update_result   -> <CASE-MAP>  Cfg_assign (newvar-r, (E_update (newvar-k, apply-sr, newvar-r)))
                      map_sync_begin      -> Cfg_skip
        type_info (list) :
                      var-1     -> list t1
                      newvar    -> t1
                      newvar-r  -> list t_i (In this context, t_i is the result type of the instruction i produces)
                      apply-r   -> t_i
        type_info (map)  :
                      var-1     -> map (t1_k, t1_v)
                      newvar    -> pair (t1_k, t1_v)
                      newvar-r  -> map (t1_k, t_i)
                      newvar-k  -> t1_k
                      apply-r   -> t_i
                      apply-sr  -> option t_i
        stack_info  : top element (list or map) of the stack will be changed into mapped element (list or map)
    *)
    (* sugar functions *)
    let nvtx () : vertex = new_vtx counter in
    let addvtx vtx flw : G.t = G.add_vertex flw vtx in
    let addedg v1 v2 flw : G.t = G.add_edge flw v1 v2 in
    let addedg_e e flw : G.t = G.add_edge_e flw e in
    (* gather infos *)
    let topvar_name = ns_hd stack_info in
    let (newvar_name, newvar_result_name) = (new_var counter, new_var counter) in
    let container_typ = map_find "inst_to_cfg : I_map : container_typ" cfg.type_info topvar_name in
    let elem_typ = begin
      match get_d container_typ with
      | T_list ty -> ty
      | T_map (tk, tv) -> gen_t (Michelson.Adt.T_pair (tk, tv))
      | _ -> fail "inst_to_cfg : I_map : elem_typ : type_error"
    end in
    (* construct cfg - up to map_body_end *)
    (* exception: the type-info of the "newvar_result_name" and the vertex-info of "result_init" will be updated after "i" converted into Cfg. *)
    let (map_v, result_init, map_setup_1, map_setup_2, map_body_begin, map_body_end) = (nvtx (), nvtx (), nvtx (), nvtx (), nvtx (), nvtx ()) in
    let flow_vtx_added_to_map_body_end = begin
      cfg.flow |> addvtx map_v |> addvtx result_init |> addvtx map_setup_1 |> addvtx map_setup_2 |> addvtx map_body_begin |> addvtx map_body_end
    end in
    let flow_edg_added_to_map_body_begin = begin
      let t_edg = G.E.create map_v If_true map_setup_1 in
      let f_edg = G.E.create map_v If_false out_v in
      flow_vtx_added_to_map_body_end |> addedg in_v result_init |> addedg result_init map_v
      |> addedg_e f_edg |> addedg_e t_edg 
      |> addedg map_setup_1 map_setup_2 |> addedg map_setup_2 map_body_begin
    end in
    let vertex_info_1 = add_skip_vinfo "I_map : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_map : vertex_info_2" vertex_info_1 map_v (Cfg_map topvar_name) in
    let vertex_info_3 = map_add "inst_to_cfg : I_map : vertex_info_3" vertex_info_2 map_setup_1 (Cfg_assign (newvar_name, E_hd topvar_name)) in
    let vertex_info_4 = map_add "inst_to_cfg : I_map : vertex_info_4" vertex_info_3 map_setup_2 (Cfg_assign (topvar_name, E_tl topvar_name)) in
    (*let vertex_info_5 = add_skip_vinfo "I_map : vertex_info_5" vertex_info_4 map_body_end in*)
    let type_info_1   = map_add "inst_to_cfg : I_map : type_info_1"   cfg.type_info newvar_name elem_typ in
    let cfg_to_map_body_begin = {cfg with flow=flow_edg_added_to_map_body_begin; vertex_info=vertex_info_4; type_info=type_info_1;} in
    let stack_info_to_map_body_begin = ns_cons newvar_name (ns_tl stack_info) in
    let (cfg_to_map_body_end, stack_info_to_map_body_end) = inst_to_cfg_handle_es counter (map_body_begin, map_body_end) (func_in_v, func_out_v) i (cfg_to_map_body_begin, stack_info_to_map_body_begin) in
    if (is_es stack_info_to_map_body_end)
    then (
      (* no variable-sync needed. connect Failed-edge and use unchanged stack_info. *)
      let (cfg_final, _) =
        (cfg_to_map_body_end, ())
        |> t_add_fail_edg (map_body_end, in_v)
        |> t_add_vinfo ~errtrace:("inst_to_cfg : I_map : is_es true : cfg_final") (map_body_end, Cfg_failwith (get_es_str stack_info_to_map_body_end))
      in
      (cfg_final, stack_info)
    )
    else (
      (* construct cfg - after map_body_end *)
      (* the variable "newvar_result_name" and the vertex "result_init" will be dealed here. *)
      let (cfg_to_map_body_end_vinfo_added, _) = t_add_vinfo ~errtrace:("inst_to_cfg : I_map : cfg_to_map_body_end_vinfo_added") (map_body_end, Cfg_skip) (cfg_to_map_body_end, ()) in
      let cfg_mbe = cfg_to_map_body_end_vinfo_added in
      let stack_info_mbe = stack_info_to_map_body_end in
      let apply_r = ns_hd stack_info_mbe in
      let result_typ = map_find "inst_to_cfg : I_map : result_typ" cfg_mbe.type_info apply_r in
      let (result_container_typ, result_container_init_expr) = begin
        match get_d container_typ with
        | Michelson.Adt.T_list _ -> (Michelson.Adt.T_list result_typ), (Tezla.Adt.E_nil result_typ)
        | Michelson.Adt.T_map (kt, _) -> (Michelson.Adt.T_map (kt, result_typ)), (E_empty_map (kt, result_typ))
        | _ -> fail "inst_to_cfg : I_map : result_container_typ : match-fail"
      end in
      let type_info_mbe_1   = map_add "inst_to_cfg : I_map : type_info_mbe_1"   cfg_mbe.type_info   newvar_result_name (gen_t result_container_typ) in
      let vertex_info_mbe_1 = map_add "inst_to_cfg : I_map : vertex_info_mbe_1" cfg_mbe.vertex_info result_init (Cfg_assign (newvar_result_name, result_container_init_expr)) in
      let (cfg_to_map_update_result, stack_info_to_map_update_result, map_sync_begin) = begin
        match get_d container_typ with
        | Michelson.Adt.T_list _ ->
          let gen_errmsg s : string = "inst_to_cfg : I_map : T_list : " ^ s in
          let (map_update_result, map_sync_begin) = (nvtx (), nvtx ()) in
          let flow_mbe_vtx_added = begin
            cfg_mbe.flow |> addvtx map_update_result |> addvtx map_sync_begin
          end in
          let flow_mbe_edg_added = begin
            flow_mbe_vtx_added |> addedg map_body_end map_update_result |> addedg map_update_result map_sync_begin
          end in
          let vertex_info_mbe_2 = map_add (gen_errmsg "vertex_info_mbe_2") vertex_info_mbe_1 map_update_result (Cfg_assign (newvar_result_name, (E_cons (apply_r, newvar_result_name)))) in
          let vertex_info_mbe_3 = add_skip_vinfo (gen_errmsg "vertex_info_mbe_3") vertex_info_mbe_2 map_sync_begin in
          ({cfg_mbe with flow=flow_mbe_edg_added; vertex_info=vertex_info_mbe_3; type_info=type_info_mbe_1;}, (ns_unlift (ns_tl stack_info_mbe)), map_sync_begin)
        | Michelson.Adt.T_map (kt, _) ->
          let gen_errmsg s : string = "inst_to_cfg : I_map : T_map : " ^ s in
          let (map_get_key, map_update_somev, map_update_result, map_sync_begin) = (nvtx (), nvtx (), nvtx (), nvtx ()) in
          let flow_mbe_vtx_added = begin
            cfg_mbe.flow |> addvtx map_get_key |> addvtx map_update_somev |> addvtx map_update_result |> addvtx map_sync_begin
          end in
          let flow_mbe_edg_added = begin
            flow_mbe_vtx_added |> addedg map_body_end map_get_key |> addedg map_get_key map_update_somev |> addedg map_update_somev map_update_result |> addedg map_update_result map_sync_begin
          end in
          let (keyvar, apply_sr) = (new_var counter, new_var counter) in
          let vertex_info_mbe_2 = map_add (gen_errmsg "vertex_info_mbe_2") vertex_info_mbe_1 map_get_key (Cfg_assign (keyvar, E_car newvar_name)) in
          let vertex_info_mbe_3 = map_add (gen_errmsg "vertex_info_mbe_3") vertex_info_mbe_2 map_update_somev (Cfg_assign (apply_sr, E_some apply_r)) in
          let vertex_info_mbe_4 = map_add (gen_errmsg "vertex_info_mbe_4") vertex_info_mbe_3 map_update_result (Cfg_assign (newvar_result_name, (E_update (keyvar, apply_sr, newvar_result_name)))) in
          let vertex_info_mbe_5 = add_skip_vinfo (gen_errmsg "vertex_info_mbe_5") vertex_info_mbe_4 map_sync_begin in
          let type_info_mbe_2   = map_add (gen_errmsg "type_info_mbe_2")   type_info_mbe_1   apply_sr (gen_t (Michelson.Adt.T_option result_typ)) in
          let type_info_mbe_3   = map_add (gen_errmsg "type_info_mbe_3")   type_info_mbe_2   keyvar   kt                    in
          ({cfg_mbe with flow=flow_mbe_edg_added; vertex_info=vertex_info_mbe_5; type_info=type_info_mbe_3;}, (ns_unlift (ns_tl stack_info_mbe)), map_sync_begin)
        | _ -> fail "inst_to_cfg : I_map : cfg_to_map_update_result : match-fail"
      end in
      (* Synchronize stack variables *)
      (* Add vertices linearly for every name-notequal stack elements, set vertex_info like (Cfg_assign (before-stack-var-name, (E_itself after-stack-var-name))) *)
      let fold2_func : (Cfg.t * Cfg.vertex) -> string -> string -> (Cfg.t * Cfg.vertex)
      = fun (acc_cfg, acc_in_v) bstack_var astack_var -> begin
        if (bstack_var = astack_var) then (acc_cfg, acc_in_v)
        else begin
          let nv = nvtx () in
          let flow_update = begin acc_cfg.flow |> addvtx nv |> addedg acc_in_v nv end in
          let vi_update = map_add ("inst_to_cfg : I_map : fold2_func : vi_update") acc_cfg.vertex_info nv (Cfg_assign (bstack_var, (E_itself astack_var))) in
          ({acc_cfg with flow=flow_update; vertex_info=vi_update;}, nv)
        end
      end in
      let cfg_final = begin 
        match Core.List.fold2 (ns_unlift (ns_tl stack_info)) (stack_info_to_map_update_result) ~init:(cfg_to_map_update_result, map_sync_begin) ~f:fold2_func with
        | Core.List.Or_unequal_lengths.Ok (cfg_r, last_in_v) -> begin
            let flow_update = cfg_r.flow |> addedg last_in_v map_v in
            {cfg_r with flow=flow_update;}
          end
        | Core.List.Or_unequal_lengths.Unequal_lengths -> fail "inst_to_cfg : I_map : cfg_final : unequal_lengths"
      end in
      (cfg_final, (ns_cons newvar_result_name stack_info))
    )
    
  | I_iter i ->
    (*  flow        : (in_v -> iter_v) & (iter_v [If_false]-> out_v)
                      & (iter_v [If_true]-> iter_setup_1) & (iter_setup_1 -> iter_setup_2 -> iter_body_begin)
                      & (iter_body_begin -> (i ...) -> iter_body_end)
                      & (iter_body_end -> (assigns ...) -> iter_v)
        variables   : var-1     : list/set/map which placed on the top of the stack at the beginning of this process.
                      elem      : indicates the element of list/set/map
        vertex_info : in_v            -> Cfg_skip
                      iter_v          -> Cfg_init var-1
                      iter_setup_1    -> Cfg_assign (elem,  E_hd var-1)
                      iter_setup_2    -> Cfg_assign (var-1, E_tl var-1)
                      iter_body_begin -> decided by i
                      iter_body_end   -> Cfg_skip
        type_info   : var-1     -> list(t1) || set(t1)  || map(t1_k, t1_v)
                      elem      -> t1       || t1       || pair(t1_k, t1_v)
        stack_info  : top element will be removed
    *)
    (* Unlike MAP instruction, ITER instruction has a single flow scheme for List, Set, and Map argument types. *)
    (* sugar functions *)
    let nvtx () : vertex = new_vtx counter in
    let addvtx vtx flw : G.t = G.add_vertex flw vtx in
    let addedg v1 v2 flw : G.t = G.add_edge flw v1 v2 in
    let addedg_e e flw : G.t = G.add_edge_e flw e in
    let gen_errmsg s : string = ("inst_to_cfg : I_iter : " ^ s) in
    (* construct cfg - except "i" and "assigns" *)
    let (iter_v, iter_setup_1, iter_setup_2, iter_body_begin, iter_body_end) = (nvtx (), nvtx (), nvtx (), nvtx (), nvtx ()) in
    let var_1 = ns_hd stack_info in
    let elem  = new_var counter in
    let tl_stack_info = ns_tl stack_info in
    let (_, elemtyp) = begin
      let open Michelson.Adt in
      let t = map_find (gen_errmsg "vartyp-elemtyp : t") cfg.type_info var_1 in
      match get_d t with
      | T_list t1       ->  (t, t1)
      | T_set t1        ->  (t, t1)
      | T_map (tk, tv)  ->  (t, gen_t (T_pair (tk, tv)))
      | _ -> fail (gen_errmsg "vartyp-elemtyp : match-failed")
    end in
    let flow_vtx_added_outline = begin
      cfg.flow |> addvtx iter_v |> addvtx iter_setup_1 |> addvtx iter_setup_2 |> addvtx iter_body_begin |> addvtx iter_body_end
    end in
    let flow_edg_added_outline = begin
      let f_edg = G.E.create iter_v If_false out_v in
      let t_edg = G.E.create iter_v If_true iter_setup_1 in
      flow_vtx_added_outline |> addedg in_v iter_v |> addedg_e f_edg |> addedg_e t_edg 
      |> addedg iter_setup_1 iter_setup_2 |> addedg iter_setup_2 iter_body_begin
    end in
    let vertex_info_ol_1 = add_skip_vinfo (gen_errmsg "vertex_info_ol_1") cfg.vertex_info in_v in
    let vertex_info_ol_2 = map_add (gen_errmsg "vertex_info_ol_2") vertex_info_ol_1 iter_v (Cfg_iter var_1) in
    let vertex_info_ol_3 = map_add (gen_errmsg "vertex_info_ol_3") vertex_info_ol_2 iter_setup_1 (Cfg_assign (elem,  E_hd var_1)) in
    let vertex_info_ol_4 = map_add (gen_errmsg "vertex_info_ol_4") vertex_info_ol_3 iter_setup_2 (Cfg_assign (var_1, E_tl var_1)) in
    (*let vertex_info_ol_5 = add_skip_vinfo (gen_errmsg "vertex_info_ol_5") vertex_info_ol_4 iter_body_end in*)
    let type_info_ol_1   = map_add (gen_errmsg "type_info_ol_1") cfg.type_info elem elemtyp in
    let stack_info_ol_1  = ns_cons elem tl_stack_info in
    let cfg_outline      = {cfg with flow=flow_edg_added_outline; vertex_info=vertex_info_ol_4; type_info=type_info_ol_1;} in
    (* construct cfg - add about "i" *)
    let (cfg_ol_end, stack_info_ol_end) = inst_to_cfg_handle_es counter (iter_body_begin, iter_body_end) (func_in_v, func_out_v) i (cfg_outline, stack_info_ol_1) in
    if (is_es stack_info_ol_end)
    then (
      (* no variable-sync needed. connect Failed-edge and use tl_stack_info instead. *)
      let (cfg_final, _) =
        (cfg_ol_end, ())
        |> t_add_fail_edg (iter_body_end, in_v)
        |> t_add_vinfo ~errtrace:(gen_errmsg "is_es true : cfg_final") (iter_body_end, Cfg_failwith (get_es_str stack_info_ol_end))
      in
      (cfg_final, tl_stack_info)
    )
    else (
      let (cfg_ol_end_iter_body_end_vinfo_added, _) = t_add_vinfo ~errtrace:(gen_errmsg "cfg_ol_end_iter_body_end_vinfo_added") (iter_body_end, Cfg_skip) (cfg_ol_end, ()) in
      (* construct cfg - add about "assigns" (* Synchronize stack variables *) *)
      (* Add vertices linearly for every name-notequal stack elements, set vertex_info like (Cfg_assign (before-stack-var-name, (E_itself after-stack-var-name))) *)
      let fold2_func : (Cfg.t * Cfg.vertex) -> string -> string -> (Cfg.t * Cfg.vertex)
      = fun (acc_cfg, acc_in_v) bstack_var astack_var -> begin
        if (bstack_var = astack_var) then (acc_cfg, acc_in_v)
        else begin
          let nv = nvtx () in
          let flow_update = begin acc_cfg.flow |> addvtx nv |> addedg acc_in_v nv end in
          let vi_update = map_add ("inst_to_cfg : I_iter : fold2_func : vi_update") acc_cfg.vertex_info nv (Cfg_assign (bstack_var, (E_itself astack_var))) in
          ({acc_cfg with flow=flow_update; vertex_info=vi_update;}, nv)
        end
      end in
      let cfg_final = begin 
        match Core.List.fold2 (ns_unlift tl_stack_info) (ns_unlift stack_info_ol_end) ~init:(cfg_ol_end_iter_body_end_vinfo_added, iter_body_end) ~f:fold2_func with
        | Core.List.Or_unequal_lengths.Ok (cfg_r, last_in_v) -> begin
            let flow_update = cfg_r.flow |> addedg last_in_v iter_v in
            {cfg_r with flow=flow_update;}
          end
        | Core.List.Or_unequal_lengths.Unequal_lengths -> fail "inst_to_cfg : I_iter : cfg_final : unequal_lengths"
      end in
      (cfg_final, tl_stack_info)
    )
    
  | I_mem ->
    (*  flow        : add new vertex between in-and-out
        variables   : var-1   : top element of the stack
                      var-2   : 2nd top element of the stack
                      result  : mem operation result value
        vertex_info : in_v        -> Cfg_skip
                      new vertex  -> Cfg_assign (result, E_mem (var-1, var-2))
        type_info   : var-1   : t1
                      var-2   : set(t1) / map (t1, tv) / big_map (t1, tv)
                      result  : bool
        stack_info  : pop two elements and push "result"
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let result_var = new_var counter in 
    let (var_1, var_2, tltl_stack_info) = begin 
      let (hdl, tltl) = ns_split_n stack_info 2 in (ns_hd hdl, ns_nth hdl 1, tltl)
    end in
    (* type constraint check *)
    let _ : unit = begin
      let open Michelson.Adt in
      let errmsg_gen s : string = "inst_to_cfg : I_mem : type constraint check : " ^ s in
      let t1 = map_find (errmsg_gen "t1") cfg.type_info var_1 in
      let t2 = map_find (errmsg_gen "t2") cfg.type_info var_2 in
      let t = 
        match get_d t2 with
        | T_set (t) -> t 
        | T_map (tk, _) -> tk
        | T_big_map (tk, _) -> tk
        | _ -> fail (errmsg_gen "match failed")
      in
      if Adt.is_typ_equal t1 t then () else fail (errmsg_gen "constraint check failed")
    end in
    (* construct cfg *)
    let emsg_gen s : string = "inst_to_cfg : I_mem : " ^ s in
    let vinfo_0 = map_add (emsg_gen "vinfo_0") cfg.vertex_info  in_v  Cfg_skip in
    let vinfo_1 = map_add (emsg_gen "vinfo_1") vinfo_0          mid_v (Cfg_assign (result_var, E_mem (var_1, var_2))) in
    let tinfo_1 = map_add (emsg_gen "tinfo_1") cfg.type_info result_var (gen_t Michelson.Adt.T_bool) in
    ({cfg with flow=flow_2; vertex_info=vinfo_1; type_info=tinfo_1;}, (ns_cons result_var tltl_stack_info))

  | I_get ->
    (*  flow        : add new vertex between in-and-out
        variables   : var-1   : top element of the stack
                      var-2   : 2nd top element of the stack
                      result  : get operation result value
        vertex_info : in_v        -> Cfg_skip
                      new vertex  -> Cfg_assign (result, E_get (var-1, var-2))
        type_info   : var-1   : tk
                      var-2   : map (tk, tv) / big_map (tk, tv)
                      result  : option (tv)
        stack_info  : pop two elements and push "result"
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let result_var = new_var counter in
    let (var_1, var_2, tltl_stack_info) = begin
      let (hdl, tltl) = ns_split_n stack_info 2 in (ns_hd hdl, ns_nth hdl 1, tltl) 
    end in
    (* type constraint check *)
    let tc_errmsg_gen s : string = "inst_to_cfg : I_get : type constraint check : " ^ s in
    let t1 = map_find (tc_errmsg_gen "t1") cfg.type_info var_1 in
    let t2 = map_find (tc_errmsg_gen "t2") cfg.type_info var_2 in
    let (tk, tv) = begin
      match get_d t2 with
      | T_map (tk, tv) -> (tk, tv)
      | T_big_map (tk, tv) -> (tk, tv)
      | _ -> fail (tc_errmsg_gen "match failed")
    end in
    let _ : unit = if Adt.is_typ_equal t1 tk then () else fail (tc_errmsg_gen "constraint check failed") in
    (* construct cfg *)
    let emsg_gen s : string = "inst_to_cfg : I_get : " ^ s in
    let vinfo_0 = map_add (emsg_gen "vinfo_0") cfg.vertex_info in_v Cfg_skip in
    let vinfo_1 = map_add (emsg_gen "vinfo_1") vinfo_0 mid_v (Cfg_assign (result_var, E_get (var_1, var_2))) in
    let tinfo_1 = map_add (emsg_gen "tinfo_1") cfg.type_info result_var (gen_t (Michelson.Adt.T_option tv)) in
    ({cfg with flow=flow_2; vertex_info=vinfo_1; type_info=tinfo_1;}, (ns_cons result_var tltl_stack_info))
    
  | I_update ->
    (*  flow        : add new vertex between in-and-out
        variables   : var-1   : top element of the stack
                      var-2   : 2nd top element of the stack
                      var-3   : 3rd top element of the stack
                      result  : get operation result value
        vertex_info : in_v        -> Cfg_skip
                      new vertex  -> Cfg_assign (result, E_update (var-1, var-2, var-3))
        type_info   : var-1   : tk
                      var-2   : bool / option (tv) / option (tv)
                      var-2   : set (tk) / map (tk, tv) / big_map (tk, tv)
                      result  : set (tk) / map (tk, tv) / big_map (tk, tv)
        stack_info  : pop three elements and push "result"
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let result_var = new_var counter in
    let (var_1, var_2, var_3, tl3_stack_info) = begin
      let (hdl, tl3) = ns_split_n stack_info 3 in (ns_hd hdl, ns_nth hdl 1, ns_nth hdl 2, tl3)
    end in
    (* type constraint check & make rty *)
    let rty = begin
      let open Michelson.Adt in
      let tc_emsg_gen s : string = "inst_to_cfg : I_update : type constraint check : " ^ s in
      let mfti s v = map_find (tc_emsg_gen s) cfg.type_info v in
      let t1 = mfti "t1" var_1 in
      match (get_d (mfti "t2" var_2), get_d (mfti "t3" var_3)) with
      | T_bool,         T_set (tk)          when (Adt.is_typ_equal t1 tk)                             -> T_set (tk)
      | T_option (tv1), T_map (tk, tv2)     when (Adt.is_typ_equal t1 tk && Adt.is_typ_equal tv1 tv2) -> T_map (tk, tv2)
      | T_option (tv1), T_big_map (tk, tv2) when (Adt.is_typ_equal t1 tk && Adt.is_typ_equal tv1 tv2) -> T_big_map (tk, tv2)
      | _ -> fail (tc_emsg_gen "match failed")
    end in
    (* construct cfg *)
    let emsg_gen s : string = "inst_to_cfg : I_update : " ^ s in
    let vinfo_0 = map_add (emsg_gen "vinfo_0") cfg.vertex_info in_v Cfg_skip in
    let vinfo_1 = map_add (emsg_gen "vinfo_1") vinfo_0 mid_v (Cfg_assign (result_var, E_update (var_1, var_2, var_3))) in
    let tinfo_1 = map_add (emsg_gen "tinfo_1") cfg.type_info result_var (gen_t rty) in
    ({cfg with flow=flow_2; vertex_info=vinfo_1; type_info=tinfo_1;}, (ns_cons result_var tl3_stack_info))

  | I_if (i1, i2) ->
    (*  flow        : (in_v [If_true]-> i1_begin) & (in_v [If_false]-> i2_begin)
                      & (i1_begin -> (i1 ...) -> i1_end) & (i2_begin -> (i2 ...) -> i2_end)
                      & (i1_end -> (renaming symbols ...) -> out_v) & (i2_end -> (renaming symbols ...) -> out_v)
        variables   : var-1 : condition boolean variable located at the top of the stack
        vertex_info : in_v : Cfg_if (var-1)
                      i1_begin, i2_begin : decided by THEN, ELSE branches.
                      i1_end, i2_end : Cfg_skip
        type_info   : no change
        stack_info  : dramatically renamed at the end.
    *)
    (* sugar functions *)
    let nvtx () : vertex = new_vtx counter in
    let addvtx vtx flw : G.t = G.add_vertex flw vtx in
    (*let addedg v1 v2 flw : G.t = G.add_edge flw v1 v2 in*)
    let addedg_e e flw : G.t = G.add_edge_e flw e in
    (* set new flow (front) *)
    let (i1_begin, i1_end, i2_begin, i2_end) = (nvtx (), nvtx (), nvtx (), nvtx ()) in
    let flow_vtx_added = begin
      cfg.flow |> addvtx i1_begin |> addvtx i1_end |> addvtx i2_begin |> addvtx i2_end
    end in
    let flow_edg_added = begin
      let true_edg = G.E.create in_v If_true i1_begin in
      let false_edg = G.E.create in_v If_false i2_begin in
      flow_vtx_added |> addedg_e true_edg |> addedg_e false_edg
    end in
    (* set vertex infos of in_v, i1_end, i2_end *)
    let var_1 : string = ns_hd stack_info in
    let vertex_info_1 = map_add "inst_to_cfg : I_if : vertex_info_1" cfg.vertex_info in_v (Cfg_if var_1) in
    (*  (* I don't remember why I add such vertex-infos for -end vertices *)
    let vertex_info_2 = add_skip_vinfo "I_if : vertex_info_2" vertex_info_1 i1_end in
    let vertex_info_3 = add_skip_vinfo "I_if : vertex_info_3" vertex_info_2 i2_end in
    let cfg_tb = {cfg with flow=flow_edg_added; vertex_info=vertex_info_3;} in
    *)
    let cfg_tb = {cfg with flow=flow_edg_added; vertex_info=vertex_info_1} in
    let tl_stack_info = ns_tl stack_info in
    let (cfg_tb_fin, stack_info_tb_fin) = inst_to_cfg_handle_es counter (i1_begin, i1_end) (func_in_v, func_out_v) i1 (cfg_tb, tl_stack_info) in
    let (cfg_eb_fin, stack_info_eb_fin) = inst_to_cfg_handle_es counter (i2_begin, i2_end) (func_in_v, func_out_v) i2 (cfg_tb_fin, tl_stack_info) in
    (* merge two stack-infos *)
    let (cfg_collect, stack_info_collect) = begin
      merge_two_stack_infos
        counter
        "inst_to_cfg : I_if"
        cfg_eb_fin
        (stack_info_tb_fin, stack_info_eb_fin)
        (i1_end, i2_end, out_v, out_v)
    end in
    (cfg_collect, stack_info_collect)

  | I_loop i ->
    (*  flow        : (in_v [If_true]-> body_begin) & (in_v [If_false] -> out_v)
                      & (body_begin -> (i ...) -> body_end) & (body_end -> (assigns ...) -> in_v)
        variables   : var-1 : condition boolean variable located at the top of the stack
        vertex_info : in_v        ->  Cfg_loop (var-1)
                      body_begin  ->  decided by "i"
                      body_end    ->  Cfg_skip
        type_info   : no change
        stack_info  : top element will be removed.
        others      : be aware of stack_info scheme when enter "body_begin" and get out of "out_v"
    *)
    (* sugar functions *)
    let nvtx () : vertex = new_vtx counter in
    let addvtx vtx flw : G.t = G.add_vertex flw vtx in
    let addedg v1 v2 flw : G.t = G.add_edge flw v1 v2 in
    let addedg_e e flw : G.t = G.add_edge_e flw e in
    (* construct cfg *)
    let (body_begin, body_end) = (nvtx (), nvtx ()) in
    let var_1 = ns_hd stack_info in
    let tl_stack_info = ns_tl stack_info in
    let flow_vtx_added = begin
      cfg.flow |> addvtx body_begin |> addvtx body_end
    end in
    let flow_edg_added = begin
      let t_edg = G.E.create in_v If_true body_begin in
      let f_edg = G.E.create in_v If_false out_v in
      flow_vtx_added |> addedg_e t_edg |> addedg_e f_edg
    end in
    let gen_emsg s : string = "inst_to_cfg : I_loop : " ^ s in
    let vinfo_1 = map_add (gen_emsg "vinfo_1") cfg.vertex_info in_v (Cfg_loop var_1) in
    (*let vinfo_2 = map_add (gen_emsg "vinfo_2") vinfo_1 body_end Cfg_skip in*)
    (* fill in the loop body *)
    let cfg_body = {cfg with flow=flow_edg_added; vertex_info=vinfo_1;} in
    let (cfg_body_end, stack_info_body_end) = inst_to_cfg_handle_es counter (body_begin, body_end) (func_in_v, func_out_v) i (cfg_body, tl_stack_info) in
    if (is_es stack_info_body_end)
    then (
      (* no variable-sync needed. connect Failed-edge and use tl_stack_info instead. *)
      let (cfg_final, _) =
        (cfg_body_end, ())
        |> t_add_fail_edg (body_end, in_v)
        |> t_add_vinfo ~errtrace:(gen_emsg "is_es true : cfg_final") (body_end, Cfg_failwith (get_es_str stack_info_body_end))
      in
      (cfg_final, tl_stack_info)
    )
    else (
      let (cfg_body_end_vinfo_added, _) = t_add_vinfo ~errtrace:(gen_emsg "cfg_body_end_vinfo_added") (body_end, Cfg_skip) (cfg_body_end, ()) in
      (* insert Cfg_assigns to sync variable names *)
      (* Add vertices linearly for every name-notequal stack elements, set vertex_info like (Cfg_assign (before-stack-var-name, (E_itself after-stack-var-name))) *)
      let fold2_func : (Cfg.t * Cfg.vertex) -> string -> string -> (Cfg.t * Cfg.vertex)
      = fun (acc_cfg, acc_in_v) bstack_var astack_var -> begin
        if (bstack_var = astack_var) then (acc_cfg, acc_in_v)
        else begin
          let nv = nvtx () in
          let flow_update = begin acc_cfg.flow |> addvtx nv |> addedg acc_in_v nv end in
          let vi_update = map_add ("inst_to_cfg : I_loop : fold2_func : vi_update") acc_cfg.vertex_info nv (Cfg_assign (bstack_var, (E_itself astack_var))) in
          ({acc_cfg with flow=flow_update; vertex_info=vi_update;}, nv)
        end
      end in
      let cfg_final = begin 
        match Core.List.fold2 (ns_unlift stack_info) (ns_unlift stack_info_body_end) ~init:(cfg_body_end_vinfo_added, body_end) ~f:fold2_func with
        | Core.List.Or_unequal_lengths.Ok (cfg_r, last_in_v) -> begin
            let flow_update = cfg_r.flow |> addedg last_in_v in_v in
            {cfg_r with flow=flow_update;}
          end
        | Core.List.Or_unequal_lengths.Unequal_lengths -> fail "inst_to_cfg : I_loop : cfg_final : unequal_lengths"
      end in
      (cfg_final, tl_stack_info)
    )

  | I_loop_left i ->
    (*  flow        : (in_v [If_true]-> unwrap_l) & (in_v [If_false]-> unwrap_r -> out_v)
                      & (unwrap_l -> body_begin -> (i ...) -> body_end -> (assigns ...) -> in_v)
        variables   : var-1   : condition or-type variable located at the top of the stack
                      var-ul  : unwrapped left value.
                      var-ur  : unwrapped right value.
        vertex_info : in_v        ->  Cfg_loop_left (var-1)
                      unwrap_l    ->  Cfg_assign (var-ul, E_unlift_or var-1)
                      unwrap_r    ->  Cfg_assign (var-ur, E_unlift_or var-1)
                      body_begin  ->  decided by "i"
                      body_end    ->  Cfg_skip
        type_info   : var-1   -> or (t1_l, t1_r)
                      var-ul  -> t1_l
                      var-ur  -> t1_r
        stack_info  : top element will be replaced into var-ur.
    *)
    (* update flow *)
    let errmsg_gen s : string = ("inst_to_cfg : I_loop_left : " ^ s) in
    let (cfg_vtx_added, (unwrap_r, unwrap_l, body_begin, body_end)) = t_add_vtx_4 counter (cfg, ()) in
    let cfg_p_edg_added = begin (* cfg_p_ name for the (cfg_edg_added, (...)) pair *)
      (cfg_vtx_added, ())
      |> t_add_tedg (in_v, unwrap_l)
      |> t_add_fedg (in_v, unwrap_r)
      |> t_add_edgs [(unwrap_r, out_v); (unwrap_l, body_begin)]
    end in
    (* put variables & update type_info *)
    let var_1 = ns_hd stack_info in
    let tl_stack_info = ns_tl stack_info in
    let (t1_l, t1_r) = begin
      let t1 = t_map_find ~errtrace:(errmsg_gen "t1_lr : t1") cfg.type_info var_1 in
      match get_d t1 with
      | Michelson.Adt.T_or (t1_l, t1_r) -> (t1_l, t1_r)
      | _ -> fail (errmsg_gen "t1_lr : match failed")
    end in
    let (cfg_updated_vul, var_ul) = t_add_nv_tinfo ~errtrace:(errmsg_gen "var_ul") counter t1_l cfg_p_edg_added in
    let (cfg_updated_vur, var_ur) = t_add_nv_tinfo ~errtrace:(errmsg_gen "var_ur") counter t1_r (cfg_updated_vul, ()) in
    (* update vertex_info *)
    let vs_pairs = [
      (in_v,     Tezla_cfg.Cfg_node.Cfg_loop_left (var_1));
      (unwrap_l, Cfg_assign (var_ul, E_unlift_or var_1));
      (unwrap_r, Cfg_assign (var_ur, E_unlift_or var_1));
      (*(body_end, Cfg_skip);*)
    ] in
    let (cfg_vi_updated, _) = t_add_vinfos ~errtrace:(errmsg_gen "cfg_p_vi_updated") vs_pairs (cfg_updated_vur, ()) in
    (* update stack_info *)
    let stack_info_before_body = ns_cons var_ul tl_stack_info in
    (* fill in the body *)
    let (cfg_body_updated, stack_info_body_updated) = inst_to_cfg_handle_es counter (body_begin, body_end) (func_in_v, func_out_v) i (cfg_vi_updated, stack_info_before_body) in
    if (is_es stack_info_body_updated)
    then (
      (* no variable-sync needed. connect Failed-edge and use tl_stack_info instead. *)
      let (cfg_final, _) =
        (cfg_body_updated, ())
        |> t_add_fail_edg (body_end, in_v)
        |> t_add_vinfo ~errtrace:(errmsg_gen "is_es true : cfg_final") (body_end, Cfg_failwith (get_es_str stack_info_body_updated))
      in
      (cfg_final, tl_stack_info)
    )
    else (
      let (cfg_body_updated_vinfo_added, _) = t_add_vinfo ~errtrace:(errmsg_gen "cfg_body_end_vinfo_added") (body_end, Cfg_skip) (cfg_body_updated, ()) in
      (* insert Cfg_assigns to sync variable names *)
      (* Add vertices linearly for every name-notequal stack elements, set vertex_info like (Cfg_assign (before-stack-var-name, (E_itself after-stack-var-name))) *)
      let fold2_func : (Cfg.t * Cfg.vertex) -> string -> string -> (Cfg.t * Cfg.vertex)
      = fun (acc_cfg, acc_in_v) bstack_var astack_var -> begin
        if (bstack_var = astack_var) then (acc_cfg, acc_in_v)
        else begin
          t_add_vtx counter (acc_cfg, ())
          |> t_add_vinfo_now ~errtrace:(errmsg_gen "fold2_func") (Cfg_assign (bstack_var, (E_itself astack_var)))
          |> t_con_vtx_back acc_in_v
        end
      end in
      let (cfg_final, _) = begin 
        match Core.List.fold2 (ns_unlift stack_info) (ns_unlift stack_info_body_updated) ~init:(cfg_body_updated_vinfo_added, body_end) ~f:fold2_func with
        | Core.List.Or_unequal_lengths.Ok (cfg_r, last_in_v) -> t_add_edg (last_in_v, in_v) (cfg_r, ())
        | Core.List.Or_unequal_lengths.Unequal_lengths -> fail "inst_to_cfg : I_loop : cfg_final : unequal_lengths"
      end in
      (cfg_final, ns_cons var_ur tl_stack_info)
    )

  | I_lambda (ty1, ty2, i) ->
    (*  CURRENT FLOW
        flow        : add new vertex between in-and-out
        variables   : v_r : added which would be pushed to stack
        vertex_info : in_v        -> Cfg_skip
                      new vertex  -> Cfg_assign (v_r, E_lambda_id (new-lambda-identifier))
        type_info   : v_r         -> T_lambda (ty1, ty2)
        stack_info  : push new vertex
        lambda_id_map : Add (new-lambda-identifier -> (lmbd_in, lmbd_out, ty1, ty2))
    *)
    (*  LAMBDA FLOW INITIALIZE
        flow        : add two vertices: lmbd_in and lmbd_out
        variables   : no change
        vertex_info : lmbd_in     -> It will be decided in "i" translation procedure.
                      lmbd_out    -> Cfg_skip
        type_info   : param_i     -> ty1
        stack_info  : push "param_i" in empty stack
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_lambda : " ^ s) in
    let lambda_id : Cfg.lambda_ident = Cfg.new_lambda_ident counter in
    let param_i = Cfg.gen_param_name lambda_id in
    let (cfg_vtx_added, (lmbd_in, lmbd_out)) = t_add_vtx_2 counter (cfg, ()) in
    let (cfg_ready, _) = begin
      (cfg_vtx_added, ())
      |> t_add_tinfo ~errtrace:(gen_emsg "param_i") (param_i, ty1)
      |> t_add_lmbdim ~errtrace:(gen_emsg "cfg_lmbdim_updated") (lambda_id, (lmbd_in, lmbd_out, ty1, ty2))
    end in
    let (cfg_lmbd, stack_info_lmbd) = inst_to_cfg_handle_es counter (lmbd_in, lmbd_out) (lmbd_in, lmbd_out) i (cfg_ready, NS [param_i]) in
    let lmbd_ret_val = ns_hd stack_info_lmbd in
    let (cfg_ended, _) = t_add_vinfo ~errtrace:(gen_emsg "add lambda_result") (lmbd_out, Cfg_assign (lmbd_ret_val, E_itself lmbd_ret_val)) (cfg_lmbd, ()) in
    template_of_push_value "I_lambda" counter (in_v, out_v) (E_lambda_id lambda_id, Michelson.Adt.T_lambda (ty1, ty2)) (cfg_ended, stack_info)


  (* To deal with this problem properly, we need to change two things
      1. Change the definition of Tezla.Adt.E_lambda 's data construction rule.
        - Before: E_lambda of typ * typ * (Tezla.Adt.stmt * string)
        - After:  E_lambda of typ * typ
      2. Change the definition of Cfg.t type to contain lambda information.
        - Precondition: E_lambda should be only appeared with "Cfg_assign (v1, (E_lambda (ty1, ty2)))" form.
        - Consider that the lambda value graph will be contained in original cfg flow.
        - Before: Cfg.t = {
                    flow : G.t;
                    vertex_info : (int, stmt) CPMap.t;
                    type_info : (string, typ) CPMap.t;
                    main_entry : vertex;
                    main_exit : vertex;
                  }
        - After:  type lamb = (vertex * vertex * typ * typ)   (* (entry, exit, param-typ, return-typ) *)
                  Cfg.t = {
                    flow : G.t;
                    vertex_info : (int, stmt) CPMap.t;
                    type_info : (string, typ) CPMap.t;
                    lambda_info : ((vertex * string), lamb) CPMap.t;  (* (assign-vertex, assigned-variable) -> (LAMBDA value's (entry, exit, param-typ, return-typ)) *)
                    lambdas : lamb Core.Set.Poly.t;   (* remember every lambda entries, exits *)
                    main_entry : vertex;
                    main_exit : vertex;
                  }
  *)

  
  | I_exec ->
    (*  flow        : add new vertex between in-and-out
        variables   : var-1   : top element of the stack. (parameter)
                      var-2   : 2nd top element of the stack. (function)
                      v_r     : get operation result value
        vertex_info : in_v        -> Cfg_skip
                      new vertex  -> Cfg_assign (v_r, E_exec (var-1, var-2))
        type_info   : var-1   : 'a
                      var-2   : T_lambda ('a, 'b)
                      v_r     : 'b
        stack_info  : pop two elements and push "v_r"
    *)
    let gen_errmsg s : string = ("inst_to_cfg : I_exec : " ^ s) in
    let (v_1, v_2, tltl_stack_info) = begin
      (ns_hd stack_info,
        ns_hd (ns_tl stack_info),
        ns_tl (ns_tl stack_info))
    end in
    let t_r = begin
      (* no type constract check here. We assume that the input program has valid types. *)
      match get_d (t_map_find ~errtrace:(gen_errmsg "t_r") cfg.type_info v_2) with
      | Michelson.Adt.T_lambda (_, r) -> r
      | _ -> fail (gen_errmsg "t_r : match failed")
    end in
    let (cfg_var_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_errmsg "cfg_var_added") counter t_r (cfg, ()) in
    let (c, _) = begin
      (cfg_var_added, ())
      |> t_add_vinfo ~errtrace:(gen_errmsg "in_v vinfo") (in_v, Cfg_skip)
      |> t_add_vtx counter
      |> t_con_vtx_front in_v
      |> t_con_vtx_backr out_v
      |> t_add_vinfo_now ~errtrace:(gen_errmsg "mid_v vinfo") (Cfg_assign (v_r, E_exec (v_1, v_2)))
    end in
    (c, ns_cons v_r tltl_stack_info)

  | I_dip i ->
    (*  flow        : add new vertex between in-and-out
        variables   : N/A
        vertex_info : no change
        type_info   : no change
        stack_info  : recursive call with one-popped-stack_info
    *)
    let gen_errmsg s : string = ("inst_to_cfg : I_dip : " ^ s) in
    let (cfg_vv_added, (v_begin, v_end)) = t_add_vtx_2 counter (cfg, ()) in
    let (cfg_v_linked, _) = begin
      (cfg_vv_added, ())
      (*|> t_add_edgs [(in_v, v_begin); (v_end, out_v);] *)
      |> t_add_edg (in_v, v_begin)
      |> t_add_vinfos ~errtrace:(gen_errmsg "add vinfos") [(in_v, Cfg_skip); (v_end, Cfg_skip); ]
    end in
    let (cfg_i_added, stack_info_i_added) = inst_to_cfg_handle_es counter (v_begin, v_end) (func_in_v, func_out_v) i (cfg_v_linked, ns_tl stack_info) in
    if (is_es stack_info_i_added)
    then (
      let cfg_vend_linked = fail_edg_add (v_end, out_v) cfg_i_added in
      (cfg_vend_linked, stack_info_i_added)
    )
    else (
      let cfg_vend_linked = edg_add (v_end, out_v) cfg_i_added in
      (cfg_vend_linked, ns_cons (ns_hd stack_info) stack_info_i_added)
    )
    

  | I_dip_n (zn, i) ->
    (*  flow        : add new vertex between in-and-out
        variables   : N/A
        vertex_info : no change
        type_info   : no change
        stack_info  : recursive call with n-popped-stack_info
    *)
    let nn : int = Z.to_int zn in
    let gen_errmsg s : string = ("inst_to_cfg : I_dip_n : " ^ s) in
    let (hdn_stack_info, tln_stack_info) = ns_split_n stack_info nn in
    let (cfg_vv_added, (v_begin, v_end)) = t_add_vtx_2 counter (cfg, ()) in
    let (cfg_v_linked, _) = begin
      (cfg_vv_added, ())
      (*|> t_add_edgs [(in_v, v_begin); (v_end, out_v);] *)
      |> t_add_edg (in_v, v_begin)
      |> t_add_vinfos ~errtrace:(gen_errmsg "add vinfos") [(in_v, Cfg_skip); (v_end, Cfg_skip); ]
    end in
    let (cfg_i_added, stack_info_i_added) = inst_to_cfg_handle_es counter (v_begin, v_end) (func_in_v, func_out_v) i (cfg_v_linked, tln_stack_info) in
    if (is_es stack_info_i_added)
    then (
      let cfg_vend_linked = fail_edg_add (v_end, out_v) cfg_i_added in
      (cfg_vend_linked, stack_info_i_added)
    )
    else (
      let cfg_vend_linked = edg_add (v_end, out_v) cfg_i_added in
      (cfg_vend_linked, ns_append hdn_stack_info stack_info_i_added)
    )

  | I_failwith ->
    (*  flow        : add new vertex between in-and-out. (new_v -> out_v) should have "Failed" edge-label.
        variables   : var-1   : top element of the stack.
        vertex_info : in_v        -> Cfg_skip
                      new vertex  -> Cfg_failwith var-1
        type_info   : no change
        stack_info  : ES (var-1)
        fail_vertices : add new vertex to the fail_vertices set
    *)
    let gen_errmsg s : string = ("inst_to_cfg : I_failwith : " ^ s) in
    let v_1 = ns_hd stack_info in
    let (cfg_vtx_added, mid_v) = begin
      (cfg, ())
      |> t_add_vinfo ~errtrace:(gen_errmsg "in_v vinfo") (in_v, Cfg_skip)
      |> t_add_vtx counter
      |> t_add_vinfo_now ~errtrace:(gen_errmsg "add_vinfo_now") (Cfg_failwith v_1)
      |> t_con_vtx_front in_v
    end in
    let (cfg_final, _) = begin
      (cfg_vtx_added, mid_v)
      |> t_add_fail_edg (mid_v, out_v)
      |> t_add_failvtx mid_v
    end in
    (cfg_final, ES v_1)

  | I_cast ty ->
    (*  flow        : add new vertex between in-and-out
        variables   : var-1   : top element of the stack.
                      v_r     : new variable
        vertex_info : in_v        -> Cfg_skip
                      new vertex  -> Cfg_assign (v_r, E_itself var-1)
        type_info   : v_r     : ty
        stack_info  : pop top element and push "v_r"
    *)
    let gen_errmsg s : string = ("inst_to_cfg : I_cast : " ^ s) in
    let (var_1, tl_stack_info) = (ns_hd stack_info, ns_tl stack_info) in
    let (cfg_var_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_errmsg "cfg_var_added") counter ty (cfg, ()) in
    let (cfg_end, _) = begin
      (cfg_var_added, ())
      |> t_add_vinfo ~errtrace:(gen_errmsg "in_v vinfo") (in_v, Cfg_skip)
      |> t_add_vtx counter
      |> t_add_vinfo_now ~errtrace:(gen_errmsg "mid_v vinfo") (Cfg_assign (v_r, E_itself var_1))
      |> t_con_vtx_front in_v
      |> t_con_vtx_back out_v
    end in
    (cfg_end, ns_cons v_r tl_stack_info)

  | I_rename ->
    (* Cfg_skip for now. Current Cfg.t does not supports any annotation. *)
    (*  flow        : connect in_v and out_v
        variables   : N/A
        vertex_info : in_v        -> Cfg_skip
        type_info   : no change
        stack_info  : no change
    *)
    let (cfg_end, _) = begin
      (cfg, ())
      |> t_add_edg (in_v, out_v)
      |> t_add_vinfo ~errtrace:("inst_to-cfg : I_rename : in_v vinfo") (in_v, Cfg_skip)
    end in
    (cfg_end, stack_info)
  
  | I_concat ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_2   : second top element of the stack. (If necessary)
                      v_r   : new variable
        vertex_info : in_v        -> Cfg_skip
                      <string, string> new vertex -> Cfg_assign (v_r, E_concat (v_1, v_2))
                      <bytes,  bytes > new vertex -> Cfg_assign (v_r, E_concat (v_1, v_2))
                      <string list   > new vertex -> Cfg_assign (v_r, E_concat_list v_1)
                      <bytes  list   > new vertex -> Cfg_assign (v_r, E_concat_list v_1)
        type_info   : <string, string> v_r        -> string
                      <bytes,  bytes > v_r        -> bytes
                      <string list   > v_r        -> string
                      <bytes  list   > v_r        -> bytes
        stack_info  : pop one or two elements and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_concat : " ^ s) in
    let (v_1, tl_stack_info) = stack_hdtl stack_info in
    let (t_1) = t_map_find ~errtrace:(gen_emsg "t_1") cfg.type_info v_1 in
    let (cfg_in_v_checked, _) = t_add_vinfo ~errtrace:(gen_emsg "") (in_v, Cfg_skip) (cfg, ()) in
    let (cfg_end, stack_info_end) = begin
      match get_d t_1 with
      | T_string -> begin
          let gen_emsg s : string = (gen_emsg ("T_string matched : " ^ s)) in
          let (v_2, tltl_stack_info) = stack_hdtl tl_stack_info in
          let (cfg_nv_added, v_r) = begin (cfg_in_v_checked, ()) |> t_add_nv_tinfo ~errtrace:(gen_emsg "cfg_nv_added") counter t_1 end in
          let (cfg_end, _) = begin 
            (cfg_nv_added, ())
            |> t_add_vtx counter
            |> t_add_vinfo_now ~errtrace:(gen_emsg "add mid_v vinfo") (Cfg_assign (v_r, E_concat (v_1, v_2)))
            |> t_con_vtx_front in_v
            |> t_con_vtx_back out_v
          end in
          (cfg_end, ns_cons v_r tltl_stack_info)
        end
      | T_bytes -> begin
          let gen_emsg s : string = (gen_emsg ("T_bytes matched : " ^ s)) in
          let (v_2, tltl_stack_info) = stack_hdtl tl_stack_info in
          let (cfg_nv_added, v_r) = begin (cfg_in_v_checked, ()) |> t_add_nv_tinfo ~errtrace:(gen_emsg "cfg_nv_added") counter t_1 end in
          let (cfg_end, _) = begin 
            (cfg_nv_added, ())
            |> t_add_vtx counter
            |> t_add_vinfo_now ~errtrace:(gen_emsg "add mid_v vinfo") (Cfg_assign (v_r, E_concat (v_1, v_2)))
            |> t_con_vtx_front in_v
            |> t_con_vtx_back out_v
          end in
          (cfg_end, ns_cons v_r tltl_stack_info)
        end
      | T_list t_i -> begin
          let gen_emsg s : string = (gen_emsg ("T_list t_i matched : " ^ s)) in
          let (cfg_nv_added, v_r) = begin (cfg_in_v_checked, ()) |> t_add_nv_tinfo ~errtrace:(gen_emsg "cfg_nv_added") counter t_i end in
          let (cfg_end, _) = begin
            (cfg_nv_added, ())
            |> t_add_vtx counter
            |> t_add_vinfo_now ~errtrace:(gen_emsg "add mid_v vinfo") (Cfg_assign (v_r, E_concat_list v_1))
            |> t_con_vtx_front in_v
            |> t_con_vtx_back out_v
          end in
          (cfg_end, ns_cons v_r tl_stack_info)
        end
      | _ -> fail (gen_emsg "t_1 : match failed")
    end in
    (cfg_end, stack_info_end)

  | I_slice ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_2   : second top element of the stack.
                      v_3   : target string / bytes
                      v_r   : new variable
        vertex_info : in_v        -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_slice (v1, v2, v3))
        type_info   : v_1   -> nat
                      v_2   -> nat
                      v_3   -> string / bytes
                      v_r   -> option(string) / option(bytes)
        stack_info  : pop three elements and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_slice : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let (v_2, ttl_si) = stack_hdtl tl_si in
    let (v_3, tttl_si) = stack_hdtl ttl_si in
    let t_3 = t_map_find ~errtrace:(gen_emsg "t_3") cfg.type_info v_3 in
    let (cfg_v_r_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "gen v_r") counter (gen_t (Michelson.Adt.T_option t_3)) (cfg, ()) in
    let (cfg_end, _) = begin
      (cfg_v_r_added, ())
      |> t_add_vinfo ~errtrace:(gen_emsg "in_v vinfo") (in_v, Cfg_skip)
      |> t_add_vtx counter
      |> t_add_vinfo_now ~errtrace:(gen_emsg "mid_v vinfo") (Cfg_assign (v_r, E_slice (v_1, v_2, v_3)))
      |> t_con_vtx_front in_v
      |> t_con_vtx_back out_v
    end in
    (cfg_end, ns_cons v_r tttl_si)

  | I_pack ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_pack v_1)
        type_info   : v_r   -> bytes
        stack_info  : pop a element and push "v_r"
    *)
    let gen_errmsg s : string = ("inst_to_cfg : I_pack : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_errmsg "v_r") counter (gen_t Michelson.Adt.T_bytes) (cfg, ()) in
    let (cfg_ended, _) = begin
      (cfg_vr_added, ())
      |> t_add_vinfo ~errtrace:(gen_errmsg "in_v vinfo") (in_v, Cfg_skip)
      |> t_add_vtx counter 
      |> t_add_vinfo_now ~errtrace:(gen_errmsg "mid_v vinfo") (Cfg_assign (v_r, E_pack v_1))
      |> t_con_vtx_front in_v
      |> t_con_vtx_back out_v
    end in
    (cfg_ended, ns_cons v_r tl_si)

  | I_unpack ty -> 
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_unpack (ty, v_1))
        type_info   : v_r   -> T_option ty
        stack_info  : pop a element and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_unpack : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "v_r") counter (gen_t (Michelson.Adt.T_option ty)) (cfg, ()) in
    let (cfg_ended, _) = begin
      (cfg_vr_added, ())
      |> t_add_vinfo ~errtrace:(gen_emsg "in_v vinfo") (in_v, Cfg_skip)
      |> t_add_vtx counter
      |> t_add_vinfo_now ~errtrace:(gen_emsg "mid_v vinfo") (Cfg_assign (v_r, E_unpack (ty, v_1)))
      |> t_con_vtx_front in_v
      |> t_con_vtx_back out_v
    end in
    (cfg_ended, ns_cons v_r tl_si)

  | I_add ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_2   : second top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_add (v_1, v_2))
        type_info   : v_r   -> t_r
                      t_r = match (t_1, t_2) with
                            | nat, nat -> nat
                            | nat, int -> int
                            | int, nat -> int
                            | int, int -> int
                            | timestamp, int -> timestamp
                            | int, timestamp -> timestamp
                            | mutez, mutez -> mutez
        stack_info  : pop two elements and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_add : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let (v_2, ttl_si) = stack_hdtl tl_si in
    let (t_1, t_2) = (
      t_map_find ~errtrace:(gen_emsg "t_1") cfg.type_info v_1,
      t_map_find ~errtrace:(gen_emsg "t_2") cfg.type_info v_2
    ) in
    let t_r = begin
      let open Michelson.Adt in
      let t_r_i = 
        match (get_d t_1, get_d t_2) with
        | T_nat, T_nat -> T_nat
        | T_nat, T_int -> T_int
        | T_int, T_nat -> T_int
        | T_int, T_int -> T_int
        | T_timestamp, T_int -> T_timestamp
        | T_int, T_timestamp -> T_timestamp
        | T_mutez, T_mutez -> T_mutez
        | _ -> fail (gen_emsg "t_r match failed")
      in
      gen_t t_r_i
    end in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_add (v_1, v_2))) cfg_vr_added in
    (cfg_ended, ns_cons v_r ttl_si)

  | I_sub ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_2   : second top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_sub (v_1, v_2))
        type_info   : v_r   -> t_r
                      t_r = match (t_1, t_2) with
                            | nat, nat -> int
                            | nat, int -> int
                            | int, nat -> int
                            | int, int -> int
                            | timestamp, int -> timestamp
                            | timestamp, timestamp -> int
                            | mutez, mutez -> mutez
        stack_info  : pop two elements and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_sub : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let (v_2, ttl_si) = stack_hdtl tl_si in
    let (t_1, t_2) = (
      t_map_find ~errtrace:(gen_emsg "t_1") cfg.type_info v_1,
      t_map_find ~errtrace:(gen_emsg "t_2") cfg.type_info v_2
    ) in
    let t_r = begin
      let open Michelson.Adt in
      let t_r_i = 
        match (get_d t_1, get_d t_2) with
        | T_nat, T_nat -> T_int
        | T_nat, T_int -> T_int
        | T_int, T_nat -> T_int
        | T_int, T_int -> T_int
        | T_timestamp, T_int -> T_timestamp
        | T_timestamp, T_timestamp -> T_int
        | T_mutez, T_mutez -> T_mutez
        | _ -> fail (gen_emsg "t_r match failed")
      in
      gen_t t_r_i
    end in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_sub (v_1, v_2))) cfg_vr_added in
    (cfg_ended, ns_cons v_r ttl_si)
    
  | I_mul ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_2   : second top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_mul (v_1, v_2))
        type_info   : v_r   -> t_r
                      t_r = match (t_1, t_2) with
                            | nat, nat -> nat
                            | nat, int -> int
                            | int, nat -> int
                            | int, int -> int
                            | mutez, nat -> mutez
                            | nat, mutez -> mutez
        stack_info  : pop two elements and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_mul : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let (v_2, ttl_si) = stack_hdtl tl_si in
    let (t_1, t_2) = (
      t_map_find ~errtrace:(gen_emsg "t_1") cfg.type_info v_1,
      t_map_find ~errtrace:(gen_emsg "t_2") cfg.type_info v_2
    ) in
    let t_r = begin
      let open Michelson.Adt in
      let t_r_i = 
        match (get_d t_1, get_d t_2) with
        | T_nat, T_nat -> T_nat
        | T_nat, T_int -> T_int
        | T_int, T_nat -> T_int
        | T_int, T_int -> T_int
        | T_mutez, T_nat -> T_mutez
        | T_nat, T_mutez -> T_mutez
        | _ -> fail (gen_emsg "t_r match failed")
      in
      gen_t t_r_i
    end in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_mul (v_1, v_2))) cfg_vr_added in
    (cfg_ended, ns_cons v_r ttl_si)

  | I_ediv ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_2   : second top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_ediv (v_1, v_2))
        type_info   : v_r   -> t_r
                      t_r = match (t_1, t_2) with
                            | nat, nat -> option (pair nat nat)
                            | nat, int -> option (pair int nat)
                            | int, nat -> option (pair int nat)
                            | int, int -> option (pair int nat)
                            | mutez, nat -> option (pair mutez mutez)
                            | mutez, mutez -> option (pair nat mutez)
        stack_info  : pop two elements and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_ediv : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let (v_2, ttl_si) = stack_hdtl tl_si in
    let (t_1, t_2) = (
      t_map_find ~errtrace:(gen_emsg "t_1") cfg.type_info v_1,
      t_map_find ~errtrace:(gen_emsg "t_2") cfg.type_info v_2
    ) in
    let t_r = begin
      let open Michelson.Adt in
      let tnat = gen_t T_nat in
      let tint = gen_t T_int in
      let tmutez = gen_t T_mutez in
      let tpair a b = gen_t (T_pair (a, b)) in
      let t_r_i = 
        match (get_d t_1, get_d t_2) with
        | T_nat, T_nat -> T_option (tpair tnat tnat)
        | T_nat, T_int -> T_option (tpair tint tnat)
        | T_int, T_nat -> T_option (tpair tint tnat)
        | T_int, T_int -> T_option (tpair tint tnat)
        | T_mutez, T_nat -> T_option (tpair tmutez tmutez)
        | T_mutez, T_mutez -> T_option (tpair tnat tmutez)
        | _ -> fail (gen_emsg "t_r match failed")
      in
      gen_t t_r_i
    end in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_ediv (v_1, v_2))) cfg_vr_added in
    (cfg_ended, ns_cons v_r ttl_si)

  | I_abs ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_abs v_1)
        type_info   : v_r   -> T_nat
        stack_info  : pop a element and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_abs : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let t_r = gen_t Michelson.Adt.T_nat in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_abs v_1)) cfg_vr_added in
    (cfg_ended, ns_cons v_r tl_si)

  | I_isnat ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_isnat v_1)
        type_info   : v_r   -> T_option (T_nat)
        stack_info  : pop a element and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_isnat : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let t_r = gen_t (Michelson.Adt.T_option (gen_t Michelson.Adt.T_nat)) in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_isnat v_1)) cfg_vr_added in
    (cfg_ended, ns_cons v_r tl_si)

  | I_int ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_int_of_nat v_1)
        type_info   : v_r   -> T_nat
        stack_info  : pop a element and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_int : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let t_r = gen_t Michelson.Adt.T_nat in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_int_of_nat v_1)) cfg_vr_added in
    (cfg_ended, ns_cons v_r tl_si)

  | I_neg ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_neg v_1)
        type_info   : v_r   -> T_int
        stack_info  : pop a element and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_neg : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let t_r = gen_t Michelson.Adt.T_int in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_neg v_1)) cfg_vr_added in
    (cfg_ended, ns_cons v_r tl_si)

  | I_lsl ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_2   : second top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_shiftL (v_1, v_2))
        type_info   : v_r   -> T_nat
        stack_info  : pop two elements and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_lsl : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let (v_2, ttl_si) = stack_hdtl tl_si in
    let t_r = gen_t Michelson.Adt.T_nat in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_shiftL (v_1, v_2))) cfg_vr_added in
    (cfg_ended, ns_cons v_r ttl_si)

  | I_lsr ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_2   : second top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_shiftR (v_1, v_2))
        type_info   : v_r   -> T_nat
        stack_info  : pop two elements and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_lsr : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let (v_2, ttl_si) = stack_hdtl tl_si in
    let t_r = gen_t Michelson.Adt.T_nat in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_shiftR (v_1, v_2))) cfg_vr_added in
    (cfg_ended, ns_cons v_r ttl_si)

  | I_or ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_2   : second top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_or (v_1, v_2))
        type_info   : v_r   -> t_r
                      t_r = match (t_1, t_2) with
                            | bool, bool -> bool
                            | nat, nat -> nat
        stack_info  : pop two elements and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_or : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let (v_2, ttl_si) = stack_hdtl tl_si in
    let (t_1, t_2) = (
      t_map_find ~errtrace:(gen_emsg "t_1") cfg.type_info v_1,
      t_map_find ~errtrace:(gen_emsg "t_2") cfg.type_info v_2
    ) in
    let t_r = begin
      let open Michelson.Adt in
      let t_r_i = 
        match (get_d t_1, get_d t_2) with
        | T_bool, T_bool -> T_bool
        | T_nat, T_nat -> T_nat
        | _ -> fail (gen_emsg "t_r match failed")
      in
      gen_t t_r_i
    end in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_or (v_1, v_2))) cfg_vr_added in
    (cfg_ended, ns_cons v_r ttl_si)

  | I_and ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_2   : second top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_and (v_1, v_2))
        type_info   : v_r   -> t_r
                      t_r = match (t_1, t_2) with
                            | bool, bool -> bool
                            | nat, nat -> nat
                            | int, nat -> nat
        stack_info  : pop two elements and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_and : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let (v_2, ttl_si) = stack_hdtl tl_si in
    let (t_1, t_2) = (
      t_map_find ~errtrace:(gen_emsg "t_1") cfg.type_info v_1,
      t_map_find ~errtrace:(gen_emsg "t_2") cfg.type_info v_2
    ) in
    let t_r = begin
      let open Michelson.Adt in
      let t_r_i = 
        match (get_d t_1, get_d t_2) with
        | T_bool, T_bool -> T_bool
        | T_nat, T_nat -> T_nat
        | T_int, T_nat -> T_nat
        | _ -> fail (gen_emsg "t_r match failed")
      in
      gen_t t_r_i
    end in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_and (v_1, v_2))) cfg_vr_added in
    (cfg_ended, ns_cons v_r ttl_si)

  | I_xor ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_2   : second top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_xor (v_1, v_2))
        type_info   : v_r   -> t_r
                      t_r = match (t_1, t_2) with
                            | bool, bool -> bool
                            | nat, nat -> nat
        stack_info  : pop two elements and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_xor : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let (v_2, ttl_si) = stack_hdtl tl_si in
    let (t_1, t_2) = (
      t_map_find ~errtrace:(gen_emsg "t_1") cfg.type_info v_1,
      t_map_find ~errtrace:(gen_emsg "t_2") cfg.type_info v_2
    ) in
    let t_r = begin
      let open Michelson.Adt in
      let t_r_i = 
        match (get_d t_1, get_d t_2) with
        | T_bool, T_bool -> T_bool
        | T_nat, T_nat -> T_nat
        | _ -> fail (gen_emsg "t_r match failed")
      in
      gen_t t_r_i
    end in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_xor (v_1, v_2))) cfg_vr_added in
    (cfg_ended, ns_cons v_r ttl_si)

  | I_not ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_not v_1)
        type_info   : v_r   -> t_r
                      t_r = match (t_1, t_2) with
                            | bool -> bool
                            | nat  -> int
                            | int  -> int
        stack_info  : pop two elements and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_not : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let t_1 = t_map_find ~errtrace:(gen_emsg "t_1") cfg.type_info v_1 in
    let t_r = begin
      let open Michelson.Adt in
      let t_r_i = 
        match get_d t_1 with
        | T_bool -> T_bool
        | T_nat -> T_int
        | T_int -> T_int
        | _ -> fail (gen_emsg "t_r match failed")
      in
      gen_t t_r_i
    end in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_not v_1)) cfg_vr_added in
    (cfg_ended, ns_cons v_r tl_si)

  | I_compare ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_2   : second top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_compare (v_1, v_2))
        type_info   : v_r   -> T_int
        stack_info  : pop two elements and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_compare : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let (v_2, ttl_si) = stack_hdtl tl_si in
    let t_r = gen_t (Michelson.Adt.T_int) in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_compare (v_1, v_2))) cfg_vr_added in
    (cfg_ended, ns_cons v_r ttl_si)

  | I_eq ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_eq v_1)
        type_info   : v_r   -> T_bool
        stack_info  : pop a element and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_eq : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let t_r = gen_t Michelson.Adt.T_bool in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_eq v_1)) cfg_vr_added in
    (cfg_ended, ns_cons v_r tl_si)

  | I_neq ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_neq v_1)
        type_info   : v_r   -> T_bool
        stack_info  : pop a element and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_neq : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let t_r = gen_t Michelson.Adt.T_bool in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_neq v_1)) cfg_vr_added in
    (cfg_ended, ns_cons v_r tl_si)

  | I_lt ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_lt v_1)
        type_info   : v_r   -> T_bool
        stack_info  : pop a element and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_lt : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let t_r = gen_t Michelson.Adt.T_bool in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_lt v_1)) cfg_vr_added in
    (cfg_ended, ns_cons v_r tl_si)

  | I_gt ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_gt v_1)
        type_info   : v_r   -> T_bool
        stack_info  : pop a element and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_gt : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let t_r = gen_t Michelson.Adt.T_bool in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_gt v_1)) cfg_vr_added in
    (cfg_ended, ns_cons v_r tl_si)

  | I_le ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_leq v_1)
        type_info   : v_r   -> T_bool
        stack_info  : pop a element and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_le : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let t_r = gen_t Michelson.Adt.T_bool in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_leq v_1)) cfg_vr_added in
    (cfg_ended, ns_cons v_r tl_si)

  | I_ge ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_geq v_1)
        type_info   : v_r   -> T_bool
        stack_info  : pop a element and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_ge : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let t_r = gen_t Michelson.Adt.T_bool in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_geq v_1)) cfg_vr_added in
    (cfg_ended, ns_cons v_r tl_si)

  | I_self ->
    let paramtyp = let get_paramtyp = (fun {Michelson.Adt.param=pty; _} -> pty) in get_paramtyp cfg.adt in
    template_of_push_value "I_self" counter (in_v, out_v) (E_self, Michelson.Adt.T_contract paramtyp) (cfg, stack_info)

  | I_contract ty ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_contract_of_address v_1)
        type_info   : v_r   -> T_option (T_contract ty)
        stack_info  : pop a element and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_contract : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let t_r = gen_t (Michelson.Adt.T_option (gen_t (Michelson.Adt.T_contract ty))) in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_contract_of_address v_1)) cfg_vr_added in
    (cfg_ended, ns_cons v_r tl_si)


  | I_transfer_tokens ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_2   : second top element of the stack.
                      v_3   : third top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_operation (O_transfer_tokens (v_1, v_2, v_3)))
        type_info   : v_r   -> T_operation
        stack_info  : pop three elements and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_transfer_tokens : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let (v_2, ttl_si) = stack_hdtl tl_si in
    let (v_3, tttl_si) = stack_hdtl ttl_si in
    let t_r = gen_t Michelson.Adt.T_operation in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_operation (O_transfer_tokens (v_1, v_2, v_3)))) cfg_vr_added in
    (cfg_ended, ns_cons v_r tttl_si)

  | I_set_delegate ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_operation (O_set_delegate v_1))
        type_info   : v_r   -> T_operation
        stack_info  : pop a element and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_set_delegate : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let t_r = gen_t Michelson.Adt.T_operation in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_operation (O_set_delegate v_1))) cfg_vr_added in
    (cfg_ended, ns_cons v_r tl_si)
  
  | I_create_account -> fail "inst_to_cfg : I_create_account : UNDEFINED"

  | I_create_contract pgm ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_2   : second top element of the stack.
                      v_3   : third top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_operation (O_create_contract (pgm, v_1, v_2, v_3)))
        type_info   : v_r   -> T_operation
        stack_info  : pop three elements and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_create_contract : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let (v_2, ttl_si) = stack_hdtl tl_si in
    let (v_3, tttl_si) = stack_hdtl ttl_si in
    let t_r = gen_t Michelson.Adt.T_operation in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_operation (O_create_contract (pgm, v_1, v_2, v_3)))) cfg_vr_added in
    (cfg_ended, ns_cons v_r tttl_si)

  | I_implicit_account ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_implicit_accout v_1)
        type_info   : v_r   -> T_contract T_unit
        stack_info  : pop a element and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_implicit_account : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let t_r = gen_t (Michelson.Adt.T_contract (gen_t Michelson.Adt.T_unit))  in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_implicit_account v_1)) cfg_vr_added in
    (cfg_ended, ns_cons v_r tl_si)

  | I_now -> template_of_push_value "I_now" counter (in_v, out_v) (E_now, Michelson.Adt.T_timestamp) (cfg, stack_info)

  | I_amount -> template_of_push_value "I_amount" counter (in_v, out_v) (E_amount, Michelson.Adt.T_mutez) (cfg, stack_info)

  | I_balance -> template_of_push_value "I_balance" counter (in_v, out_v) (E_balance, Michelson.Adt.T_mutez) (cfg, stack_info)

  | I_check_signature -> 
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_2   : second top element of the stack.
                      v_3   : third top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_check_signature (v_1, v_2, v_3))
        type_info   : v_r   -> T_bool
        stack_info  : pop three elements and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_check_signature : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let (v_2, ttl_si) = stack_hdtl tl_si in
    let (v_3, tttl_si) = stack_hdtl ttl_si in
    let t_r = gen_t Michelson.Adt.T_bool in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_check_signature (v_1, v_2, v_3))) cfg_vr_added in
    (cfg_ended, ns_cons v_r tttl_si)
  
  | I_blake2b ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_blake2b v_1)
        type_info   : v_r   -> T_bytes
        stack_info  : pop a element and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_blake2b : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let t_r = gen_t Michelson.Adt.T_bytes  in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_blake2b v_1)) cfg_vr_added in
    (cfg_ended, ns_cons v_r tl_si)

  | I_sha256 ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_sha256 v_1)
        type_info   : v_r   -> T_bytes
        stack_info  : pop a element and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_sha256 : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let t_r = gen_t Michelson.Adt.T_bytes  in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_sha256 v_1)) cfg_vr_added in
    (cfg_ended, ns_cons v_r tl_si)

  | I_sha512 ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_sha512 v_1)
        type_info   : v_r   -> T_bytes
        stack_info  : pop a element and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_sha512 : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let t_r = gen_t Michelson.Adt.T_bytes  in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_sha512 v_1)) cfg_vr_added in
    (cfg_ended, ns_cons v_r tl_si)

  | I_hash_key ->
    (*  flow        : add new vertex between in-and-out
        variables   : v_1   : top element of the stack.
                      v_r   : new variable
        vertex_info : in_v       -> Cfg_skip
                      new vertex -> Cfg_assign (v_r, E_hash_key v_1)
        type_info   : v_r   -> T_key_hash
        stack_info  : pop a element and push "v_r"
    *)
    let gen_emsg s : string = ("inst_to_cfg : I_hash_key : " ^ s) in
    let (v_1, tl_si) = stack_hdtl stack_info in
    let t_r = gen_t Michelson.Adt.T_key_hash  in
    let (cfg_vr_added, v_r) = t_add_nv_tinfo ~errtrace:(gen_emsg "vr_added") counter t_r (cfg, ()) in
    let (cfg_ended, _) = t_add_typical_vertex (gen_emsg "cfg_ended") counter (in_v, out_v) (Cfg_assign (v_r, E_hash_key v_1)) cfg_vr_added in
    (cfg_ended, ns_cons v_r tl_si)

  | I_steps_to_quota -> template_of_push_value "I_steps_to_quota" counter (in_v, out_v) (E_steps_to_quota, Michelson.Adt.T_nat) (cfg, stack_info)

  | I_source -> template_of_push_value "I_source" counter (in_v, out_v) (E_source, Michelson.Adt.T_address) (cfg, stack_info)

  | I_sender -> template_of_push_value "I_sender" counter (in_v, out_v) (E_sender, Michelson.Adt.T_address) (cfg, stack_info)

  | I_chain_id -> template_of_push_value "I_chain_id" counter (in_v, out_v) (E_chain_id, Michelson.Adt.T_chain_id) (cfg, stack_info)

  | I_noop -> 
    (* update in_v and just connect in_v and out_v *)
    let cfg_end = begin
      (cfg, ())
      |> t_add_vinfo ~errtrace:("inst_to_cfg : I_noop : in_v") (in_v, Cfg_skip)
      |> t_add_edg (in_v, out_v) |> Stdlib.fst
    end in
    (cfg_end, stack_info)
    

  | I_unpair ->
    (* UNPAIR = {DUP; CAR; DIP CDR} *)
    let open Michelson.Adt in
    let iseq x y = gen_t (I_seq (x, y)) in
    let idup = gen_t I_dup in
    let icar = gen_t I_car in
    let icdr = gen_t I_cdr in
    let idip x = gen_t (I_dip x) in
    inst_to_cfg_handle_es counter (in_v, out_v) (func_in_v, func_out_v) (iseq idup (iseq icar (idip icdr))) (cfg, stack_info)

  | _ -> fail "inst_to_cfg : not implemented."
end


and inst_to_cfg_handle_es : cfgcon_ctr -> (Cfg.vertex * Cfg.vertex) -> (Cfg.vertex * Cfg.vertex) -> Adt.inst -> (Cfg.t * stack_info_t) -> (Cfg.t * stack_info_t) = 
begin
  (* It handles error-stack-info cases for typical instruction cases. *)
  let open Cfg in
  fun counter (in_v, out_v) (func_in_v, func_out_v) ist (cfg, stack_info) ->
  match stack_info with
  | NS _ -> inst_to_cfg counter (in_v, out_v) (func_in_v, func_out_v) ist (cfg, stack_info)
  | ES v -> begin   (* deal with error stack *)
      let (cfg_end, _) = 
        (cfg, ())
        |> t_add_vinfo ~errtrace:("inst_to_cfg_handle_es : ES") (in_v, Cfg_failwith v)
        |> t_add_fail_edg (in_v, out_v)
      in
      (cfg_end, ES v)
    end
end
  

let adt_to_cfg : Adt.t -> Cfg.t
= let open Cfg in
  let imap_add = map_add "adt_to_cfg" in
  let smap_add = map_add "adt_to_cfg" in  (* this duplicated might be refactored later. *)
  (* FUNCTION BEGIN *)
  fun adt ->
  let counter : cfgcon_ctr = {vertex_counter=(ref (-1)); var_counter=(ref (-1)); lambda_counter=(ref 0);} in  (* initialize counter *)
  (*  INITIALIZE GRAPH 
      flow          : two vertices. main_entry and main_exit.
      vertex_info   : main_exit vertex -> Cfg_skip
                      main_entry vertex's vertex-info will be updated in "inst_to_cfg" function.
      type_info     : type of parameter-storage will be added
      stack_info    : put the parameter-storage value to entry-v.
      adt           : input adt itself.
      lambda_id_map : empty map.
      fail_vertices : empty set.
  *)
  let entry_v = new_vtx counter in
  let exit_v = new_vtx counter in
  let flow_1 = G.add_vertex (G.add_vertex G.empty entry_v) exit_v in
  let param_storage_type = gen_t (Michelson.Adt.T_pair (adt.param, adt.storage)) in
  (*let vertex_info_1 = imap_add (imap_add CPMap.empty entry_v Tezla_cfg.Cfg_node.Cfg_skip) exit_v Tezla_cfg.Cfg_node.Cfg_skip in*)
  (*let vertex_info_1 = imap_add CPMap.empty exit_v Tezla_cfg.Cfg_node.Cfg_skip in*)
  let type_info_1 = smap_add CPMap.empty param_storage_name param_storage_type in
  let stack_info_1 : stack_info_t = NS [Cfg.param_storage_name] in
  let cfg_init = {
    flow          = flow_1;
    vertex_info   = CPMap.empty;
    type_info     = type_info_1;
    main_entry    = entry_v;
    main_exit     = exit_v;
    adt           = adt;
    lambda_id_map = CPMap.empty;
    fail_vertices = Core.Set.Poly.empty; } in
  let (cfg_last, stack_info) = inst_to_cfg_handle_es counter (cfg_init.main_entry, cfg_init.main_exit) (cfg_init.main_entry, cfg_init.main_exit) adt.code (cfg_init, stack_info_1) in
  let hd_stack_info = ns_hd stack_info in
  let vertex_info_last = imap_add cfg_last.vertex_info exit_v (Tezla_cfg.Cfg_node.Cfg_assign (hd_stack_info, E_itself hd_stack_info)) in
  {cfg_last with vertex_info=vertex_info_last;}
  (* TODO : if necessary, update exit node's stack info. *)
