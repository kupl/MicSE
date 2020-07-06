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
  { flow = flow;
    (*node_info = node_info;*)
    vertex_info = Core.Hashtbl.fold node_info ~init:ProverLib.Cfg.CPMap.empty ~f:(fun ~key ~data acc -> ProverLib.Cfg.CPMap.add_exn acc ~key:key ~data:data);
    type_info = ProverLib.Cfg.CPMap.empty;
    main_entry = main_entry;
    main_exit = main_exit;
  }


(*****************************************************************************)
(*****************************************************************************)
(* Adt to Cfg                                                                *)
(*****************************************************************************)
(*****************************************************************************)

type ctr = int ref (* counter *)
type cfgcon_ctr = {   (* counter for cfg construction *)
  vertex_counter : ctr;
  var_counter : ctr;
}
let new_vtx : cfgcon_ctr -> Core.Int.t = fun c -> (incr c.vertex_counter; !(c.vertex_counter))
let new_var : cfgcon_ctr -> Core.String.t = fun c -> (incr c.var_counter; "v" ^ (string_of_int (!(c.var_counter))))


let gen_t : 'a -> 'a Michelson.Adt.t = fun x -> {pos = Michelson.Location.Unknown; d = x;}

let get_d : 'a Michelson.Adt.t -> 'a = fun x -> x.Michelson.Adt.d


let map_add : string -> ('k, 'v) Core.Map.Poly.t -> 'k -> 'v -> ('k, 'v) Cfg.CPMap.t
=fun caller_name m k v -> begin
  match Core.Map.Poly.add m ~key:k ~data:v with
  | `Ok m' -> m'
  | `Duplicate -> fail (caller_name ^ " : map_add : duplicated entry.")
end
let map_find : string -> ('k, 'v) Core.Map.Poly.t -> 'k -> 'v
=fun caller_name m k -> begin
  match Core.Map.Poly.find m k with
  | Some v -> v
  | None -> fail (caller_name ^ " : map_find : not found.")
end

(* typical flow manipulation procedure *)
let add_typical_vertex : cfgcon_ctr -> (Cfg.vertex * Cfg.vertex) -> Cfg.t -> (Cfg.G.t * Cfg.vertex)
= let open Cfg in
  fun counter (in_v, out_v) cfg -> begin
  let mid_v = new_vtx counter in
  let flow_1 = G.add_vertex cfg.flow mid_v in
  let flow_2 = G.add_edge (G.add_edge flow_1 in_v mid_v) mid_v out_v in
  (flow_2, mid_v)
end

(* Merge two stacks (variable-renaming process). It reconstructs flow, type-info too. *)
(* Precondition : two stacks have same length and type scheme. *)
let merge_two_stack_infos : cfgcon_ctr -> string -> Cfg.t -> (string list * string list) -> (Cfg.vertex * Cfg.vertex * Cfg.vertex * Cfg.vertex) -> (Cfg.t * string list)
= let open Cfg in
  (* sugar functions *)
  let addvtx vtx flw : G.t = G.add_vertex flw vtx in
  let addedg v1 v2 flw : G.t = G.add_edge flw v1 v2 in
  (* FUNCTION BEGIN *)
  fun counter errmsg_trace cfg (stack_info_1, stack_info_2) (in_1_v, in_2_v, out_1_v, out_2_v) -> begin
  let errmsg_gen s : string = (errmsg_trace ^ "merge_two_stack_infos : " ^ s) in
  (* before folding, we need to update vertex_info of in_1_v and in_2_v. Their stmt will become Cfg_skip. *)
  let vertex_info_1 = map_add (errmsg_gen "vertex_info_1") cfg.vertex_info in_1_v Cfg_skip in
  let vertex_info_2 = map_add (errmsg_gen "vertex_info_2") vertex_info_1 in_2_v Cfg_skip in
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
  let fold_result = Core.List.fold2 stack_info_1 stack_info_2 ~init:(cfg_1, [], in_1_v, out_1_v) ~f:fold_func in
  let (cfg_3, stack_info_3_rev, last_in_1_v, last_in_2_v) = 
  begin
    match fold_result with
    | Core.List.Or_unequal_lengths.Ok a -> a
    | _ -> fail (errmsg_trace ^ "merge_two_stack_infos : Core.List.Or_unequal_lengths : case unequal_lengths")
  end in
  (* connect in and out vertices *)
  let flow_4 = begin cfg_3.flow |> addedg last_in_1_v out_1_v |> addedg last_in_2_v out_2_v end in
  ({cfg_3 with flow=flow_4;}, Core.List.rev stack_info_3_rev)
end


(* construct control flow graph from instruction *)
let rec inst_to_cfg : cfgcon_ctr -> (Cfg.vertex * Cfg.vertex) -> Adt.inst -> (Cfg.t * string list) -> (Cfg.t * string list)
= let open Cfg in
  (* sugar functions *)
  let add_skip_vinfo : string -> (int, Cfg.stmt) Cfg.CPMap.t -> Cfg.vertex -> (int, Cfg.stmt) Cfg.CPMap.t
  =fun errmsg_trace vinfo in_v -> begin 
    map_add ("inst_to_cfg : " ^ errmsg_trace) vinfo in_v Cfg_skip
  end in
  (* FUNCTION BEGIN *)
  (* COMMENT TEMPLATE *)
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_skip
        type_info   : no change
        stack_info  : no change
    *)
  fun counter (in_v, out_v) ist (cfg, stack_info) -> begin
  match get_d ist with 
  | I_seq (i1, i2) ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_skip
        type_info   : no change
        stack_info  : no change
    *)
    let mid_v = new_vtx counter in
    let flow_1 = G.add_vertex cfg.flow mid_v in
    let vertex_info_1 = add_skip_vinfo "I_seq : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_seq : vertex_info_2" vertex_info_1 mid_v Cfg_skip in
    let cfg_1 = {cfg with flow=flow_1; vertex_info=vertex_info_2;} in
    (* fill about two instructions *)
    let (cfg_2, stack_info_1) = inst_to_cfg counter (in_v, mid_v) i1 (cfg_1, stack_info) in
    let (cfg_3, stack_info_2) = inst_to_cfg counter (mid_v, out_v) i2 (cfg_2, stack_info_1) in
    (cfg_3, stack_info_2)

  | I_drop ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_drop [...]
        type_info   : no change
        stack_info  : drop one element from the top of the stack.
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let vertex_info_1 = add_skip_vinfo "I_drop : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_drop : vertex_info_2" vertex_info_1 mid_v (Cfg_drop [Core.List.hd_exn stack_info]) in
    let stack_info_1 = Core.List.tl_exn stack_info in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2;}, stack_info_1)

  | I_drop_n zn ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_drop [...] (* string list, which is flattened string lists. *)
        type_info   : no change
        stack_info  : drop "zn" elements from the top of the stack.
    *)
    let nn = Z.to_int zn in
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let (slist, stack_info_1) = Core.List.split_n stack_info nn in
    let vertex_info_1 = add_skip_vinfo "I_drop_n : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_drop_n : vertex_info_2" vertex_info_1 mid_v (Cfg_drop slist) in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2;}, stack_info_1)

  | I_dup ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_assign (new-var, E_dup var)
        type_info   : no change
        stack_info  : add new variable on top
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let vertex_info_1 = add_skip_vinfo "I_dup : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_dup : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, E_dup (Core.List.hd_exn stack_info))) in
    let stack_info_1 = (Core.List.hd_exn stack_info) :: stack_info in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2;}, stack_info_1)

  | I_swap ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_swap
        type_info   : no change
        stack_info  : swap top two elements
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let vertex_info_1 = add_skip_vinfo "I_swap : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_swap : vertex_info_2" vertex_info_1 mid_v Cfg_swap in
    let stack_info_1 = let (h, t) = Core.List.split_n stack_info 2 in Core.List.rev_append h t in
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
    let stack_info_1 = let (h, t) = Core.List.split_n stack_info nn in ((Core.List.hd_exn t) :: h) @ (Core.List.tl_exn t) in
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
    let stack_info_1 = let (th, tt) = Core.List.split_n (Core.List.tl_exn stack_info) nn in th @ (Core.List.hd_exn stack_info :: tt) in
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
    let stack_info_1 = nv_name :: stack_info in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2; type_info=type_info_1;}, stack_info_1)

  | I_some ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_assign (new-var, (E_some var))
        type_info   : new-var -> T_option (ty)
        stack_info  : exchange var to new-var
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let v_name = Core.List.hd_exn stack_info in
    let si_tail = Core.List.tl_exn stack_info in
    let vertex_info_1 = add_skip_vinfo "I_some : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_some : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, E_some v_name)) in
    let v_type = map_find "inst_to_cfg : I_some : v_type" cfg.type_info v_name in
    let type_info_1 = map_add "inst_to_cfg : I_some : type_info_1" cfg.type_info nv_name (gen_t (Michelson.Adt.T_option v_type)) in
    let stack_info_1 = nv_name :: si_tail in
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
    let stack_info_1 = nv_name :: stack_info in
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
    let stack_info_1 = nv_name :: stack_info in
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
    let topvar_name : string = Core.List.hd_exn stack_info in
    let vertex_info_1 = map_add "inst_to_cfg : I_if_none : vertex_info_1" cfg.vertex_info in_v (Cfg_if_none topvar_name) in
    let vertex_info_2 = add_skip_vinfo "I_if_none : vertex_info_2" vertex_info_1 i1_end in
    let vertex_info_3 = add_skip_vinfo "I_if_none : vertex_info_3" vertex_info_2 i2_end in
    let cfg_1 = {cfg with flow=flow_edg_added; vertex_info=vertex_info_3;} in
    (* complete THEN branch (i1_begin ~ i1_end) *)
    let (cfg_2, stack_info_1) = inst_to_cfg counter (i1_begin, i1_end) i1 (cfg_1, stack_info) in
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
    let stack_info_2  = i2_unwrap_var :: (Core.List.tl_exn stack_info) in
    let cfg_3 = {cfg_2 with flow=flow_i2_ready; vertex_info=vertex_info_4; type_info=type_info_1;} in
    let (cfg_4, stack_info_3) = inst_to_cfg counter (i2_ready, i2_end) i2 (cfg_3, stack_info_2) in
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
    let topvar_name : string = Core.List.hd_exn stack_info in
    let vertex_info_1 = map_add "inst_to_cfg : I_if_some : vertex_info_1" cfg.vertex_info in_v (Cfg_if_none topvar_name) in
    let vertex_info_2 = add_skip_vinfo "I_if_some : vertex_info_2" vertex_info_1 i1_end in
    let vertex_info_3 = add_skip_vinfo "I_if_some : vertex_info_3" vertex_info_2 i2_end in
    let cfg_1 = {cfg with flow=flow_edg_added; vertex_info=vertex_info_3;} in
    (* complete THEN branch (i1_begin ~ i1_end) *)
    let (cfg_2, stack_info_1) = inst_to_cfg counter (i1_begin, i1_end) i1 (cfg_1, stack_info) in
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
      match topvar_typ with | {pos = _; d = Michelson.Adt.T_option tt} -> tt | _ -> fail "inst_to_cfg : I_if_none : topvar_unwrap_typ"
    end in
    let type_info_1   = map_add "inst_to_cfg : I_if_none : type_info_1" cfg_2.type_info i2_unwrap_var topvar_unwrap_typ in
    let stack_info_2  = i2_unwrap_var :: (Core.List.tl_exn stack_info) in
    let cfg_3 = {cfg_2 with flow=flow_i2_ready; vertex_info=vertex_info_4; type_info=type_info_1;} in
    let (cfg_4, stack_info_3) = inst_to_cfg counter (i2_ready, i2_end) i2 (cfg_3, stack_info_2) in
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

  | I_pair ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : new vertex -> Cfg_assign (new-var, (E_pair (var-1, var-2)))
        type_info   : new-var -> T_pair (t1, t2)
        stack_info  : pop two, push new-var.
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let var_1   = Core.List.hd_exn stack_info in
    let var_2   = begin Core.List.tl_exn stack_info |> Core.List.hd_exn end in
    let typ_1   = map_find "inst_to_cfg : I_pair : typ_1" cfg.type_info var_1 in
    let typ_2   = map_find "inst_to_cfg : I_pair : typ_2" cfg.type_info var_2 in
    let vertex_info_1 = add_skip_vinfo "I_pair : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_pair : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, E_pair (var_1, var_2))) in
    let type_info_1 = map_add "inst_to_cfg : I_pair : type_info_1" cfg.type_info nv_name (gen_t (Michelson.Adt.T_pair (typ_1, typ_2))) in
    let stack_info_1 = nv_name :: (Core.List.split_n stack_info 2 |> Stdlib.snd) in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2; type_info=type_info_1;}, stack_info_1)

  | I_car ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : new vertex -> Cfg_assign (new-var, (E_car var-1))
        type_info   : new-var -> (match t1 with | T_pair (t1, t2) -> t1 | _ -> error case )
        stack_info  : replace top one into new variable.
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let var_1   = Core.List.hd_exn stack_info in
    let typ_1   = map_find "inst_to_cfg : I_car : typ_1" cfg.type_info var_1 in
    let vertex_info_1 = add_skip_vinfo "I_car : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_car : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, E_car var_1)) in
    let type_info_1 = begin
      match get_d typ_1 with
      | T_pair (t1, _) -> map_add "inst_to_cfg : I_car : type_info_1" cfg.type_info nv_name t1
      | _ -> fail "inst_to_cfg : I_car : type_info_1 : match failed"
    end in
    let stack_info_1 = nv_name :: (Core.List.tl_exn stack_info) in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2; type_info=type_info_1}, stack_info_1)

    | I_cdr ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : new vertex -> Cfg_assign (new-var, (E_cdr var-1))
        type_info   : new-var -> (match t1 with | T_pair (t1, t2) -> t2 | _ -> error case )
        stack_info  : replace top one into new variable.
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let var_1   = Core.List.hd_exn stack_info in
    let typ_1   = map_find "inst_to_cfg : I_cdr : typ_1" cfg.type_info var_1 in
    let vertex_info_1 = add_skip_vinfo "I_cdr : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_cdr : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, E_cdr var_1)) in
    let type_info_1 = begin
      match get_d typ_1 with
      | T_pair (_, t2) -> map_add "inst_to_cfg : I_cdr : type_info_1" cfg.type_info nv_name t2
      | _ -> fail "inst_to_cfg : I_cdr : type_info_1 : match failed"
    end in
    let stack_info_1 = nv_name :: (Core.List.tl_exn stack_info) in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2; type_info=type_info_1}, stack_info_1)

    | I_left ty ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : new vertex -> Cfg_assign (new-var, (E_left (var-1, T_or(t1, ty))))
        type_info   : new-var -> T_or (t1, ty)
        stack_info  : replace top one into new variable.
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let var_1   = Core.List.hd_exn stack_info in
    let typ_1   = map_find "inst_to_cfg : I_left : typ_1" cfg.type_info var_1 in
    let typ_2   = gen_t (Michelson.Adt.T_or (typ_1, ty)) in
    let vertex_info_1 = add_skip_vinfo "I_left : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_left : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, E_left (var_1, typ_2))) in
    let type_info_1 = map_add "inst_to_cfg : I_left : type_info_1" cfg.type_info nv_name typ_2 in
    let stack_info_1 = nv_name :: (Core.List.tl_exn stack_info) in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2; type_info=type_info_1}, stack_info_1)

    | I_right ty ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : new vertex -> Cfg_assign (new-var, (E_right (var-1, T_or(ty, t1))))
        type_info   : new-var -> T_or (ty, t1)
        stack_info  : replace top one into new variable.
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let nv_name = new_var counter in
    let var_1   = Core.List.hd_exn stack_info in
    let typ_1   = map_find "inst_to_cfg : I_right : typ_1" cfg.type_info var_1 in
    let typ_2   = gen_t (Michelson.Adt.T_or (ty, typ_1)) in
    let vertex_info_1 = add_skip_vinfo "I_right : vertex_info_1" cfg.vertex_info in_v in
    let vertex_info_2 = map_add "inst_to_cfg : I_right : vertex_info_2" vertex_info_1 mid_v (Cfg_assign (nv_name, E_right (var_1, typ_2))) in
    let type_info_1 = map_add "inst_to_cfg : I_right : type_info_1" cfg.type_info nv_name typ_2 in
    let stack_info_1 = nv_name :: (Core.List.tl_exn stack_info) in
    ({cfg with flow=flow_2; vertex_info=vertex_info_2; type_info=type_info_1}, stack_info_1)

  | _ -> fail "inst_to_cfg : not implemented." (* TODO *)
end
  

let adt_to_cfg : Adt.t -> Cfg.t
= let open Cfg in
  let imap_add = map_add "adt_to_cfg" in
  let smap_add = map_add "adt_to_cfg" in  (* this duplicated might be refactored later. *)
  (* FUNCTION BEGIN *)
  fun adt ->
  let counter : cfgcon_ctr = {vertex_counter=(ref (-1)); var_counter=(ref (-1));} in  (* initialize counter *)
  (*  INITIALIZE GRAPH 
      flow : two vertices. main_entry and main_exit.
      vertex_info : both vertices are Cfg_skip
      type_info : type of parameter-storage will be added
      stack_info : put the parameter-storage value to entry-v.
  *)
  let entry_v = new_vtx counter in
  let exit_v = new_vtx counter in
  let flow_1 = G.add_vertex (G.add_vertex G.empty entry_v) exit_v in
  let param_storage_type = gen_t (Michelson.Adt.T_pair (adt.param, adt.storage)) in
  let vertex_info_1 = imap_add (imap_add CPMap.empty entry_v Tezla_cfg.Cfg_node.Cfg_skip) exit_v Tezla_cfg.Cfg_node.Cfg_skip in
  let type_info_1 = smap_add CPMap.empty param_storage_name param_storage_type in
  let stack_info_1 : string list = [Cfg.param_storage_name] in
  let cfg_init = {
    flow = flow_1;
    vertex_info = vertex_info_1;
    type_info = type_info_1;
    main_entry = entry_v;
    main_exit = exit_v; } in
  inst_to_cfg counter (cfg_init.main_entry, cfg_init.main_exit) adt.code (cfg_init, stack_info_1)
  |> Stdlib.fst
  (* TODO : if necessary, update exit node's stack info. *)
