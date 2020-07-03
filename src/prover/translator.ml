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
let new_v : ctr -> Core.Int.t = fun rx -> (incr rx; !rx)
(*let new_var : ctr -> Core.String.t = fun rx -> (incr rx; "t" ^ (string_of_int (!rx)))*)


let gen_t : 'a -> 'a Michelson.Adt.t = fun x -> {pos = Michelson.Location.Unknown; d = x;}

let get_d : 'a Michelson.Adt.t -> 'a = fun x -> x.Michelson.Adt.d


let map_add : string -> ('k, 'v) Core.Map.Poly.t -> 'k -> 'v -> ('k, 'v) Cfg.CPMap.t
=fun caller_name m k v -> begin
  match Core.Map.Poly.add m ~key:k ~data:v with
  | `Ok m' -> m'
  | `Duplicate -> fail (caller_name ^ " : map_add : duplicated entry.")
end
let _ (* map_find *) : string -> ('k, 'v) Core.Map.Poly.t -> 'k -> 'v
=fun caller_name m k -> begin
  match Core.Map.Poly.find m k with
  | Some v -> v
  | None -> fail (caller_name ^ " : map_find : not found.")
end

(* typical flow manipulation procedure *)
let add_typical_vertex : ctr -> (Cfg.vertex * Cfg.vertex) -> Cfg.t -> (Cfg.G.t * Cfg.vertex)
= let open Cfg in
  fun counter (in_v, out_v) cfg -> begin
  let mid_v = new_v counter in
  let flow_1 = G.add_vertex cfg.flow mid_v in
  let flow_2 = G.add_edge (G.add_edge flow_1 in_v mid_v) mid_v out_v in
  (flow_2, mid_v)
end


(* construct control flow graph from instruction *)
let rec inst_to_cfg : ctr -> (Cfg.vertex * Cfg.vertex) -> Adt.inst -> (Cfg.t * string list) -> (Cfg.t * string list)
= let open Cfg in
  (* FUNCTION BEGIN *)
  (* Common operation for (almost) every patterns:
      cop_1 : stack_info of out_v should be updated.
  *)
  fun counter (in_v, out_v) ist (cfg, stack_info) -> begin
  match get_d ist with 
  | I_seq (i1, i2) ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_skip
        type_info   : no change
        others      : cop_1 will be violated
        stack_info  : no change
    *)
    let mid_v = new_v counter in
    let flow_1 = G.add_vertex cfg.flow mid_v in
    let vertex_info_1 = map_add "inst_to_cfg : vertex_info_1" cfg.vertex_info mid_v Cfg_skip in
    let cfg_1 = {cfg with flow=flow_1; vertex_info=vertex_info_1;} in
    (* fill about two instructions *)
    let (cfg_2, stack_info_1) = inst_to_cfg counter (in_v, mid_v) i1 (cfg_1, stack_info) in
    let (cfg_3, stack_info_2) = inst_to_cfg counter (mid_v, out_v) i2 (cfg_2, stack_info_1) in
    (cfg_3, stack_info_2)

  | I_drop ->
    (*  flow        : add new vertex between in-and-out
        vertex_info : Cfg_drop [...]
        type_info   : no change
        others      : no change
        stack_info  : drop one element from the top of the stack.
    *)
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let vertex_info_1 = map_add "inst_to_cfg : I_drop : vertex_info_1" cfg.vertex_info mid_v (Cfg_drop [Core.List.hd_exn stack_info]) in
    let stack_info_1 = Core.List.tl_exn stack_info in
    ({cfg with flow=flow_2; vertex_info=vertex_info_1;}, stack_info_1)

  | I_drop_n zn ->
    (*  flow : add new vertex between in-and-out
        vertex_info : Cfg_drop [...] (* string list, which is flattened string lists. *)
        type_info : no change
        others    : no change
        stack_info : drop "zn" elements from the top of the stack.
    *)
    let nn = Z.to_int zn in
    let (flow_2, mid_v) = add_typical_vertex counter (in_v, out_v) cfg in
    let (slist, stack_info_1) = Core.List.split_n stack_info nn in
    let vertex_info_1 = map_add "inst_to_cfg : I_drop_n : vertex_info_1" cfg.vertex_info mid_v (Cfg_drop slist) in
    ({cfg with flow=flow_2; vertex_info=vertex_info_1;}, stack_info_1)

  | _ -> fail "inst_to_cfg : not implemented." (* TODO *)
end
  

let adt_to_cfg : Adt.t -> Cfg.t
= let open Cfg in
  let imap_add = map_add "adt_to_cfg" in
  let smap_add = map_add "adt_to_cfg" in  (* this duplicated might be refactored later. *)
  (* FUNCTION BEGIN *)
  fun adt ->
  let counter = ref (-1) in  (* initialize counter *)
  (*  INITIALIZE GRAPH 
      flow : two vertices. main_entry and main_exit.
      vertex_info : both vertices are Cfg_skip
      type_info : type of parameter-storage will be added
      stack_info : put the parameter-storage value to entry-v.
  *)
  let entry_v = new_v counter in
  let exit_v = new_v counter in
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
