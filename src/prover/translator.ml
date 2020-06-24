open ProverLib.Cfg
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
let tcfg_cast_edge_label : TezlaCfg.edge_label -> edge_label = function
  | Normal -> Normal
  | If_true -> If_true
  | If_false -> If_false
let tcfg_get_id n = n.Tezla_cfg.Cfg_node.id
let tcfg_get_stmt n = n.Tezla_cfg.Cfg_node.stmt

let of_tezlaCfg tcfg =
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
    node_info = node_info;
    main_entry = main_entry;
    main_exit = main_exit;
  }
