(*****************************************************************************)
(*****************************************************************************)
(* Exceptions                                                                *)
(*****************************************************************************)
(*****************************************************************************)

exception Exn_Cfg of string

let fail s = raise (Exn_Cfg s)


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
  Batteries.Hashtbl.iter (fun k v -> Core.Hashtbl.Poly.add cht k v |> Stdlib.ignore) bht;
  cht


(*****************************************************************************)
(*****************************************************************************)
(* Graph (Flow)                                                              *)
(*****************************************************************************)
(*****************************************************************************)

type vertex = int
type edge_label = | Normal | If_true | If_false

module V = struct
  type t = int
  let compare = Int.compare
  let hash x = x
  let equal = Int.equal
end

module E = struct
  type t = edge_label
  let default = Normal
  let compare = Stdlib.compare
end

module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (V) (E)


(*****************************************************************************)
(*****************************************************************************)
(* Node Information                                                          *)
(*****************************************************************************)
(*****************************************************************************)

type loc = Unknown | Loc of int * int
type ident = string
type decl = ident
type typ = Tezla.Adt.typ
type expr = Tezla.Adt.expr
type stmt = TezlaCfg.Node.stmt

type t = {
  flow : G.t;
  node_info : (vertex, stmt) Core.Hashtbl.t;
  main_entry : vertex;
  main_exit : vertex;
}


(*****************************************************************************)
(*****************************************************************************)
(* Casting                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

let of_tezlaCfg tcfg =
  let open Core in
  let vertices : int List.t = TezlaCfg.TCfg.labels tcfg |> c_list_of_batset in
  (* Get entry and exit nodes *)
  let t_vertices = List.filter vertices (fun n -> TezlaCfg.TCfg.is_extremal tcfg n) in (* terminal vertices in Cfg *)
  let inflow_len g n = TezlaCfg.TCfg.inflow g n |> List.length in   (* # of inflow edges *)
  let outflow_len g n = TezlaCfg.TCfg.outflow g n |> List.length in  (* # of outflow edges *)
  let entry_vertices = List.filter t_vertices (fun n -> (inflow_len tcfg n = 0) && (outflow_len tcfg n > 0)) in
  let exit_vertices  = List.filter t_vertices (fun n -> (inflow_len tcfg n > 0) && (outflow_len tcfg n = 0)) in
  let enlen = List.length entry_vertices in
  let exlen = List.length exit_vertices in
  let main_entry, main_exit = 
    if (enlen <> 1 || exlen <> 1)
    then fail ("of_tezlaCfg : size(entry_vertices) =" ^ (string_of_int enlen) ^ ", size(exit_vertices) =" ^ (string_of_int exlen))
    else (List.hd_exn entry_vertices, List.hd_exn exit_vertices)
  in
  (* Get flow and node informations *)
  let t_tbl : (int, TezlaCfg.Node.t) Core.Hashtbl.t = TezlaCfg.TCfg.get_blocks tcfg |> c_hashtbl_of_b_hashtbl in (* tezlaCfg table *)
  (* Get flow (edges) *)
  let history : int Core.Set.Poly.t = Set.Poly.empty in (* vertex visit history *)
  let worklist : int List.t = [main_entry] in
  let rec work : int List.t -> G.t -> G.t
  =fun wl g -> begin
    match wl with
    | [] -> g
    | v :: tail -> begin
        if Set.mem history v
        then work tail g
        else begin
          (work tail g) (* PLACEHOLDER, TODO *)
        end
      end
  end in
  (*let _ = tcfg.flow in*)
  let flow : G.t = work [main_entry] G.empty in
  (* Get node_info *)
  let node_info : (vertex, stmt) Core.Hashtbl.t = Core.Hashtbl.Poly.create () in
  let _ = Core.Hashtbl.iter t_tbl (fun x -> Core.Hashtbl.add node_info x.id x.stmt |> Stdlib.ignore) in
  { flow = G.empty;
    node_info = Hashtbl.Poly.create ();
    main_entry = main_entry;
    main_exit = main_exit;
  }