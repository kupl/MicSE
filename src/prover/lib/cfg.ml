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

type t = {flow: G.t; node_info: (vertex, stmt) Core.Hashtbl.t}


(*****************************************************************************)
(*****************************************************************************)
(* Casting                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

let of_tezlaCfg tcfg =
  let open TezlaCfg in
  let open Batteries in
  let vertices : int list = TCfg.labels tcfg |> BatSet.elements in
  (*let io_vertices = *){flow = G.empty; node_info = Core.Hashtbl.Poly.create ();}