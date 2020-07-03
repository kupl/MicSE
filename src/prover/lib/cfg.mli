


(*****************************************************************************)
(*****************************************************************************)
(* Graph (Flow)                                                              *)
(*****************************************************************************)
(*****************************************************************************)

type vertex = int
type edge_label = | Normal | If_true | If_false

module V : Graph.Sig.COMPARABLE with type t = vertex         (* VERTEX *)
module E : Graph.Sig.ORDERED_TYPE_DFT with type t = edge_label   (* EDGE LABLE *)
module G : module type of Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (V) (E)

val is_edge_normal : E.t -> bool

val is_edge_true : E.t -> bool

val is_edge_false : E.t -> bool

val string_of_vertex : vertex -> string

(*****************************************************************************)
(*****************************************************************************)
(* Node Information                                                          *)
(*****************************************************************************)
(*****************************************************************************)

(* loc, ident, decl, typ, expr, stmt will be modified *)
type loc = Unknown | Loc of int * int
type ident = string
type decl = ident
type typ = Tezla.Adt.typ
type expr = Tezla.Adt.expr
type stmt = TezlaCfg.Node.stmt
  (*
  type stmt =
  | Cfg_assign of string * expr
  | Cfg_skip
  | Cfg_drop of string list
  | Cfg_swap
  | Cfg_dig
  | Cfg_dug
  | Cfg_if of string
  | Cfg_if_none of string
  | Cfg_if_left of string
  | Cfg_if_cons of string
  | Cfg_loop of string
  | Cfg_loop_left of string
  | Cfg_map of string
  | Cfg_iter of string
  | Cfg_failwith of string
  *)


(*module IntMap : module type of Core.Map.Make (Core.Int)
module StringMap : module type of Core.Map.Make (Core.String) *)

module CPMap : module type of Core.Map.Poly

type t = {
  flow : G.t;
  vertex_info : (int, stmt) CPMap.t;  (* vertex-number -> stmt *)
  type_info : (string, typ) CPMap.t;  (* variable-name -> typ *)  
  main_entry : vertex;
  main_exit : vertex;
}

val param_storage_name : string

val read_stmt_from_vtx : t -> vertex -> stmt

val read_succ_from_vtx : t -> vertex -> (E.t * V.t) list

val is_main_entry : t -> vertex -> bool

val is_main_exit : t -> vertex -> bool

val string_of_ident : ident -> string