(* Tezla_cfg.Cfg_node : tezla-cfg/src/lib/Cfg_node.ml *)
module Node : sig
  type loc = Tezla_cfg.Cfg_node.loc
  type ident = string
  type decl = ident
  type typ = Tezla.Adt.typ
  type expr = Tezla.Adt.expr    (* tezla/src/lib/adt.mli *)
  type stmt = Tezla_cfg.Cfg_node.stmt
    (*
      (*contents:*)
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
  
  type t = Tezla_cfg.Cfg_node.t

  val to_string : t -> string
  val create_node : ?id:int -> stmt -> t
end


(* Tezla_Cfg.Flow_graph : tezla-cfg/src/lib/flow_graph.ml *)
open Batteries

type program = Michelson.Adt.program
type vertex = Node.t
type expr = Node.expr
type edge_label = Tezla_cfg.Flow_graph.Cfg.edge_label   (* type edge_label = Normal | If_true | If_false *)

module V : Graph.Sig.COMPARABLE with type t = Node.t
module E : Graph.Sig.ORDERED_TYPE_DFT with type t = edge_label
module G : module type of Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (Tezla_cfg.Flow_graph.Cfg.V) (Tezla_cfg.Flow_graph.Cfg.E)
module Display : module type of Tezla_cfg.Flow_graph.Cfg.Display
module Wrapper : module type of Tezla_cfg.Flow_graph.Cfg.Wrapper

type t = Tezla_cfg.Flow_graph.Cfg.t
(*
  type t = TCfg.t = {
    blocks : (int, vertex) Hashtbl.t;
    flow : G.t;
    functions : (int, string) Hashtbl.t;
    mutable extremals : int list;
    mutable extremalsR : int list;
  }
*)

val create : unit -> t
val inflow : t -> int -> int list
val outflow : t -> int -> int list
val is_extremal : t -> int -> bool
val is_extremalR : t -> int -> bool
val add : t -> string -> vertex -> unit
val get : t -> int -> vertex
val connect : t -> ?label:edge_label -> vertex -> vertex -> unit
val get_blocks : t -> (int, vertex) Hashtbl.t
val get_func_id : t -> int -> string
val extremal : t -> int -> unit
val extremalR : t -> int -> unit
val labels : t -> int Set.t
val dot_output : t -> string -> unit
val display_with_gv : t -> unit
val show : t -> unit
val generate_from_program : program -> t