open Batteries

module type Flow = sig
  type block
  type vertex
  type t = {
    correspondence: (block, vertex) Hashtbl.t; 
    nodes: vertex Set.t;
    initial: vertex Set.t;
    final: vertex Set.t;
    flow: (vertex * vertex) Set.t
  }
  val init : block -> block
  val final : block -> block Set.t
  val flow : block -> t
  val flowR : block -> t
end

module type Flow_graph = sig
  type expr
  type vertex
  type edge_label = Normal | If_true | If_false
  type program
  type t

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
end

module type Inter_flow_graph = sig
  include Flow_graph

  val inter_flow : t -> (int * int * int * int) list
  (* TODO: val callees : t -> int -> int list *)
end
