(*****************************************************************************)
(*****************************************************************************)
(* Invariants                                                                *)
(*****************************************************************************)
(*****************************************************************************)

type vertex = Pre.Lib.Cfg.vertex

type formula = Vlang.t

type t = { id: vertex; formula: formula option }
and inv = t

val create_dummy_inv : vertex -> t

val create_inv : vertex -> formula -> t

val string_of_inv : t -> string


(*****************************************************************************)
(*****************************************************************************)
(* Invariant Map                                                             *)
(*****************************************************************************)
(*****************************************************************************)

module Map : sig
  module Vtx : sig
    module Key : sig
      type t = vertex
      val compare : t -> t -> int
      val sexp_of_t : t -> Core.Sexp.t
      val t_of_sexp : Core.Sexp.t -> t
    end
  
    include module type of Key
    include module type of Core.Comparable.Make (Key)
  end
  module T = Vtx.Map

  type t = inv T.t

  val empty : t
  val is_empty : t -> bool
  val add : t -> key:vertex -> data:inv -> t
  val find : t -> vertex -> inv
  val mem : t -> vertex -> bool
  val fold : t -> init:'a -> f:(key:vertex -> data:inv -> 'a -> 'a) -> 'a
  val map : t -> f:(key:vertex -> data:inv -> 'a) -> 'a T.t
  val exists : t -> f:(key:vertex -> data:inv -> bool) -> bool
  val merge : t -> t -> t
  val to_string : t -> string
end


(*****************************************************************************)
(*****************************************************************************)
(* Worklist                                                                  *)
(*****************************************************************************)
(*****************************************************************************)

module WorkList : sig
  type t = Map.t list

  val empty : t
  val is_empty : t -> bool
  val push : t -> Map.t -> t
  val pop : t -> (Map.t * t)
end