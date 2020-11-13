type vertex = Pre.Lib.Cfg.vertex
type formula = Vlang.t


(*****************************************************************************)
(*****************************************************************************)
(* Invariants                                                                *)
(*****************************************************************************)
(*****************************************************************************)

module T : sig
  module CPSet = Core.Set.Poly
  type t = { id: vertex; formula: formula CPSet.t }
  
  val create : vtx:vertex -> t
  val create_with_formulae : vtx:vertex -> fl:formula list -> t
  val read_formula : t -> formula
  val update : t -> f:formula -> t
  val order : inv1:t -> inv2:t -> bool (* inv1 <= inv2 ? true : false *)
  val join : inv1:t -> inv2:t -> t
  val to_string : t -> string
end


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
  module VtxMap = Vtx.Map

  type t = T.t VtxMap.t

  val empty : t
  val is_empty : t -> bool
  val add : t -> key:vertex -> data:T.t -> t
  val find : t -> vertex -> T.t option
  val find_empty : t -> vertex -> T.t
  val mem : t -> vertex -> bool
  val fold : t -> init:'a -> f:(key:vertex -> data:T.t -> 'a -> 'a) -> 'a
  val map : t -> f:(key:vertex -> data:T.t -> 'a) -> 'a VtxMap.t
  val exists : t -> f:(key:vertex -> data:T.t -> bool) -> bool
  val order : m1:t -> m2:t -> bool (* m1 <= m2 ? true : false *)
  val join : t -> t -> t
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