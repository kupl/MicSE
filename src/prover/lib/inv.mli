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
  exception Error of string

  module VtxMap = Core.Map.Poly

  type t = (Pre.Lib.Cfg.vertex, T.t) VtxMap.t

  val empty : t
  val is_empty : t -> bool
  val add : t -> key:vertex -> data:T.t -> t
  val find : t -> vertex -> T.t option
  val find_empty : t -> vertex -> T.t
  val mem : t -> vertex -> bool
  val fold : t -> init:'a -> f:(key:vertex -> data:T.t -> 'a -> 'a) -> 'a
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
  exception Error of string

  type t = {
    last_enable: Map.t;
    candidate: Map.t list;
    expired: Map.t list;
  }


  val empty : t
  val is_empty : t -> bool
  val mem : Map.t list -> Map.t -> bool
  val push : t -> Map.t -> t
  val push_list : t -> Map.t list -> t
  val push_force : t -> Map.t -> t
  val pop : t -> (Map.t * t)
  val map : t -> f:(Map.t -> Map.t) -> t
  val update_last_enable : t -> new_:Map.t -> t
end