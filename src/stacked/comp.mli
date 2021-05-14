(* Component of value *)

exception Error of string


(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

module TComparable : sig
  exception Error of string

  module T : sig
    type t = Tz.mich_t Tz.cc
    val compare : t -> t -> int
    val t_of_sexp : Core.Sexp.t -> t
    val sexp_of_t : t -> Core.Sexp.t
  end

  include module type of Core.Comparable.Make(T)
end

module CTMap = TComparable.Map


(******************************************************************************)
(******************************************************************************)
(* Component Collector                                                        *)
(******************************************************************************)
(******************************************************************************)

(*****************************************************************************
  The type t is information from each component of the Tz.mich_v.
  Each component is extracted from the given Tz.mich_v.
*****************************************************************************)
type t = {
  cp_typ          : Tz.mich_t Tz.cc;        (* type of component *)
  cp_loc          : int;                    (* location of component in stack *)
  cp_base_var     : Tz.mich_v Tz.cc option; (* base variable expression *)
  cp_precond_lst  : Tz.mich_f list;         (* precondition list of component *)
  cp_value        : Tz.mich_v Tz.cc;        (* value expression of component *)
}

val base_comp_from_v : ?loc:int -> Tz.mich_v Tz.cc -> t
val base_comp_from_mci : (Tz.mich_cut_info * int * Tz.mich_t Tz.cc) -> t option

val collect : t -> (t Core.Set.Poly.t) CTMap.t -> (t Core.Set.Poly.t) CTMap.t
val merge : (t Core.Set.Poly.t) CTMap.t -> (t Core.Set.Poly.t) CTMap.t -> (t Core.Set.Poly.t) CTMap.t


(******************************************************************************)
(******************************************************************************)
(* Utils                                                                      *)
(******************************************************************************)
(******************************************************************************)

val fold_precond : t list -> Tz.mich_f