(* MState : State merge *)

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(* Set of Igdt.igdt *)
module ISet : module type of Core.Set.Make (Igdt.IGDT_cmp)

(******************************************************************************)
(******************************************************************************)
(* Constraint from Merging                                                    *)
(******************************************************************************)
(******************************************************************************)

val eq_fmla :
  Tz.mich_sym_ctxt * Tz.mich_v Tz.cc ->
  Tz.mich_sym_ctxt * Tz.mich_v Tz.cc ->
  Tz.mich_f list

val trx_image_equality_fmla :
  Tz.mich_sym_ctxt ->
  Tz.mich_sym_ctxt ->
  Tz.trx_image ->
  Tz.trx_image ->
  Tz.mich_f list

val stack_equality_fmlas :
  Tz.mich_sym_ctxt * Tz.mich_sym_ctxt ->
  Tz.mich_cut_category * Tz.mich_cut_category ->
  Tz.sym_image * Tz.sym_image ->
  Tz.mich_f list

(******************************************************************************)
(******************************************************************************)
(* Merged State                                                               *)
(******************************************************************************)
(******************************************************************************)

type t = (Tz.sym_state * Tz.mich_f list) list [@@deriving sexp, compare, equal]

type summary = {
  sm_rmci : Tz.r_mich_cut_info;
  sm_s_id : Tz.sym_state_id;
}
[@@deriving sexp, compare, equal]

module SMY_cmp : sig
  type t = summary [@@deriving sexp, compare]
end

val init : Tz.sym_state -> t

val cons : Tz.sym_state -> t -> t

val get_constraint : t -> Tz.mich_f list

val get_first_ss : t -> Tz.sym_state

val get_last_ss : t -> Tz.sym_state

val get_tail_ms : t -> t

val get_length : t -> int

val get_summary : t -> summary

val cut_first_found_loop : t -> (t * t) option

val extract_trx_state : t -> Tz.sym_state list
