(* Vc: Verification condition manager *)

exception VcError of string

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(* Set of Tz.mich_f *)
module MFSet : module type of Core.Set.Make (Tz.MichF_cmp)

(******************************************************************************)
(******************************************************************************)
(* Smt Encoder                                                                *)
(******************************************************************************)
(******************************************************************************)

module Encoder : sig
  val cv_mt : Smt.Ctx.t -> Tz.mich_t -> Smt.Sort.t

  val cv_mtcc : Smt.Ctx.t -> Tz.mich_t Tz.cc -> Smt.Sort.t

  val cv_mv : Smt.Ctx.t -> Tz.mich_v -> Smt.Expr.t

  val cv_mvcc : Smt.Ctx.t -> Tz.mich_v Tz.cc -> Smt.Expr.t

  val cv_compare :
    Smt.Ctx.t -> Tz.mich_t * Smt.Expr.t -> Tz.mich_t * Smt.Expr.t -> Smt.Expr.t

  val cv_mf : Smt.Ctx.t -> Tz.mich_f -> Smt.Formula.t
end

(******************************************************************************)
(******************************************************************************)
(* Utility                                                                    *)
(******************************************************************************)
(******************************************************************************)

val get_hd1 : 'a list -> 'a

val get_hd2 : 'a list -> 'a * 'a

val property_of_query : Tz.mich_cut_info -> Tz.sym_image -> Tz.mich_f

val apply_initial_storage :
  Tz.mich_cut_info -> Tz.sym_image -> Tz.mich_v Tz.cc -> Tz.mich_f

(******************************************************************************)
(******************************************************************************)
(* Verification Condition                                                     *)
(******************************************************************************)
(******************************************************************************)

val gen_query_vc : Inv.inv_map -> Tz.sym_state -> Tz.mich_f

val gen_query_vc_from_ms : Inv.inv_map -> MState.t -> Tz.mich_f

val gen_query_vc_from_ms_with_init_strg :
  Inv.inv_map -> Tz.mich_v Tz.cc -> MState.t -> Tz.mich_f

val gen_inductiveness_vc : Inv.inv_map -> Tz.sym_state -> Tz.mich_f

val gen_preservation_vc : MFSet.t -> MState.t -> Tz.mich_f option

val gen_initial_inv_vc : MFSet.t -> Tz.mich_v Tz.cc -> Tz.sym_state -> Tz.mich_f

(******************************************************************************)
(******************************************************************************)
(* Verification                                                               *)
(******************************************************************************)
(******************************************************************************)
val gen_ctx : unit -> Smt.Ctx.t

val gen_solver : Smt.Ctx.t -> Smt.Solver.t

val check_val :
  Smt.Ctx.t ->
  Smt.Solver.t ->
  Tz.mich_f ->
  Smt.Solver.validity * Smt.Model.t option

val check_sat :
  Smt.Ctx.t ->
  Smt.Solver.t ->
  Tz.mich_f ->
  Smt.Solver.satisfiability * Smt.Model.t option
