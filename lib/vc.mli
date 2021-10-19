(* Vc: Verification condition manager *)

exception VcError of string

(******************************************************************************)
(******************************************************************************)
(* Smt Encoder                                                                *)
(******************************************************************************)
(******************************************************************************)

module Encoder : sig
  val cv_mtcc_i : Smt.Ctx.t -> Tz.mich_t Tz.cc -> Smt.Sort.t

  val cv_mtcc : Smt.Ctx.t -> Tz.mich_t Tz.cc -> Smt.Sort.t

  val cv_mvcc_i :
    sctx:Tz.mich_sym_ctxt -> Smt.Ctx.t -> Tz.mich_v Tz.cc -> Smt.Expr.t

  val cv_mvcc :
    sctx:Tz.mich_sym_ctxt -> Smt.Ctx.t -> Tz.mich_v Tz.cc -> Smt.Expr.t

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

val property_of_query :
  sctx:Tz.mich_sym_ctxt -> Tz.mich_cut_info -> Tz.sym_image -> Tz.mich_f

val fmla_for_initial_storage :
  sctx:Tz.mich_sym_ctxt ->
  Tz.mich_cut_info ->
  Tz.sym_image ->
  Tz.mich_v Tz.cc ->
  Tz.mich_f

val apply_inv_at_start :
  sctx:Tz.mich_sym_ctxt ->
  Tz.mich_cut_info ->
  Tz.sym_image ->
  Tz.mich_f ->
  Tz.mich_f

val apply_inv_at_block :
  sctx:Tz.mich_sym_ctxt ->
  Tz.mich_cut_info ->
  Tz.sym_image ->
  Tz.mich_f ->
  Tz.mich_f

val apply_inv_with_initial_storage :
  sctx:Tz.mich_sym_ctxt ->
  Tz.mich_cut_info ->
  Tz.sym_image ->
  Tz.mich_v Tz.cc ->
  Tz.mich_f ->
  Tz.mich_f

(******************************************************************************)
(******************************************************************************)
(* Verification Condition                                                     *)
(******************************************************************************)
(******************************************************************************)

(* Strongest Postcondition ****************************************************)

val gen_sp : Tz.sym_state -> Tz.mich_f -> Tz.mich_f

val gen_sp_from_ms : MState.t -> Tz.mich_f -> Tz.mich_f

(* Verification Condition *****************************************************)

val gen_query_vc : Inv.inv_map -> Tz.sym_state -> Tz.mich_f

val gen_query_vc_from_ms : Inv.inv_map -> MState.t -> Tz.mich_f

val gen_inductiveness_vc : Inv.inv_map -> Tz.sym_state -> Tz.mich_f

val gen_preservation_vc : Inv.cand -> MState.t -> Tz.mich_f

val gen_initial_inv_vc :
  Inv.inv_map -> Tz.mich_v Tz.cc -> Tz.sym_state -> Tz.mich_f

val gen_refute_vc : Tz.mich_v Tz.cc -> MState.t -> Tz.mich_f

val gen_precond_vc : Inv.cand -> MState.t -> Tz.mich_f

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

(* DEBUGGING FUNCTION *)
val debug_check_sat :
  Smt.Ctx.t ->
  Smt.Solver.t ->
  Tz.mich_f ->
  (Tz.mich_f * Smt.Solver.satisfiability * Smt.Model.t option) list
(* DEBUGGING FUNCTION *)

val is_cand_sat : Smt.Ctx.t -> Smt.Solver.t -> Inv.cand -> bool

val do_cand_sat_istrg :
  Smt.Ctx.t ->
  Smt.Solver.t ->
  Tz.mich_v Tz.cc ->
  Tz.sym_state ->
  Tz.r_mich_cut_info ->
  Inv.cand ->
  bool
