(* Vc: Verification condition manager *)

exception VcError of string

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

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
  Smt.Ctx.t ->
  Tz.mich_t * Smt.Expr.t ->
  Tz.mich_t * Smt.Expr.t ->
  Smt.Expr.t

  val cv_mf : Smt.Ctx.t -> Tz.mich_f -> Smt.Formula.t
end
