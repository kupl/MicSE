exception Not_Implemented_f of ProverLib.Vlang.t
exception Not_Implemented_e of ProverLib.Vlang.Expr.t

val smtsort_of_vlangtyp : ProverLib.Vlang.Ty.t -> ProverLib.Smt.ZSort.t
val sort_of_typt : Pre.Lib.Adt.typ -> ProverLib.Smt.ZSort.t
val smtexpr_of_compare : ProverLib.Vlang.Expr.t -> ProverLib.Vlang.Expr.t -> ProverLib.Smt.ZExpr.t
val smtexpr_of_vlangexpr : ProverLib.Vlang.Expr.t -> ProverLib.Smt.ZExpr.t
val smtexpr_of_vlangformula : ProverLib.Vlang.t -> ProverLib.Smt.ZExpr.t

val verify : ProverLib.Vlang.Formula.t -> ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option