exception Not_Implemented_f of ProverLib.Vlang.t
exception Not_Implemented_e of ProverLib.Vlang.Expr.t

val sort_of_typt : ProverLib.Smt.typ -> ProverLib.Smt.z_sort

val smtsort_of_vlangtyp : ProverLib.Vlang.Ty.t -> ProverLib.Smt.z_sort
val smtexpr_of_compare : ProverLib.Vlang.Expr.t -> ProverLib.Vlang.Expr.t -> ProverLib.Smt.z_expr
val smtexpr_of_vlangexpr : ProverLib.Vlang.Expr.t -> ProverLib.Smt.z_expr
val smtexpr_of_vlangformula : ProverLib.Vlang.t -> ProverLib.Smt.z_expr
