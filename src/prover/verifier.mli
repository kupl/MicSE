open ProverLib

(************************************************)
(************************************************)

val verify : Vlang.t -> bool

val sort_of_typt : Smt.typ -> Smt.z_sort

val sort_of_inner_type : Smt.typ -> Smt.z_sort list

val create_convert_vformula : Vlang.v_formula -> Smt.z_expr

val create_convert_vexp : Vlang.v_exp -> Smt.z_expr