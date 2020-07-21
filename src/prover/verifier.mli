open ProverLib

(************************************************)
(************************************************)

val verify : Vlang.t -> bool

val sort_of_typt : Vc.typ -> Vc.z_sort

val sort_of_inner_type : Vc.typ -> Vc.z_sort list

val create_convert_vformula : Vlang.v_formula -> Vc.z_expr

val create_convert_vexp : Vlang.v_exp -> Vc.z_expr