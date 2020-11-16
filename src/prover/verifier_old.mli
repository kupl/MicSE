(*
open ProverLib

(************************************************)
(************************************************)

val verify : Vlang.t -> Pre.Lib.Cfg.t -> bool * (Smt.z_expr * Smt.z_expr) option

val sort_of_typt : Smt.typ -> Smt.z_sort

val sort_of_inner_type : Smt.typ -> Smt.z_sort list

val create_convert_vformula : Vlang.v_formula -> Smt.z_expr

val create_convert_vobj : Vlang.v_obj -> Smt.z_expr

val create_param_storage_from_model : Smt.model -> Pre.Lib.Cfg.t -> (Smt.z_expr * Smt.z_expr) option
*)