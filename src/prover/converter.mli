open ProverLib

type typ_map = (Vlang.var, Vlang.typ) Cfg.CPMap.t

(************************************************)
(************************************************)

val convert : Bp.t -> Cfg.t -> Vlang.t

val sp : (Vlang.t * Vlang.t * typ_map) -> Bp.inst -> (Vlang.t * Vlang.t * typ_map)

val create_rename_var : Vlang.var -> Vlang.var

val create_rewrite_formula : Vlang.var -> Vlang.var -> Vlang.v_formula -> Vlang.v_formula

val create_rewrite_exp : Vlang.var -> Vlang.var -> Vlang.v_exp -> Vlang.v_exp

val create_convert_data : Vlang.data -> Vlang.typ -> Vlang.v_exp

val create_convert_exp : Vlang.exp -> Vlang.typ -> Vlang.v_exp
