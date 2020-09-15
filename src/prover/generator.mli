open ProverLib

(************************************************)
(************************************************)

val apply : Inv.Map.t -> Bp.t list -> Bp.t list

val generate : Bp.raw_t_list -> Pre.Lib.Cfg.t -> Inv.Map.t list

val read_param_storage : Pre.Lib.Cfg.t -> Vlang.v_exp * Vlang.typ

val create_initial_worklist : Pre.Lib.Cfg.t -> Inv.vertex list -> Inv.vertex list -> Inv.WorkList.t

val create_formula_from_param_storage : Pre.Lib.Cfg.t -> Vlang.v_formula