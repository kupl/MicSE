open ProverLib

(************************************************)
(************************************************)

val extract : Pre.Lib.Cfg.t -> Bp.raw_t_list

val translate : Bp.t -> Bp.vertex -> Pre.Lib.Cfg.t -> Bp.t list

val create_basic_safety_property : Bp.vertex -> Bp.exp -> Bp.typ -> Bp.inst option

val update_current_bp : Bp.t -> Bp.inst option -> Bp.t