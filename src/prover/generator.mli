open ProverLib

(************************************************)
(************************************************)

val generate : Bp.raw_t_list -> Pre.Lib.Cfg.t -> Bp.t list

val update_bp : Bp.inv_map -> Bp.t -> Bp.t