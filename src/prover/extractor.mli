open ProverLib

(************************************************)
(************************************************)

val extract : Cfg.t -> Bp.raw_t_list

val translate : Bp.t -> Cfg.vertex -> Cfg.t -> Bp.t list