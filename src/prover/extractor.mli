open ProverLib

(************************************************)
(************************************************)

val extract : Cfg.t -> (Bp.t list * Cfg.vertex list)

val translate : Bp.t -> Cfg.vertex -> Cfg.t -> Bp.t list