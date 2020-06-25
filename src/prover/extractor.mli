open ProverLib

(************************************************)
(************************************************)

val apply : Bp.inv_map -> Bp.t list -> Bp.t list

val extract : Cfg.t -> (Bp.t list * Cfg.vertex list)

val translate : Bp.t -> Cfg.vertex -> Cfg.t -> Bp.t list