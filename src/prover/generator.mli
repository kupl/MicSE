open ProverLib

(************************************************)
(************************************************)

val initial_inv_worklist : Inv.vertex list -> Inv.vertex list -> Inv.WorkList.t

val apply : Inv.Map.t -> Bp.t list -> Bp.t list

val generate : Bp.raw_t_list -> Pre.Lib.Cfg.t -> Bp.t list

val update_bp : Inv.Map.t -> Bp.t -> Bp.t