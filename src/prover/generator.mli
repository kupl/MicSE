open ProverLib

(************************************************)
(************************************************)

val apply : Inv.Map.t -> Bp.t list -> Bp.t list

val generate : Bp.lst -> Pre.Lib.Cfg.t -> Inv.Map.t list -> Inv.Map.t list

val read_param_storage : Pre.Lib.Cfg.t -> Vlang.v_obj

val create_initial_worklist : Bp.lst -> Inv.WorkList.t