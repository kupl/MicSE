open ProverLib

(************************************************)
(************************************************)

val apply : Inv.Map.t -> Bp.t list -> Bp.t list

val generate : Bp.raw_t_list -> Pre.Lib.Cfg.t -> Inv.Map.t list

val read_param_storage : Pre.Lib.Cfg.t -> Vlang.v_obj

val create_initial_worklist : Inv.vertex list -> Inv.vertex list -> Inv.WorkList.t