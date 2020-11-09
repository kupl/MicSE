open ProverLib

(************************************************)
(************************************************)

val loop_inv_vtx : (Bp.vertex list) ref

val exit_var : (Bp.var option) ref

val extract : Pre.Lib.Cfg.t -> Bp.lst

val translate : Bp.t -> Bp.vertex -> Pre.Lib.Cfg.t -> Bp.t list

val translate_search_normal : Pre.Lib.Cfg.t -> Bp.t -> (Bp.edge * Bp.vertex) -> Bp.t list -> Bp.t list

val translate_search_branch : Pre.Lib.Cfg.t -> (Bp.t * Bp.t) -> (Bp.edge * Bp.vertex) -> Bp.t list -> Bp.t list

val create_bp_of_branch : Bp.t -> Bp.vertex -> Bp.cond -> (Bp.t * Bp.t)

val create_bp_of_loop : Bp.t -> Bp.vertex -> (Bp.t * Bp.t * Bp.t)

val create_basic_safety_property : Bp.vertex -> Bp.exp -> Bp.typ -> (Bp.vertex * Bp.inst) option

val read_loc_of_check : Pre.Lib.Cfg.t -> Bp.vertex -> Bp.loc 

val update_current_bp : Bp.t -> (Bp.vertex * Bp.inst) option -> Bp.t