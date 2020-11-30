exception Error of string

exception InvalidExtraction of (Pre.Lib.Cfg.stmt * string)

(************************************************)
(************************************************)

val loop_inv_vtx : (ProverLib.Bp.vertex list) ref

val exit_var : (ProverLib.Bp.var option) ref
val read_exit_var : Pre.Lib.Cfg.t -> unit

(************************************************)

val update_current_bp : ProverLib.Bp.t -> (ProverLib.Bp.vertex * ProverLib.Bp.inst) option -> ProverLib.Bp.t

val create_bp_of_branch : ProverLib.Bp.t -> ProverLib.Bp.vertex -> ProverLib.Bp.cond -> (ProverLib.Bp.t * ProverLib.Bp.t)
val create_bp_of_loop : ProverLib.Bp.t -> ProverLib.Bp.vertex -> (ProverLib.Bp.t * ProverLib.Bp.t * ProverLib.Bp.t)
val create_basic_safety_property : ProverLib.Bp.vertex -> ProverLib.Bp.exp -> ProverLib.Bp.typ -> (ProverLib.Bp.vertex * ProverLib.Bp.inst) option

val read_loc_of_check : Pre.Lib.Cfg.t -> ProverLib.Bp.vertex -> ProverLib.Bp.loc 

(************************************************)

val translate : ProverLib.Bp.t -> ProverLib.Bp.vertex -> Pre.Lib.Cfg.t -> ProverLib.Bp.t list
val translate_search_normal : Pre.Lib.Cfg.t -> ProverLib.Bp.t -> (ProverLib.Bp.edge * ProverLib.Bp.vertex) -> ProverLib.Bp.t list -> ProverLib.Bp.t list
val translate_search_branch : Pre.Lib.Cfg.t -> (ProverLib.Bp.t * ProverLib.Bp.t) -> (ProverLib.Bp.edge * ProverLib.Bp.vertex) -> ProverLib.Bp.t list -> ProverLib.Bp.t list

val extract : Pre.Lib.Cfg.t -> ProverLib.Bp.lst