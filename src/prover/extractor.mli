exception Error of string
exception InvalidExtraction of (Pre.Lib.Cfg.stmt * string)

module CPSet = Core.Set.Poly

(************************************************)
(************************************************)

module CFGUtils : sig
  exception Error of string

  val read_var_type : Pre.Lib.Cfg.t -> Pre.Lib.Cfg.ident -> Pre.Lib.Mich.typ Pre.Lib.Mich.t
  val read_exit_var : Pre.Lib.Cfg.t -> Pre.Lib.Cfg.ident
end

(************************************************)

module BPUtils : sig
  type foldingType = (Pre.Lib.Cfg.vertex * ProverLib.Bp.inst) option

  exception Error of string

  val create_loop_bp : ProverLib.Bp.t -> Pre.Lib.Cfg.vertex -> (ProverLib.Bp.t * ProverLib.Bp.t * ProverLib.Bp.t)
  val read_loc_of_check : Pre.Lib.Cfg.t -> Pre.Lib.Cfg.vertex -> ProverLib.Bp.loc
  val update_current_bp : ProverLib.Bp.t -> foldingType -> ProverLib.Bp.t
end

(************************************************)

module InstUtils : sig
  val create_inst_assume : Pre.Lib.Cfg.vertex -> ProverLib.Bp.cond -> BPUtils.foldingType
  val create_inst_assert : Pre.Lib.Cfg.vertex -> ProverLib.Bp.cond -> ProverLib.Bp.loc -> ProverLib.Bp.category -> BPUtils.foldingType
  val create_inst_assert_basic_prop : Pre.Lib.Cfg.vertex -> Pre.Lib.Cfg.expr -> Pre.Lib.Mich.typ Pre.Lib.Mich.t -> BPUtils.foldingType
  val create_inst_assign : Pre.Lib.Cfg.vertex -> Pre.Lib.Cfg.ident -> Pre.Lib.Cfg.expr -> BPUtils.foldingType
  val create_inst_skip : Pre.Lib.Cfg.vertex -> BPUtils.foldingType
end

(************************************************)

val translate : Pre.Lib.Cfg.t -> Pre.Lib.Cfg.vertex -> ProverLib.Bp.t -> (ProverLib.Bp.t list * Pre.Lib.Cfg.vertex CPSet.t)

val extract : Pre.Lib.Cfg.t -> ProverLib.Bp.lst