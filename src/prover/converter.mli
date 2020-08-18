open ProverLib

type object_typ =
  | Mutez_Map
  | Mutez
  
(************************************************)
(************************************************)

val type_map : (Pre.Lib.Cfg.ident, Pre.Lib.Cfg.typ) Pre.Lib.Cfg.CPMap.t ref

val convert : Bp.t -> Pre.Lib.Cfg.t -> (Vlang.t * Query.t list)

val sp : (Vlang.t * Query.t list) -> Bp.inst -> (Vlang.t * Query.t list)

val create_rename_var : Vlang.var -> Vlang.var

val create_rewrite_formula : Vlang.var -> Vlang.var -> Vlang.v_formula -> Vlang.v_formula

val create_rewrite_exp : Vlang.var -> Vlang.var -> Vlang.v_exp -> Vlang.v_exp

val create_convert_data : Vlang.data -> Vlang.typ -> Vlang.v_exp

val create_convert_cond : Bp.cond -> Vlang.v_formula

val create_convert_exp : Vlang.exp -> Vlang.typ -> Vlang.v_exp
