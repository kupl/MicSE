open ProverLib

type object_typ =
  | Mutez_Map
  | Mutez
  
(************************************************)
(************************************************)

val type_map : (Cfg.ident, Cfg.typ) Cfg.CPMap.t ref

val convert : Bp.t -> Cfg.t -> Vlang.t

val sp : (Vlang.t * Vlang.t) -> Bp.inst -> (Vlang.t * Vlang.t)

val create_rename_var : Vlang.var -> Vlang.var

val create_rewrite_formula : Vlang.var -> Vlang.var -> Vlang.v_formula -> Vlang.v_formula

val create_rewrite_exp : Vlang.var -> Vlang.var -> Vlang.v_exp -> Vlang.v_exp

val create_convert_data : Vlang.data -> Vlang.typ -> Vlang.v_exp

val create_convert_cond : Bp.cond -> Vlang.v_formula

val create_convert_exp : Vlang.exp -> Vlang.typ -> Vlang.v_exp
