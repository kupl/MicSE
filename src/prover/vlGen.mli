(* Vlang generator (from Cfg Stmt & Cfg Expression). 
    This module does not consider any variable renaming issues *)

exception InvalidConversion_Expr of PreLib.Cfg.expr

val read_type_cfgvar : PreLib.Cfg.t -> PreLib.Cfg.ident -> ProverLib.Vlang.typ

val create_var_of_cfgvar : ProverLib.GlVar.Env.t ref -> PreLib.Cfg.t -> PreLib.Cfg.ident -> ProverLib.Vlang.Expr.t

val expr_of_cfgexpr : ProverLib.GlVar.Env.t ref -> PreLib.Cfg.t -> PreLib.Cfg.expr -> ProverLib.Vlang.Expr.t
