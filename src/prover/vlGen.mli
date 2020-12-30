(* Vlang generator (from Cfg Stmt & Cfg Expression). 
    This module does not consider any variable renaming issues *)

exception InvalidConversion_Expr of PreLib.Cfg.expr

val expr_of_cfgexpr : ProverLib.GlVar.Env.t -> PreLib.Cfg.t -> PreLib.Cfg.expr -> ProverLib.Vlang.Expr.t
