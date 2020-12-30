(* Vlang generator (from Cfg Stmt & Cfg Expression). 
    This module does not consider any variable renaming issues *)

val expr_of_cfgexpr : ProverLib.GlVar.Env.t -> PreLib.Cfg.expr -> ProverLib.Vlang.Expr.t
