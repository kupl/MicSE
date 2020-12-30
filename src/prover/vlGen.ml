(* Vlang generator (from Cfg Stmt & Cfg Expression). 
    This module does not consider any variable renaming issues *)

let expr_of_cfgexpr : ProverLib.GlVar.Env.t -> PreLib.Cfg.expr -> ProverLib.Vlang.Expr.t
=fun _ _ -> V_now (* TODO. PLACEHOLDER *)
