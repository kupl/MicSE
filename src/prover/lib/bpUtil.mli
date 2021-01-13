module CPSet = Core.Set.Poly
module CPMap = Core.Map.Poly


(* "bp_substitution" 
    [INPUT]
    - Basicpath
    [OUTPUT]
    - Substitution Environment (variable -> Vlang.Expr.t)
    [Alg]
      Env <- Empty Map
      for every instruction in basicpath,
        if instruction = BI_assign (v, e)
          e' <- SUBSTITUTE (e, Env)
          Env[v] = e'
        else
          pass
      return Env
*)
val bp_substitution : Bp.t -> ((Vlang.Ty.t * PreLib.Cfg.ident), Vlang.Expr.t) CPMap.t

val collect_components : ((Vlang.Ty.t * PreLib.Cfg.ident), Vlang.Expr.t) CPMap.t -> Vlang.Component.t
