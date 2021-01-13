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

  Vlang-Expressions in this environment will be used in invariant generation procedure.
*)
let bp_substitution : Bp.t -> ((Vlang.Ty.t * PreLib.Cfg.ident), Vlang.Expr.t) CPMap.t
= let open Bp in
  let env_subst : ((Vlang.Ty.t * PreLib.Cfg.ident), Vlang.Expr.t) CPMap.t -> Vlang.Expr.t -> Vlang.Expr.t
  =fun env e -> begin
    let varsubs : Vlang.Expr.t -> Vlang.Expr.t
    = (function
      | V_var (t, v) -> CPMap.find env (t, v) |> (function | None -> Vlang.Expr.V_var (t, v) | Some e -> e)
      | _ as e -> e
      )
    in
    Vlang.RecursiveMappingExprTemplate.map_expr_inner ~expr_f:varsubs e
  end in (* internal function env_subst end *)
  fun bp -> begin
  List.fold_left
    ( fun acc_env bn ->
      match bn.inst with
      | BI_assign (t, v, e) -> 
        let e' = env_subst acc_env e |> VlangUtil.FormulaUtils.optimize_var in
        CPMap.update acc_env (t, v) ~f:(function | _ -> e')
      | BI_assert _ | BI_assume _ | BI_skip -> acc_env
    )
    CPMap.empty
    bp.content
end (* function bp_substitution end *)


let collect_components : ((Vlang.Ty.t * PreLib.Cfg.ident), Vlang.Expr.t) CPMap.t -> Vlang.Component.t
=fun env -> begin
  let (non_dup_ids, _) : (Vlang.Ty.t * PreLib.Cfg.ident) CPSet.t * Vlang.Expr.t CPSet.t =
    CPMap.fold 
      env 
      ~init:(CPSet.empty, CPSet.empty) 
      ~f:(fun ~key ~data (acc_nid, acc_eset) -> 
        if CPSet.mem acc_eset data 
        then (acc_nid, acc_eset)
        else (CPSet.add acc_nid key, CPSet.add acc_eset data)
      )
  in
  CPSet.map non_dup_ids ~f:(fun (t, id) -> Vlang.Component.comp_of_vexpr_t (t, V_var (t,id)))
end (* function collect_components end *)
