(* Verification Condition Generator (from BasicPath) *)
(* IT CONSIDERS VARIABLE RENAMING *)

(* SUGAR *)
module CPSet = Core.Set.Poly
module CPMap = Core.Map.Poly

type v_cond = {
  path_vc : ProverLib.Vlang.t;
  query_vcs : (ProverLib.Vlang.t * ProverLib.Bp.query_category * PreLib.Cfg.vertex) CPSet.t;
}

type v_cond_ingr = ProverLib.Inv.t -> v_cond



module NameEnv = struct
  type t = (string, string) CPMap.t

  let new_var : string -> string = fun x -> x ^ "#"

  let get : t -> string -> string
  =fun env v -> begin
    try
      PreLib.Cfg.t_map_find ~errtrace:("VcGen.NameEnv.get : " ^ v) env v
    with
    | Stdlib.Failure _ -> v
  end

  let update : t -> string -> t
  =fun env v -> begin
    CPMap.update env v ~f:(function | None -> v | Some vv -> new_var vv)
  end
  
  (* For convenience, "rename_expr" iterates for all variable entries in environment,
    which is inefficient. One easy implementation to deal with this problem is
    finding all variable in expression/formula and iterates only for found variables,
    but writing a long match-with expression to deal with Vlang.Expr.t is too boring
    task than enduring current inefficient implementation's performance. [TODO]
  *)
  let rename_fmla : t -> ProverLib.Vlang.t -> ProverLib.Vlang.t
  = let open ProverLib.Vlang in
    fun env fmla -> begin
    CPMap.fold 
      env 
      ~init:fmla 
      ~f:(fun ~key ~data fmla_acc -> 
        Renaming.var_in_expr_formula key data fmla_acc
      )
  end

  let rename_expr : t -> ProverLib.Vlang.Expr.t -> ProverLib.Vlang.Expr.t
  = let open ProverLib.Vlang in
    fun env expr -> begin
      CPMap.fold
        env
        ~init:expr
        ~f:(fun ~key ~data expr_acc ->
          Renaming.var_in_expr key data expr_acc
        )
  end

end (* module NameEnv end *)


type pp_cond = (ProverLib.Vlang.t * ProverLib.Vlang.t) (* precondition & postcondition *)


type sp_fold_acc = {
  sfa_name_env : (string, string) CPMap.t;  (* name environment to introduce new variable name *)
  sfa_str_post : ProverLib.Vlang.t; (* strongest postcondition *)
  sfa_queries : (pp_cond * ProverLib.Bp.query_category * PreLib.Cfg.vertex) list; (* query list *)
}

(* strooooooongest postcondition *)
let sp : sp_fold_acc -> ProverLib.Bp.basic_node -> sp_fold_acc
= let open ProverLib.Bp in
  let open ProverLib.Vlang in
  fun {sfa_name_env; sfa_str_post; sfa_queries} {glenv_ref=_; cfgvtx; inst} -> begin
  match inst with
  | BI_assume fml -> (
      let fml' = NameEnv.rename_fmla sfa_name_env fml in
      {sfa_name_env; sfa_str_post=(VF_and [sfa_str_post; fml']); sfa_queries}
    )
  | BI_assert (fml, qc) -> (
      let fml' = NameEnv.rename_fmla sfa_name_env fml in
      {sfa_name_env; sfa_str_post; sfa_queries=(((sfa_str_post, fml'), qc, cfgvtx)::sfa_queries)}
    )
  | BI_assign (t, v, e) -> (
      let e' = NameEnv.rename_expr sfa_name_env e in
      let new_nenv = NameEnv.update sfa_name_env v in
      let v' = NameEnv.get new_nenv v in
      let newfml = Formula.VF_eq (V_var (t, v'), e') in
      {sfa_name_env=new_nenv; sfa_str_post=(VF_and [sfa_str_post; newfml]); sfa_queries}
    )
  | BI_skip -> {sfa_name_env=sfa_name_env; sfa_str_post=sfa_str_post; sfa_queries=sfa_queries;} (* Nothing happen *)
end (* function sp end *)

(* renaming process performed here *)
let construct_verifier_vc : PreLib.Cfg.t -> ProverLib.Bp.t -> v_cond_ingr
= let open ProverLib.Vlang in
  fun cfg {entry_vtx; exit_vtx; content} -> begin
    (* 1. evaluate strongest postcondition 
        - name_env : environment for naming
        - str_post : strongest postcondition. 
          - ((entry-inv /\ str_post) -> exit_inv) will be used to check validity of the given invariant.
        - queries : (precond, postcond) list 
          - it should be converted to ((entry-inv /\ precond) -> postcond) using invariant later.
    *)
    let initv : sp_fold_acc = {
      sfa_name_env = CPMap.empty;
      sfa_str_post = Formula.VF_true;
      sfa_queries = [];
    } in
    let sp_fold_result : sp_fold_acc =
      List.fold_left sp initv content
    in
    (* 2. make a space to insert invariant 
        In this implementation, we need to change the variable name in the exit_invariant.
    *)
    (fun {trx_inv; loop_inv} ->
      let entry_inv : Formula.t =
        if entry_vtx = cfg.main_entry then trx_inv else
        PreLib.Cfg.t_map_find 
          ~errtrace:("VcGen.construct_verifier_vc : 2 : entry_inv : " ^ (Stdlib.string_of_int entry_vtx))
          loop_inv entry_vtx
      in
      let exit_inv : Formula.t = 
        if exit_vtx = cfg.main_exit then trx_inv else
        let found_exit_inv = 
          PreLib.Cfg.t_map_find 
          ~errtrace:("VcGen.construct_verifier_vc : 2 : exit_inv : " ^ (Stdlib.string_of_int entry_vtx))
          loop_inv exit_vtx
        in
        NameEnv.rename_fmla sp_fold_result.sfa_name_env found_exit_inv
      in
      let pvc : ProverLib.Vlang.t = 
        Formula.VF_imply (
          Formula.VF_and [entry_inv; sp_fold_result.sfa_str_post],
          exit_inv
        ) 
      in
      let qvcl : (ProverLib.Vlang.t * ProverLib.Bp.query_category * PreLib.Cfg.vertex) list =
        List.map 
          (fun((pre_c, post_c), qc, vtx) ->
            let vc = Formula.VF_imply (
              Formula.VF_and [entry_inv; pre_c],
              post_c
            ) in
            (vc, qc, vtx)
          )
          sp_fold_result.sfa_queries
      in
      {path_vc=pvc; query_vcs=(CPSet.of_list qvcl)}
    )
end (* function construct_verifier_vc end *)
