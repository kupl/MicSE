(* Verification Condition Generator (from BasicPath) *)
(* IT CONSIDERS VARIABLE RENAMING *)

(* SUGAR *)
module CPSet = Core.Set.Poly
module CPMap = Core.Map.Poly



type query_vc = {
  qvc_fml : ProverLib.Vlang.t;            (* verification condition which should be VALID. formula. *)
  qvc_cat : ProverLib.Bp.query_category;  (* query cateogry *)
  qvc_vtx : PreLib.Cfg.vertex;            (* the vertex-location where the query comes from *)
  qvc_bp  : ProverLib.Bp.t;               (* the basic-path which contains this query *)
}

type v_cond = {
  path_vc : ProverLib.Vlang.t;            (* verification condition which should be SATISFIABLE. formula. *)
  query_vcs : query_vc CPSet.t;           (* queries. see above explanation. *)
}

type v_cond_ingr = ProverLib.Inv.t -> v_cond  (* path-vc and query_vcs will be constructed using the given invariant candidate. *)



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

let sp_for_main_exit_vtx : sp_fold_acc -> ProverLib.Bp.basic_node -> sp_fold_acc
= let open ProverLib.Bp in
  let open ProverLib.Vlang in
  fun {sfa_name_env; sfa_str_post; sfa_queries} {glenv_ref; cfgvtx; inst} -> begin
  (* we assume that the main_exit_vtx comes with the statement "BI_assign (rhs-var, E_itself (lhs-var))" 
    and the rhs-var's type is same as the (operation list * storage) type.
    So, this function will add additional basic-node implicitly, "BI_assign (storage-var, E_cdr (rhs-var))"
  *)
  match inst with
  | BI_assign (t, v, _) ->
    let sp_acc' = sp {sfa_name_env; sfa_str_post; sfa_queries} {glenv_ref; cfgvtx; inst} in
    let storage_vtyp = TypeUtil.get_innertyp2 t |> Stdlib.snd in
    let last_inst : inst = BI_assign (storage_vtyp, !glenv_ref.gv_storage, Expr.V_cdr (V_var (t, v))) in
    sp sp_acc' {glenv_ref; cfgvtx; inst=last_inst}
  | _ -> Stdlib.failwith ("VcGen.sp_for_main_exit_vtx : match failed with the basic_node : " ^ (JsonRep.of_basic_node {glenv_ref; cfgvtx; inst} |> Yojson.Basic.pretty_to_string))
end (* function sp_for_main_exit_vtx end *)


(* renaming process performed here *)
let construct_verifier_vc : PreLib.Cfg.t -> ProverLib.Bp.t -> v_cond_ingr
= let open ProverLib.Bp in
  let open ProverLib.Vlang in
  fun cfg {entry_vtx; exit_vtx; content; appeared_vars} -> begin
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
    let sp_fold_result_orig : sp_fold_acc =
      (* originally, it just needs "sp" function, but the bad design in Cfg makes this procedure
        should check whether the basic-node's vertex is main-exit or not for every recursive call.
      *)
      (* "List.fold_left sp initv content" *)
      List.fold_left 
        (fun sf_acc bnode -> (if bnode.cfgvtx = cfg.main_exit then sp_for_main_exit_vtx else sp) sf_acc bnode)
        initv
        content
    in
    (* 1.5. Vlang Formula Optimization on "sp_fold_result_orig"
    *)
    let sp_fold_result : sp_fold_acc =
      let nopt = ProverLib.VlangUtil.NaiveOpt.run in (* sugar *)
      {sp_fold_result_orig with
        sfa_str_post = nopt sp_fold_result_orig.sfa_str_post;
        sfa_queries = List.map (fun ((pre, post), qc, vtx) -> ((nopt pre, nopt post), qc, vtx)) sp_fold_result_orig.sfa_queries;
      }
    in
    (* 2. make a space to insert invariant 
        In this implementation, we need to change the variable name in the exit_invariant.
    *)
    (fun {trx_inv; loop_inv} ->
      (* find invariant for entry-vtx *)
      let entry_inv : Formula.t =
        if entry_vtx = cfg.main_entry then trx_inv else
        PreLib.Cfg.t_map_find 
          ~errtrace:("VcGen.construct_verifier_vc : 2 : entry_inv : " ^ (Stdlib.string_of_int entry_vtx))
          loop_inv entry_vtx
      in
      (* find invariant for exit-vtx *)
      let exit_inv : Formula.t = 
        if exit_vtx = cfg.main_exit then trx_inv else
        (* There are no dedicated invariant for FAILWITH node. So we just put trx-inv instead. *)
        let is_exitvtx_failwith : bool =
          PreLib.Cfg.t_map_find
            ~errtrace:("VcGen.construct_verifier_vc : 2 : is_exitvtx_failwith : " ^ (Stdlib.string_of_int exit_vtx))
            cfg.vertex_info
            exit_vtx
          |> (function | Cfg_failwith _ -> true | _ -> false)
        in
        if is_exitvtx_failwith then trx_inv else
        (* If exit-vtx is not main-exit-vtx and not failwith-vtx, find loop invariant. *)
        let found_exit_inv =
          PreLib.Cfg.t_map_find 
          ~errtrace:("VcGen.construct_verifier_vc : 2 : found_exit_inv : exit_inv : " ^ (Stdlib.string_of_int exit_vtx))
          loop_inv exit_vtx
        in
        NameEnv.rename_fmla sp_fold_result.sfa_name_env found_exit_inv
      in
      (* construct a path verification-condition *)
      let pvc : ProverLib.Vlang.t = 
        Formula.VF_imply (
          Formula.VF_and [entry_inv; sp_fold_result.sfa_str_post],
          exit_inv
        )
        (*
        (* Formula Optimization inserted to enhance formula readability *)
        |> ProverLib.VlangUtil.NaiveOpt.run
        *)
      in
      (* construct a query verification-condition list *)
      let qvcl : (ProverLib.Vlang.t * ProverLib.Bp.query_category * PreLib.Cfg.vertex) list =
        List.map 
          (fun((pre_c, post_c), qc, vtx) ->
            let vc = 
              Formula.VF_imply (Formula.VF_and [entry_inv; pre_c], post_c)
              (*
              (* Formula Optimization inserted to enhance formula readability *)
              |> ProverLib.VlangUtil.NaiveOpt.run
              *)
            in
            (vc, qc, vtx)
          )
          sp_fold_result.sfa_queries
      in
      (* query_vcs type is changed. below code will convert qvcl to appropriate form. *)
      let qvcs : query_vc CPSet.t =
        List.fold_left
          (fun accs (qvc_fml, qvc_cat, qvc_vtx) ->
            CPSet.add accs { qvc_fml; qvc_cat; qvc_vtx; qvc_bp={entry_vtx; exit_vtx; content; appeared_vars}; }
          )
          CPSet.empty
          qvcl
      in
      {path_vc=pvc; query_vcs=qvcs}
    )
end (* function construct_verifier_vc end *)


let construct_initstg_vc : ProverLib.GlVar.Env.t ref -> PreLib.Cfg.t -> PreLib.Adt.data option -> (ProverLib.Inv.t -> ProverLib.Vlang.t)
= let open ProverLib.Vlang in
  fun glenv_ref cfg init_stg_opt -> begin
  match init_stg_opt with
  | None -> (fun _ -> Formula.VF_true)
  | Some stg -> begin
      (* get the vlang-type of "param-storage" variable first. *)
      let stg_vtyp : Ty.t = 
        PreLib.Cfg.t_map_find
          ~errtrace:("Prover.VcGen.construct_initstg_vc : Some stg : ps_vtyp")
          cfg.type_info
          PreLib.Cfg.param_storage_name
        |> PreLib.Mich.get_d
        |> TypeUtil.ty_of_michtyp
        |> TypeUtil.get_innertyp2
        |> Stdlib.snd
      in
      (* construct formula and hide it in (Inv.t -> Vlang.t) function. *)
      let stgvar : Expr.t = V_var (stg_vtyp, !glenv_ref.gv_storage) in
      let stg_vexpr : Expr.t = VlGen.create_expr_of_michdata stg stg_vtyp in
      (fun {trx_inv; loop_inv=_} ->
        let open Formula in
        (* Create the formula ((trxStorage = storage) -> trxInvariant). 
          It should be checked whether the formula is VALID before passing the invariant to validator. *)
        VF_imply (VF_eq (stgvar, stg_vexpr), trx_inv)
      )
    end
end (* function construct_initstg_vc end *)
