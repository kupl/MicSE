(* Invariant *)

(* Sugar *)
module CPSet = Core.Set.Poly
module CPMap = Core.Map.Poly


type t = {
    trx_inv : Vlang.t CPSet.t;  (* Transaction invariant *)
    loop_inv : (int, Vlang.t CPSet.t) CPMap.t;  (* Loop invariant : loop-vertex -> Vlang formula *)
}


(* invgen_info should contain information about which variables can be used when generating invariants. 
    Query-Verification condition will convey a set of variables which appeared in that basic-path,
    so this datatype does not need to carry a set of variables appeared in each basicpaths.
*)
type invgen_info = {
    igi_stgcomp : Vlang.Component.t; (* storage's available vlang-expr (component) set *)
    igi_glvar_comp : Vlang.Component.t;  (* global variables. they are constnat in only one transaction execution (without storage) *)
    igi_loopv_set : PreLib.Cfg.vertex CPSet.t;  (* a vertex set contains every loop vertices *)
    igi_entryvtx : PreLib.Cfg.vertex; (* cfg entry vertex *)
    igi_exitvtx : PreLib.Cfg.vertex;  (* cfg exit vertex *)
    igi_avs_pre : (PreLib.Cfg.vertex, Vlang.Component.t) CPMap.t;  (* available variable set (available variable "before" execute the stmt) *)
}


let inv_true_gen : invgen_info -> t
=fun {igi_stgcomp=_; igi_glvar_comp=_; igi_loopv_set; igi_entryvtx=_; igi_exitvtx=_; igi_avs_pre=_} -> begin
  let open Vlang.Formula in
  { trx_inv = CPSet.singleton VF_true;
    loop_inv = CPSet.fold igi_loopv_set ~init:CPMap.empty ~f:(fun accm loopv -> PreLib.Cfg.t_map_add ~errtrace:("ProverLib.Inv.inv_true_gen") accm loopv (CPSet.singleton VF_true));
  }
end


(* In our blueprint, Invariant is used for a verifier, not refuter. 
  So it is easy to generate invgen_info from a contract using cfg only.
*)
let gen_invgen_info_for_single_contract_verification : Pre.Lib.Cfg.t -> invgen_info
=fun cfg -> begin
  let glenv : GlVar.Env.t = GlVar.Env.t_for_single_contract_verification in
  (* collect storage-related vlang expressions *)
  let (param_typ, strg_typ) : Vlang.Ty.t * Vlang.Ty.t =
    PreLib.Cfg.t_map_find
      ~errtrace:("ProverLib.Inv.gen_invgen_info_for_single_contract_verification : param_storage")
      cfg.type_info
      PreLib.Cfg.param_storage_name
    |> Vlang.TypeUtil.ty_of_mty
    |> Vlang.TypeUtil.get_innertyp2
  in
  let strg_var : Vlang.Expr.t = V_var (strg_typ, glenv.gv_storage) in 
  let strg_comp : Vlang.Component.t = Vlang.Component.gather strg_var in
  (* global variables (except storage) cannot be used in transaction invariant.
      But they are behaves like a constant, so it can be used in generating loop invariant.
  *)
  let basic_glvar_comp : Vlang.Component.t = CPSet.of_list [
    (param_typ, Vlang.Expr.V_var (param_typ, glenv.gv_param)) |> Vlang.Component.comp_of_vexpr_t;
    (* strg_typ, Vlang.Expr.V_var (strg_typ, glenv.gv_storage); *)
    (Vlang.Ty.T_mutez, Vlang.Expr.V_var (Vlang.Ty.T_mutez, glenv.gv_amount)) |> Vlang.Component.comp_of_vexpr_t;
    (Vlang.Ty.T_mutez, Vlang.Expr.V_var (Vlang.Ty.T_mutez, glenv.gv_balance)) |> Vlang.Component.comp_of_vexpr_t;
    (Vlang.Ty.T_address, Vlang.Expr.V_var (Vlang.Ty.T_address, glenv.gv_sender)) |> Vlang.Component.comp_of_vexpr_t;
    (Vlang.Ty.T_address, Vlang.Expr.V_var (Vlang.Ty.T_address, glenv.gv_source)) |> Vlang.Component.comp_of_vexpr_t;
  ]
  and loopvtx_set : int CPSet.t = begin 
    CPMap.fold 
      ~init:(CPSet.empty) 
      cfg.vertex_info 
      ~f:(fun ~key:(v) ~data:(stmt) accset -> 
        match stmt with
        | Cfg_loop _ | Cfg_loop_left _ | Cfg_map _ | Cfg_iter _ -> CPSet.add accset v
        | _ -> accset
      )
  end
  and av_pre : (PreLib.Cfg.vertex, Vlang.Component.t) CPMap.t = 
    (* "premap" : collected available-variable information *)
    let premap = CPMap.map (Pre.Analyzer.AvailVar.run cfg) ~f:(fun (pre,_) -> Pre.Analyzer.AvailVar.abs_set_concr pre) in
    (* "premap" again : remove the variable name "PreLib.Cfg.param_storage_name" 
        "igi_avs_pre" is used for invariant synthesis, so the removal of global variables does not harm anything.
    *)
    let premap = CPMap.map premap ~f:(fun s -> CPSet.filter s ~f:(fun v -> v <> PreLib.Cfg.param_storage_name)) in
    (* convert variable to component, using "Cfg.type_info", "Vlang.TypeUtil.ty_of_mty", and "Vlang.Component.comp_of_vexpr_t" *)
    (* "get_vty" is a utility function to convert michelson variable to vlang type. *)
    let get_vty : string -> Vlang.Ty.t
    =fun v -> begin
      let mty = PreLib.Cfg.t_map_find ~errtrace:("Inv.gen_invgen_info_for_single_contract_verification : av_pre : get_mty") cfg.type_info v in
      Vlang.TypeUtil.ty_of_mty mty
    end in
    CPMap.map premap ~f:(fun s -> CPSet.map s ~f:(fun v -> Vlang.Component.comp_of_vexpr (Vlang.Expr.V_var (get_vty v, v))))
  in
  {igi_stgcomp=strg_comp; igi_glvar_comp=basic_glvar_comp; igi_loopv_set=loopvtx_set; igi_entryvtx=cfg.main_entry; igi_exitvtx=cfg.main_exit; igi_avs_pre=av_pre;}
end (* function gen_invgen_info_for_single_contract_verification end *)

let strengthen_worklist : (t * t CPSet.t * t CPSet.t) -> t CPSet.t
=fun ({trx_inv=cur_trxinv; loop_inv=cur_loopinv}, inv_wl, invs_collected) -> begin
  let newly_generated_inv : t CPSet.t = 
  CPSet.map
    inv_wl
    ~f:(
      fun {trx_inv; loop_inv} ->
      let new_trxinv = CPSet.union trx_inv cur_trxinv in
      let new_loopinv =
        CPMap.mapi
          loop_inv
          ~f:(
            fun ~key ~data -> (* key = loop vertex; data = specific loop invariant for one vertex *)
            try
              let curloopinv_i : Vlang.t CPSet.t = 
                PreLib.Cfg.t_map_find
                  ~errtrace:("Prover.Inv.strengthen_worklist : curloopinv_i : " ^ (Stdlib.string_of_int key))
                  cur_loopinv
                  key
              in
              CPSet.union data curloopinv_i
            with
              (* "PreLib.Cfg.t_map_find" emits error when the "key" is not a loop-vertex but just a "failwith" vertex.
                  If this case happens, do not update it.
                  According to the value "exit_inv" in "Prover.VcGen.construct_verifier_vc" implementation,
                  every invariant for failwith vertex will be ignored.
              *)
              | PreLib.Cfg.Exn_Cfg _ -> data           
          )
      in
      {trx_inv=new_trxinv; loop_inv=new_loopinv}
    )
  in
  (* remove already-used invariants with "CPSet.diff" *)
  CPSet.diff newly_generated_inv invs_collected
end (* function strengthen_worklist end *)

(* "inv_to_formula" just connect invariant set using "VF_and". *)
let inv_to_formula : Vlang.t CPSet.t -> Vlang.t
=fun fset -> begin 
  VF_and (CPSet.to_list fset)
end (* function inv_to_formula end *)
