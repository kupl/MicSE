(* Invariant *)

(* Sugar *)
module CPSet = Core.Set.Poly
module CPMap = Core.Map.Poly


type t = {
    trx_inv : Vlang.t;  (* Transaction invariant *)
    loop_inv : (int, Vlang.t) CPMap.t;  (* Loop invariant : loop-vertex -> Vlang formula *)
}


(* invgen_info should contain information about which variables can be used when generating invariants. 
    Query-Verification condition will convey a set of variables which appeared in that basic-path,
    so this datatype does not need to carry a set of variables appeared in each basicpaths.
*)
type invgen_info = {
    igi_stgcomp : Vlang.Component.t; (* storage's available vlang-expr (component) set *)
    igi_glvar_set : (Vlang.Ty.t * Vlang.Expr.t) CPSet.t;  (* global variables. they are constnat in only one transaction execution (without storage) *)
    igi_loopv_set : PreLib.Cfg.vertex CPSet.t;  (* a vertex set contains every loop vertices *)
    igi_entryvtx : PreLib.Cfg.vertex; (* cfg entry vertex *)
    igi_exitvtx : PreLib.Cfg.vertex;  (* cfg exit vertex *)
}


let inv_true_gen : invgen_info -> t
=fun {igi_stgcomp=_; igi_glvar_set=_; igi_loopv_set; igi_entryvtx=_; igi_exitvtx=_} -> begin
  let open Vlang.Formula in
  { trx_inv = VF_true;
    loop_inv = CPSet.fold igi_loopv_set ~init:CPMap.empty ~f:(fun accm loopv -> PreLib.Cfg.t_map_add ~errtrace:("ProverLib.Inv.inv_true_gen") accm loopv (VF_true));
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
  let basic_glvar_set : (Vlang.Ty.t * Vlang.Expr.t) CPSet.t = CPSet.of_list [
    param_typ, Vlang.Expr.V_var (param_typ, glenv.gv_param);
    (* strg_typ, Vlang.Expr.V_var (strg_typ, glenv.gv_storage); *)
    Vlang.Ty.T_mutez, Vlang.Expr.V_var (Vlang.Ty.T_mutez, glenv.gv_amount);
    Vlang.Ty.T_mutez, Vlang.Expr.V_var (Vlang.Ty.T_mutez, glenv.gv_balance);
    Vlang.Ty.T_address, Vlang.Expr.V_var (Vlang.Ty.T_address, glenv.gv_sender);
    Vlang.Ty.T_address, Vlang.Expr.V_var (Vlang.Ty.T_address, glenv.gv_source);
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
  end in
  {igi_stgcomp=strg_comp; igi_glvar_set=basic_glvar_set; igi_loopv_set=loopvtx_set; igi_entryvtx=cfg.main_entry; igi_exitvtx=cfg.main_exit;}
end (* function gen_invgen_info_for_single_contract_verification end *)

let strengthen_worklist : (t * t CPSet.t) -> t CPSet.t
= let open Vlang in
  fun ({trx_inv=cur_trxinv; loop_inv=cur_loopinv}, inv_wl) -> begin
  CPSet.map
    inv_wl
    ~f:(
      fun {trx_inv; loop_inv} ->
      let new_trxinv = Formula.VF_and [trx_inv; cur_trxinv] in
      let new_loopinv =
        CPMap.mapi
          loop_inv
          ~f:(
            fun ~key ~data -> (* key = loop vertex; data = specific loop invariant for one vertex *)
            let curloopinv_i : Vlang.t = 
              PreLib.Cfg.t_map_find
                ~errtrace:("Prover.Inv.strengthen_worklist : curloopinv_i : " ^ (Stdlib.string_of_int key))
                cur_loopinv
                key
            in
            Formula.VF_and [data; curloopinv_i]
          )
      in
      {trx_inv=new_trxinv; loop_inv=new_loopinv}
    )
end
