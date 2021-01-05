(* Invariant *)

(* Sugar *)
module CPSet = Core.Set.Poly
module CPMap = Core.Map.Poly


type t = {
    trx_inv : Vlang.t;  (* Transaction invariant *)
    loop_inv : (int, Vlang.t) CPMap.t;  (* Loop invariant : loop-vertex -> Vlang formula *)
}


(* invgen_info should contain information about which variables can be used when generating invariants. 
    In current implementation, we will remain loop-invariant's component empty. 
    Available variables are enough to generate invariant.
*)
type invgen_info = {
    igi_trx : Vlang.Component.t; (* available vlang-expr (component) set *)
    (* "igi_loop" : loop-vertex -> available vlang-expr set * (component-set (empty in current implementation)) *)
    igi_loop : (int, (Vlang.Ty.t * Vlang.Expr.t) CPSet.t * Vlang.Component.t CPSet.t) CPMap.t;
    igi_entryvtx : PreLib.Cfg.vertex; (* cfg entry vertex *)
    igi_exitvtx : PreLib.Cfg.vertex;  (* cfg exit vertex *)
}


let inv_true_gen : invgen_info -> t
=fun {igi_trx=_; igi_loop; igi_entryvtx=_; igi_exitvtx=_} -> begin
  let open Vlang.Formula in
  { trx_inv = VF_true;
    loop_inv = CPMap.map igi_loop ~f:(fun _ -> VF_true);
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
  let basic_var_set : (Vlang.Ty.t * Vlang.Expr.t) CPSet.t = CPSet.of_list [
    param_typ, Vlang.Expr.V_var (param_typ, glenv.gv_param);
    strg_typ, Vlang.Expr.V_var (strg_typ, glenv.gv_storage);
    Vlang.Ty.T_mutez, Vlang.Expr.V_var (Vlang.Ty.T_mutez, glenv.gv_amount);
    Vlang.Ty.T_mutez, Vlang.Expr.V_var (Vlang.Ty.T_mutez, glenv.gv_balance);
    Vlang.Ty.T_address, Vlang.Expr.V_var (Vlang.Ty.T_address, glenv.gv_sender);
    Vlang.Ty.T_address, Vlang.Expr.V_var (Vlang.Ty.T_address, glenv.gv_source);
  ] in
  let avar_pre_info : Pre.Analyzer.AvailVar.t = Pre.Analyzer.AvailVar.run cfg
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
  let var_to_vlvar : string -> Vlang.Ty.t * Vlang.Expr.t
  =fun v -> begin
    let typ = PreLib.Cfg.t_map_find ~errtrace:("ProverLib.Inv.gen_invgen_info_for_single_contract_verification : var_to_vlvar : " ^ v) cfg.type_info v |> Vlang.TypeUtil.ty_of_mty in
    (typ, Vlang.Expr.V_var (typ, v))
  end in
  let absset_filter_pre : (Pre.Analyzer.AvailVar.abs_set * Pre.Analyzer.AvailVar.abs_set) -> (Vlang.Ty.t * Vlang.Expr.t) CPSet.t = begin
    function | (Pre.Analyzer.AvailVar.S s, _) -> CPSet.union basic_var_set (CPSet.map s ~f:var_to_vlvar) | (Pre.Analyzer.AvailVar.Top, _) -> basic_var_set
  end
  in
  let igi_loop_partial : (int, (Vlang.Ty.t * Vlang.Expr.t) CPSet.t) CPMap.t = 
    CPSet.fold 
      ~init:(CPMap.empty)   
      loopvtx_set 
      ~f:(fun accmap lvtx -> 
          CPMap.add accmap ~key:lvtx ~data:(CPMap.find_exn avar_pre_info lvtx |> absset_filter_pre)
          |> (function `Duplicate -> accmap | `Ok m -> m)
      )
  in
  (* add some empty sets in igi_loop *)
  let igi_loop : (int, (Vlang.Ty.t * Vlang.Expr.t) CPSet.t * Vlang.Component.t CPSet.t) CPMap.t =
    CPMap.map igi_loop_partial ~f:(fun x -> (x, CPSet.empty)) 
  in
  {igi_trx=strg_comp; igi_loop=igi_loop; igi_entryvtx=cfg.main_entry; igi_exitvtx=cfg.main_exit;}
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
