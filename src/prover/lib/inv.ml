(* Invariant *)

(* Sugar *)
module CPSet = Core.Set.Poly
module CPMap = Core.Map.Poly


type t = {
    trx_inv : Vlang.t;  (* Transaction invariant *)
    loop_inv : (int, Vlang.t) CPMap.t;  (* Loop invariant : loop-vertex -> Vlang formula *)
}


(* invgen_info should contain information about which variables can be used when generating invariants. *)
type invgen_info = {
    igi_trx : string CPSet.t; (* available variable set *)
    igi_loop : (int, string CPSet.t) CPMap.t; (* loop-vertex -> available variable set *)
}

(* In our blueprint, Invariant is used for a verifier, not refuter. 
  So it is easy to generate invgen_info from a contract using cfg only.
*)
let gen_invgen_info_for_single_contract_verification : Pre.Lib.Cfg.t -> invgen_info
=fun cfg ->
  let glenv : GlVar.Env.t = GlVar.Env.t_for_single_contract_verification in
  let strg_var : string = glenv.gv_storage in 
  let strg_var_set : string CPSet.t = CPSet.singleton strg_var in
  let basic_var_set : string CPSet.t = CPSet.of_list [
    glenv.gv_param;
    glenv.gv_storage;
    glenv.gv_amount;
    glenv.gv_balance;
    glenv.gv_sender;
    glenv.gv_source;
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
  end
  and absset_filter_pre : (Pre.Analyzer.AvailVar.abs_set * Pre.Analyzer.AvailVar.abs_set) -> string CPSet.t = begin
    function | (Pre.Analyzer.AvailVar.S s, _) -> CPSet.union basic_var_set s | (Pre.Analyzer.AvailVar.Top, _) -> basic_var_set
  end
  in
  let igi_loop : (int, string CPSet.t) CPMap.t = 
    CPSet.fold 
      ~init:(CPMap.empty)   
      loopvtx_set 
      ~f:(fun accmap lvtx -> 
          CPMap.add accmap ~key:lvtx ~data:(CPMap.find_exn avar_pre_info lvtx |> absset_filter_pre)
          |> (function `Duplicate -> accmap | `Ok m -> m)
      )
  in
  {igi_trx=strg_var_set; igi_loop=igi_loop;}
  
