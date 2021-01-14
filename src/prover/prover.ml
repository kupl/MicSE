module BpGen = BpGen
module InvGen = InvGen
module Validator = Validator
module VcGen = VcGen
module Verifier = Verifier
module VlGen = VlGen

type run_ret = {
  best_inv : ProverLib.Inv.t;
  proved : VcGen.query_vc Core.Set.Poly.t;
  unproved : VcGen.query_vc Core.Set.Poly.t;
}

type run_env = {
  worklist : ProverLib.Inv.t Core.Set.Poly.t;
  invs_collected : ProverLib.Inv.t Core.Set.Poly.t;
  timer : Utils.Timer.t ref;
  igi : ProverLib.Inv.invgen_info;  (* information for invariant generation process *)
  vcl : (ProverLib.Inv.t -> VcGen.v_cond) list; (* the list of verification condition *)
  isc : ProverLib.Inv.t -> ProverLib.Vlang.t; (* initial-storage condition *)
  istg_exists : bool; (* is initial storage exists? if exists, true, else, false. *)
  ret_opt : run_ret option;
}


(* let init_naive_timeout_func : unit -> (unit -> bool)
=fun () -> begin
  let initial_time = Sys.time () in
  (fun () -> ((Sys.time ()) > (initial_time +. Stdlib.float_of_int !(Utils.Options.prover_time_budget))))
end *)


(* "select better run-result" is not strictly defined, but it can be naively defined,
    by selecting run-result which is inductive & has less unproved queries.
*)
let update_runret_opt : (run_ret option * run_ret option) -> run_ret option
=fun (old_rr_opt, new_rr_opt) -> begin
  match old_rr_opt, new_rr_opt with
  | None, None -> None
  | None, Some _ -> new_rr_opt
  | Some _, None -> old_rr_opt
  | Some {unproved=u1; _},
    Some {unproved=u2; _} ->
      if Core.Set.Poly.length u1 > Core.Set.Poly.length u2 then new_rr_opt else old_rr_opt
end (* function update_runret_opt end *)



(* "run" is the infinite-loop, escapes in the following conditions
  - worklist is empty
  - timeout
  - verification success
*)
let rec run : run_env -> run_ret option
= let open ProverLib in
  let module CPSet = Core.Set.Poly in
  fun {worklist; invs_collected; timer; igi; vcl; isc; istg_exists; ret_opt} -> begin
  (* check escape condition *)
  if CPSet.is_empty worklist || Utils.Timer.is_timeout timer then ret_opt else
  (* choose a candidate invariant from worklist *)
  let inv_candidate : Inv.t = CPSet.choose_exn worklist in
  let new_invs_collected : Inv.t CPSet.t = CPSet.add invs_collected inv_candidate in
  let new_worklist_1 : Inv.t CPSet.t = CPSet.remove worklist inv_candidate in
  (* validate *)
  let val_res : Validator.validate_result = Validator.validate (timer, inv_candidate, vcl, isc) in
  let cur_retopt = if val_res.inductive then Some {best_inv = inv_candidate; proved = val_res.p; unproved = val_res.u} else None in
  (* if verification succeeds *)
  if val_res.inductive && CPSet.is_empty val_res.Validator.u then Some {best_inv=inv_candidate; proved=val_res.p; unproved=val_res.u} else
  (* else verification succeeds *)
  (* generator *)
  let new_worklist_2 = CPSet.union (InvGen.generate (val_res, igi, inv_candidate, istg_exists, new_invs_collected)) new_worklist_1 in (* TODO *)
  (* else verification succeeds - if inductive, update worklist *)
  let new_worklist_3 = if val_res.inductive then Inv.strengthen_worklist (inv_candidate, new_worklist_2, new_invs_collected) else new_worklist_2 in
  (* additional process - snapshot the best result *)
  let new_retopt = update_runret_opt (ret_opt, cur_retopt) in
  (* recursive call until escape *)
  run {
    worklist = new_worklist_3;
    invs_collected = new_invs_collected;
    timer = timer;
    igi = igi;
    vcl = vcl;
    isc = isc;
    istg_exists = istg_exists;
    ret_opt = new_retopt;
  }
end (* function run end *)


let main : PreLib.Cfg.t -> PreLib.Adt.data option -> unit
= let open PreLib in
  let open ProverLib in
  let module CPSet = Core.Set.Poly in
  let module CPMap = Core.Map.Poly in
  fun cfg init_stg_opt -> begin
  (* 1. Basic-Path Construction *)
  let prv_glenv_ref : GlVar.Env.t ref = ref GlVar.Env.t_for_single_contract_verification in
  let basic_vtxlst_set : (Cfg.vertex list) CPSet.t = BpGen.collect_bp_vtx cfg cfg.main_entry in
  let bpgen_func : Cfg.vertex list -> Bp.t = BpGen.bp_of_vtxlst prv_glenv_ref cfg in
  let bps_orig : Bp.t CPSet.t = CPSet.map basic_vtxlst_set ~f:bpgen_func in
  let bps : Bp.t CPSet.t =
    (* OPTIMIZATION *)
    if !Utils.Options.flag_bpopt_rsi then CPSet.map bps_orig ~f:Bp.remove_skip_inst else bps_orig
  in
  let _ = 
    (* PRINT *)
    if !Utils.Options.flag_bp_print 
    then Stdlib.print_endline (Bp.simple_stringRep_of_tset ~pretty:(!Utils.Options.flag_bp_print_pretty) bps)
    else ()
  in
  (* 2. Verification Condition Construction 
      - Since CPSet cannot contain a function, we store (Inv.t -> v_cond) functions in list.
  *)
  let vcgen : Bp.t -> (Inv.t -> VcGen.v_cond) = VcGen.construct_verifier_vc cfg in
  let vcl : (Inv.t -> VcGen.v_cond) list = CPSet.fold bps ~init:[] ~f:(fun accl bp -> (vcgen bp) :: accl) in
  (* 3. Invariant Synthesis & Prove Loop 
      - The formula "init_stg_cond inv-candidate" should be valid.
        It should be checked before chekc other validities embedded in "vcl".
  *)
  let ivg_info : Inv.invgen_info = Inv.gen_invgen_info_for_single_contract_verification cfg in
  let init_stg_cond : Inv.t -> Vlang.t = VcGen.construct_initstg_vc prv_glenv_ref cfg init_stg_opt in (* init_stg_cond : initial-storage-condition deprecated *)
  let run_result_opt : run_ret option =
    run {
      worklist = CPSet.singleton (Inv.inv_true_gen ivg_info);
      invs_collected = CPSet.empty;
      timer = Utils.Timer.create ~budget:!Utils.Options.prover_time_budget;
      igi = ivg_info;
      vcl = vcl;
      isc = init_stg_cond;
      istg_exists = Option.is_some init_stg_opt;
      ret_opt = None;
    }
  in
  (* interpret prover result *)
  let _ = 
    (match run_result_opt with
    | None -> print_endline "Failure to create invariant that satisfies inductiveness."
    | Some {best_inv; proved; unproved} ->
        print_endline "Best Performed Invariant - Transaction Invariant:";
        print_endline (Vlang.Formula.to_string (VlangUtil.NaiveOpt.run (Inv.inv_to_formula best_inv.trx_inv)));
        print_endline "Best Performed Invariant - Loop Invariant:";
        CPMap.iteri best_inv.loop_inv ~f:(fun ~key ~data -> print_endline ((Stdlib.string_of_int key) ^ " : " ^ (Vlang.Formula.to_string (VlangUtil.NaiveOpt.run (Inv.inv_to_formula data)))));
        print_endline ("Proved Queries : " ^ string_of_int (CPSet.length proved));
        CPSet.iter proved ~f:(fun p -> print_endline (Vlang.Formula.to_string (VlangUtil.NaiveOpt.run p.qvc_fml)));
        print_endline ("Unproved Queries : " ^ string_of_int (CPSet.length unproved));
        CPSet.iter unproved ~f:(fun p -> print_endline (Vlang.Formula.to_string (VlangUtil.NaiveOpt.run p.qvc_fml)));
    ) (* TODO *)
  in
  ()
end (* function prove end *)
