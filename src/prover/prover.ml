(* module BpGen = BpGen
module VcGen = VcGen
module VlGen = VlGen *)

let prove : PreLib.Cfg.t -> PreLib.Adt.data option -> unit
= let open PreLib in
  let open ProverLib in
  let module CPSet = Core.Set.Poly in
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
  let init_stg_cond : Inv.t -> Vlang.t = VcGen.construct_initstg_vc prv_glenv_ref cfg init_stg_opt in

  (* *. Ignore values to remove compiler warnings & Debugging *)
  let trinv = Inv.inv_true_gen ivg_info in
  let _ = 
    List.iter 
      ( fun vc -> 
        let vcond = vc trinv in
        Vlang.string_of_formula vcond.VcGen.path_vc |> Stdlib.print_endline;
        CPSet.iter vcond.query_vcs ~f:(fun vc -> vc.qvc_fml |> Vlang.string_of_formula |> Stdlib.print_endline)
      )
      vcl
  in
  let _ = Stdlib.ignore vcl; ignore ivg_info; ignore init_stg_cond in
  ()

end (* function prove end *)
