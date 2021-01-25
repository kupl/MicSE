(* Refuter proceduere *)

(* "refuted_queries" : Global variable which contains already refuted queries. Similar to "Prover.Results.unproved_queries". *)
let refuted_queries : (ProverLib.Bp.query_category * PreLib.Mich.loc) Core.Set.Poly.t ref = ref Core.Set.Poly.empty  (* for PROVER-REFUTER-REFUTER SYNC process *)

(*
  [Overview]
  There are two main differences between prover and refuter.
  1. Refuter uses loop-unrolled cfg & transaction-unrolled basicpaths to find 
    In contract, Prover uses transaction invariant and loop invariants.
  2. Refuter tries to find INVALIDness of the query (which is not UNKNOWN.)
    In other words, the SMT model which makes the query invalid is what refuter finds.
    Prover tries to show VALIDness of the query.
  
  [INPUT]
  - Cfg: Control flow graph of the given smart contract
  - Cfg-construction Counter (Type "PreLib.Cfg.cfgcon_ctr")
  - Initial Storage (optional)
  - # of Transaction-Unrolling
  - # of Loop-Unrolling

  [OUTPUT]
  - unit

  [PROCEDURE]
  1. Unrolling every loops in the Cfg.
  2. Generate BasicPaths N times. (We call them transaction basicpaths.)
    Each time a different global-variable environment will be used to generate basicpaths.
    - It looks inefficient, but this process doesn't take a long time compared to Z3 solving.
    - In addition, this procedure is more clear than 
      generating basicpaths mutating every Bp.basic-nodes.
    - Every main-exit vertex's (global-var-storage := main-exit-vtx-var) assignments will be
      normally injected by "Prover.VcGen.sp_for_main_exit_vtx"
      used by "Prover.VcGen.construct_verifier_vc".
    - So there are no problem when just concatenating transaction-basicpaths into one basicpaths.
      (If the implementation doesn't change any vertex-id in basicpath.)
    - Remove any basicpath that ends with non-main-exit vertex.
      - Currently, the basicpath ends with FAILWITH instruction are the only case 
        which does not ends with non-main-exit.
  3. Transaction-Basicpaths-Concat & Refute Loop
    3.1. Concat transaction Basicpaths up to N times.
    - Transaction-unrolling order is strictly defined by the global-variable environment,
      this is why we generate basicpaths N times in the previous procedure.
    - While concatenate, put storage-assignments (e.g. BI_assign (trx-storage-2, trx-storage-1))
      between every two transaction basicpaths. It is important to connect transaction-result
      storage value to next transaction's storage value.
    - If initial storage value is given, assign that initial storage value to first
      transaction storage variable.
    3.2. Collect Queries
    - After substitute basicpath to verification-conditions, 
      remove queries which are not located at the last transaction basicpath.
      It helps to specify which transaction violates safety condition 
      in multiple transaction scenario.
    - If there are no queries left, skip 3.3.
    3.3. Refute
    - Check the validity of the query's verification condition.
    - If the result is "INVALID", it means that the model that SMT passes
      is the concrete scenario that violates safety condition (query).
    3.4. If Refute Fails, try other transaction-basicpath concatenation.
*)


let main : (PreLib.Cfg.t * PreLib.Cfg.cfgcon_ctr) -> PreLib.Adt.data option -> int -> int -> unit
= let open PreLib in
  let open ProverLib in
  let open Prover in
  let module CPSet = Core.Set.Poly in
  let module CPMap = Core.Map.Poly in
  (* control-flow-graph, initial-storage-option, transaction-unrolling-number, loop-unrolling-number *)
  fun (cfg, cfgcon_counter) init_stg_opt trx_ur_num loop_ur_num -> begin
  (* 0. Set the timer *)
  let timer = Utils.Timer.create ~budget:!Utils.Options.refuter_sub_time_budget in
  (* 1. Unrolling loops in the Cfg
      To remove any confusion, it shadows the name "cfg".
  *)
  let param_cfg = cfg in
  let {cfg=cfg; vtxrel=unrollcfg_vtxrel; _;} : CfgUtil.LoopUnrolling.UnrollParam.pt = 
    CfgUtil.LoopUnrolling.unroll (CfgUtil.LoopUnrolling.construct_pt (cfg, cfgcon_counter, loop_ur_num))
  in
  let vtx_to_michloc : Cfg.vertex -> Mich.loc
  =fun vtx -> begin
    (* "vtx_to_michloc" : vertex -> michelson-location 
        it should refer "unrollcfg_vtxrel", in current implementation.
        this function considers that the "unrollcfg_vtxrel"'s data is singleton set.
    *)
    let s = PreLib.Cfg.t_map_find ~errtrace:("Refuter.main : vtx_to_michloc : vtxrel") unrollcfg_vtxrel vtx in
    if CPSet.is_empty s then (((*debug*) print_endline ("DEBUG : Refuter.main : vtx_to_michloc : set-is-empty : vtx=" ^ Stdlib.string_of_int vtx)); PreLib.Mich.Unknown) else
    (* INFO: "choose_exn" below is safe from exception. *)
    let origin_vtx = CPSet.choose_exn s in
    PreLib.Cfg.t_map_find ~errtrace:("Refuter.main : vtx_to_michloc : posinfo") param_cfg.pos_info origin_vtx
  end in
  (* 
    (* deprecated since more specific unrolling procedure introduced above. *)
  let (cfg, _) : Cfg.t * Cfg.cfgcon_ctr = CfgUtil.LoopUnrolling.run (cfg, cfgcon_counter, loop_ur_num) in
  *)
  (* 2. Generate BasicPaths N times *)
  (* Global-Variable-Env-Numbers : [0, N) *)
  let trx_bp_map : (int, Bp.t CPSet.t) CPMap.t =
    let rec foldf : (int, Bp.t CPSet.t) CPMap.t -> int -> (int, Bp.t CPSet.t) CPMap.t
    =fun accmap n -> begin
      (* escape condition *)
      if n >= trx_ur_num then accmap else
      if Utils.Timer.is_timeout timer
      then (
        let _ = print_endline ("Timeout before collecting trx_bp_map with integer-argument : " ^ (Stdlib.string_of_int n)) in
        Stdlib.failwith "Refuter : Main : trx_bp_map"
      )
      else 
      (* debug *) let _ = print_endline ("debug : trx_bp_map : " ^ (string_of_int n)) in
      (* basic-path construction. It is similar to basicpath-generation part in "Prover.main" *)
      let prv_glenv_ref : GlVar.Env.t ref = ref (GlVar.Env.gen n) in
      let basic_vtxlst_set : (Cfg.vertex list) CPSet.t = BpGen.collect_bp_vtx cfg cfg.main_entry in
      (* debug *) let _ = print_endline ("basic_vtxlst_set size : " ^ (string_of_int (CPSet.length basic_vtxlst_set))) in 
      let bpgen_func : Cfg.vertex list -> Bp.t = BpGen.bp_of_vtxlst prv_glenv_ref cfg in
      let bps_orig : Bp.t CPSet.t = 
        CPSet.map basic_vtxlst_set ~f:bpgen_func 
        (* filter out any basicpaths that does not end with main-exit vertex. *)
        |> CPSet.filter ~f:(fun bp -> bp.Bp.exit_vtx = cfg.main_exit)
      in
      let bps : Bp.t CPSet.t =
        (* OPTIMIZATION *)
        if !Utils.Options.flag_bpopt_rsi then CPSet.map bps_orig ~f:Bp.remove_skip_inst else bps_orig
      in
      let new_acc : (int, Bp.t CPSet.t) CPMap.t = 
        CPMap.add accmap ~key:n ~data:bps
        |> (function | `Duplicate -> Stdlib.failwith "Refuter.main : trx_bp_map : foldf : CPMap.add duplicated" | `Ok m -> m)
      in
      foldf new_acc (n+1)
    end in  (* internal function foldf end *)
    foldf CPMap.empty 0
  in
  (* debug *) let _ = print_endline ("trx_bp_map size : " ^ (string_of_int (CPMap.length trx_bp_map))) in
  (* debug *) let _ = print_endline ("trx_bp_map entry-0 size : " ^ (string_of_int (CPSet.length (CPMap.find_exn trx_bp_map 0)))) in
  (*(* debug *) let _ = print_endline ("trx_bp_map entry-1 size : " ^ (string_of_int (CPSet.length (CPMap.find_exn trx_bp_map 1)))) in*)
  (* 3. Transaction BasicPath Concat & Refute Loop *)
  (* First, get the storage type. *)
  let stg_vtyp : Vlang.Ty.t =  
    PreLib.Cfg.t_map_find
      ~errtrace:("Refuter.main : append_trxbp_back : param_storage")
      cfg.type_info
      PreLib.Cfg.param_storage_name
    |> Vlang.TypeUtil.ty_of_mty
    |> Vlang.TypeUtil.get_innertyp2
    |> Stdlib.snd
  in
  (* "append_trxbp_back (bp, stgvar) trxbp" 
      generates a concatenated basicpath (bp -> (trxbp-stgvar := stgvar) -> trxbp)
      where "trxbp-stgvar" is the global-storage variable of "trxbp".
      "trxbp-stgvar" will be get from the "trxbp"'s first basicnode's "Bp.glenv_ref".
  *)
  let append_trxbp_back : (Bp.t * string) -> Bp.t -> (Bp.t * string)
  =fun (bp, stgvar) trxbp -> begin
    (* "List.hd" used. exception "Stdlib.Failure" might occure here. *)
    let trxbp_head_bn : Bp.basic_node = List.hd trxbp.content in
    (* "strg_bn" will be located between "bp" and "trxbp". *)
    let strg_bn : Bp.basic_node = 
      (* for new basicnode, global variable environment will be the same as trxbp's environment. *)
      { glenv_ref = trxbp_head_bn.glenv_ref;
        cfgvtx = trxbp.entry_vtx;
        inst = BI_assign (stg_vtyp, !(trxbp_head_bn.glenv_ref).gv_storage, V_var (stg_vtyp, stgvar));
      }
    in
    (* PRECONDITION
        "append_trxbp_back" assumes that the the both "bp" and "trxbp" has the same
        "entry_vtx" and "exit_vtx", since two basicpaths are generated from same loop-unrolled-cfg
        and the basicpaths which ends with "failwith" vertex are all deleted above.
    *)
    let new_bp : Bp.t = 
      { entry_vtx = bp.entry_vtx;
        exit_vtx = bp.exit_vtx;
        content = bp.content @ (strg_bn :: trxbp.content);
        (* appeared_vars are used for Prover's invariant generation, not in Refuter.
          So do not worry about the possibility of complexity increasing 
          comes from big "appeared_vars".
        *)
        appeared_vars = CPSet.union bp.appeared_vars trxbp.appeared_vars;
      } 
    in
    (new_bp, !(trxbp_head_bn.glenv_ref).gv_storage)
  end in (* internal function append_trxbp_back end *)
  (* "concat_trxbps [trxbp-0; trxbp-1; trxbp-2]" generates a concatenated basicpath
        "((trxstg-0 := init-stg-val)) -> trxbp-0 
        -> (trxstg-1 := trxstg-0) -> trxbp-1
        -> (trxstg-2 := trxstg-1) -> trxbp-2".
      The order of transaction-basicpath in the parameter list should be ordered well.
      If initial storage value does not exists, the first basic-node will not be inserted.
      "concat_trxbps" is not a recursive function, since it uses "List.fold_left append_trxbp_back"
  *)
  let concat_trxbps : Bp.t list -> Bp.t
  =fun bplst -> begin
    match bplst with
    | [] -> Stdlib.failwith "Refuter.main : concat_trxbps : bplst match failed"
    | h :: t ->
      (* "List.hd" used. exception "Stdlib.Failure" might occur here *)
      let headbn : Bp.basic_node = List.hd h.content in
      let (concatenated, _) : Bp.t * string = List.fold_left append_trxbp_back (h, !(headbn.glenv_ref).gv_storage) t in
      (match init_stg_opt with
      | None -> concatenated
      | Some stg ->
        let stg_vexpr : Vlang.Expr.t = VlGen.create_expr_of_michdata stg stg_vtyp in
        let init_bn : Bp.basic_node = {
          glenv_ref = headbn.glenv_ref;
          cfgvtx = headbn.cfgvtx;
          inst = BI_assign (stg_vtyp, !(headbn.glenv_ref).gv_storage, stg_vexpr);
        } in
        {concatenated with content=(init_bn :: concatenated.content)}
      )
  end in (* internal function concat_trxbps end *)
  (* 3.1. Concat basicpaths *)
  (* This implementation will generate every length-N transaction combination. *)
  let generated_complete_combination : Bp.t CPSet.t = 
    let combination_list : Bp.t list CPSet.t = 
      let rec foldf : Bp.t list CPSet.t -> int -> Bp.t list CPSet.t
      =fun accset n -> begin
        (* foldf escape condition *)
        if n < 0 then accset else
        (* debug *) let _ = print_endline ("combination_list - foldf : accset size : " ^ string_of_int (CPSet.length accset)) in
        (* WARNING: trx-basicpaths of transaction-N. "Option.get" might raise "Invalid_argument" *)
        let trx_bps : Bp.t CPSet.t = CPMap.find trx_bp_map n |> Option.get in
        (* append each list *)
        let newset : Bp.t list CPSet.t = 
          CPSet.fold 
            trx_bps 
            ~init:CPSet.empty 
            ~f:(
              fun accs_i bp -> 
              CPSet.union (
                (* Timeout - escape condition *)
                if Utils.Timer.is_timeout timer
                  then (
                    let _ = print_endline ("Timeout after collecting " ^ (Stdlib.string_of_int (CPSet.length accs_i)) ^ " combination_list with integer-argument : ") in
                    Stdlib.failwith "Refuter : Main : generated_complete_combination : combination_list"
                  )
                  else 
                CPSet.map accset ~f:(fun bpl -> bp :: bpl)) 
                accs_i
            ) 
        in
        (* recursive call *)
        foldf newset (n-1)
      end in
      foldf (CPSet.singleton []) (trx_ur_num-1)
    in
    CPSet.map combination_list ~f:concat_trxbps    
  in  (* internal function generate_complete_combination end *)
  (* debug *) let _ = print_endline ("generated_complete_combination size : " ^ string_of_int (CPSet.length generated_complete_combination)) in
  (* 3.2. Remove unwanted queries *)
  (* "remove_assertion_except bp n" removes every "BI_assert" instructions 
      except the basic_node's "!glenv_ref.gv_num" is equal to "n".
      Be careful, in the context of "concat_trxbps", 
      "n-th" basicpath starts from "1-th", has a "gv_num" (n-1).
  *)
  let remove_assertion_except : int -> Bp.t -> Bp.t
  =fun n bp -> begin
    (* filter function : if ((not BI_assert) or (gv_num = "n")) then true else false *)
    let filter_f : Bp.basic_node -> bool = fun bn -> 
      Stdlib.not ((function | Bp.BI_assert _ -> true | _ -> false) bn.inst) 
      || !(bn.glenv_ref).gv_num = n
    in
    {bp with content=(List.filter filter_f bp.content)}
  end in (* internal function remove_assertion_except end *)
  (* 3.3. Refute *)
  (* refute each basicpaths/queries until timeout or run out of basicpaths. *)
  let true_inv : Inv.t = {trx_inv=(CPSet.singleton Vlang.Formula.VF_true); loop_inv=CPMap.empty} in (* Invariant "True" *)
  let rec refute : (Bp.t CPSet.t * (VcGen.query_vc * Smt.ZModel.t) CPSet.t) -> (Utils.Timer.t ref) -> (Bp.t CPSet.t * (VcGen.query_vc * Smt.ZModel.t) CPSet.t)
  =fun (worklist, refuted_set) timer -> begin
    (* check escape condition *)
    if CPSet.is_empty worklist || Utils.Timer.is_timeout timer then (worklist, refuted_set) else
    (* choose a basicpath from worklist. *)
    let bp_picked : Bp.t = CPSet.choose_exn worklist in
    let new_worklist_1 : Bp.t CPSet.t = CPSet.remove worklist bp_picked in
    (* Perform "3.2." here.
      (integer argument for "remove_assertion_except" is hardcoded.) 
    *)
    let bp_picked : Bp.t = bp_picked |> remove_assertion_except (trx_ur_num-1) in
    (* extract queries *)
    let query_vcs : VcGen.query_vc CPSet.t = 
      let vcond : VcGen.v_cond = (VcGen.construct_verifier_vc cfg bp_picked) true_inv in
      vcond.query_vcs
      (* REMOVE DUPLICATED QUERIES (in one basicpath) *)
      |> CPSet.to_list
      |> Validator.QuerySet.of_list
      |> Validator.QuerySet.elements
      |> CPSet.of_list
    in
    (* Refute Query. Similar to query-validation procedure in "Prover.Validator.validate" *)
    let (pset, _, _) : (VcGen.query_vc * Smt.ZModel.t) CPSet.t * (VcGen.query_vc CPSet.t) * (VcGen.query_vc CPSet.t) =
      let open VcGen in
      let rec foldf : ((VcGen.query_vc * Smt.ZModel.t) CPSet.t * (VcGen.query_vc CPSet.t) * (VcGen.query_vc CPSet.t)) -> (VcGen.query_vc CPSet.t) -> ((VcGen.query_vc * Smt.ZModel.t) CPSet.t * (VcGen.query_vc CPSet.t) * (VcGen.query_vc CPSet.t))
      (* refuted-query accumulator & unrefuted-query accumulator & ignored-query accumulator *)
      =fun (r_acc, u_acc, ig_acc) qset -> begin
        (* check if timeover *)
        if Utils.Timer.is_timeout timer then (r_acc, CPSet.union u_acc qset, ig_acc) else
        (* if the qset empty then escape *)
        if CPSet.is_empty qset then (r_acc, u_acc, ig_acc) else
        (* pick a query in qset *)
        let q_picked : VcGen.query_vc = CPSet.choose_exn qset in
        let rqset : VcGen.query_vc CPSet.t = CPSet.remove qset q_picked in
        (* PROVER-REFUTER-REFUTER SYNC
            if the query is already proved (Prover.Results.is_prover_used && (query is not in Prover.Results.unproved_queries))
            or the query is already refuted (query in refuted_queries),
            then skip it (put it to ignored-query set).
        *)
        let q_id : ProverLib.Bp.query_category * PreLib.Mich.loc = (q_picked.qvc_cat, vtx_to_michloc q_picked.qvc_vtx) in
        let is_q_loc_unknown : bool = Stdlib.snd q_id = PreLib.Mich.Unknown in
        let is_q_proved_already : bool = Stdlib.not is_q_loc_unknown && !Prover.Results.is_prover_used && (CPSet.mem !Prover.Results.unproved_queries q_id) in
        let is_q_refuted_already : bool = Stdlib.not is_q_loc_unknown && (CPSet.mem !refuted_queries q_id) in
        if (is_q_proved_already || is_q_refuted_already) then (foldf (r_acc, u_acc, (CPSet.add ig_acc q_picked)) rqset) else
        (* this fold-function separates refuted-queries and unrefuted/unknown-queries *)
        let q_validity, modelopt = Verifier.verify q_picked.qvc_fml in
        let (nracc, nuacc) =
          if Smt.ZSolver.is_invalid q_validity
          then (
            (* PROVER-REFUTER-REFUTER SYNC - collect refuted query *)
            let _ = refuted_queries := (CPSet.add !refuted_queries q_id) in
            (* WARNING: "Option.get" below might raise the exception "Invalid_argument" *)
            let mdl : Smt.ZModel.t = Option.get modelopt in
            (CPSet.add r_acc (q_picked, mdl), u_acc)
          )
          else (r_acc, CPSet.add u_acc q_picked)
        in
        foldf (nracc, nuacc, ig_acc) rqset
      end in
      foldf (CPSet.empty, CPSet.empty, CPSet.empty) query_vcs
    in
    (* recursive call refuter *)
    refute (new_worklist_1, CPSet.union pset refuted_set) timer
  end in (* internal function refute end s*)
  (* CALL REFUTER *)
  let (unsearched_set, refuted_set) : Bp.t CPSet.t * (VcGen.query_vc * Smt.ZModel.t) CPSet.t = 
    refute 
      (generated_complete_combination, CPSet.empty) 
      timer
  in
  (* Interpret Refuter Result *)
  let _ = (
    print_endline ("Refuter tries to run " ^ (Stdlib.string_of_int (CPSet.length generated_complete_combination)) ^ " basicpaths.");
    print_endline ("Refuter runs " ^ (Stdlib.string_of_int (CPSet.length generated_complete_combination - CPSet.length unsearched_set)) ^ " basicpaths.");
    print_endline ("Refuter produces " ^ (Stdlib.string_of_int (CPSet.length refuted_set)) ^ " scenarios.");
    print_endline ("Scenarios :: ");
    let scenario_idx = ref 0 in
    CPSet.iter
      refuted_set
      ~f:(
        fun (qvc, mdl) ->
        let _ = Stdlib.incr scenario_idx in
        let trace = RefuterLib.Trace.gen cfg trx_ur_num !scenario_idx qvc mdl in
        trace |> RefuterLib.Trace.to_string |> print_endline;
        (* print_newline ();
        print_endline (mdl |> ProverLib.Smt.ZModel.to_string) *)
      );
  ) in
  ()
end (* function main end *)


(* "run_multiple n" runs the "main" function n times, changing the number of transaction-unrolling number *)
let run_multiple : (PreLib.Cfg.t * PreLib.Cfg.cfgcon_ctr) -> PreLib.Adt.data option -> unit
=fun (cfg, cfgcounter) init_stg_opt -> begin
  let timer = Utils.Timer.create ~budget:!Utils.Options.refuter_total_time_budget in
  for i = 1 to !Utils.Options.transaction_unroll_num do
    if Utils.Timer.is_timeout timer then () else
    main (cfg, cfgcounter) init_stg_opt i !Utils.Options.loop_unroll_num
  done;
  ()
end (* function run_multiple end *)