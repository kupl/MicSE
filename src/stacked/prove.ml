(* Prover *)

exception ProverTimeout of Utils.Timer.t ref


(*****************************************************************************)
(*****************************************************************************)
(* Utilities                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

(*  Input
    - Michelson Program
    - Initial Storage (Michelson Form, Option)
    
    Output
    - Initial Storage (Tz Form, Option)
    - Initial Symbolic State
    - Cache (For debugging purpose - Set of entered loop & lambdas)
    - State Set (Symbolic Execution Result)
*)
let gen_sset : PreLib.Adt.t -> (PreLib.Adt.data option) -> ((Tz.mich_v Tz.cc option) * Tz.sym_state * (Se.cache ref) * Se.state_set)
= fun mich_program mich_init_stg_opt -> begin
  (* Prepare Michelson program and initial-storage if needed *)
  let mich_program : PreLib.Mich.program = 
    mich_program
    |> PreLib.Mich.subst_standard_macro_all_pgm
    |> PreLib.Mich.optm_all_pgm
  in
  (* Convert them to Tz form *)
  let (param_typ, storage_typ, inst) : (Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc) =
    TzCvt.M2T.cv_program mich_program in
  let tz_init_stg_opt : Tz.mich_v Tz.cc option =
    Option.bind mich_init_stg_opt (fun x -> Some (TzCvt.M2T.cv_datat mich_program.storage x)) in
  (* Symbolic Execution - Collect Path Conditions & Queries *)
  let (init_ss, cache, sset) = 
    Se.run_contract_in_fog (param_typ, storage_typ, inst) in
  (tz_init_stg_opt, init_ss, cache, sset)
end (* function gen_sset end *)

let check_validity : Tz.mich_f -> ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option
= fun fmla -> ProverLib.Smt.ZSolver.check_validity [TzCvt.T2S.cv_mf fmla]

(* 
  Input
  - Timer
  - Every blocked symbolic-states
  - Invariant Map

  Output
  - Is the given invariant has inductiveness (Boolean)
  - If not, why? ((Failed-symtstate * validity-check-result) Option)

  System Effect
  - Utils.Options.z3_time_budget (internally restricts z3 time limit)
  - Utils.Options.prover_time_budget (total prover time limit - timeout will raise ProverTimeout)
*)
let check_inv_inductiveness : 
  Utils.Timer.t ref 
  -> Tz.sym_state Tz.PSet.t 
  -> Se.invmap 
  -> (bool * (Tz.sym_state * (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option)) option)
= fun timer blocked_sset invm -> begin
  Tz.PSet.fold blocked_sset ~init:(true, None)
    ~f:(fun (accb, accopt) bl_ss -> 
      (* If the prover time budget runs out, raise ProverTimeout *)
      if Utils.Timer.is_timeout timer then Stdlib.raise (ProverTimeout timer) else
      (* If the invariant-map already fails to show inductiveness, skip the rest (when accb = false) *)
      if Stdlib.not accb then (accb, accopt) else
      let vld_r : ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option = Se.inv_induct_fmla_i bl_ss invm |> check_validity in
      if ProverLib.Smt.ZSolver.is_valid (Stdlib.fst vld_r) then (true, None) else (false, Some (bl_ss, vld_r))
    )
end (* function check_inv_inductiveness end *)

(* 
  Input
  - Timer
  - Every query symbolic-states
  - Invariant Map
  
  Output
  - Solved query symbolic-states
    - The query symbolic-state can be returned only if the whole same-position queries are solved at once.
  - Failed query symbolic-states
  - Untouched query symbolic-states

  System-Effect
  - Utils.Options.z3_time_budget (internally restricts z3 time limit)
  - Utils.Options.prover_time_budget (total prover time limit - timeout will return accumulated result)
*)
let solve_queries : 
  Utils.Timer.t ref 
  -> (Tz.sym_state * Se.query_category) Tz.PSet.t 
  -> Se.invmap 
  -> ((Tz.mich_cut_info * Se.query_category), (Tz.sym_state * Se.query_category) Tz.PSet.t) Tz.PMap.t
      * ((Tz.sym_state * Se.query_category) * (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option)) Tz.PSet.t
      * (Tz.sym_state * Se.query_category) Tz.PSet.t
= fun timer ss_qcat_set invm -> begin
  (* Gather same-position queries using Tz.ss_block_mci *)
  let spos_qmap : ((Tz.mich_cut_info * Se.query_category), (Tz.sym_state * Se.query_category) Tz.PSet.t) Tz.PMap.t = 
    Tz.PSet.fold ss_qcat_set ~init:(Tz.PMap.empty)
      ~f:(fun accmap (ss, qcat) ->
        Tz.PMap.change accmap (ss.ss_block_mci, qcat) 
          ~f:(function None -> Some (Tz.PSet.singleton (ss, qcat)) | Some s -> Some (Tz.PSet.add s (ss, qcat)))
      )
  in
  (* Fold: Solve each queryset *)
  Tz.PMap.fold spos_qmap ~init:(Tz.PMap.empty, Tz.PSet.empty, ss_qcat_set)
    ~f:(fun ~key ~data (acc_solved_smap, acc_failed_sset, acc_untouched_sset) ->
      (* check timeout *)
      if Utils.Timer.is_timeout timer then (acc_solved_smap, acc_failed_sset, acc_untouched_sset) else
      (* Fold: Solve each query in queryset *)
      let (qset_solved_sset, qset_failed_sset) : 
        (Tz.sym_state * Se.query_category) Tz.PSet.t
        * ((Tz.sym_state * Se.query_category) * (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option)) Tz.PSet.t
      = Tz.PSet.fold data ~init:(Tz.PSet.empty, Tz.PSet.empty)
          ~f:(fun (acc_qset_solved_sset, acc_qset_failed_sset) query_ss_cat ->
            (* check timeout *)
            if Utils.Timer.is_timeout timer then (acc_qset_solved_sset, acc_qset_failed_sset) else
            (* Solve the validity of the given query (query_ss_cat) *)
            let vld_r : ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option = Se.inv_query_fmla query_ss_cat invm |> check_validity in
            (* If valid, put the query to "solved-sset", else to "failed_sset" *)
            if ProverLib.Smt.ZSolver.is_valid (Stdlib.fst vld_r) 
            then (Tz.PSet.add acc_qset_solved_sset query_ss_cat, acc_qset_failed_sset)
            else (acc_qset_solved_sset, Tz.PSet.add acc_qset_failed_sset (query_ss_cat, vld_r))
          )
      in
      (* check if all queries in "data" solved *)
      let is_all_solved = (Tz.PSet.length data = Tz.PSet.length qset_solved_sset) in
      (* If all queries in the same position solved, accumulate them to "acc_solved_smap". Else, accumulate them to "acc_failed_set". *)
      let new_acc_solved_smap = (if is_all_solved then Tz.PMap.add_exn acc_solved_smap ~key:key ~data:data else acc_solved_smap) in
      let new_acc_failed_sset = (
        if is_all_solved then acc_failed_sset else 
        let failform_of_solved = Tz.PSet.map qset_solved_sset ~f:(fun x -> (x, (ProverLib.Smt.ZSolver.VAL, None))) in
        Tz.PSet.union (Tz.PSet.union failform_of_solved qset_failed_sset) acc_failed_sset
      ) in
      (* Remove queries from untouched query set *)
      let new_acc_untouched_sset = Tz.PSet.diff (Tz.PSet.diff acc_untouched_sset qset_solved_sset) (Tz.PSet.map qset_failed_sset ~f:Stdlib.fst) in
      (new_acc_solved_smap, new_acc_failed_sset, new_acc_untouched_sset)
    )
end (* function solve_queries_wl end *)


(*****************************************************************************)
(*****************************************************************************)
(* Small functionalities, returns unit value only                            *)
(*****************************************************************************)
(*****************************************************************************)

let f_count_sset : Se.state_set -> unit
= let sz s = Tz.PSet.length s in
  fun {running; blocked; queries; terminated;} -> begin
  Printf.printf "#running=%d, #blocked=%d, #queries=%d, #terminated=%d\n" (sz running) (sz blocked) (sz queries) (sz terminated)
end (* function f_count_sset end *)

let f_print_blocked_paths_pretty : Se.state_set -> unit
= fun sset -> begin
  let strop_j = TzCvt.T2Jnocc.cv_p1_ss_strop (Tz.PSet.choose_exn sset.blocked) in
  let paths_j = Tz.PSet.map sset.blocked ~f:(TzCvt.T2Jnocc.cv_p1_ss_path) in
  print_endline (strop_j |> Yojson.Safe.to_basic |> Yojson.Basic.pretty_to_string);
  (Tz.PSet.iter paths_j ~f:(fun x -> x |> Yojson.Safe.to_basic |> Yojson.Basic.pretty_to_string |> print_endline))
end (* function f_print_sset end *)

