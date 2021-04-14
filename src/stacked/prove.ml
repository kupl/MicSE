(* Prover *)

exception Error of string


(*****************************************************************************)
(*****************************************************************************)
(* Types                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

type 'a set = 'a Tz.PSet.t
type ('a, 'b) map = ('a, 'b) Tz.PMap.t

type query_state = Tz.sym_state * Se.query_category
type query_id = Tz.mich_cut_info * Se.query_category

type solved_query_state = (query_state * Tz.mich_f * Utils.Timer.time) 
type failed_query_state = (query_state * (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option) * Tz.mich_f * Utils.Timer.time)

type worklist = Se.invmap list

type ret = {
  solved_map: (query_id, solved_query_state set) map;
  failed_set: failed_query_state set;
  untouched_set: query_state set;
}


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
  - Initial Storage (optional)
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
  -> (Tz.mich_v Tz.cc * Tz.sym_state) option
  -> Tz.sym_state set
  -> Se.invmap 
  -> (bool * (Tz.sym_state * (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option)) option * Utils.Timer.time)
= fun timer init_stg_ss_opt blocked_sset invm -> begin
  (* If initial storage exists, check if the transaction invariant satisfies the initial storage *)
  let init_stg_sat : bool = 
    (match init_stg_ss_opt with
    | None -> true
    | Some (istg, init_ss) ->
      (match Tz.PMap.find invm init_ss.ss_entry_mci with
        | None -> Error "check_inv_inductiveness : init_stg_sat" |> raise
        | Some inv_f ->
          let sat_f : Tz.mich_f = inv_f [Tz.MV_pair (init_ss.ss_optt.optt_param, istg) |> Tz.gen_dummy_cc] in
          check_validity sat_f |> Stdlib.fst |> ProverLib.Smt.ZSolver.is_valid
      )
    ) 
  in
  if Stdlib.not init_stg_sat then (false, None, Utils.Timer.read_interval timer) else
  let start_time : Utils.Timer.time = Utils.Timer.read_interval timer in
  let get_elapsed_time : unit -> Utils.Timer.time = fun () -> Utils.Timer.read_interval timer - start_time in
  (* check inductiveness by checking each paths *)
  Tz.PSet.fold blocked_sset ~init:(true, None, get_elapsed_time ())
    ~f:(fun (accb, accopt, etime) bl_ss -> 
      (* If the prover time budget runs out, raise ProverTimeout *)
      if Utils.Timer.is_timeout timer then (false, None, etime) else
      (* If the invariant-map already fails to show inductiveness, skip the rest (when accb = false) *)
      if Stdlib.not accb then (accb, accopt, etime) else
      let vld_r : ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option = Se.inv_induct_fmla_i bl_ss invm |> check_validity in
      let elapsed_time : Utils.Timer.time = get_elapsed_time () in
      if ProverLib.Smt.ZSolver.is_valid (Stdlib.fst vld_r) then (true, None, elapsed_time) else (false, Some (bl_ss, vld_r), elapsed_time)
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
  -> query_state set
  -> Se.invmap
  -> ret
= fun timer ss_qcat_set invm -> begin
  (* Gather same-position queries using Tz.ss_block_mci *)
  let spos_qmap : ((Tz.mich_cut_info * Se.query_category), (Tz.sym_state * Se.query_category) Tz.PSet.t) Tz.PMap.t =
    Tz.pmap_of_pset ss_qcat_set ~key_f:(fun (ss, qcat) -> (ss.ss_block_mci, qcat)) ~data_f:(fun x -> x)
  in
  (* Fold: Solve each queryset *)
  let smap, fset, uset = Tz.PMap.fold spos_qmap ~init:(Tz.PMap.empty, Tz.PSet.empty, ss_qcat_set)
    ~f:(fun ~key ~data (acc_solved_smap, acc_failed_sset, acc_untouched_sset) ->
      (* check timeout *)
      if Utils.Timer.is_timeout timer then (acc_solved_smap, acc_failed_sset, acc_untouched_sset) else
      (* Fold: Solve each query in queryset *)
      let (qset_solved_sset, qset_failed_sset) : 
        ((Tz.sym_state * Se.query_category) * Tz.mich_f * Utils.Timer.time) Tz.PSet.t
        * ((Tz.sym_state * Se.query_category) * (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option) * Tz.mich_f * Utils.Timer.time) Tz.PSet.t
      = Tz.PSet.fold data ~init:(Tz.PSet.empty, Tz.PSet.empty)
          ~f:(fun (acc_qset_solved_sset, acc_qset_failed_sset) query_ss_cat ->
            (* check timeout *)
            if Utils.Timer.is_timeout timer then (acc_qset_solved_sset, acc_qset_failed_sset) else
            (* Solve the validity of the given query (query_ss_cat) *)
            let start_time : Utils.Timer.time = Utils.Timer.read_interval timer in
            let fmla = Se.inv_query_fmla query_ss_cat invm in
            let vld_r : ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option = check_validity fmla in
            let elapsed_time : Utils.Timer.time = Utils.Timer.read_interval timer - start_time in
            (* If valid, put the query to "solved-sset", else to "failed_sset" *)
            if ProverLib.Smt.ZSolver.is_valid (Stdlib.fst vld_r) 
            then (Tz.PSet.add acc_qset_solved_sset (query_ss_cat, fmla, elapsed_time), acc_qset_failed_sset)
            else (acc_qset_solved_sset, Tz.PSet.add acc_qset_failed_sset (query_ss_cat, vld_r, fmla, elapsed_time))
          )
      in
      (* check if all queries in "data" solved *)
      let is_all_solved : bool = (Tz.PSet.length data = Tz.PSet.length qset_solved_sset) in
      (* If all queries in the same position solved, accumulate them to "acc_solved_smap". Else, accumulate them to "acc_failed_set". *)
      let new_acc_solved_smap = (if is_all_solved then Tz.PMap.add_exn acc_solved_smap ~key:key ~data:(qset_solved_sset) else acc_solved_smap) in
      let new_acc_failed_sset = (
        if is_all_solved then acc_failed_sset else 
        let failform_of_solved = Tz.PSet.map qset_solved_sset ~f:(fun (x, fmla, etime) -> (x, (ProverLib.Smt.ZSolver.VAL, None), fmla, etime)) in
        Tz.PSet.union (Tz.PSet.union failform_of_solved qset_failed_sset) acc_failed_sset
      ) in
      (* Remove queries from untouched query set *)
      let qset_solved_notime_form = Tz.PSet.map qset_solved_sset ~f:(fun (x, _, _) -> x) in
      let qset_failed_notime_form = Tz.PSet.map qset_failed_sset ~f:(fun (x, _, _, _) -> x) in
      let new_acc_untouched_sset = Tz.PSet.diff (Tz.PSet.diff acc_untouched_sset qset_solved_notime_form) qset_failed_notime_form in
      (new_acc_solved_smap, new_acc_failed_sset, new_acc_untouched_sset)
    ) in
  { solved_map=smap; failed_set=fset; untouched_set=uset }
end (* function solve_queries_wl end *)


let remove_solved_queries :
  query_state set
  -> (query_id, solved_query_state set) map
  -> query_state set
= fun all_queries solved_queries -> begin
  Tz.PMap.fold solved_queries ~init:all_queries
    ~f:(fun ~key:_ ~data acc_remain_queries -> Tz.PSet.diff acc_remain_queries (Tz.PSet.map data ~f:(fun (x,_,_) -> x)))
end (* function remove_solved_queries end *)


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
  print_endline (strop_j |> Yojson.Safe.pretty_to_string);
  (Tz.PSet.iter paths_j ~f:(fun x -> x |> Yojson.Safe.pretty_to_string |> print_endline))
end (* function f_print_sset end *)

let f_print_queries_pretty : Se.state_set -> unit
= fun sset -> begin
  let strop_j = TzCvt.T2Jnocc.cv_p1_ss_strop (Tz.PSet.choose_exn sset.queries |> Stdlib.fst) in
  let queries_j = Tz.PSet.map sset.queries ~f:(fun (ss, qc) -> `Assoc ["query-cat", TzCvt.S2J.cv_qc qc; "sym-state", TzCvt.T2Jnocc.cv_p1_ss_path ss]) in
  print_endline (strop_j |> Yojson.Safe.pretty_to_string);
  (Tz.PSet.iter queries_j ~f:(fun x -> x |> Yojson.Safe.pretty_to_string |> print_endline))
end (* function f_print_queries_pretty *)

let f_print_query_solved_result_simple_pretty : ret -> unit
= fun { solved_map; failed_set; untouched_set } -> begin
  (* Size calculation *)
  let solved_queries_size : int = Tz.PMap.fold solved_map ~init:0 ~f:(fun ~key:_ ~data acc -> acc + Tz.PSet.length data) in
  let (failed_size, untouched_size) = (Tz.PSet.length failed_set, Tz.PSet.length untouched_set) in
  (* Get query results *)
  let solved_queries : ((Tz.mich_cut_info * Se.query_category) * ((Utils.Timer.time * Tz.mich_f) Tz.PSet.t)) list = 
    solved_map
    |> Tz.PMap.map ~f:(fun data -> Tz.PSet.map data ~f:(fun (_, fmla, tm) -> (tm, fmla)))
    |> Tz.PMap.fold ~init:[] ~f:(fun ~key ~data accl -> (key, data) :: accl)
  in
  let failed_queries : ((Tz.mich_cut_info * Se.query_category) * ((ProverLib.Smt.ZSolver.validity * Utils.Timer.time * Tz.mich_f) Tz.PSet.t)) list =
    failed_set
    |> Tz.pmap_of_pset ~key_f:(fun ((ss,qc),(_,_),_,_) -> (ss.Tz.ss_block_mci,qc)) ~data_f:(fun (_,(vl,_),fmla,tm) -> (vl,tm,fmla))
    |> Tz.PMap.fold ~init:[] ~f:(fun ~key ~data accl -> (key, data) :: accl)
  in
  let untouched_queries : ((Tz.mich_cut_info * Se.query_category) * int) list = 
    Tz.PSet.map untouched_set ~f:(fun (ss, qc) -> (ss.ss_block_mci, qc)) 
    |> Tz.PSet.map ~f:(fun x -> let cnt = Tz.PSet.count untouched_set ~f:(fun (ss, qc) -> (ss.ss_block_mci, qc) = x) in (x, cnt))
    |> Tz.PSet.to_list 
  in
  (* Print Sizes *)
  let _ : unit = 
    Printf.printf 
      "#Total: %d\n#SolvedElem: %d\n#FailedSetElem: %d\n#UntouchedSetElem: %d\n"
      (solved_queries_size + failed_size + untouched_size) solved_queries_size failed_size untouched_size
  in
  (* Print Queries *)
  let _ : unit = 
    (* Print the number of queries *)
    let _ : unit =
      let (slen, flen, ulen) = (List.length solved_queries, List.length failed_queries, List.length untouched_queries) in
      Printf.printf
        "#Total Q: %d\n#Solved Q: %d\n#Failed Q: %d\n#Untouched Q: %d\n"
        (slen + flen + ulen) slen flen ulen
    in
    (* Print solved_queries *)
    let _ : unit =
      print_newline ();
      print_endline "<< SOLVED >>";
      print_newline ();
      List.iter 
        (fun ((mci, qc), tfset) ->
          `Assoc ["cut-info", TzCvt.T2Jnocc.cv_mich_cut_info mci;
                  "query-category", TzCvt.S2J.cv_qc qc;]
          |> Yojson.Safe.pretty_to_string |> print_endline;
          ();
          Tz.PSet.iter 
            ~f:(fun (t,fmla) ->
              `Tuple [`Int t; TzCvt.T2Jnocc.cv_mf fmla] 
              |> Yojson.Safe.to_string 
              |> print_endline
            )
            tfset
        ) 
        solved_queries
    in
    (* Print failed_queries *)
    let _ : unit = 
      print_newline ();
      print_endline "<< FAILED >>";
      print_newline ();
      List.iter 
      (fun ((mci, qc), vtfset) ->
        `Assoc ["cut-info", TzCvt.T2Jnocc.cv_mich_cut_info mci;
                "query-category", TzCvt.S2J.cv_qc qc;]
        |> Yojson.Safe.pretty_to_string |> print_endline;
        ();
        Tz.PSet.iter 
          ~f:(fun (v, t,fmla) ->
            `Tuple [`String (ProverLib.Smt.ZSolver.string_of_validity v);`Int t; TzCvt.T2Jnocc.cv_mf fmla] 
            |> Yojson.Safe.to_string 
            |> print_endline
          )
          vtfset
      ) 
      failed_queries
    in
    (* Print untouched_queries *)
    let _ : unit =
      print_newline ();
      print_endline "<< UNTOUCHED >>";
      print_newline ();
      let sqjl = 
        List.map
          (fun ((mci, qc), cnt) ->
            `Assoc ["cut-info", TzCvt.T2Jnocc.cv_mich_cut_info mci;
                    "query-category", TzCvt.S2J.cv_qc qc;
                    "count", `Int cnt;]
          )
          untouched_queries
      in
      `List sqjl |> Yojson.Safe.pretty_to_string |> print_endline
    in
    ()
  in
  ()
end (* function f_print_query_solved_result_simple_pretty end *)


(*****************************************************************************)
(*****************************************************************************)
(* Main function                                                             *)
(*****************************************************************************)
(*****************************************************************************)

(*  Input
    - Initial Storage (Tz Form, Option)
    - Initial Symbolic State
    - State Set (Symbolic Execution Result)
    
    Output
    - Solved query symbolic-states
      - The query symbolic-state can be returned only if the whole same-position queries are solved at once.
    - Failed query symbolic-states
    - Untouched query symbolic-states

    System-Effect
    - Utils.Options.z3_time_budget (internally restricts z3 time limit)
    - Utils.Options.prover_time_budget (total prover time limit - timeout will return accumulated result)
*)
let main : (Tz.mich_v Tz.cc option) * Tz.sym_state -> Se.state_set -> ret
= let extract_unsolved_queries : ret -> query_state set (* Gather not solved queries from the previous result *)
  = fun prev_res -> begin
    let qset_failed_notime_form = Tz.PSet.map prev_res.failed_set ~f:(fun (x, _, _, _) -> x) in
    Tz.PSet.union qset_failed_notime_form prev_res.untouched_set
  end in (* function extract_unsolved_queries end *)
  let union_result : prev_res:ret -> cur_res:ret -> ret (* Union solved query into current result *)
  = let pm_add_if_possible k v pmap = Tz.PMap.add pmap ~key:k ~data:v |> (function | `Ok m -> m | `Duplicate -> pmap) in (* syntax sugar *)
    fun ~prev_res ~cur_res -> begin
    let new_solved_map : (query_id, solved_query_state set) map = 
      cur_res.solved_map 
      |> Tz.PMap.fold ~init:prev_res.solved_map ~f:(fun ~key ~data acc_sm -> (
          pm_add_if_possible key data acc_sm)) in
    { cur_res with solved_map=new_solved_map }
  end in (* function union_result end *)
  fun (tz_init_stg_opt, init_ss) sset -> begin
  let init_stg_ss_opt : (Tz.mich_v Tz.cc * Tz.sym_state) option =
    Option.bind tz_init_stg_opt (fun init_stg -> Some (init_stg, init_ss)) in
  let inv_init : Se.invmap = sset.Se.blocked |> Se.true_invmap_of_blocked_sset in
  let w_init : worklist = [inv_init] in
  let res_init : ret = { solved_map=Tz.PMap.empty; failed_set=Tz.PSet.empty; untouched_set=sset.queries } in
  let comp_map : InvSyn.comp_map = InvSyn.bake_comp_map sset in
  let timer : Utils.Timer.t ref = Utils.Timer.create ~budget:!(Utils.Options.prover_time_budget) in
  let rec prove_loop : worklist -> ret -> ret = fun w prev_res -> begin
    if Utils.Timer.is_timeout timer || Core.List.is_empty w then prev_res else
    let inv_cand : Se.invmap =
      w |> Core.List.hd
      |> (function Some i -> i | None -> Error "main : prove_loop : inv_cand" |> Stdlib.raise) in
    let w' : worklist = 
      Core.List.tl w
      |> (function Some i -> i | None -> Error "main : prove_loop : w'" |> Stdlib.raise) in
    let inductive, _, _ = check_inv_inductiveness timer init_stg_ss_opt sset.blocked inv_cand in
    if inductive then
      let uqset : query_state set = extract_unsolved_queries prev_res in
      let cur_res : ret = solve_queries timer uqset inv_cand in
      let res : ret = union_result ~prev_res ~cur_res in
      if Tz.PSet.length res.failed_set = 0 && Tz.PSet.length res.untouched_set = 0 then res
      else
        let w'' : worklist = InvSyn.generate (res.failed_set, inv_cand, init_stg_ss_opt, comp_map) in
        let w''' : worklist = w'@w'' in
        prove_loop w''' res
    else prove_loop w' prev_res
    end in
  let res : ret = prove_loop w_init res_init in
  res
end (* function main end *)