(* Refuter *)

(* IMPORTANT NOTE
  Every concepts in "Se" module were designed with forward symbolic execution in mind.
  So, well-constructed "Se.sym_state" and "Se.state_set" contains 
  various Tezos system abstraction such as fixed/dynamic blockchain status
  and internal operation queues (though queue will be changed into stack for later
  Tezos version).
  
  However, this "Refute" module uses state-merging (module "Merge") to construct
  symbolic-executed results from back to forward, it is impossible to form soundly
  blockchain-specific abstractions when merging two different transaction states.

  Therefore, "Merge" and "Refute" modules will not strictly follows the Tezos
  abstraction defined in "Se".
   
  Instead, they will consider following Tezos/Michelson abstractions only:
  [ Refute Target Properties ]
  - "Tz.ss_entry_mci", "Tz.ss_entry_symstack", "Tz.ss_block_mci", "Tz.ss_symstack", "Tz.ss_constraints"
  - "Se.query_category"
  - "Jc.Rcfv" to track non-michelson contexts
  - "Jc.Stvn" to avoid variable name conflicts when merging two states
*)

let check_ppath_validity : Utils.Timer.t ref -> (Tz.mich_v Tz.cc option) -> Se.invmap -> Merge.ms -> (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option * Utils.Timer.time)
= fun timer tz_init_stg_option invm ms -> begin
  let start_time = Utils.Timer.read_interval timer in
  let (ss, qc) = (ms.ms_state, (match ms.ms_querycat with | Some c -> c | None -> Stdlib.failwith Stdlib.__LOC__)) in
  let qfmla : Tz.mich_f = 
    match (Merge.is_trxentry_path ms, tz_init_stg_option) with
    | true, Some istg -> 
      let precond : Tz.mich_f = Tz.MF_eq (Tz.MV_cdr (List.hd ms.ms_state.ss_entry_symstack) |> Tz.gen_dummy_cc, istg) in
      Se.inv_query_fmla_with_precond (ss, qc) invm precond
    | true, None -> Se.inv_query_fmla (ss, qc) invm
    | false, _ -> Se.inv_query_fmla (ss, qc) invm
  in
  (* let _ = Utils.Log.debug (fun m -> m "[%s] %s" (Stdlib.__LOC__) (TzCvt.T2Jnocc.cv_mf qfmla |> Yojson.Safe.to_string)) in *)
  let (vld, mopt) = Prove.check_validity qfmla in
  let elapsed_time = Utils.Timer.read_interval timer - start_time in
  (vld, mopt, elapsed_time)
end (* functino check_ppath_validity *)

(* "check_ppath_validity_fmla_included" is just a copy of "check_ppath_validity", but returns mich_f value too *)
let check_ppath_validity_fmla_included : Utils.Timer.t ref -> (Tz.mich_v Tz.cc option) -> Se.invmap -> Merge.ms -> (Tz.mich_f * ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option * Utils.Timer.time)
= fun timer tz_init_stg_option invm ms -> begin
  let start_time = Utils.Timer.read_interval timer in
  let (ss, qc) = (ms.ms_state, (match ms.ms_querycat with | Some c -> c | None -> Stdlib.failwith Stdlib.__LOC__)) in
  let qfmla : Tz.mich_f = 
    match (Merge.is_trxentry_path ms, tz_init_stg_option) with
    | true, Some istg -> 
      let precond : Tz.mich_f = Tz.MF_eq (Tz.MV_cdr (List.hd ms.ms_state.ss_entry_symstack) |> Tz.gen_dummy_cc, istg) in
      Se.inv_query_fmla_with_precond (ss, qc) invm precond
    | true, None -> Se.inv_query_fmla (ss, qc) invm
    | false, _ -> Se.inv_query_fmla (ss, qc) invm
  in
  (* let _ = Utils.Log.debug (fun m -> m "[%s] %s" (Stdlib.__LOC__) (TzCvt.T2Jnocc.cv_mf qfmla |> Yojson.Safe.to_string)) in *)
  let (vld, mopt) = Prove.check_validity qfmla in
  let elapsed_time = Utils.Timer.read_interval timer - start_time in
  (qfmla, vld, mopt, elapsed_time)
end (* function check_ppath_validity_fmla_included end *)


(*

(******************************************************************************)
(******************************************************************************)
(* Types                                                                      *)
(******************************************************************************)
(******************************************************************************)

(* typew with prefix "rqu" :
  these types exists to describe "f_refute_queries_naive_unroll"'s return type.
*)

(* rqu : query result summary - summary for a query-id *)
type rqu_s = 
  | Rqus_proved
  | Rqus_refuted of Prove.query_state * Merge.ms * Tz.mich_f * ProverLib.Smt.ZModel.t * Utils.Timer.time
  | Rqus_unknown
  | Rqus_untouched

(* rqu : smt-result for rqu_d *)
type rqu_smtr = 
  | Rquds_proved of Utils.Timer.time
  | Rquds_refuted of Tz.mich_f * ProverLib.Smt.ZModel.t * Utils.Timer.time
  | Rquds_unknown of Utils.Timer.time
  | Rquds_untouched

(* rqu : query result details - depicts every results of  *)
type rqu_d = (Prove.query_state * (Merge.ms * rqu_smtr) list) list

(* rqu : return type *)
type rqu_ret = (Prove.query_id, rqu_s * (rqu_d option)) Tz.PMap.t




(******************************************************************************)
(******************************************************************************)
(* Small functionalities, returns unit value only                             *)
(******************************************************************************)
(******************************************************************************)

type _rqu_w2 = Prove.query_state * rqu_smtr
type _rqu_w1 = Prove.query_id * (_rqu_w2 list)

type f_refute_queries_naive_unroll_param = {
  rqu_timer : Utils.Timer.t ref;
  rqu_sset : Se.state_set;
  rqu_pret : Prove.ret;
}

type f_refute_queries_naive_unroll_output = {
  rquo_v : rqu_ret;
}

let f_refute_queries_naive_unroll : f_refute_queries_naive_unroll_param -> f_refute_queries_naive_unroll_output
= fun param -> begin
  let {rqu_timer=timer; rqu_sset=sset; rqu_pret=pret;} = param in
  let {running=_; blocked; queries; terminated=_;} : Se.state_set = sset in
  let {solved_map; failed_set; untouched_set;} : Prove.ret = pret in

  (* Generate summary "rqu_s" from the details "rqu_d" *)
  let rec gen_summary : rqu_d -> rqu_s =
    fun qs_msl_l -> begin
    match qs_msl_l with 
    | [] -> Rqus_unknown
    | (qs, msl_l) :: t -> 
      (List.find_opt (function | (_, Rquds_refuted _) -> true | _ -> false) msl_l)
      |> (function 
        | None -> gen_summary t 
        | Some (ms, Rquds_refuted (mf, mdl, time)) -> Rqus_refuted (qs, ms, mf, mdl, time)
        | Some _ -> Stdlib.failwith Stdlib.__LOC__
        )
  end in (* internal function gen_summary end *)

  (* Worklist Alg. 2 (inner-recursion : construct detail of the given query-state and udpate rqu_d) *)
  let rec worklist_a2 : (_rqu_w2 list * rqu_d) -> (_rqu_w2 list * rqu_d)
  = fun (qrl, det_acc) -> begin
  end in  (* internal function worklist_a2 end *)

  (* Worklist Alg. 1 (outer-recursion : construct summary of the given query-id and update rqu_ret) *)
  let rec worklist_a1 : (_rqu_w1 list * rqu_ret) -> (_rqu_w1 list * rqu_ret)
  = fun (wl, ret_acc) -> begin
    (* Check escape conditions *)
    (match (wl, Utils.Timer.is_timeout timer) with
    | [], _ -> ([], ret_acc) (* "worklist_a1" ends *)
    | _ :: _, true -> (
        (* Timeout. Set remaining query-ids to Rqus_untouched *)
        List.fold_left 
          (fun accr (qid, w2l) -> 
            let d_of_w2l : rqu_d = List.map (fun (qs, _) -> (qs, [])) w2l in
            Tz.PMap.add accr ~key:qid ~data:(Rqus_untouched, Some d_of_w2l)
            |> (function | `Duplicate -> Stdlib.failwith Stdlib.__LOC__ | `Ok q -> q)
          ) 
          ret_acc 
          wl
        |> (fun rv -> ([], rv))
      )
    | (qid, w2l) :: t, false -> (
        (* Normal Execution. Call worklist_a2, construct summary and update it, and then call worklist_a1 again *)
        worklist_a2 (w2l, [])
        |> (fun (_, d) -> 
            (* If refuted exists, Rqus_refuted, else Rqus_unknown will be assigned in summary *)
            Tz.PMap.add ret_acc ~key:qid ~data:(gen_summary d, Some d)
            |> (function | `Duplicate -> Stdlib.failwith Stdlib.__LOC__ | `Ok q -> q)
          )
        |> (fun ret -> worklist_a1 (t, ret))
      )
    )
  end in (* internal function worklist_a1 end *)

  (* "failed_query_state" to "rqu_smtr" *)
  let fqs_to_smtr : Prove.failed_query_state -> rqu_smtr
  = fun (_, (vld, mopt), mf, time) -> begin
    match vld with
    | ProverLib.Smt.ZSolver.VAL -> Rquds_proved time
    | ProverLib.Smt.ZSolver.INVAL -> Rquds_refuted (mf, (Option.get mopt), time)
    | ProverLib.Smt.ZSolver.UNKNOWN -> Rquds_unknown time
  end in (* internal function fqs_to_smtr *)

  (* "initial_worklist" : Collect failed/untouched query-states, divided by query-id *)
  (* Convert from "(Prove.query_id, ((Prove.query_state * rqu_smtr) list) Tz.PMap.t" to "_rqu_w list" *)
  let initial_worklist : _rqu_w1 list = 
    Tz.PMap.empty
    |> (fun retacc ->
        Tz.PSet.fold failed_set ~init:retacc
          ~f:(fun accm fqs ->
            let ((ss, qc), (_, _), _, _) = fqs in
            let (k, v) = ((ss.ss_block_mci, qc), ((ss, qc), fqs_to_smtr fqs)) in
            Tz.PMap.update accm k ~f:(function | None -> [v] | Some s -> v :: s)
          )
      )
    |> (fun retacc ->
      Tz.PSet.fold untouched_set ~init:retacc
        ~f:(fun accm (ss, qc) ->
          let (k, v) = ((ss.ss_block_mci, qc), ((ss, qc), Rquds_untouched)) in
          Tz.PMap.update accm k ~f:(function | None -> [v] | Some s -> v :: s)
        )
      )
    (* Convert from "(Prove.query_id, ((Prove.query_state * rqu_smtr) list) Tz.PMap.t" to "_rqu_w list" *)
    |> Tz.PMap.to_alist
  in
  
  (* Run worklist Alg. *)
  (initial_worklist, Tz.PMap.empty)
  |> worklist_a1
  (* Fill in "Already Proved Queries" as Rquq*)
  |> (fun (_, ret) -> 
      Tz.PMap.fold solved_map ~init:ret 
        ~f:(fun ~key ~data:_ accr -> 
            Tz.PMap.add accr ~key:key ~data:(Rqus_proved, None)
            |> (function | `Duplicate -> Stdlib.failwith Stdlib.__LOC__ | `Ok q -> q)
        )
    )
  |> (fun x -> {rquo_v = x;})
end (* function "f_refute_queries_naive_unroll" end *)

(* 
let f_refute_queries_naive_unroll : Utils.Timer.t ref -> Se.state_set -> Prove.ret -> frqnu_ret
= fun timer sset pret -> begin
  let {running=_; blocked; queries; terminated=_;} : Se.state_set = sset in
  let {solved_map; failed_set; untouched_set;} : Prove.ret = pret in
  (* Construct *)
  (* Gather same-position queries using Tz.ss_block_mci
    => but solved-queries and solved-query
  *)
  let spos_qmap : ((Tz.mich_cut_info * Se.query_category), (Tz.sym_state * Se.query_category) Tz.PSet.t) Tz.PMap.t =
    Tz.pmap_of_pset queries ~key_f:(fun (ss, qcat) -> (ss.ss_block_mci, qcat)) ~data_f:(fun x -> x)
  in
  (* Gather query (query-mci * query-category) by removing proved queries *)
end (* function "f_refute_queries_naive_unroll" end *)
 *)


*)
