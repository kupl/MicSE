(*  It can take same options as MicSE, but the options unrelated with (Preprocessing & Refuter) will be ignored.
    Process returns 0 if normally executed.
*)

(* Example Usages
  * If you want to make sure it's working
      dune exec -- micse.utils.stacked_refuter -input [PROJECT-ROOT]/benchmarks/toy/add1.tz
  * (3) uses refuter two time budgets ( and you might want to set z3 timeout too )
      dune exec -- micse.utils.stacked_refuter -input [PROJECT-ROOT]/benchmarks/toy/add1.tz -initial_storage [PROJECT-ROOT]/benchmarks/ -refuter_timeout_t 600 -refuter_timeout_s 120 -z3_timeout 10
*)

let trx_unroll_NUM = 4
let loop_unroll_NUM = 2

let merged_state_set_size_limit = 10000

let main : unit -> unit
= let open Stacked in
  fun () -> begin
  let pgmfilecontent : Stacked.Mich.program = !Utils.Options.input_file |> Stacked.Parse.parse in
  let strgfilecontentopt : (Mich.data Mich.t) option = 
    if !Utils.Options.initial_storage_file = "" 
      then None 
      else Some (Stacked.Parse.parse_data !Utils.Options.initial_storage_file)
  in
  let _ = Utils.Log.app (fun m -> m "Initial Storage Provided (Bool) : %b" (strgfilecontentopt <> None)) in
  let (tz_init_stg_opt, init_ss, cache, sset) = 
    Stacked.Prove.gen_sset pgmfilecontent strgfilecontentopt
  in
  let _ = ignore (init_ss, cache) in
  let classified_blocked_sset : (Tz.mich_cut_info, Tz.sym_state Tz.PSet.t) Tz.PMap.t = 
    Tz.pmap_of_pset sset.Se.blocked 
      ~key_f:(fun x -> x.ss_block_mci)
      ~data_f:(fun x -> x)
  in
  let classified_queries_sset : (Tz.mich_cut_info, (Tz.sym_state * Se.query_category) Tz.PSet.t) Tz.PMap.t = 
    Tz.pmap_of_pset sset.Se.queries
      ~key_f:(fun (x, _) -> x.ss_block_mci)
      ~data_f:(fun x -> x)
  in
  let expand_param : Merge.expand_param = {
    ep_bss = classified_blocked_sset;
    ep_uloop_lim = loop_unroll_NUM;
    ep_utrx_lim = trx_unroll_NUM;
  } in

(*

  (*****************************************************************************)
  (* 1. Simple expand & Count the size of the expanded set                     *)
  (*****************************************************************************)

  let rec expand_fixpoint : Merge.ms Tz.PSet.t -> Merge.ms Tz.PSet.t
  = fun msset -> begin
    if (Tz.PSet.length msset > merged_state_set_size_limit) then msset else
    let msset' = Merge.expand expand_param msset in
    let _ = Utils.Log.app (fun m -> m "expand_fixpoint : msset-size : %d, msset'-size : %d" (Tz.PSet.length msset) (Tz.PSet.length msset')) in
    if Tz.PSet.is_empty msset' then msset else expand_fixpoint msset'
  end in (* internal function expand_fixpoint end *)

  let _ =
    let open Tz in
    let open Merge in
    let open Utils in
    PMap.iteri classified_queries_sset 
      ~f:(fun ~key ~data -> 
        let _ = Log.app (fun m -> m "Query MCI = %s" (TzCvt.T2J.cv_mich_cut_info key |> Yojson.Safe.pretty_to_string)) in
        PSet.map data ~f:(fun (ss, qc) -> {ms_state=ss; ms_te_count=0; ms_le_count=PMap.empty; ms_le_stack=[]; ms_iinfo=empty_ms_iter_info; ms_querycat=(Some qc);})
        |> expand_fixpoint
        |> (fun fpset -> Log.app (fun m -> m "Set-size = %d" (PSet.length fpset)))
      )
  in

*)

(*

  (*****************************************************************************)
  (* 2. Simple expand & If refuted, stop.                                      *)
  (*****************************************************************************)

  let rec expand_and_refute : Utils.Timer.t ref -> Se.invmap -> int -> Merge.ms Tz.PSet.t -> ((ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option * Utils.Timer.time) option * int)
  = fun timer invm acc_count msset -> begin
    let setsize = Tz.PSet.length msset in
    if (setsize > merged_state_set_size_limit) then (None, acc_count) else
    let _ = Utils.Log.app (fun m -> m "expand_and_refute : msset-size : %d" setsize) in
    let (result, new_acc_count) : (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option * Utils.Timer.time) option * int = 
      Tz.PSet.fold msset ~init:(None, acc_count)
        ~f:(fun (accopt, accc) ms -> 
          if (accopt <> None) || (Merge.is_trxentry_path ms |> Stdlib.not) 
          then (accopt, accc)
          else (
            let (vld, mopt, time) = Refute.check_ppath_validity timer invm ms in
            if ProverLib.Smt.ZSolver.is_invalid vld then (Some (vld, mopt, time), (accc+1)) else (None, (accc+1))
          )
        )
    in
    (match result with 
      | None -> expand_and_refute timer invm new_acc_count (Merge.expand expand_param msset)
      | Some s -> (Some s, new_acc_count)
    )
  end in (* internal function expand_and_find end *)

  let _ =
    let open Tz in
    let open Merge in
    let open Utils in
    let true_invmap : Se.invmap = Se.true_invmap_of_blocked_sset sset.Se.blocked in
    PMap.iteri classified_queries_sset 
      ~f:(fun ~key ~data -> 
        let timer : Utils.Timer.t ref = (Utils.Timer.create ~budget:0) in
        let _ = Log.app (fun m -> m "Query MCI = %s" (TzCvt.T2J.cv_mich_cut_info key |> Yojson.Safe.pretty_to_string)) in
        PSet.map data ~f:(fun (ss, qc) -> {ms_state=ss; ms_te_count=0; ms_le_count=PMap.empty; ms_le_stack=[]; ms_iinfo=empty_ms_iter_info; ms_querycat=(Some qc);})
        |> expand_and_refute timer true_invmap 0
        |> (fun (result, acc_count) -> (Utils.Timer.read_interval timer, result, acc_count))
        |> (fun (time, result, acc_count) -> 
            match result with 
            | None -> Log.app (fun m -> m "Cannot Refute this query / elapsed_time : %d / Searched TP : %d" time acc_count)
            | Some (_, _, time) -> Log.app (fun m -> m "Refuted / elapsed_time : %d / Searched TP : %d" time acc_count)
          )
      )
  in

*)

  (*****************************************************************************)
  (* 2.1. Simple expand & If refuted, stop.                                    *)
  (*****************************************************************************)

  (* INFO : It uses "Utils.Options.refuter_total_time_budget" for refuter total time budget, 
            and "Utils.Options.refuter_sub_time_budget" for query-id refuting time budget.
  *)

  let rec expand_and_refute : Utils.Timer.t ref -> Se.invmap -> (int * int) -> Merge.ms Tz.PSet.t -> ((ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option * Utils.Timer.time) option * int * int)
  = fun timer invm (acc_ppcount, acc_count) msset -> begin
    let setsize = Tz.PSet.length msset in
    if (setsize > merged_state_set_size_limit) then (None, acc_ppcount, acc_count) else
    let _ = Utils.Log.app (fun m -> m "<< prune_expand_and_refute : msset-size : %d >>" setsize) in
    let (result, filtered_paths, new_acc_ppcount, new_acc_count) : (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option * Utils.Timer.time) option * (Merge.ms Tz.PSet.t) * int * int = 
      let timeout_printed = ref false in
      Tz.PSet.fold msset ~init:(None, Tz.PSet.empty, acc_ppcount, acc_count)
        ~f:(fun (accopt, accp, accppc, accc) ms -> 
          let orig = (accopt, accp, accppc, accc) in
          if Utils.Timer.is_timeout timer then (if !timeout_printed then () else (timeout_printed := true; Utils.Log.warn (fun m -> m "Query-Id Refuter Timeout")); orig) else
          if (accopt <> None) then (accopt, accp, accppc, accc) else
          (match (Merge.is_trxentry_path ms) with
          | false -> (
              (* 1. If the path is Partial Path - Just Expand *)
              (None, Tz.PSet.add accp ms, accppc+1, accc)
            )
          | true -> (
              (* 2. If the path is Total Path *)
              let (vld, mopt, time) = Refute.check_ppath_validity timer tz_init_stg_opt invm ms in
              let _ = Utils.Log.info (fun m -> m "Total-Path\tValidity : %s\tAcc-Time : %d" (ProverLib.Smt.ZSolver.string_of_validity vld) (Utils.Timer.read_interval timer)) in
              match (ProverLib.Smt.ZSolver.is_invalid vld, ProverLib.Smt.ZSolver.is_valid vld) with
              | true, _ -> (Some (vld, mopt, time), accp, accppc, accc+1)
              | false, _ -> (None, Tz.PSet.add accp ms, accppc, accc+1)
            )
          )
        )
    in
    if (Utils.Timer.is_timeout timer) then (Utils.Log.warn (fun m -> m "Query-Id Refuter Halted"); (None, new_acc_ppcount, new_acc_count)) else
    let _ = Utils.Log.app (fun m -> m "FilteredPathSize : %d" (Tz.PSet.length filtered_paths)) in
    (match result with 
      | None -> 
        if (Tz.PSet.length filtered_paths = 0) 
        then (None, new_acc_ppcount, new_acc_count) 
        else (expand_and_refute timer invm (new_acc_ppcount, new_acc_count) (Merge.expand expand_param filtered_paths))
      | Some s -> (Some s, new_acc_ppcount, new_acc_count)
    )
  end in (* internal function expand_and_find end *)

  let _ =
    let open Tz in
    (* let open Merge in *)
    let open Utils in
    let true_invmap : Se.invmap = Se.true_invmap_of_blocked_sset sset.Se.blocked in
    let total_refuter_timer : Utils.Timer.t ref = (Utils.Timer.create ~budget:(!Utils.Options.refuter_total_time_budget)) in
    PMap.iteri classified_queries_sset 
      ~f:(fun ~key ~data -> 
        let _ = Log.app (fun m -> m "\nQuery MCI = %s" (TzCvt.T2J.cv_mich_cut_info key |> Yojson.Safe.pretty_to_string)) in
        if Utils.Timer.is_timeout total_refuter_timer then (Log.warn (fun m -> m "Refuter Total Timeout")) else
        let timer : Utils.Timer.t ref = (Utils.Timer.create ~budget:(!Utils.Options.refuter_sub_time_budget)) in
        PSet.map data 
          ~f:(fun (ss, qc) -> Merge.construct_first_ms (ss, Some qc))
        |> expand_and_refute timer true_invmap (0,0)
        |> (fun (result, acc_ppcount, acc_count) -> (Utils.Timer.read_interval timer, result, acc_ppcount, acc_count))
        |> (fun (time, result, acc_ppcount, acc_count) -> 
            match result with 
            | None -> Log.app (fun m -> m "Unknown / elapsed_time : %d / Searched PP : %d / Searched TP : %d" time acc_ppcount acc_count)
            | Some (_, _, time) -> Log.app (fun m -> m "Refuted / elapsed_time : %d / Searched PP : %d / Searched TP : %d" time acc_ppcount acc_count)
          )
      )
  in









(*

  (*****************************************************************************)
  (* 3. Simple expand & pruning & If refuted, stop.                            *)
  (*****************************************************************************)

  (* INFO : It uses "Utils.Options.refuter_total_time_budget" for refuter total time budget, 
            and "Utils.Options.refuter_sub_time_budget" for query-id refuting time budget.
  *)

  let rec prune_expand_and_refute : Utils.Timer.t ref -> Se.invmap -> (int * int) -> Merge.ms Tz.PSet.t -> ((ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option * Utils.Timer.time) option * int * int)
  = fun timer invm (acc_ppcount, acc_count) msset -> begin
    let setsize = Tz.PSet.length msset in
    let max_trx_num : int = Tz.PSet.fold msset ~init:0 ~f:(fun accn ms -> Stdlib.max accn ms.ms_te_count) in
    if (setsize > merged_state_set_size_limit) then (let _ = Utils.Log.warn (fun m -> m "path explodes with size %d" setsize) in (None, acc_ppcount, acc_count)) else
    let _ = Utils.Log.app (fun m -> m "<< prune_expand_and_refute : msset-size : %d >>" setsize) in
    let _ = Utils.Log.app (fun m -> m "largest-trxnum : %d" max_trx_num) in
    let (result, filtered_paths, new_acc_ppcount, new_acc_count) : (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option * Utils.Timer.time) option * (Merge.ms Tz.PSet.t) * int * int = 
      let timeout_printed = ref false in
      Tz.PSet.fold msset ~init:(None, Tz.PSet.empty, acc_ppcount, acc_count)
        ~f:(fun (accopt, accp, accppc, accc) ms -> 
          let orig = (accopt, accp, accppc, accc) in
          if Utils.Timer.is_timeout timer then (if !timeout_printed then () else (timeout_printed := true; Utils.Log.warn (fun m -> m "Query-Id Refuter Timeout")); orig) else
          if (accopt <> None) then (accopt, accp, accppc, accc) else
          (match (Merge.is_trxentry_path ms) with
          | false -> (
              (* 1. If the path is Partial Path *)
              let (vld, mopt, time) = Refute.check_ppath_validity timer None invm ms in
              let _ = ignore (mopt, time) in
              let _ = Utils.Log.info (fun m -> m "Partial-Path\tValidity : %s\tAcc-Time : %d" (ProverLib.Smt.ZSolver.string_of_validity vld) (Utils.Timer.read_interval timer)) in
              match (ProverLib.Smt.ZSolver.is_invalid vld, ProverLib.Smt.ZSolver.is_valid vld) with
              | _, true  -> ((* partialpath & valid *) (None, accp, accppc+1, accc))
              | _, false -> ((* partialpath & unknown *) (None, Tz.PSet.add accp ms, accppc+1, accc))
            )
          | true -> (
              (* 2. If the path is Total Path *)
              let (vld, mopt, time) = Refute.check_ppath_validity timer tz_init_stg_opt invm ms in
              let _ = Utils.Log.info (fun m -> m "Total-Path\tValidity : %s\tAcc-Time : %d" (ProverLib.Smt.ZSolver.string_of_validity vld) (Utils.Timer.read_interval timer)) in
              match (tz_init_stg_opt, ProverLib.Smt.ZSolver.is_invalid vld, ProverLib.Smt.ZSolver.is_valid vld) with
              | Some _, true, _ -> (
                  (* init-stg given & totalpath & refuted *)
                  (* let _ = Utils.Log.debug (fun m -> m "%s" (ProverLib.Smt.ZModel.to_string (Option.get mopt))) in *)
                  (Some (vld, mopt, time), accp, accppc, accc+1)
                )
              | Some _, _, true -> (
                  (* init-stg given & totalpath & valid *)
                  (* Check if the path can be excluded *)
                  let (vld2, mopt2, time2) = Refute.check_ppath_validity timer None invm ms in
                  let _ = Utils.Log.info (fun m -> m "-- Prunable-Check : %b\tAcc-Time : %d\t" (vld2 = ProverLib.Smt.ZSolver.VAL) (Utils.Timer.read_interval timer)) in
                  let _ = ignore (mopt2, time2) in
                  match (ProverLib.Smt.ZSolver.is_invalid vld2, ProverLib.Smt.ZSolver.is_valid vld2) with
                  | _, true -> (* valid ==> able to exclude *) (None, accp, accppc, accc+1)
                  | _, _ -> (* invalid or unknown ==> cannot exclude *) (None, Tz.PSet.add accp ms, accppc, accc+1)
                )
              | Some _, false, false -> (
                  (* init-stg given & totalpath & unknown *)
                  (None, Tz.PSet.add accp ms, accppc, accc+1)
                )
              | None, true, _ -> (
                  (* init-stg not given & totalpath & refuted *)
                  (* let _ = Utils.Log.debug (fun m -> m "%s" (ProverLib.Smt.ZModel.to_string (Option.get mopt))) in *)
                  (Some (vld, mopt, time), accp, accppc, accc+1)
                )
              | None, _, true -> (
                  (* init-stg not given & totalpath & valid *)
                  (None, accp, accppc, accc+1)
                )
              | None, false, false -> (
                  (* init-stg not given & totalpath & unknown *)
                  (None, Tz.PSet.add accp ms, accppc, accc+1)
                )
            )
          )
        )
    in
    if (Utils.Timer.is_timeout timer) then (Utils.Log.warn (fun m -> m "Query-Id Refuter Halted"); (None, new_acc_ppcount, new_acc_count)) else
    let _ = Utils.Log.app (fun m -> m "FilteredPathSize : %d" (Tz.PSet.length filtered_paths)) in
    (match result with 
      | None -> 
        if (Tz.PSet.length filtered_paths = 0) 
        then (None, new_acc_ppcount, new_acc_count) 
        else (prune_expand_and_refute timer invm (new_acc_ppcount, new_acc_count) (Merge.expand expand_param filtered_paths))
      | Some s -> (Some s, new_acc_ppcount, new_acc_count)
    )
  end in (* internal function expand_and_find end *)

  let _ =
    let open Tz in
    let open Merge in
    let open Utils in
    let true_invmap : Se.invmap = Se.true_invmap_of_blocked_sset sset.Se.blocked in
    let total_refuter_timer : Utils.Timer.t ref = (Utils.Timer.create ~budget:(!Utils.Options.refuter_total_time_budget)) in
    PMap.iteri classified_queries_sset 
      ~f:(fun ~key ~data -> 
        let _ = Log.app (fun m -> m "\nQuery MCI = %s" (TzCvt.T2J.cv_mich_cut_info key |> Yojson.Safe.pretty_to_string)) in
        if Utils.Timer.is_timeout total_refuter_timer then (Log.warn (fun m -> m "Refuter Total Timeout")) else
        let timer : Utils.Timer.t ref = (Utils.Timer.create ~budget:(!Utils.Options.refuter_sub_time_budget)) in
        PSet.map data 
          ~f:(fun (ss, qc) -> ~f:(fun (ss, qc) -> Merge.construct_first_ms (ss, Some qc))
        |> prune_expand_and_refute timer true_invmap (0,0)
        |> (fun (result, acc_ppcount, acc_count) -> (Utils.Timer.read_interval timer, result, acc_ppcount, acc_count))
        |> (fun (time, result, acc_ppcount, acc_count) -> 
            match result with 
            | None -> Log.app (fun m -> m "Unknown / elapsed_time : %d / Searched PP : %d / Searched TP : %d" time acc_ppcount acc_count)
            | Some (_, _, time) -> Log.app (fun m -> m "Refuted / elapsed_time : %d / Searched PP : %d / Searched TP : %d" time acc_ppcount acc_count)
          )
      )
  in

*)



  ()
end (* function main end *)

let _ = begin
  let _ = Utils.Options.create_options () in
  let _ = Printexc.record_backtrace true in
  let _ = Utils.Log.create () in
  try
    if !Utils.Options.input_file <> ""
    then
      let _ = main () in
      ()
    else
      raise (Failure "No_Input")
  with
  | exc -> prerr_endline (Printexc.to_string exc);
end