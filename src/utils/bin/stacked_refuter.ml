(*  It can take same options as MicSE, but the options unrelated with (Preprocessing & Refuter) will be ignored.
    Process returns 0 if normally executed.
*)

(* Example Usages
  * If you want to make sure it's working
      dune exec -- micse.utils.stacked_refuter -input [PROJECT-ROOT]/benchmarks/toy/add1.tz
*)

let trx_unroll_NUM = 1
let loop_unroll_NUM = 2

let merged_state_set_size_limit = 10000

let main : unit -> unit
= let open Stacked in
  fun () -> begin
  let pgmfilecontent : PreLib.Adt.t = !Utils.Options.input_file |> PreLib.Adt.parse in
  let strgfilecontentopt : PreLib.Adt.data option = 
    if !Utils.Options.initial_storage_file = "" 
      then None 
      else Some (PreLib.Adt.parse_data !Utils.Options.initial_storage_file)
  in
  let (tz_init_stg_opt, init_ss, cache, sset) = 
    Stacked.Prove.gen_sset pgmfilecontent strgfilecontentopt
  in
  let _ = ignore (tz_init_stg_opt, init_ss, cache) in
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





  (*****************************************************************************)
  (* 3. Simple expand & pruning & If refuted, stop.                            *)
  (*****************************************************************************)

  let rec prune_expand_and_refute : Utils.Timer.t ref -> Se.invmap -> (int * int) -> Merge.ms Tz.PSet.t -> ((ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option * Utils.Timer.time) option * int * int)
  = fun timer invm (acc_ppcount, acc_count) msset -> begin
    let setsize = Tz.PSet.length msset in
    if (setsize > merged_state_set_size_limit) then (None, acc_ppcount, acc_count) else
    let _ = Utils.Log.app (fun m -> m "prune_expand_and_refute : msset-size : %d" setsize) in
    let (result, filtered_paths, new_acc_ppcount, new_acc_count) : (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option * Utils.Timer.time) option * (Merge.ms Tz.PSet.t) * int * int = 
      Tz.PSet.fold msset ~init:(None, Tz.PSet.empty, acc_ppcount, acc_count)
        ~f:(fun (accopt, accp, accppc, accc) ms -> 
          if (accopt <> None) (*|| (Merge.is_trxentry_path ms |> Stdlib.not) *)
          then (accopt, accp, accppc, accc)
          else (
            let (vld, mopt, time) = Refute.check_ppath_validity timer invm ms in
            match (Merge.is_trxentry_path ms, ProverLib.Smt.ZSolver.is_invalid vld, ProverLib.Smt.ZSolver.is_valid vld) with
            | true, true, _ -> (* totalpath & refuted *) (Some (vld, mopt, time), accp, accppc, accc+1)
            | true, false, _ -> (* totalpath & unknown *) (None, Tz.PSet.add accp ms, accppc, accc+1)
            | false, _, true -> (* partialpath & valid *) (None, accp, accppc+1, accc)
            | false, _, false -> (* partialpath & unknown *) (None, Tz.PSet.add accp ms, accppc+1, accc)
            )
            (* if ProverLib.Smt.ZSolver.is_invalid vld then (Some (vld, mopt, time), (accc+1)) else (None, (accc+1)) *)
        )
    in
    (match result with 
      | None -> prune_expand_and_refute timer invm (new_acc_ppcount, new_acc_count) (Merge.expand expand_param filtered_paths)
      | Some s -> (Some s, new_acc_ppcount, new_acc_count)
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
        |> prune_expand_and_refute timer true_invmap (0,0)
        |> (fun (result, acc_ppcount, acc_count) -> (Utils.Timer.read_interval timer, result, acc_ppcount, acc_count))
        |> (fun (time, result, acc_ppcount, acc_count) -> 
            match result with 
            | None -> Log.app (fun m -> m "Cannot Refute this query / elapsed_time : %d / Searched PP : %d / Searched TP : %d" time acc_ppcount acc_count)
            | Some (_, _, time) -> Log.app (fun m -> m "Refuted / elapsed_time : %d / Searched PP : %d / Searched TP : %d" time acc_ppcount acc_count)
          )
      )
  in


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