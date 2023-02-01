(* ExecFlow provides main execution flow functions for testing. *)

open! Core

(******************************************************************************)
(******************************************************************************)
(* Execution Flow Components                                                  *)
(******************************************************************************)
(******************************************************************************)

let initial_system_setting : string array option -> unit =
  fun argv_opt ->
  let _ = Utils.Log.debug (fun m -> m "ExecFlow.initial_system_setting start") in
  let _ = Utils.Argument.create argv_opt in
  let _ = Utils.Log.create () in
  let _ = Printexc.record_backtrace true in
  let _ =
     if !Utils.Argument.set_random_seed
     then Random.self_init ()
     else Random.init 0
  in
  let _ = Utils.Log.debug (fun m -> m "ExecFlow.initial_system_setting end") in
  ()
(* function initial_system_setting end *)

let parsing : unit -> Mich.program * Mich.data Mich.t option =
  fun () ->
  let _ = Utils.Log.debug (fun m -> m "ExecFlow.parsing start") in
  let (pgm : Mich.program) =
     MichParse.parse ~filename:!Utils.Argument.input_file
     |> Mich.subst_standard_macro_all_pgm
     |> Mich.optm_all_pgm
     |> Mich.fill_position_all_pgm ~update_loc:false
  and (strg_opt : Mich.data Mich.t option) =
     if equal_string !Utils.Argument.input_storage_file ""
     then None
     else
       Some (MichParse.parse_data ~filename:!Utils.Argument.input_storage_file)
  in
  let _ =
     Utils.Log.info (fun m ->
         m "> # of Instructions: %d" (Mich.count_inst_pgm pgm)
     )
  in
  let _ = Utils.Log.debug (fun m -> m "ExecFlow.parsing end") in
  (pgm, strg_opt)
(* function parsing end *)

let tz_rep :
    Mich.program * Mich.data Mich.t option ->
    (Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc)
    * Tz.mich_v Tz.cc option =
  fun (mich_pgm, mich_init_strg_opt) ->
  let _ = Utils.Log.debug (fun m -> m "ExecFlow.tz_rep start") in
  let (tz_pgm : Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc) =
     TzUtil.M2T.cv_program mich_pgm
  in
  let (tz_init_strg_opt : Tz.mich_v Tz.cc option) =
     Option.map mich_init_strg_opt ~f:(fun d ->
         TzUtil.M2T.cv_datat mich_pgm.storage d
     )
  in
  let _ = Utils.Log.debug (fun m -> m "ExecFlow.tz_rep end") in
  (tz_pgm, tz_init_strg_opt)
(* function tz_rep end *)

let sym_exec :
    Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc ->
    Se.se_result * Tz.sym_state =
  fun tz_pgm ->
  let _ = Utils.Log.debug (fun m -> m "ExecFlow.sym_exec start") in
  let result = Se.run_inst_entry tz_pgm in
  let _ = Utils.Log.debug (fun m -> m "ExecFlow.sym_exec end") in
  result

(* function sym_exec end *)

(******************************************************************************)
(******************************************************************************)
(* Execution Flow                                                             *)
(******************************************************************************)
(******************************************************************************)

let upto_initial_system_setting : string array option -> unit =
  (fun args -> initial_system_setting args)
(* function upto_initial_system_setting end *)

let upto_parsing : string array option -> Mich.program * Mich.data Mich.t option
    =
  fun argv_opt ->
  upto_initial_system_setting argv_opt;
  let ((mich_pgm : Mich.program), (mich_init_strg_opt : Mich.data Mich.t option))
      =
     parsing ()
  in
  (mich_pgm, mich_init_strg_opt)
(* function upto_parsing end *)

let upto_tz_rep :
    string array option ->
    (Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc)
    * Tz.mich_v Tz.cc option =
  fun argv_opt ->
  let ((mich_pgm : Mich.program), (mich_init_strg_opt : Mich.data Mich.t option))
      =
     upto_parsing argv_opt
  in
  let ( (tz_pgm : Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc),
        (tz_init_strg_opt : Tz.mich_v Tz.cc option)
      ) =
     tz_rep (mich_pgm, mich_init_strg_opt)
  in
  (tz_pgm, tz_init_strg_opt)
(* function upto_tz_rep end *)

let upto_sym_exec :
    string array option ->
    (Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc)
    * Tz.mich_v Tz.cc option
    * Se.se_result
    * Tz.sym_state =
  fun argv_opt ->
  let ( (tz_pgm : Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc),
        (tz_init_strg_opt : Tz.mich_v Tz.cc option)
      ) =
     upto_tz_rep argv_opt
  in
  let ((se_result : Se.se_result), (se_init_state : Tz.sym_state)) =
     sym_exec tz_pgm
  in
  (tz_pgm, tz_init_strg_opt, se_result, se_init_state)
(* function upto_sym_exec end *)

let refuter_naive_run : string array option -> Res.config * Res.res =
  fun argv_opt ->
  let ( (_, _, (tz_code : Tz.mich_i Tz.cc)),
        (init_strg_opt : Tz.mich_v Tz.cc option),
        (se_result : Se.se_result),
        (init_state : Tz.sym_state)
      ) =
     upto_sym_exec argv_opt
  in
  let (cfg : Res.config) =
     Res.init_config tz_code init_strg_opt se_result init_state
  in
  let (init_res : Res.res) = Res.init_res cfg in
  let _ =
     (* cfg.cfg_m_view and res.r_cands debugging info *)
     let open Se in
     let module RMCIMap = Se.SSGraph.RMCIMap in
     RMCIMap.iteri cfg.cfg_m_view ~f:(fun ~key ~data:x ->
         Utils.Log.debug (fun m ->
             m
               "%s:\n\t> # of pred state: %d\n\t> # of succ state: %d\n\t> # of candidates: %d"
               (Tz.sexp_of_r_mich_cut_info key |> Core.Sexp.to_string)
               (SSet.length x.pred) (SSet.length x.succ)
               (Inv.find_cand_by_rmci init_res.r_cands key |> Map.length)
         )
     )
  in
  let (res : Res.res) = Refute.naive_run cfg init_res in
  (cfg, res)
(* function refuter_naive_run end *)

let prover_naive_run : string array option -> Res.config * Res.res =
  fun argv_opt ->
  let ( (_, _, (tz_code : Tz.mich_i Tz.cc)),
        (init_strg_opt : Tz.mich_v Tz.cc option),
        (se_result : Se.se_result),
        (init_state : Tz.sym_state)
      ) =
     upto_sym_exec argv_opt
  in
  let (cfg : Res.config) =
     Res.init_config tz_code init_strg_opt se_result init_state
  in
  let (init_res : Res.res) = Res.init_res cfg in
  let _ =
     (* cfg.cfg_m_view and res.r_cands debugging info *)
     let open Se in
     let module RMCIMap = Se.SSGraph.RMCIMap in
     RMCIMap.iteri cfg.cfg_m_view ~f:(fun ~key ~data:x ->
         Utils.Log.debug (fun m ->
             m
               "%s:\n\t> # of pred state: %d\n\t> # of succ state: %d\n\t> # of candidates: %d"
               (Tz.sexp_of_r_mich_cut_info key |> Core.Sexp.to_string)
               (SSet.length x.pred) (SSet.length x.succ)
               (Inv.find_cand_by_rmci init_res.r_cands key |> Map.length)
         )
     )
  in
  let (res : Res.res) = Prove.naive_run cfg init_res in
  (cfg, res)
(* function prover_naive_run end *)

let prover_refuter_toss : string array option -> Res.config * Res.res =
   let open Res in
   fun argv_opt ->
   let ( (_, _, (tz_code : Tz.mich_i Tz.cc)),
         (init_strg_opt : Tz.mich_v Tz.cc option),
         (se_result : Se.se_result),
         (init_state : Tz.sym_state)
       ) =
      upto_sym_exec argv_opt
   in
   let (cfg : Res.config) =
      Res.init_config tz_code init_strg_opt se_result init_state
   in
   let (init_res : Res.res) = Res.init_res cfg in
   let _ =
      (* cfg.cfg_m_view and res.r_cands debugging info *)
      let open Se in
      let module RMCIMap = Se.SSGraph.RMCIMap in
      RMCIMap.iteri cfg.cfg_m_view ~f:(fun ~key ~data:x ->
          Utils.Log.debug (fun m ->
              m
                "%s:\n\t> # of pred state: %d\n\t> # of succ state: %d\n\t> # of candidates: %d"
                (Tz.sexp_of_r_mich_cut_info key |> Core.Sexp.to_string)
                (SSet.length x.pred) (SSet.length x.succ)
                (Inv.find_cand_by_rmci init_res.r_cands key |> Map.length)
          )
      )
   in
   let (res : Res.res) = Manage.naive_run cfg init_res in
   (cfg, res)
(* function prover_refuter_toss end *)

let refuter_trxpath_run :
    score_f_gen:(Res.config -> Res.res -> Tz.qid -> MState.t -> float) ->
    string array option ->
    Res.config * Res.res =
   let open Res in
   fun ~score_f_gen argv_opt ->
   let ( (_, _, (tz_code : Tz.mich_i Tz.cc)),
         (init_strg_opt : Tz.mich_v Tz.cc option),
         (se_result : Se.se_result),
         (init_state : Tz.sym_state)
       ) =
      upto_sym_exec argv_opt
   in
   let _ =
      Utils.Log.debug (fun m ->
          m "Execflow : Execflow.refuter_trxpath_naive_run"
      )
   in
   let (cfg : Res.config) =
      Res.init_config tz_code init_strg_opt se_result init_state
   in
   let (init_res : Res.res) = Res.init_res cfg in
   let (init_res : Res.res) =
      let open Res.PPath in
      {
        init_res with
        r_qr_lst =
          List.map init_res.r_qr_lst ~f:(fun qres ->
              {
                qres with
                qr_exp_ppaths =
                  QIDMap.find_exn cfg.cfg_query_paths qres.qr_qid
                  |> List.map ~f:(fun ms ->
                         {
                           pp_mstate = ms;
                           pp_score = [];
                           pp_satisfiability = None;
                         }
                     )
                  |> Res.PPSet.of_list;
              }
          );
      }
   in
   let _ =
      (* cfg.cfg_m_view and res.r_cands debugging info *)
      let open Se in
      let module RMCIMap = Se.SSGraph.RMCIMap in
      RMCIMap.iteri cfg.cfg_m_view ~f:(fun ~key ~data:x ->
          Utils.Log.debug (fun m ->
              m
                "%s:\n\t> # of pred state: %d\n\t> # of succ state: %d\n\t> # of candidates: %d"
                (Tz.sexp_of_r_mich_cut_info key |> Core.Sexp.to_string)
                (SSet.length x.pred) (SSet.length x.succ)
                (Inv.find_cand_by_rmci init_res.r_cands key |> Map.length)
          )
      )
   in
   (* Initial Path Refuting *)
   let (init_res : Res.res) =
      {
        init_res with
        r_qr_lst =
          List.map init_res.r_qr_lst ~f:(fun qres ->
              let (total_ppaths, rft_ppath_opt) =
                 let f (tpl_acc, rftopt_acc) pp =
                    (* 4.f.1. Check escape condition
                            - Check if already refuted path found
                            - No timeout check
                    *)
                    if Option.is_some rftopt_acc
                    then (tpl_acc, rftopt_acc)
                    else (
                      (* 4.f.2. check refutability *)
                      match
                        Refute.refute cfg.cfg_smt_ctxt cfg.cfg_smt_slvr
                          cfg.cfg_istrg pp
                      with
                      | (Some tp, Some mdl) -> (tp :: tpl_acc, Some (pp, mdl))
                      | (Some tp, None)     -> (tp :: tpl_acc, None)
                      | (None, _)           ->
                        (tpl_acc, rftopt_acc)
                        (* (None, None) case exists only *)
                    )
                 in
                 Res.PPSet.fold qres.qr_exp_ppaths ~init:([], None) ~f
              in
              let qr_total_ppaths = total_ppaths @ qres.qr_total_ppaths
              and qr_last_picked_paths = qres.qr_exp_ppaths
              and qr_rft_ppath = rft_ppath_opt
              and qr_exp_cnt : int =
                 Res.PPSet.length qres.qr_exp_ppaths + qres.qr_exp_cnt
              and qr_rft_flag : Res.refuter_flag =
                 match rft_ppath_opt with
                 | None   -> RF_u
                 | Some _ -> RF_r
              in
              {
                qres with
                qr_rft_flag;
                qr_total_ppaths;
                qr_last_picked_paths;
                qr_rft_ppath;
                qr_exp_cnt;
              }
          );
      }
   in
   let (res : Res.res) =
      Refute.trxpath_guided_run cfg ~score_f:(score_f_gen cfg init_res) init_res
   in
   (cfg, res)
(* function refuter_trxpath_run end *)

let refuter_trxpath_naive_run : string array option -> Res.config * Res.res =
   refuter_trxpath_run ~score_f_gen:Refute.shortest_first_score_f_gen

let refuter_trxpath_featurediff_run :
    string array option -> Res.config * Res.res =
   refuter_trxpath_run ~score_f_gen:Refute.featurediff_first_score_f_gen

let prover_adv_refuter_toss : string array option -> Res.config * Res.res =
   let open Res in
   fun argv_opt ->
   let ( (_, _, (tz_code : Tz.mich_i Tz.cc)),
         (init_strg_opt : Tz.mich_v Tz.cc option),
         (se_result : Se.se_result),
         (init_state : Tz.sym_state)
       ) =
      upto_sym_exec argv_opt
   in
   let (cfg : Res.config) =
      Res.init_config tz_code init_strg_opt se_result init_state
   in
   let (init_res : Res.res) = Res.init_res cfg in
   let (init_res : Res.res) =
      let open Res.PPath in
      {
        init_res with
        r_qr_lst =
          List.map init_res.r_qr_lst ~f:(fun qres ->
              {
                qres with
                qr_exp_ppaths =
                  QIDMap.find_exn cfg.cfg_query_paths qres.qr_qid
                  |> List.map ~f:(fun ms ->
                         {
                           pp_mstate = ms;
                           pp_score = [];
                           pp_satisfiability = None;
                         }
                     )
                  |> Res.PPSet.of_list;
              }
          );
      }
   in
   let _ =
      (* cfg.cfg_m_view and res.r_cands debugging info *)
      let open Se in
      let module RMCIMap = Se.SSGraph.RMCIMap in
      RMCIMap.iteri cfg.cfg_m_view ~f:(fun ~key ~data:x ->
          Utils.Log.debug (fun m ->
              m
                "%s:\n\t> # of pred state: %d\n\t> # of succ state: %d\n\t> # of candidates: %d"
                (Tz.sexp_of_r_mich_cut_info key |> Core.Sexp.to_string)
                (SSet.length x.pred) (SSet.length x.succ)
                (Inv.find_cand_by_rmci init_res.r_cands key |> Map.length)
          )
      )
   in
   let score_f = Refute.featurediff_first_score_f_gen cfg init_res in
   let (res : Res.res) = Manage.adv_run cfg ~score_f init_res in
   (cfg, res)
(* function prover_adv_refuter_toss end *)

let prover_trxpath_naive_refuter_toss :
    string array option -> Res.config * Res.res =
   let open Res in
   fun argv_opt ->
   let ( (_, _, (tz_code : Tz.mich_i Tz.cc)),
         (init_strg_opt : Tz.mich_v Tz.cc option),
         (se_result : Se.se_result),
         (init_state : Tz.sym_state)
       ) =
      upto_sym_exec argv_opt
   in
   let (cfg : Res.config) =
      Res.init_config tz_code init_strg_opt se_result init_state
   in
   let (init_res : Res.res) = Res.init_res cfg in
   let (init_res : Res.res) =
      let open Res.PPath in
      {
        init_res with
        r_qr_lst =
          List.map init_res.r_qr_lst ~f:(fun qres ->
              {
                qres with
                qr_exp_ppaths =
                  QIDMap.find_exn cfg.cfg_query_paths qres.qr_qid
                  |> List.map ~f:(fun ms ->
                         {
                           pp_mstate = ms;
                           pp_score = [];
                           pp_satisfiability = None;
                         }
                     )
                  |> Res.PPSet.of_list;
              }
          );
      }
   in
   let _ =
      (* cfg.cfg_m_view and res.r_cands debugging info *)
      let open Se in
      let module RMCIMap = Se.SSGraph.RMCIMap in
      RMCIMap.iteri cfg.cfg_m_view ~f:(fun ~key ~data:x ->
          Utils.Log.debug (fun m ->
              m
                "%s:\n\t> # of pred state: %d\n\t> # of succ state: %d\n\t> # of candidates: %d"
                (Tz.sexp_of_r_mich_cut_info key |> Core.Sexp.to_string)
                (SSet.length x.pred) (SSet.length x.succ)
                (Inv.find_cand_by_rmci init_res.r_cands key |> Map.length)
          )
      )
   in
   let score_f = Refute.shortest_first_score_f_gen cfg init_res in
   let (res : Res.res) = Manage.adv_run cfg ~score_f init_res in
   (cfg, res)
(* function prover_trxpath_naive_refuter_toss end *)
