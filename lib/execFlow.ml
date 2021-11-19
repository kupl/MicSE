(* ExecFlow provides main execution flow functions for testing. *)

open! Core

(******************************************************************************)
(******************************************************************************)
(* Execution Flow Components                                                  *)
(******************************************************************************)
(******************************************************************************)

let initial_system_setting : string array option -> unit =
  fun argv_opt ->
  let _ = Utils.Argument.create argv_opt in
  let _ = Utils.Log.create () in
  let _ = Printexc.record_backtrace true in
  let _ =
     if !Utils.Argument.set_random_seed
     then Random.self_init ()
     else Random.init 0
  in
  ()
(* function initial_system_setting end *)

let parsing : unit -> Mich.program * Mich.data Mich.t option =
  fun () ->
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
  (pgm, strg_opt)
(* function parsing end *)

let tz_rep :
    Mich.program * Mich.data Mich.t option ->
    (Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc)
    * Tz.mich_v Tz.cc option =
  fun (mich_pgm, mich_init_strg_opt) ->
  let (tz_pgm : Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc) =
     TzUtil.M2T.cv_program mich_pgm
  in
  let (tz_init_strg_opt : Tz.mich_v Tz.cc option) =
     Option.map mich_init_strg_opt ~f:(fun d ->
         TzUtil.M2T.cv_datat mich_pgm.storage d
     )
  in
  (tz_pgm, tz_init_strg_opt)
(* function tz_rep end *)

let sym_exec :
    Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc ->
    Se.se_result * Tz.sym_state =
  (fun tz_pgm -> Se.run_inst_entry tz_pgm)
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
