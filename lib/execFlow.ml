(* ExecFlow provides main execution flow functions for testing. *)

(******************************************************************************)
(******************************************************************************)
(* Execution Flow Components                                                  *)
(******************************************************************************)
(******************************************************************************)

let initial_system_setting : string array option -> unit =
  fun argv_opt ->
  Utils.Argument.create argv_opt;
  Utils.Log.create ();
  Printexc.record_backtrace true;
  ()

let parsing : unit -> Mich.program * Mich.data Mich.t option =
  fun () ->
  let pgm =
     MichParse.parse ~filename:!Utils.Argument.input_file
     |> Mich.subst_standard_macro_all_pgm
     |> Mich.optm_all_pgm
     |> Mich.fill_position_all_pgm ~update_loc:false
  and strg_opt =
     if !Utils.Argument.input_storage_file = ""
     then None
     else
       Some (MichParse.parse_data ~filename:!Utils.Argument.input_storage_file)
  in
  (pgm, strg_opt)

let tz_rep :
    Mich.program * Mich.data Mich.t option ->
    (Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc)
    * Tz.mich_v Tz.cc option =
  fun (mich_pgm, mich_init_strg_opt) ->
  let tz_pgm = TzUtil.M2T.cv_program mich_pgm in
  let tz_init_strg_opt =
     Option.map
       (fun d -> TzUtil.M2T.cv_datat mich_pgm.storage d)
       mich_init_strg_opt
  in
  (tz_pgm, tz_init_strg_opt)

let sym_exec :
    Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc ->
    Se.se_result * Tz.sym_state =
   Se.run_inst_entry

(******************************************************************************)
(******************************************************************************)
(* Execution Flow                                                             *)
(******************************************************************************)
(******************************************************************************)

let upto_initial_system_setting : string array option -> unit =
   initial_system_setting

let upto_parsing : string array option -> Mich.program * Mich.data Mich.t option
    =
  fun argv_opt ->
  upto_initial_system_setting argv_opt;
  let (mich_pgm, mich_init_strg_opt) = parsing () in
  (mich_pgm, mich_init_strg_opt)

let upto_tz_rep :
    string array option ->
    (Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc)
    * Tz.mich_v Tz.cc option =
  fun argv_opt ->
  let (mich_pgm, mich_init_strg_opt) = upto_parsing argv_opt in
  let (tz_pgm, tz_init_strg_opt) = tz_rep (mich_pgm, mich_init_strg_opt) in
  (tz_pgm, tz_init_strg_opt)

let upto_sym_exec :
    string array option ->
    (Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc)
    * Tz.mich_v Tz.cc option
    * Se.se_result
    * Tz.sym_state =
  fun argv_opt ->
  let (tz_pgm, tz_init_strg_opt) = upto_tz_rep argv_opt in
  let (se_result, se_init_state) = sym_exec tz_pgm in
  (tz_pgm, tz_init_strg_opt, se_result, se_init_state)

let refuter_naive_run : string array option -> Res.config * Res.res =
  fun argv_opt ->
  let sym_exec_res = upto_sym_exec argv_opt in
  let (_, init_strg_opt, se_result, init_state) = sym_exec_res in
  (* let _ =
        (* se_result debugging *)
        Se.SSet.iter se_result.sr_blocked ~f:(fun ss ->
            Utils.Log.debug (fun m ->
                m "%s  //  %s"
                  (Tz.sexp_of_mich_cut_info ss.ss_start_mci |> Core.Sexp.to_string)
                  (Tz.sexp_of_mich_cut_info ss.ss_block_mci |> Core.Sexp.to_string)
            )
        )
     in *)
  let cfg = Res.init_config init_strg_opt se_result init_state in
  let _ =
     (* cfg.cfg_m_view debugging info *)
     let mv = cfg.cfg_m_view in
     let open Se in
     let module RMCIMap = Se.SSGraph.RMCIMap in
     let _ =
        RMCIMap.iteri mv ~f:(fun ~key ~data:x ->
            let l = SSet.length in
            let (p, s) = (x.pred, x.succ) in
            Utils.Log.debug (fun m ->
                m "%s : %d %d"
                  (Tz.sexp_of_r_mich_cut_info key |> Core.Sexp.to_string)
                  (l p) (l s)
            )
        )
     in
     ()
  in
  let init_res = Res.init_res cfg in
  let res = Refute.naive_run cfg init_res in
  (cfg, res)
