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

let get_config_base :
    (Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc)
    * Tz.mich_v Tz.cc option
    * Se.se_result
    * Tz.sym_state ->
    Res.config =
  fun (_, tz_init_strg_opt, se_result, se_init_state) ->
  let tz_init_strg =
     match tz_init_strg_opt with
     | Some v -> v
     | None   -> failwith "ExecFlow : config_base : cfg_istrg = None"
  in
  let smt_ctxt = Vc.gen_ctx () in
  {
    cfg_timer = Utils.Time.create ~budget:!Utils.Argument.total_timeout ();
    cfg_memory = Utils.Memory.create ~budget:!Utils.Argument.memory_bound ();
    cfg_istate = se_init_state;
    cfg_istrg = tz_init_strg;
    cfg_se_res = se_result;
    cfg_m_view =
      Se.SSGraph.construct_mci_view ~basic_blocks:se_result.sr_blocked;
    cfg_imap =
      Igdt.get_igdts_map se_result.sr_blocked tz_init_strg Igdt.MVSet.empty;
    cfg_smt_ctxt = smt_ctxt;
    cfg_smt_slvr = Vc.gen_solver smt_ctxt;
  }

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
  let (_, _, se_result, _) = sym_exec_res in
  let cfg = get_config_base sym_exec_res in
  let init_res = Refute.naive_run_init_res se_result in
  let res = Refute.naive_run cfg init_res in
  (cfg, res)
