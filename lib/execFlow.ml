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

let upto_sym_exec : string array option -> Se.se_result * Tz.sym_state =
  fun argv_opt ->
  let (tz_pgm, _) = upto_tz_rep argv_opt in
  sym_exec tz_pgm

let refuter_naive_run :
    string array option -> (Smt.Model.t * MState.t) option * Refute.MSSet.t =
  fun argv_opt ->
  let (tz_pgm, tz_init_strg_opt) = upto_tz_rep argv_opt in
  let (se_result, _) = sym_exec tz_pgm in
  let init_msset = se_result.sr_queries |> Refute.MSSet.map ~f:MState.init in
  let timer = Utils.Time.create ~budget:!Utils.Argument.total_timeout ()
  and init_strg =
     tz_init_strg_opt
     |> function
     | Some s -> s
     | None   -> failwith "ExecFlow.refuter_naive_run : init_strg : unexpected"
  and smt_ctxt = Vc.gen_ctx () in
  let smt_slvr = Vc.gen_solver smt_ctxt
  and invmap = Inv.gen_true_inv_map se_result
  and m_view =
     Se.SSGraph.construct_mci_view ~basic_blocks:se_result.sr_blocked
  in
  Refute.naive_run ~timer ~init_strg ~smt_ctxt ~smt_slvr ~invmap ~m_view
    init_msset
