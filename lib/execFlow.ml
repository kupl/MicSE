(* ExecFlow provides main execution flow functions for testing. *)

(*****************************************************************************)
(*****************************************************************************)
(* Execution Flow Components                                                 *)
(*****************************************************************************)
(*****************************************************************************)

let initial_system_setting : unit -> unit =
  fun () ->
  Utils.Argument.create ();
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
    Tz.program * Tz.mich_v Tz.cc option =
  fun (mich_pgm, mich_init_strg_opt) ->
  let tz_pgm = Tz.M2T.cv_program mich_pgm in
  let tz_init_strg_opt =
     Option.map (fun d -> Tz.M2T.cv_datat mich_pgm.storage d) mich_init_strg_opt
  in
  (tz_pgm, tz_init_strg_opt)

(*****************************************************************************)
(*****************************************************************************)
(* Execution Flow                                                            *)
(*****************************************************************************)
(*****************************************************************************)

let upto_initial_system_setting : unit -> unit = initial_system_setting

let upto_parsing : unit -> Mich.program * Mich.data Mich.t option =
  fun () ->
  initial_system_setting ();
  let (mich_pgm, mich_init_strg_opt) = parsing () in
  (mich_pgm, mich_init_strg_opt)

let upto_tz_rep : unit -> Tz.program * Tz.mich_v Tz.cc option =
  fun () ->
  initial_system_setting ();
  let (mich_pgm, mich_init_strg_opt) = parsing () in
  let (tz_pgm, tz_init_strg_opt) = tz_rep (mich_pgm, mich_init_strg_opt) in
  (tz_pgm, tz_init_strg_opt)
