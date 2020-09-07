(* Data parsing test.
    It can take same options as MicSE, but the options unrelated with Preprocessing will be ignored.
    Process returns 0 if normally executed.
*)

(* Example Usages
  * Simplest form
      dune exec -- micse.utils.test_data_parse -initial_storage [filepath]
*)

let _ =
  let _ = Utils.Options.create_options () in
  let _ = Printexc.record_backtrace false in
  let dt = PreLib.Adt.parse_data !Utils.Options.initial_storage_file in
  let _ = Stdlib.print_endline (PreLib.Mich.string_of_datat_ol dt) in
  Stdlib.exit 0
