(* It only preprocesses the given Michelson code.
    It can take same options as MicSE, but the options unrelated with Preprocessing will be ignored.
    Process returns 0 if normally executed.
*)

(* Example Usages
  * If you want to make sure it's working (WARNING: process will return 1 for any OCaml error, with no error message printed.)
      dune exec -- micse.utils.construct_cfg -input [PROJECT-ROOT]/benchmarks/toy/add1.tz
  * print cfg
      dune exec -- micse.utils.construct_cfg -input [PROJECT-ROOT]/benchmarks/toy/add1.tz -cfg_print_dot
  * get optimized-cfg (RECOMMENDED, remove some redundant vertices)
      dune exec -- micse.utils.construct_cfg -input [PROJECT-ROOT]/benchmarks/toy/add1.tz -cfgopt -cfg_print_dot
*)

let _ =
  let _ = Utils.Options.create_options () in
  let _ = Printexc.record_backtrace false in
  let _ = Pre.pre_process (!Utils.Options.input_file) in
  Stdlib.exit 0
