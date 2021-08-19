(*****************************************************************************)
(*****************************************************************************)
(* Run Available Variable Analyzer                                           *)
(*****************************************************************************)
(*****************************************************************************)

(* It preprocesses cfg & run available variable analyzer & show results.
    It can take same options as MicSE, but the options unrelated with Preprocessing will be ignored.
    Process returns 0 if normally executed.
*)

(* Example Usages
  * run available variable analyzer & print cfg (Run on optimized cfg)
      dune exec -- micse.utils.avar_analysis -input [PROJECT-ROOT]/benchmarks/toy/add1.tz -cfgopt -cfg_print_dot
*)

(*  type abs_set = Top | S of string CPSet.t  (* variable set - abstract domain *) *)
let string_of_absset : Pre.Analyzer.AvailVar.abs_set -> string
= let open Pre.Analyzer.AvailVar in
  function
  | Top -> "AbsSet-Top"
  | S sset -> CPSet.fold sset ~init:"{" ~f:(fun acc x -> acc ^ ", " ^ x) |> (fun s -> s ^ "}")

let string_of_analyzer_result : (int, Pre.Analyzer.AvailVar.abs_set * Pre.Analyzer.AvailVar.abs_set) Pre.Analyzer.AvailVar.CPMap.t -> string
= let open Pre.Analyzer.AvailVar in
  fun r -> CPMap.fold r ~init:"" ~f:(fun ~key ~data acc -> acc ^ "\n" ^ (Stdlib.string_of_int key) ^ "\n  " ^  (string_of_absset (fst data)) ^ "\n  " ^ (string_of_absset (snd data)))

let _ =
  let _ = Utils.Options.create_options () in
  let _ = Printexc.record_backtrace false in
  let pre_ret = Pre.pre_process (!Utils.Options.input_file) in
  let retv = Pre.Analyzer.AvailVar.run pre_ret.cfg in
  Stdlib.print_endline (string_of_analyzer_result retv);
  Stdlib.exit 0

