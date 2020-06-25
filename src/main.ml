let input_file = ref ""

let options =
  [
    ("-input", (Arg.String (fun s -> input_file := s)), "File path for input michelson program.");
  ]

let main : unit -> unit
=fun () -> begin
  let filepath = !input_file in
  let ast = Tezla.Parsing_utils.parse_with_error filepath in
  let cfg = Tezla_cfg.Flow_graph.Cfg.generate_from_program ast in
  let cfg = Prover.Translator.of_tezlaCfg cfg in
  let (bps, _) = Prover.Extractor.extract cfg in
  let _ = Core.List.iteri bps ~f:(fun idx bp -> (
    let str = "(" ^ (string_of_int idx) ^ ")\n\n" in
    let str = str ^ ProverLib.Bp.to_string bp in
    let _ = print_endline (str ^ "\n") in
    ()
  )) in
  ()
end

and activate_detector : string -> unit
=fun s -> begin
  match s with
  | _ -> invalid_arg "invalid option"
end

let _ = begin
  let usageMsg = "micse-main -input filename" in
  let _ = Arg.parse options activate_detector usageMsg in
  let _ = Printexc.record_backtrace true in
  try
    if !input_file <> ""
    then
      let _ = main () in
      ()
    else
      raise (Failure "No_Input")
  with
  | exc -> prerr_endline (Printexc.to_string exc); prerr_endline (Printexc.get_backtrace())
end