let input_file = ref ""

(* FLAGS - Parsed Michelson File *)
let flag_adt_print = ref false

(* FLAGS - Control Flow Graph *)
let flag_cfgopt_rsv = ref false (* remove-skip-vertices *)
let flag_cfg_print_dot = ref false

let options =
  [
    ("-input", (Arg.String (fun s -> input_file := s)), "File path for input michelson program.");
    ("-adt_print", (Arg.Set flag_adt_print), "Print parsed Michelson file.");
    ("-cfgopt_rsv", (Arg.Set flag_cfgopt_rsv), "Remove all trivial skip vertices in control flow graph. WARNING: It does not remove any vertex-information in Cfg");
    ("-cfg_print_dot", (Arg.Set flag_cfg_print_dot), "Print control flow graph in 'dot' format.");
  ]

let main : unit -> unit
=fun () -> begin
  let filepath = !input_file in
  (* Parse Michelson File *)
  let adt = ProverLib.Adt.parse filepath in
  (* FLAGS - Parsed Michelson File *)
  let _ : unit = (if (!flag_adt_print) then (ProverLib.Adt.pp Format.std_formatter adt) else ()) in

  (* Construct control flow graph *)
  let cfg_first = Prover.Translator.adt_to_cfg adt in
  let cfg_optimized = (if (!flag_cfgopt_rsv) then (ProverLib.Cfg.remove_meaningless_skip_vertices_fixpoint cfg_first) else (cfg_first)) in

  let cfg = cfg_optimized in
  let _ : unit = (if (!flag_cfg_print_dot) then print_endline (ProverLib.Cfg.cfg_to_dotformat cfg) else ()) in

  (* Construct basic path *)
  let raw_bp_list = Prover.Extractor.extract cfg in
  let bp_list = Prover.Generator.generate raw_bp_list cfg in

  (* Verify each basic path *)
  let _ = Core.List.iter bp_list ~f:(fun bp -> (
    let vlang_vc = Prover.Converter.convert bp cfg in
    let verify_result = Prover.Verifier.verify vlang_vc in
    print_endline (string_of_bool verify_result)
  )) in

  (* Get *)
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