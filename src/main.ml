let main : unit -> unit
=fun () -> begin
  let filepath = !Lib.Options.input_file in
  (* Parse Michelson File *)
  let adt = Pre.Lib.Adt.parse filepath in
  (* FLAGS - Parsed Michelson File *)
  let _ : unit = (if (!Lib.Options.flag_adt_print) then (Pre.Lib.Adt.pp Format.std_formatter adt) else ()) in

  (* Construct control flow graph *)
  let cfg_first = Pre.Translator.adt_to_cfg adt in
  let cfg_rsv_optimized = (if (!Lib.Options.flag_cfgopt_rsv) then (Pre.Lib.Cfg.remove_meaningless_skip_vertices_fixpoint cfg_first) else (cfg_first)) in
  let cfg_rfv_optimized = (if (!Lib.Options.flag_cfgopt_rfv) then (Pre.Lib.Cfg.remove_meaningless_fail_vertices_fixpoint cfg_rsv_optimized) else (cfg_rsv_optimized)) in
  let cfg_optimized = cfg_rfv_optimized in

  let cfg = cfg_optimized in
  let _ : unit = (if (!Lib.Options.flag_cfg_print_dot) then print_endline (Pre.Lib.Cfg.cfg_to_dotformat cfg) else ()) in

  (* Construct basic path *)
  let raw_bp_list = Prover.Extractor.extract cfg in
  let bp_list = Prover.Generator.generate raw_bp_list cfg in

  (* Verify each basic path *)
  let _ = Core.List.iter bp_list ~f:(fun bp -> (
    let vlang_vc = Prover.Converter.convert bp cfg in
    let verify_result = Prover.Verifier.verify vlang_vc cfg in
    print_endline (if verify_result then "valid" else "invalid")
  )) in

  (* Get *)
  ()
end

let _ = begin
  let open Lib.Options in
  let usageMsg = "micse -input filename" in
  let _ = Arg.parse options activate_detector usageMsg in
  let _ = Printexc.record_backtrace true in

  (* Set custom options *)
  let _ = set_all_cfg_opt () in

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