
let main : unit -> unit
=fun () -> begin
  let _ = Format.print_string "\n> Parse" in
  let filepath = !Utils.Options.input_file in
  let ast = Utils.Tezlalib.parse filepath in
  let _ = Utils.Tezlalib.print_original Format.std_formatter ast in
  let _ = Format.print_string "\n> Convert" in
  let conv = Utils.Tezlalib.convert ast in
  let _ = Utils.Tezlalib.print_convertd Format.std_formatter conv in
  let cfg = Utils.Tezlalib.cfg ast in
  let _ = Utils.Tezlalib.writefile_cfg cfg "toymain_output_cfg.dot" in
  let _ = Utils.Tezlalib.display_cfg cfg in
  ()
end

let _ = begin
  let usageMsg = "micse-main -input filename" in
  let _ = Arg.parse Utils.Options.options Utils.Options.activate_detector usageMsg in
  let _ = Printexc.record_backtrace true in
  try
    if !Utils.Options.input_file <> ""
    then
      let _ = main () in
      ()
    else
      raise (Failure "No_Input")
  with
  | exc -> prerr_endline (Printexc.to_string exc); prerr_endline (Printexc.get_backtrace())
end