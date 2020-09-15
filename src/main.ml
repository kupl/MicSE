let main : unit -> unit
=fun () -> begin
  let cfg, init_stg_opt = Pre.pre_process (!Utils.Options.input_file) in
  let _ = Prover.prove cfg init_stg_opt in
  ()
end

let _ = begin
  let _ = Utils.Options.create_options () in
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