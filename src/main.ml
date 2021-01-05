let main : unit -> unit
= let open PreLib in
  fun () -> begin
  (* 1. Cfg Construction *)
  let (cfg, init_stg_opt) : Cfg.t * (Adt.data option) = Pre.pre_process (!Utils.Options.input_file) in
  (* 2. Run Prover *)
  let _ = Prover.main cfg init_stg_opt in
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