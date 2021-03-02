let main : unit -> unit
= fun () -> begin
  (* 1. Cfg Construction *)
  let pre_ret : Pre.pre_ret = Pre.pre_process (!Utils.Options.input_file) in
  (* 2. Run Prover *)
  (* The proved query-ids are stored as the side-effect of the values in Prover.Results. *)
  (* Standard-output prints specific prover results *)
  let _ = Prover.main pre_ret.cfg pre_ret.init_stg_opt in
  (* 3. Run Refuter *)
  (* The refuted query-ids are stored as the side-effect of the value "Refuter.refuted_queries" *)
  (* Standard-output prints specific refuter results *)
  let _ = Refuter.run_multiple (pre_ret.cfg, pre_ret.cfgcon_counter) pre_ret.init_stg_opt in
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
  | exc -> prerr_endline (Printexc.to_string exc);
end