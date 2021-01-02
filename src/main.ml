let main : unit -> unit
=fun () -> begin
  (* let cfg, init_stg_opt = Pre.pre_process (!Utils.Options.input_file) in *)
  let cfg, _ = Pre.pre_process (!Utils.Options.input_file) in
  let basic_vtxset = Prover.BpGen.collect_bp_vtx cfg cfg.main_entry in
  let _ = Core.Set.Poly.iter
            basic_vtxset
            ~f:(fun vl -> List.iter (fun x -> Stdlib.print_int x; Stdlib.print_string " ") vl)
  in
  let _ = print_newline () in
  let prv_glenv_ref = ref (ProverLib.GlVar.Env.gen 0) in
  let bpgen = Prover.BpGen.bp_of_vtxlst prv_glenv_ref cfg in
  let bps = Core.Set.Poly.map basic_vtxset ~f:bpgen in
  let _ = Core.Set.Poly.iter 
            bps 
            ~f:(fun bp -> 
                ProverLib.Bp.JsonRep.of_t bp 
                |> Yojson.Basic.pretty_to_string 
                |> Stdlib.print_endline
            )
  in
  (* let _ = Prover.prove cfg init_stg_opt in *)
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