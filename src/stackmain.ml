(* Main Entrypoint for "stacked" branch *)

let f_generate_sset : unit -> unit
= fun () -> begin
  let pgmfilecontent : PreLib.Adt.t = !Utils.Options.input_file |> PreLib.Adt.parse in
  let strgfilecontentopt : PreLib.Adt.data option =
    if !Utils.Options.initial_storage_file = "" then None else
    Some (PreLib.Adt.parse_data !Utils.Options.initial_storage_file)
  in
  let (tz_init_stg_opt, init_ss, cache, sset) =
    Stacked.Prove.gen_sset pgmfilecontent strgfilecontentopt
  in
  let _ = ignore (tz_init_stg_opt, init_ss, cache) in
  Stacked.Prove.f_count_sset sset;
  Stacked.Prove.f_print_blocked_paths_pretty sset
end (* function f_generate_sset end *)

let f_show_trueinv_solving : unit -> unit
= fun () -> begin
  let pgmfilecontent : PreLib.Adt.t = 
    !Utils.Options.input_file 
    |> PreLib.Adt.parse 
    |> PreLib.Mich.subst_standard_macro_all_pgm 
    |> PreLib.Mich.optm_all_pgm 
    |> PreLib.Mich.fill_position_all_pgm ~update_loc:false in
  let strgfilecontentopt : PreLib.Adt.data option =
    if !Utils.Options.initial_storage_file = "" then None else
    Some (PreLib.Adt.parse_data !Utils.Options.initial_storage_file)
  in
  let (tz_init_stg_opt, init_ss, cache, sset) =
    ( try
        Stacked.Prove.gen_sset pgmfilecontent strgfilecontentopt
      with
      | Stacked.Se.DebugInstSS (i,ss) -> Stacked.TzCvt.T2J.cv_micc i |> Yojson.Safe.pretty_to_string |> print_endline
                          ; `List (List.map Stacked.TzCvt.T2Jnocc.cv_mvcc ss.ss_symstack) |> Yojson.Safe.pretty_to_string |> print_endline
                          ; Stdlib.failwith "Stacked.Se.DebugInstSS"
    )

  in
  let _ = ignore (tz_init_stg_opt, init_ss, cache) in
  Stacked.Prove.f_count_sset sset;
  print_newline ();
  (
    try
      Stacked.Prove.solve_queries 
        (Utils.Timer.create ~budget:!(Utils.Options.prover_time_budget))
        sset.queries
        (Stacked.Se.true_invmap_of_blocked_sset sset.Stacked.Se.blocked)
      |> Stacked.Prove.f_print_query_solved_result_simple_pretty
    with 
    | Stacked.TzCvt.T2S.SMT_Encode_Error_f (vf, _) ->
      Stacked.TzCvt.T2Jnocc.cv_mf vf
      |> Yojson.Safe.pretty_to_string |> print_endline
      ; Stdlib.failwith "Stacked.TzCvt.T2S.SMT_Encode_Error_f"
  )
end (* function f_show_trueinv_solving *)

let main : unit -> unit
= fun () -> begin
  f_show_trueinv_solving ()
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