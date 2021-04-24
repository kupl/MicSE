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
  (* Stacked.Prove.f_print_blocked_paths_pretty sset; *)
  Stacked.Prove.f_print_queries_pretty sset;
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
      | Stacked.Se.DebugTT (t1, t2) -> 
                          Stacked.TzCvt.T2Jnocc.cv_mt t1 |> Yojson.Safe.pretty_to_string |> print_endline
                          ; Stacked.TzCvt.T2Jnocc.cv_mt t2 |> Yojson.Safe.pretty_to_string |> print_endline
                          ; Stdlib.failwith "Stacked.Se.DebugTT"
    )

  in
  let _ = ignore (tz_init_stg_opt, init_ss, cache) in
  Stacked.Prove.f_count_sset sset;
  print_newline ();
  (
    try
      Stacked.Prove.main
        (tz_init_stg_opt, init_ss)
        sset
      |> Stacked.Prove.f_print_query_solved_result_simple_pretty
      (* Stacked.Prove.solve_queries 
        (Utils.Timer.create ~budget:!(Utils.Options.prover_time_budget))
        sset.queries
        (Stacked.Se.true_invmap_of_blocked_sset sset.Stacked.Se.blocked)
      |> Stacked.Prove.f_print_query_solved_result_simple_pretty *)
    with 
    | Stacked.TzCvt.T2S.SMT_Encode_Error_f (vf, _, line) ->
      Stacked.TzCvt.T2Jnocc.cv_mf vf
      |> Yojson.Safe.pretty_to_string |> print_endline
      ; Stdlib.failwith ("Stacked.TzCvt.T2S.SMT_Encode_Error_f : " ^ (Stdlib.string_of_int line))
    | Stacked.TzCvt.T2S.SMT_Encode_Error_e (vv, _, line) ->
      Stacked.TzCvt.T2Jnocc.cv_mvcc vv
      |> Yojson.Safe.pretty_to_string |> print_endline
      ; Stdlib.failwith ("Stacked.TzCvt.T2S.SMT_Encode_Error_e : " ^ (Stdlib.string_of_int line))
  )
end (* function f_show_trueinv_solving *)

let main : unit -> unit
= fun () -> begin
  f_generate_sset ()
end

let _ = begin
  (* 1. Initialize Input Arguments *)
  Utils.Options.create_options ();
  (* 2. Initialize Logger *)
  Utils.Log.create ();
  (* 3. Enable Error Backtrace *)
  Printexc.record_backtrace true;
  try
    (* 4. Check Input File Path Nullity *)
    if !Utils.Options.input_file <> ""
    then main ()
    else Failure "no input." |> Stdlib.raise
  with
  | exc -> Utils.Log.err (fun m -> m "%s" (exc |> Printexc.to_string))
end