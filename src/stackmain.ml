(* Main Entrypoint for "stacked" branch *)

let f_generate_sset : unit -> unit
= fun () -> begin
  let pgmfilecontent : Stacked.Mich.program = !Utils.Options.input_file |> Stacked.Parse.parse in
  let strgfilecontentopt : (Stacked.Mich.data Stacked.Mich.t) option =
    if !Utils.Options.initial_storage_file = "" then None else
    Some (Stacked.Parse.parse_data !Utils.Options.initial_storage_file)
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
  let pgmfilecontent : Stacked.Mich.program = 
    !Utils.Options.input_file 
    |> Stacked.Parse.parse 
    |> Stacked.Mich.subst_standard_macro_all_pgm 
    |> Stacked.Mich.optm_all_pgm 
    |> Stacked.Mich.fill_position_all_pgm ~update_loc:false in
  let strgfilecontentopt : (Stacked.Mich.data Stacked.Mich.t) option =
    if !Utils.Options.initial_storage_file = "" then None else
    Some (Stacked.Parse.parse_data !Utils.Options.initial_storage_file)
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
    | Stacked.TzCvt.T2S.Not_Implemented_f f as e -> Utils.Log.err (fun m -> m "TzCvt : T2S : Not Implemented Smt Encoding for Formula\n\tFormula: %s\n\tBack Traces:\n%s" (f |> Stacked.TzCvt.T2Jnocc.cv_mf |> Yojson.Safe.to_string) (Printexc.get_backtrace ())); Stdlib.raise e
    | Stacked.TzCvt.T2S.Not_Implemented_e v as e -> Utils.Log.err (fun m -> m "TzCvt : T2S : Not Implemented Smt Encoding for Value\n\tFormula: %s\n\tBack Traces:\n%s" (v |> Stacked.TzCvt.T2Jnocc.cv_mvcc |> Yojson.Safe.to_string) (Printexc.get_backtrace ())); Stdlib.raise e
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
  | exc -> Utils.Log.err (fun m -> m "%s\n%s" (exc |> Printexc.to_string) (Printexc.get_backtrace ()))
end