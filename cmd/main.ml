exception Error of string

let main : unit -> unit =
  fun () ->
  let (mich_pgm, mich_init_strg_opt) = Lib.ExecFlow.parsing () in
  let (tz_pgm, _) = Lib.ExecFlow.tz_rep (mich_pgm, mich_init_strg_opt) in
  let _ = Lib.ExecFlow.sym_exec tz_pgm in
  ()

let _ =
   (* 1. Initialize Input Arguments *)
   Utils.Argument.create None;
   (* 2. Initialize Logger *)
   Utils.Log.create ();
   (* 3. Enable Error Backtrace *)
   Printexc.record_backtrace true;
   try
     (* 4. Check Input File Path Nullity *)
     if !Utils.Argument.input_file <> ""
     then main ()
     else Stdlib.raise (Error "No input.")
   with
   | exc ->
     Utils.Log.err (fun m ->
         m "%s\n%s" (exc |> Printexc.to_string) (Printexc.get_backtrace ())
     )
