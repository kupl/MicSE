(* Logger *)

(******************************************************************************)
(******************************************************************************)
(* Setting                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Setting = struct
  let log_counter : Mtime_clock.counter
  = Mtime_clock.counter ()
  
  let reporter : unit -> Logs.reporter
  = fun () -> begin
    let report = fun _ level ~over k msgf -> begin
      let k' = fun _ -> begin
        over (); k ()
      end in (* k' function end *)
      let dt = log_counter |> Mtime_clock.count |> Mtime.Span.to_ms in
      msgf @@ (fun ?header ?tags fmt -> (
        ignore (tags);
        match level with
        | Logs.App  -> Format.kfprintf k' Format.std_formatter ("%a@[" ^^ fmt ^^ "@]@.") Logs.pp_header (level, header)
        | _         -> Format.kfprintf k' Format.err_formatter ("%a[%0+7.0fms] @[" ^^ fmt ^^ "@]@.") Logs.pp_header (level, header) dt))
    end in (* report function end *)
    { Logs.report = report }
  end (* reporter function end *)

  let msg : Logs.level -> 'a Logs.log
  = fun level log -> begin
    Logs.msg level log
  end (* msg function end *)
end


(******************************************************************************)
(******************************************************************************)
(* Functions                                                                  *)
(******************************************************************************)
(******************************************************************************)

let app : ('a, unit) Logs.msgf -> unit
= fun log -> begin
  Setting.msg Logs.App log;
  Stdlib.flush Stdlib.stdout
end (* function app end *)

let debug : ('a, unit) Logs.msgf -> unit
= fun log -> begin
  Setting.msg Logs.Debug log;
  Stdlib.flush Stdlib.stderr
end (* function info end *)


let info : ('a, unit) Logs.msgf -> unit
= fun log -> begin
  Setting.msg Logs.Info log;
  Stdlib.flush Stdlib.stderr
end (* function info end *)

let warn : ('a, unit) Logs.msgf -> unit
= fun log -> begin
  Setting.msg Logs.Warning log;
  Stdlib.flush Stdlib.stderr
end (* function warn end *)

let err : ('a, unit) Logs.msgf -> unit
= fun log -> begin
  Setting.msg Logs.Error log;
  Stdlib.flush Stdlib.stderr
end (* function err end *)

let create : unit -> unit
= fun () -> begin
  Logs.set_reporter (Setting.reporter ());
  if !Options.flag_debug then (Logs.set_level (Some Logs.Debug))
  else if !Options.flag_verbose then (Logs.set_level (Some Logs.Info))
  else (Logs.set_level (Some Logs.Warning));
  info (fun m -> m "Logger Start.");
  ()
end (* function create end *)