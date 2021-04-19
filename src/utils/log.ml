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
    (* reporter function start *)
    let report = fun _ level ~over k msgf -> begin
      (* report function start *)
      let k' = fun _ -> begin
        (* k' function start *)
        over (); k ()
        (* k' function end *)
      end in
      let dt = log_counter |> Mtime_clock.count |> Mtime.Span.to_ms in
      msgf @@ (fun ?header ?tags fmt -> (
        ignore (tags);
        match level with
        | Logs.App  -> Format.kfprintf k' Format.std_formatter ("%a@[" ^^ fmt ^^ "@]@.") Logs.pp_header (level, header)
        | _         -> Format.kfprintf k' Format.err_formatter ("%a[%0+7.0fms] @[" ^^ fmt ^^ "@]@.") Logs.pp_header (level, header) dt))
      (* report function end *)
    end in
    { Logs.report = report }
    (* reporter function end *)
  end

  let msg : Logs.level -> 'a Logs.log
  = fun level log -> begin
    (* msg function start *)
    Logs.msg level log
    (* msg function end *)
  end
end


(******************************************************************************)
(******************************************************************************)
(* Functions                                                                  *)
(******************************************************************************)
(******************************************************************************)

let app : ('a, unit) Logs.msgf -> unit
= fun log -> begin
  (* app function start *)
  Setting.msg Logs.App log
  (* app function end *)
end

let debug : ('a, unit) Logs.msgf -> unit
= fun log -> begin
  (* info function start *)
  Setting.msg Logs.Debug log
  (* info function end *)
end


let info : ('a, unit) Logs.msgf -> unit
= fun log -> begin
  (* info function start *)
  Setting.msg Logs.Info log
  (* info function end *)
end

let warn : ('a, unit) Logs.msgf -> unit
= fun log -> begin
  (* warn function start *)
  Setting.msg Logs.Warning log
  (* warn function end *)
end

let err : ('a, unit) Logs.msgf -> unit
= fun log -> begin
  (* err function start *)
  Setting.msg Logs.Error log
  (* err function end *)
end

let create : unit -> unit
= fun () -> begin
  (* create function start *)
  Logs.set_reporter (Setting.reporter ());
  if !Options.flag_debug then (Logs.set_level (Some Logs.Debug))
  else if !Options.flag_verbose then (Logs.set_level (Some Logs.Info))
  else (Logs.set_level (Some Logs.Warning));
  info (fun m -> m "Logger Start.");
  ()
  (* create function end *)
end