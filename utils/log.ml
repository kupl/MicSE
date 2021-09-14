(* Logger *)

open! Core

(******************************************************************************)
(******************************************************************************)
(* Setting                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Setting = struct
  let log_counter : Mtime_clock.counter = Mtime_clock.counter ()

  let reporter : unit -> Logs.reporter =
    fun () ->
    let report _ level ~over k msgf =
       let k' _ =
          over ();
          k ()
       in
       (* k' function end *)
       let dt = log_counter |> Mtime_clock.count |> Mtime.Span.to_ms in
       msgf
       @@ fun ?header ?tags fmt ->
       ignore tags;
       match level with
       | Logs.App ->
         Format.kfprintf k' Format.std_formatter
           ("%a @[" ^^ fmt ^^ "@]@.")
           Logs.pp_header (level, header)
       | _        ->
         Format.kfprintf k' Format.err_formatter
           ("%a [%06.0fms] @[" ^^ fmt ^^ "@]@.")
           Logs.pp_header (level, header) dt
    in
    (* inner-function report end *)
    { Logs.report }
  (* function reporter end *)

  let msg : Logs.level -> 'a Logs.log = (fun level log -> Logs.msg level log)
  (* function msg end *)
end

(******************************************************************************)
(******************************************************************************)
(* Functions                                                                  *)
(******************************************************************************)
(******************************************************************************)

let app : ('a, unit) Logs.msgf -> unit =
  fun log ->
  Setting.msg Logs.App log;
  Out_channel.flush Out_channel.stdout
(* function app end *)

let debug : ('a, unit) Logs.msgf -> unit =
  fun log ->
  Setting.msg Logs.Debug log;
  Out_channel.flush Out_channel.stderr
(* function info end *)

let info : ('a, unit) Logs.msgf -> unit =
  fun log ->
  Setting.msg Logs.Info log;
  Out_channel.flush Out_channel.stderr
(* function info end *)

let warn : ('a, unit) Logs.msgf -> unit =
  fun log ->
  Setting.msg Logs.Warning log;
  Out_channel.flush Out_channel.stderr
(* function warn end *)

let err : ('a, unit) Logs.msgf -> unit =
  fun log ->
  Setting.msg Logs.Error log;
  Out_channel.flush Out_channel.stderr
(* function err end *)

let create : unit -> unit =
  fun () ->
  Logs_threaded.enable ();
  Logs.set_reporter (Setting.reporter ());
  if !Argument.debug_mode
  then Logs.set_level (Some Logs.Debug)
  else if !Argument.verbose_mode
  then Logs.set_level (Some Logs.Info)
  else Logs.set_level (Some Logs.Warning);
  info (fun m -> m "Logger Start.");
  ()
(* function create end *)
