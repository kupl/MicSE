(* Logger  *)

(******************************************************************************)
(******************************************************************************)
(* Setting                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Setting : sig
  val log_counter : Mtime_clock.counter
  
  val reporter : unit -> Logs.reporter
end


(******************************************************************************)
(******************************************************************************)
(* Functions                                                                  *)
(******************************************************************************)
(******************************************************************************)

val app : ('a, unit) Logs.msgf -> unit
val debug : ('a, unit) Logs.msgf -> unit
val info : ('a, unit) Logs.msgf -> unit
val warn : ('a, unit) Logs.msgf -> unit
val err : ('a, unit) Logs.msgf -> unit

val create : unit -> unit