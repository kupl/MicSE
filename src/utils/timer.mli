(* Timer *)

(******************************************************************************)
(******************************************************************************)
(* Setting                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Setting : sig
  type t = {
    counter : Mtime_clock.counter;
    timeout : bool;
    budget  : float;
  }

  val create : ?budget:int -> unit -> t
  val read_elapsed_time : t -> float
  val read_is_timeout : t -> bool
end


(******************************************************************************)
(******************************************************************************)
(* Functions                                                                  *)
(******************************************************************************)
(******************************************************************************)

type t = Setting.t Stdlib.ref
type time = int

val time_curr : unit -> int
val string_of_curr_time : unit -> string
val create : ?budget:int -> unit -> t
val read_interval : t -> int
val check_timeout : t -> unit
val is_timeout : t -> bool
val string_of_elapsed_time : t -> string
