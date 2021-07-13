(* Timer *)

(******************************************************************************)
(******************************************************************************)
(* Setting                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Setting = struct
  type t = {
    counter : Mtime_clock.counter;
    timeout : bool;
    budget  : float;
  }

  let create : ?budget:int -> unit -> t
  = fun ?(budget=0) () -> begin
    { counter = Mtime_clock.counter ();
      timeout = false;
      budget  = float_of_int (budget * 1000); }
  end (* function create end *)

  let read_elapsed_time : t -> float
  = fun timer -> begin
    timer.counter |> Mtime_clock.count |> Mtime.Span.to_ms
  end (* function read_time end *)

  let read_is_timeout : t -> bool
  = fun timer -> begin
    (read_elapsed_time timer) >= timer.budget
  end (* function read_is_timeout end *)
end


(******************************************************************************)
(******************************************************************************)
(* Functions                                                                  *)
(******************************************************************************)
(******************************************************************************)

type t = Setting.t Stdlib.ref
type time = int

(* Current time from start of the execution *)
let time_curr : unit -> time
= fun () -> begin
  Mtime_clock.elapsed () |> Mtime.Span.to_ms |> int_of_float
end (* function time_curr end *)

(* Current time from start of the execution *)
let string_of_curr_time : unit -> string
= fun () -> (time_curr () |> string_of_int) ^ "ms"

let create : ?budget:time -> unit -> t
= fun ?(budget=0) () -> begin
  Stdlib.ref (Setting.create ~budget ())
end (* function create end *)

let read_interval : t -> time
= fun timer -> Setting.read_elapsed_time !timer |> int_of_float

let check_timeout : t -> unit
= fun timer -> begin
  if Setting.read_is_timeout !timer
  then timer := { !timer with timeout=true; }
  else ()
end (* function check_timeout end *)

let is_timeout : t -> bool
= fun timer -> begin
  let _ = check_timeout timer in
  !timer.timeout
end

let string_of_elapsed_time : t -> string
= fun timer -> (read_interval timer |> string_of_int) ^ "ms"
