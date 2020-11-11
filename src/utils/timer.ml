(*****************************************************************************)
(*****************************************************************************)
(* Flag                                                                      *)
(*****************************************************************************)
(*****************************************************************************)

type flag = bool

let flag_set : flag
=true

let flag_unset : flag
=false


(*****************************************************************************)
(*****************************************************************************)
(* Time                                                                      *)
(*****************************************************************************)
(*****************************************************************************)

type time = int (* second *)

let time_curr : unit -> time (* Current time from start of the execution *)
=fun () -> begin
  let t = Unix.times () in
  Float.to_int (t.tms_utime)
end

let string_of_time : time -> string
=fun t -> (string_of_int t) ^ "s"


(*****************************************************************************)
(*****************************************************************************)
(* Timer                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

type t = {
  timeout: flag;
  budget: time;
  start_time: time;
}

let create : budget:time -> t ref
=fun ~budget -> ref { timeout=flag_unset; budget=budget; start_time=(time_curr ()) }

let read_interval : t ref -> time
=fun timer -> begin
  let cur_time = time_curr () in
  (cur_time - !timer.start_time)
end

let check_timeout : t ref -> unit
=fun timer -> begin
  if (read_interval timer) >= !timer.budget
  then timer := { !timer with timeout=flag_set }
  else ()
end

let is_timeout : t ref -> bool
=fun timer -> begin
  let _ = check_timeout timer in
  !timer.timeout
end

