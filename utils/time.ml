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
    budget : float option;
  }

  let create : ?budget:int -> unit -> t =
    fun ?(budget = -1) () ->
    {
      counter = Mtime_clock.counter ();
      timeout = false;
      budget = (if budget < 0 then None else Some (float_of_int (budget * 1000)));
    }
  (* function create end *)

  let read_time_elapsed : t -> float =
    (fun timer -> timer.counter |> Mtime_clock.count |> Mtime.Span.to_ms)
  (* function read_time_elapsed end *)

  let read_time_remain : t -> float =
    fun timer ->
    if Option.is_none timer.budget
    then 0.
    else Float.max (Option.get timer.budget -. read_time_elapsed timer) 0.
  (* function read_time_remain end *)

  let read_is_timeout : t -> bool =
    fun timer ->
    if Option.is_none timer.budget
    then false
    else read_time_elapsed timer >= Option.get timer.budget
  (* function read_is_timeout end *)

  let update_timeout : t -> t =
    fun timer ->
    if read_is_timeout timer then { timer with timeout = true } else timer
  (* function update_timeout end *)
end

(******************************************************************************)
(******************************************************************************)
(* Functions                                                                  *)
(******************************************************************************)
(******************************************************************************)

type t = Setting.t Stdlib.ref

type time = int

(* Current time from start of the execution *)
let time_curr : unit -> time =
  (fun () -> Mtime_clock.elapsed () |> Mtime.Span.to_ms |> int_of_float)
(* function time_curr end *)

(* Current time from start of the execution *)
let string_of_curr_time : unit -> string =
  (fun () -> (time_curr () |> string_of_int) ^ "ms")

let create : ?budget:time -> unit -> t =
  (fun ?(budget = 0) () -> Stdlib.ref (Setting.create ~budget ()))
(* function create end *)

let read_interval : t -> time =
  (fun timer -> Setting.read_time_elapsed !timer |> int_of_float)

let read_elapsed_time : t -> time = read_interval

let read_remaining : t -> time =
  (fun timer -> Setting.read_time_remain !timer |> int_of_float)

let check_timeout : t -> unit =
  fun timer ->
  if Setting.read_is_timeout !timer
  then timer := Setting.update_timeout !timer
  else ()
(* function check_timeout end *)

let is_timeout : t -> bool =
  fun timer ->
  let _ = check_timeout timer in
  !timer.timeout

let string_of_elapsed_time : t -> string =
  (fun timer -> (read_interval timer |> string_of_int) ^ "ms")

let sexp_of_t : t -> Core.Sexp.t =
   let open Core in
   fun timer ->
   List [ Atom "budget"; sexp_of_option sexp_of_float !timer.budget ]

let t_of_sexp : Core.Sexp.t -> t =
   let open Core in
   fun sexp ->
   match sexp with
   | List [ Atom "budget"; budget ] ->
     let (budget_opt : float option) = option_of_sexp float_of_sexp budget in
     create ()
       ~budget:(Option.value budget_opt ~default:Float.zero |> int_of_float)
   | _ -> Failure "" |> raise

let compare : t -> t -> int =
  let open Core in
  fun timer1 timer2 ->
  compare_option compare_float !timer1.budget !timer2.budget 

let equal : t -> t -> bool = 
  let open Core in
  fun timer1 timer2 ->
  equal_option equal_float !timer1.budget !timer2.budget