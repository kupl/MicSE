(* Timer *)

open! Core

module Mtime_s = struct
  include Mtime

  let t_of_sexp : Sexp.t -> t = (fun sexp -> int64_of_sexp sexp |> of_uint64_ns)
  (* function t_of_sexp end *)

  let sexp_of_t : t -> Sexp.t = (fun t -> to_uint64_ns t |> sexp_of_int64)
  (* function sexp_of_t end *)
end

module SMap = Map.Make (String)

(******************************************************************************)
(******************************************************************************)
(* Setting                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Setting = struct
  type t = {
    (* in nano second *)
    created_at : Mtime_s.t;
    expired_at : Mtime_s.t option;
    last_check : Mtime_s.t ref SMap.t;
  }
  [@@deriving sexp, compare, equal]

  let (us : int64) = Int64.of_int 1000

  let (ms : int64) = Int64.of_int (1000 * 1000)

  let (s : int64) = Int64.of_int (1000 * 1000 * 1000)

  let create : budget:int -> key_lst:string list -> t =
    fun ~budget ~key_lst ->
    let (now : Mtime_s.t) = Mtime_clock.now () in
    let (check_alist : (string * Mtime_s.t ref) list) =
       List.stable_dedup key_lst |> List.map ~f:(fun key -> (key, ref now))
    in
    {
      created_at = now;
      expired_at =
        ( if budget < 0
        then None
        else
          Int64.of_int budget
          |> Int64.( * ) s
          |> Mtime_s.Span.of_uint64_ns
          |> Mtime_s.add_span now
        );
      last_check = SMap.of_alist_exn check_alist;
    }
  (* function create end *)

  let read_time_elapsed : t -> Mtime_s.span =
    (fun time -> Mtime_s.span (Mtime_clock.now ()) time.created_at)
  (* function read_time_elapsed end *)

  let read_time_remain : t -> Mtime_s.span =
    fun time ->
    let (now : Mtime_s.t) = Mtime_clock.now () in
    let (expired : Mtime_s.t) = Option.value time.expired_at ~default:now in
    if Mtime_s.is_later expired ~than:now
    then Mtime_s.span expired now
    else Mtime_s.Span.zero
  (* function read_time_remain end *)

  let read_is_timeout : t -> bool =
    fun time ->
    let (now : Mtime_s.t) = Mtime_clock.now () in
    let (expired : Mtime_s.t) = Option.value time.expired_at ~default:now in
    Mtime_s.is_earlier expired ~than:now
  (* function read_is_timeout end *)

  let check : t -> key:string -> Mtime_s.span =
    fun time ~key ->
    SMap.find time.last_check key
    |> function
    | None          -> Failure "Time : Setting : check : key is unset" |> raise
    | Some time_ref ->
      let (last_time : Mtime_s.t) = !time_ref in
      let _ = time_ref := Mtime_clock.now () in
      Mtime_s.span !time_ref last_time
  (* function check end *)
end

(******************************************************************************)
(******************************************************************************)
(* Functions                                                                  *)
(******************************************************************************)
(******************************************************************************)

type t = Setting.t ref [@@deriving sexp, compare, equal]

type time = int64

let time_curr : unit -> time =
  (fun () -> Mtime_clock.elapsed () |> Mtime_s.Span.to_uint64_ns)
(* function time_curr end *)

let string_of_curr_time : unit -> string =
  fun () ->
  ((time_curr () |> Float.of_int64) /. (Setting.s |> Float.of_int64)
  |> Float.to_string_hum ~decimals:6 ~delimiter:','
  )
  ^ " sec"
(* function string_of_curr_time end *)

let create : ?budget:int -> ?key_lst:string list -> unit -> t =
  fun ?(budget = -1) ?(key_lst = []) () ->
  Stdlib.ref (Setting.create ~budget ~key_lst)
(* function create end *)

let read_elapsed_time : t -> time =
  (fun time -> Setting.read_time_elapsed !time |> Mtime_s.Span.to_uint64_ns)
(* function read_interval end *)

let read_interval : t -> time = (fun time -> read_elapsed_time time)
(* function read_interval end *)

let read_remaining : t -> time =
  (fun time -> Setting.read_time_remain !time |> Mtime_s.Span.to_uint64_ns)
(* function read_remaining end *)

let is_timeout : t -> bool = (fun time -> Setting.read_is_timeout !time)
(* function is_timeout end *)

let string_of_elapsed_time : t -> string =
  fun time ->
  ((read_interval time |> Float.of_int64) /. (Setting.s |> Float.of_int64)
  |> Float.to_string_hum ~decimals:6 ~delimiter:','
  )
  ^ " sec"
(* function string_of_elapsed_time end *)

let string_of_remaining_time : t -> string =
  fun time ->
  ((read_remaining time |> Float.of_int64) /. (Setting.s |> Float.of_int64)
  |> Float.to_string_hum ~decimals:6 ~delimiter:','
  )
  ^ " sec"
(* function string_of_remaining_time end *)

let read_elapsed_time_from_last_check : t -> key:string -> time =
  (fun time ~key -> Setting.check !time ~key |> Mtime_s.Span.to_uint64_ns)
(* function read_elapsed_time_from_last_check end *)

let string_of_elapsed_time_from_last_check : t -> key:string -> string =
  fun time ~key ->
  ((read_elapsed_time_from_last_check time ~key |> Float.of_int64)
   /. (Setting.s |> Float.of_int64)
  |> Float.to_string_hum ~decimals:6 ~delimiter:','
  )
  ^ " sec"
(* function string_of_elapsed_time_from_last_check end *)
