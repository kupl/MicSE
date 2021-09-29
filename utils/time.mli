(* Timer *)

module Mtime_s : sig
  include module type of Mtime

  val t_of_sexp : Core.Sexp.t -> t

  val sexp_of_t : t -> Core.Sexp.t
end

module SMap : module type of Core.Map.Make (Core.String)

(******************************************************************************)
(******************************************************************************)
(* Setting                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Setting : sig
  type t = {
    (* in nano second *)
    created_at : Mtime_s.t;
    expired_at : Mtime_s.t option;
    last_check : Mtime_s.t ref SMap.t;
  }
  [@@deriving sexp, compare, equal]

  val us : int64

  val ms : int64

  val s : int64

  val create : budget:int -> key_lst:string list -> t

  val read_time_elapsed : t -> Mtime_s.span

  val read_time_remain : t -> Mtime_s.span

  val read_is_timeout : t -> bool

  val check : t -> key:string -> Mtime_s.span
end

(******************************************************************************)
(******************************************************************************)
(* Functions                                                                  *)
(******************************************************************************)
(******************************************************************************)

type t = Setting.t Stdlib.ref [@@deriving sexp, compare, equal]

type time = int64

(* [time_curr] is current time from start of the execution *)
val time_curr : unit -> time

(* [string_of_curr_time] is string format of current time from start of the execution *)
val string_of_curr_time : unit -> string

(* [create] makes time manager with optional time budget (in seconds) *)
val create : ?budget:int -> ?key_lst:string list -> unit -> t

(* [read_elapsed_time] read elapsed time since the time manager created *)
val read_elapsed_time : t -> time

(* [read_interval] is same with [read_elapsed_time] *)
val read_interval : t -> time

(* [read_elapsed_time] read remaining time until the time manager expired *)
val read_remaining : t -> time

(* [is_timeout] check time manager whether timed out or not *)
val is_timeout : t -> bool

(* [string_of_elapsed_time] is string format of elapsed time since the time manager created *)
val string_of_elapsed_time : t -> string

(* [string_of_remaining_time] is string format of remaining time until the time manager expired *)
val string_of_remaining_time : t -> string

(* [read_elapsed_time_from_last_check] read elapsed time since the last checked time about input key *)
val read_elapsed_time_from_last_check : t -> key:string -> time

(* [string_of_remaining_time] is string format of elapsed time since the last checked time about input key *)
val string_of_elapsed_time_from_last_check : t -> key:string -> string
