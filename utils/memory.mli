(* Memory Manager *)

(******************************************************************************)
(******************************************************************************)
(* Setting                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Setting : sig
  type typ =
    | Self
    | Children
    | Both
  [@@deriving sexp, compare, equal]

  type t = {
    typ : typ;
    created_at : int64;
    expired_at : int64 option;
  }
  [@@deriving sexp, compare, equal]

  val kB : int64

  val mB : int64

  val gB : int64

  val unit_size : int64 option

  (* [cur_max_memory] is current maximum memory usage in bytes *)
  val cur_max_memory : typ -> int64

  val create : ?budget:int -> ?typ:typ -> unit -> t

  val read_memory_used : t -> int64

  val read_memory_remain : t -> int64

  val read_is_memoryout : t -> bool
end

(******************************************************************************)
(******************************************************************************)
(* Functions                                                                  *)
(******************************************************************************)
(******************************************************************************)

type t = Setting.t Stdlib.ref [@@deriving sexp, compare, equal]

type typ = Setting.typ

type memory = int64 (* in GigaBytes *)

(* [memory_curr] is current maximum usage of memory in this execution *)
val memory_curr : ?typ:typ -> unit -> memory

(* [string_of_curr_memory] is string format of current maximum usage of memory in this execution *)
val string_of_curr_memory : ?typ:typ -> unit -> string

(* [create] makes memory manager with optional memory budget (in gigabytes) *)
val create : ?budget:int -> ?typ:typ -> unit -> t

(* [read_used_memory] read used memory since the memory manager created *)
val read_used_memory : t -> memory

(* [read_remaining_memory] read remaining memory until the memory manager created *)
val read_remaining_memory : t -> memory

(* [is_memoryout] check memory manager whether memory bound out or not *)
val is_memoryout : t -> bool

(* [read_used_memory] is string format of used memory since the memory manager created *)
val string_of_used_memory : t -> string

(* [string_of_remaining_memory] is string format of remaining memory until the memory manager created *)
val string_of_remaining_memory : t -> string
