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

  type t = {
    typ : typ;
    base : int;
    memoryout : bool;
    budget : int option;
  }

  val kB : int

  val mB : int

  val gB : int

  val unit_size : int option

  val cur_max_memory : typ -> int

  val create : ?budget:int -> ?typ:typ -> unit -> t

  val read_memory_usage : t -> int

  val read_memory_remain : t -> int

  val read_is_memoryout : t -> bool

  val update_memoryout : t -> t
end

(******************************************************************************)
(******************************************************************************)
(* Functions                                                                  *)
(******************************************************************************)
(******************************************************************************)

type t = Setting.t Stdlib.ref

type typ = Setting.typ

type memory = int (* in GigaBytes *)

val memory_curr : ?typ:typ -> unit -> memory

val string_of_curr_memory : ?typ:typ -> unit -> string

val create : ?budget:int -> ?typ:typ -> unit -> t

val read_used_memory : t -> memory

val read_remaining : t -> memory

val check_memoryout : t -> unit

val is_memoryout : t -> bool

val string_of_used_memory : t -> string
