type t [@@deriving sexp, compare, equal]
(* type t = (Tz.sym_state * Tz.mich_f list) list [@@deriving sexp, compare, equal] *)

val init : Tz.sym_state -> t

val cons : Tz.sym_state -> t -> t

val get_constraint : t -> Tz.mich_f list

val get_first_ss : t -> Tz.sym_state

val get_last_ss : t -> Tz.sym_state

val get_tail_ms : t -> t

val cut_first_found_loop : t -> t option

val get_length : t -> int
