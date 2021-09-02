type t = (Tz.sym_state * Tz.mich_f list) list [@@deriving sexp, compare, equal]

val init : Tz.sym_state -> t

val cons : Tz.sym_state -> t -> t

val cut_first_found_loop : Tz.mich_cut_info -> t -> t option

val get_constraint : t -> Tz.mich_f list
