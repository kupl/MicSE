type t [@@deriving sexp, compare, equal]
(* type t = (Tz.sym_state * Tz.mich_f list) list [@@deriving sexp, compare, equal] *)

type summary = {
  sm_rmci : Tz.r_mich_cut_info;
  sm_s_id : Tz.sym_state_id;
}
[@@deriving sexp, compare, equal]

module SMY_cmp : sig
  type t = summary [@@deriving sexp, compare]
end

val init : Tz.sym_state -> t

val cons : Tz.sym_state -> t -> t

val get_constraint : t -> Tz.mich_f list

val get_first_ss : t -> Tz.sym_state

val get_last_ss : t -> Tz.sym_state

val get_tail_ms : t -> t

val cut_first_found_loop : t -> (t * t) option

val get_length : t -> int

val get_summary : t -> summary
