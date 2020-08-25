(*****************************************************************************)
(*****************************************************************************)
(* Queries                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

type vertex = Pre.Lib.Cfg.vertex
and stmt = Pre.Lib.Cfg.stmt

type cond = Bp.cond
and loc = Bp.loc
and category = Bp.category

type formula = Vlang.t

type t = {
  id: int;
  query: formula;
  loc: loc;
  typ: category;
  status: status;
}

and status =
  | Q_proven
  | Q_nonproven
  | Q_unproven of (Smt.z_expr * Smt.z_expr) option

val compare_query : t -> t -> int

val query_id : int ref

val create_query_id : unit -> int

val create_status_proven : status

val create_status_nonproven : status

val create_status_unproven : (Smt.z_expr * Smt.z_expr) option -> status

val create_new_query : formula -> loc -> category -> t

val update_status : t -> status -> t