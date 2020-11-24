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
  | Q_unproven of Smt.ZModel.t option

val compare_query : t -> t -> int

val query_id : int ref

val create_query_id : unit -> int

val create_new_query : formula -> loc:loc -> category:category -> t

val update_status : t -> status -> t