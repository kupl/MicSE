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

let compare_query : t -> t -> int
=fun x y -> Int.compare x.id y.id

let query_id : int ref
=ref 0

let create_query_id : unit -> int
=fun () -> query_id := !query_id + 1; !query_id

let create_new_query : formula -> loc:loc -> category:category -> t
=fun f ~loc ~category -> begin
  let id = create_query_id () in
  { id=id; query=f; loc=loc; typ=category; status=Q_nonproven }
end

let update_status : t -> status -> t
=fun q s -> { q with status=s }