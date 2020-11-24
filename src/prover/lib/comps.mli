(*****************************************************************************)
(*****************************************************************************)
(* Components                                                                *)
(*****************************************************************************)
(*****************************************************************************)

type typ = Vlang.Ty.t
and formula = Vlang.v_formula
and expr = Vlang.Expr.t

type t = {
    mutez : component list;
    mutez_map : component list;
  }
and component = expr * approach list
and approach = (formula -> formula)

val empty : t

val read_components : expr -> t -> t

val append_approach : t -> approach -> t
