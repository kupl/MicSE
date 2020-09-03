(*****************************************************************************)
(*****************************************************************************)
(* Components                                                                *)
(*****************************************************************************)
(*****************************************************************************)

type typ = Vlang.typ
and exp = Vlang.v_exp
and formula = Vlang.v_formula

type t = {
    mutez : component list;
    mutez_map : component list;
  }
and component = exp * typ * approach list
and approach = (formula -> formula)

val empty : t

val read_components : exp -> typ -> t -> t

val append_approach : t -> approach -> t