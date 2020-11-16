(*
(*****************************************************************************)
(*****************************************************************************)
(* Components                                                                *)
(*****************************************************************************)
(*****************************************************************************)

type typ = Vlang.typ
and formula = Vlang.v_formula
and obj = Vlang.v_obj

type t = {
    mutez : component list;
    mutez_map : component list;
  }
and component = obj * approach list
and approach = (formula -> formula)

val empty : t

val read_components : obj -> t -> t

val append_approach : t -> approach -> t
*)