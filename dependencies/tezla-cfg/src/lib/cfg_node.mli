type loc = Unknown | Loc of int * int

type ident = string

type decl = ident

type typ = Tezla.Adt.typ

type expr = Tezla.Adt.expr

type stmt =
  | Cfg_assign of string * expr
  | Cfg_skip
  | Cfg_drop of string list
  | Cfg_swap
  | Cfg_dig
  | Cfg_dug
  | Cfg_if of string
  | Cfg_if_none of string
  | Cfg_if_left of string
  | Cfg_if_cons of string
  | Cfg_loop of string
  | Cfg_loop_left of string
  | Cfg_map of string
  | Cfg_iter of string
  | Cfg_failwith of string

type t = { id : int; stmt : stmt }

val to_string : t -> string

val create_node : ?id:int -> stmt -> t
