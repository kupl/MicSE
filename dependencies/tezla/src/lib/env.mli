type elem = string * Adt.typ

type env = Failed | Stack of elem Functional_stack.t

val next_var : int ref -> string

val empty_env : env

val failed_env : env

val push : elem -> env -> env

val pop : env -> elem * env

val drop : env -> env

val peek : env -> elem

val swap : env -> env

val dig : env -> Z.t -> env

val dug : env -> Z.t -> env

val dip : env -> Z.t -> elem list * env

val dup : env -> elem * env
