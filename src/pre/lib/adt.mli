type t = Michelson.Adt.program

type typ = Michelson.Adt.typ Michelson.Adt.t
type inst = Michelson.Adt.inst Michelson.Adt.t
type data = Michelson.Adt.data Michelson.Adt.t
type operation = Tezla.Adt.operation

val parse : string -> t

val pp : Format.formatter -> t -> unit

val string_of_typt : typ -> string

val is_typ_equal : typ -> typ -> bool