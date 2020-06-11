type t = Michelson.Adt.program

val parse : string -> t

val pp : Format.formatter -> t -> unit
