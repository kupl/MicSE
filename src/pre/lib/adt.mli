type t = Mich.program

type typ = Mich.typ Mich.t
type inst = Mich.inst Mich.t
type data = Mich.data Mich.t
type operation = Operation.t

val parse : string -> t       (* parameter string syntax: filename(filepath) *)

val parse_data : string -> data (* parameter string syntax: filename(filepath) *)

val string_of_typt : typ -> string

val is_typ_equal : typ -> typ -> bool