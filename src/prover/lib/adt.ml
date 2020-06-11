type t = Michelson.Adt.program

let parse : string -> t
=fun filename -> begin
  let ast = Tezla.Parsing_utils.parse_with_error filename in
  ast
end

let pp : Format.formatter -> t -> unit
=fun fmt ast -> begin
  let _ = Format.pp_print_string fmt "\n" in
  let _ = Michelson.Pp.program fmt ast in
  ()
end