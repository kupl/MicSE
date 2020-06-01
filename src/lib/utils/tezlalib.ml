let parse : string -> Michelson.Adt.program
=fun filename -> begin
  let ast = Tezla.Parsing_utils.parse_with_error filename in
  ast
end

let convert : Michelson.Adt.program -> Tezla.Adt.stmt
=fun ast -> begin
  let counter = ref 0 in
  let converted_ast = Tezla.Converter.convert_program counter ast in
  converted_ast
end

let print_original : Format.formatter -> Michelson.Adt.program -> unit
=fun fmt ast -> begin
  let _ = Format.pp_print_string fmt "\n" in
  let _ = Michelson.Pp.program fmt ast in
  ()
end

let print_convertd : Format.formatter -> Tezla.Adt.stmt -> unit
=fun fmt converted_ast -> begin
  let (_, filename) = Core.String.rsplit2_exn (!Options.input_file) ~on:'/' in
  let _ = Tezla.Pp.program fmt ((), (), (converted_ast, filename)) in
  ()
end