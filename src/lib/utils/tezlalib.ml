let parse : string -> Michelson.Adt.program
=fun filename -> begin
  let ast = Tezla.Parsing_utils.parse_with_error filename in
  ast
end

let convert : Michelson.Adt.program -> Tezla.Adt.stmt
=fun ast -> begin
  let counter = ref (-1) in
  let converted_ast = Tezla.Converter.convert_program counter ast in
  converted_ast
end

let cfg : Michelson.Adt.program -> Tezla_cfg.Flow_graph.Cfg.t
=fun ast -> begin
  let converted_cfg = Tezla_cfg.Flow_graph.Cfg.generate_from_program ast in
  converted_cfg
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

let display_cfg : Tezla_cfg.Flow_graph.Cfg.t -> unit
=fun cfg -> begin
  let _ = Tezla_cfg.Flow_graph.Cfg.show cfg in
  ()
end