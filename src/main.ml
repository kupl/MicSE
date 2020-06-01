let parse : string -> Michelson.Adt.program = fun s ->
  let lexbuf = Lexing.from_string s in
  let program : Michelson.Adt.program = Michelson.Parser.start Michelson.Lexer.next_token lexbuf in
  program

let main : unit -> unit
=fun () -> begin
  let _ = print_endline "Hello!" in
  let filepath = !Utils.Options.input_file in
  let _ = Tezla.Parsing_utils.parse filepath in
  let _ = print_endline "Bye!" in
  ()
end

let _ = begin
  let usageMsg = "micse-main -input filename" in
  let _ = Arg.parse Utils.Options.options Utils.Options.activate_detector usageMsg in
  let _ = Printexc.record_backtrace true in
  try
    if !Utils.Options.input_file <> ""
    then
      let _ = main () in
      ()
    else
      raise (Failure "No_Input")
  with
  | exc -> prerr_endline (Printexc.to_string exc); prerr_endline (Printexc.get_backtrace())
end