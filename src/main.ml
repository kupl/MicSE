(*open Tezla*)

let parse : string -> Michelson.Adt.program = fun s ->
  let lexbuf = Lexing.from_string s in
  let program : Michelson.Adt.program = Michelson.Parser.start Michelson.Lexer.next_token lexbuf in
  program

let main : unit -> unit
=fun () -> begin
  print_endline ("Hello, World!");
  let _ = parse "PUSH nat 0" in
  print_endline "BYE WORLD";
end

let _ = main ()