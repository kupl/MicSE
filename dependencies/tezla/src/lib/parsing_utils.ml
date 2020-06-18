open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error filename =
  let open Michelson in
  let in_c = open_in filename in
  let lexbuf = Lexing.from_channel in_c in
  try
    let res = Parser.start Lexer.next_token lexbuf in
    let () = close_in in_c in
    res
  with
  | Lexer.Lexing_error msg as e ->
      let () = Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg in
      let () = close_in in_c in
      raise e
  | Parser.Error as e ->
      let () =
        Printf.fprintf stderr "%s%a: syntax error\n" filename print_position
          lexbuf
      in
      let () = close_in in_c in
      raise e

let parse filename =
  let open Michelson in
  Parser.start Lexer.next_token (Lexing.from_channel (open_in filename))
