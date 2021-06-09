type t = Mich.program

type typ = Mich.typ Mich.t
type inst = Mich.inst Mich.t
type data = Mich.data Mich.t
type operation = Operation.t

let print_position outx lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse : string -> t
=fun filename -> begin
  let in_c = Stdlib.open_in filename in
  let lexbuf = Lexing.from_channel in_c in
  try
    let res = Parser.start Lexer.next_token lexbuf in
    let () = close_in in_c in
    res
  with
  | Lexer.Lexing_error msg as e ->
      Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
      close_in in_c;
      raise e
  | Parser.Error as e ->
      Printf.fprintf stderr "%s%a: syntax error\n" filename print_position lexbuf;
      close_in in_c;
      raise e
end

let parse_data : string -> data
=fun filename -> begin
  let in_c = Stdlib.open_in filename in
  let lexbuf = Lexing.from_channel in_c in
  try
    let res = Parser.data_entry Lexer.next_token lexbuf in
    let () = close_in in_c in
    res
  with
  | Lexer.Lexing_error msg as e ->
      Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
      close_in in_c;
      raise e
  | Parser.Error as e ->
      Printf.fprintf stderr "%s%a: syntax error\n" filename print_position lexbuf;
      close_in in_c;
      raise e
end



let string_of_typt = Mich.string_of_typt

let rec is_typ_equal
= let open Mich in
  fun t1 t2 -> begin
  (*("DEBUG: " ^ (string_of_typt t1) ^ ", " ^ (string_of_typt t2)) |> print_endline |> Stdlib.ignore;*)
  match t1.d, t2.d with
  (* typ with one arg-typ *)
  | T_option tt1, T_option tt2
  | T_list tt1, T_list tt2
  | T_set tt1, T_set tt2
  | T_contract tt1, T_contract tt2  -> is_typ_equal tt1 tt2
  (* typ with two arg-typs *)
  | T_pair (tt11, tt12), T_pair (tt21, tt22)
  | T_or (tt11, tt12), T_or (tt21, tt22)
  | T_lambda (tt11, tt12), T_lambda (tt21, tt22)
  | T_map (tt11, tt12), T_map (tt21, tt22)
  | T_big_map (tt11, tt12), T_big_map (tt21, tt22) -> (is_typ_equal tt11 tt21) && (is_typ_equal tt12 tt22)
  (* typ with no arg-typ. I guess the Polymorphic Compare Operator will work well for distinguishing two data contructors. *)
  | _ -> (t1.d = t2.d)
end
