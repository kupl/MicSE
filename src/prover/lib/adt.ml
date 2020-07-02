type t = Michelson.Adt.program

type typ = Michelson.Adt.typ Michelson.Adt.t
type inst = Michelson.Adt.inst Michelson.Adt.t
type data = Michelson.Adt.data Michelson.Adt.t

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


let rec string_of_typt
= let a1 s tt = "(" ^ s ^ " " ^ (string_of_typt tt) ^ ")" in
  let a2 s tt1 tt2 = "(" ^ s ^ " " ^ (string_of_typt tt1) ^ " " ^ (string_of_typt tt2) ^ ")" in
  let open Michelson.Adt in
  fun t -> begin
  match t.d with
  | T_key               -> "key"
  | T_unit              -> "unit"
  | T_signature         -> "signature"
  | T_option t          -> a1 "option" t
  | T_list t            -> a1 "list" t
  | T_set t             -> a1 "set" t
  | T_operation         -> "operation"
  | T_contract t        -> a1 "contract" t
  | T_pair (t1, t2)     -> a2 "pair" t1 t2
  | T_or (t1, t2)       -> a2 "or" t1 t2
  | T_lambda (t1, t2)   -> a2 "lambda" t1 t2
  | T_map (t1, t2)      -> a2 "map" t1 t2
  | T_big_map (t1, t2)  -> a2 "big_map" t1 t2
  | T_chain_id          -> "chain_id"
  | T_int               -> "int"
  | T_nat               -> "nat"
  | T_string            -> "string"
  | T_bytes             -> "bytes"
  | T_mutez             -> "mutez"
  | T_bool              -> "bool"
  | T_key_hash          -> "key_hash"
  | T_timestamp         -> "timestamp"
  | T_address           -> "address"
end

let rec is_typ_equal
= let open Michelson.Adt in
  fun t1 t2 -> begin
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