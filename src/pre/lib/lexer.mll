{
  open Parser
  open Lexing

  exception Lexing_error of string

  let kwd_tbl = [ "parameter", PARAMETER; "storage", STORAGE; "code", CODE;
    "Unit", UNIT; "True", BOOLEAN true; "False", BOOLEAN false; "Pair", PAIR;
    "Left", LEFT; "Right", RIGHT; "Some", SOME; "None", NONE; "Elt", ELT;
    "DROP", I_DROP; "DUP", I_DUP; "SWAP", I_SWAP; "DIG", I_DIG; "DUG", I_DUG;
    "PUSH", I_PUSH; "SOME", I_SOME; "NONE", I_NONE; "UNIT", I_UNIT;
    "IF_NONE", I_IF_NONE; "PAIR", I_PAIR; "CAR", I_CAR; "CDR", I_CDR;
    "LEFT", I_LEFT; "RIGHT", I_RIGHT; "IF_LEFT", I_IF_LEFT; "NIL", I_NIL;
    "CONS", I_CONS; "IF_CONS", I_IF_CONS; "SIZE", I_SIZE;
    "EMPTY_SET", I_EMPTY_SET; "EMPTY_MAP", I_EMPTY_MAP;
    "EMPTY_BIG_MAP", I_EMPTY_BIG_MAP; "MAP", I_MAP; "ITER", I_ITER;
    "MEM", I_MEM; "GET", I_GET; "UPDATE", I_UPDATE; "IF", I_IF; "LOOP", I_LOOP;
    "LOOP_LEFT", I_LOOP_LEFT; "LAMBDA", I_LAMBDA; "EXEC", I_EXEC; "DIP", I_DIP;
    "FAILWITH", I_FAILWITH; "CAST", I_CAST; "RENAME", I_RENAME;
    "CONCAT", I_CONCAT; "SLICE", I_SLICE; "PACK", I_PACK; "UNPACK", I_UNPACK;
    "ADD", I_ADD; "SUB", I_SUB; "MUL", I_MUL; "EDIV", I_EDIV; "ABS", I_ABS;
    "ISNAT", I_ISNAT; "INT", I_INT; "NEG", I_NEG; "LSL", I_LSL; "LSR", I_LSR;
    "OR", I_OR; "AND", I_AND; "XOR", I_XOR; "NOT", I_NOT; "COMPARE", I_COMPARE;
    "EQ", I_EQ; "NEQ", I_NEQ; "LT", I_LT; "GT", I_GT; "LE", I_LE; "GE", I_GE;
    "SELF", I_SELF; "CONTRACT", I_CONTRACT;
    "TRANSFER_TOKENS", I_TRANSFER_TOKENS; "SET_DELEGATE", I_SET_DELEGATE;
    "CREATE_ACCOUNT", I_CREATE_ACCOUNT; "CREATE_CONTRACT", I_CREATE_CONTRACT;
    "IMPLICIT_ACCOUNT", I_IMPLICIT_ACCOUNT; "NOW", I_NOW; "AMOUNT", I_AMOUNT;
    "BALANCE", I_BALANCE; "CHECK_SIGNATURE", I_CHECK_SIGNATURE;
    "BLAKE2B", I_BLAKE2B; "SHA256", I_SHA256; "SHA512", I_SHA512;
    "HASH_KEY", I_HASH_KEY; "STEPS_TO_QUOTA", I_STEPS_TO_QUOTA;
    "SOURCE", I_SOURCE; "SENDER", I_SENDER; "ADDRESS", I_ADDRESS;
    "CHAIN_ID", I_CHAIN_ID; "UNPAIR", I_UNPAIR; "key", T_KEY; "unit", T_UNIT;
    "signature", T_SIGNATURE;    "option", T_OPTION; "list", T_LIST;
    "set", T_SET; "operation", T_OPERATION; "contract", T_CONTRACT;
    "pair", T_PAIR; "or", T_OR; "lambda", T_LAMBDA; "map", T_MAP;
    "big_map", T_BIG_MAP; "chain_id", T_CHAIN_ID; "int", T_INT; "nat", T_NAT;
    "string", T_STRING; "bytes", T_BYTES; "mutez", T_MUTEZ; "bool", T_BOOL;
    "key_hash", T_KEY_HASH; "timestamp", T_TIMESTAMP; "address", T_ADDRESS;
    ]

  let id_or_kwd s = try List.assoc s kwd_tbl with _ -> IDENT s
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let number = digit+
let space = ' ' | '\t' | '\r'
let string_content = "\\\"" | '\r' | '\n' | '\t' | '\b' | "\\\\" | [^ '"' '\\']
let string = '"' string_content* '"'
let new_line = '\n' | "\r\n"
let ident = letter (letter | digit | '_')*
let hex = "0x" ['0'-'9' 'a'-'f' 'A'-'F']*
let annotbody = (letter | digit | '_' | '.' | '%' | '@' | ':')+

let commentheader = '#'

let micse_check = "__MICSE_CHECK"

rule next_token = parse
  | commentheader { comment lexbuf }
  | new_line      { new_line lexbuf; next_token lexbuf }
  | space+        { next_token lexbuf }
  | string as s   { STRING s }
  | ident as s    { id_or_kwd s }
  | hex as s      { HEX s }
  | number as s   { NUM (Z.of_string s) }
  | ';'           { SEMICOLON }
  | '{'           { LB }
  | '}'           { RB }
  | '('           { LP }
  | ')'           { RP }
  | '-'           { MINUS }
  | '%'           { percent_annot lexbuf }
  | '@'           { at_annot lexbuf }
  | ':'           { colon_annot lexbuf }
  | eof           { EOF }
  | _ as c        { raise (Lexing_error ("Illegal character: " ^ String.make 1 c)) }

and comment = parse
  | micse_check   { I_MICSE_CHECK }
  | new_line  { new_line lexbuf; next_token lexbuf }
  | eof       { EOF }
  | _         { comment lexbuf }

and colon_annot = parse
  | annotbody as s { COLON_ANNOT s }
  | space          { COLON_ANNOT "" }
  | _ as c         { raise (Lexing_error ("Illegal character in colon_annot: " ^ String.make 1 c)) }

and at_annot = parse
  | annotbody as s { AT_ANNOT s }
  | space          { AT_ANNOT "" }
  | _ as c         { raise (Lexing_error ("Illegal character in at_annot: " ^ String.make 1 c)) }

and percent_annot = parse
  | annotbody as s { PERCENT_ANNOT s }
  | space          { PERCENT_ANNOT "" }
  | _ as c         { raise (Lexing_error ("Illegal character in percent_annot: " ^ String.make 1 c)) }