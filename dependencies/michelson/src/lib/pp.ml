open Adt
open Format

let rec data fmt d =
  match d.d with
  | D_int d -> Z.pp_print fmt d
  | D_string s | D_bytes s -> fprintf fmt "\"%s\"" s
  | D_elt (d_1, d_2) -> fprintf fmt "Elt %a %a" data d_1 data d_2
  | D_left d -> fprintf fmt "Left %a" data d
  | D_right d -> fprintf fmt "Right %a" data d
  | D_some d -> fprintf fmt "Some %a" data d
  | D_none -> fprintf fmt "None"
  | D_unit -> fprintf fmt "Unit"
  | D_bool b -> fprintf fmt (match b with true -> "True" | false -> "False")
  | D_pair (d_1, d_2) -> fprintf fmt "(Pair %a %a)" data d_1 data d_2
  | D_list d ->
      let () = pp_print_string fmt "{" in
      let () =
        pp_print_list
          ~pp_sep:(fun fmt () -> pp_print_string fmt "; ")
          data fmt d
      in
      pp_print_string fmt "}"

let rec typ fmt t =
  let print_string = pp_print_string fmt in
  match t.d with
  | T_int -> print_string "int"
  | T_nat -> print_string "nat"
  | T_string -> print_string "string"
  | T_bytes -> print_string "bytes"
  | T_mutez -> print_string "mutez"
  | T_bool -> print_string "bool"
  | T_key_hash -> print_string "key_hash"
  | T_timestamp -> print_string "timestamp"
  | T_address -> print_string "address"
  | T_key -> print_string "key"
  | T_unit -> print_string "unit"
  | T_signature -> print_string "signature"
  | T_option t -> fprintf fmt "(option %a)" typ t
  | T_list t -> fprintf fmt "(list %a)" typ t
  | T_set t -> fprintf fmt "(set %a)" typ t
  | T_operation -> print_string "operation"
  | T_contract t -> fprintf fmt "(contract %a)" typ t
  | T_pair (t_1, t_2) -> fprintf fmt "(pair %a %a)" typ t_1 typ t_2
  | T_or (t_1, t_2) -> fprintf fmt "(or %a %a)" typ t_1 typ t_2
  | T_lambda (t_1, t_2) -> fprintf fmt "(lambda %a %a)" typ t_1 typ t_2
  | T_map (t_1, t_2) -> fprintf fmt "(map %a %a)" typ t_1 typ t_2
  | T_big_map (t_1, t_2) -> fprintf fmt "(big_map %a %a)" typ t_1 typ t_2
  | T_chain_id -> fprintf fmt "chain_id"

let rec inst fmt i =
  match i.d with
  | I_abs -> fprintf fmt "ABS"
  | I_drop -> fprintf fmt "DROP"
  | I_dup -> fprintf fmt "DUP"
  | I_swap -> fprintf fmt "SWAP"
  | I_some -> fprintf fmt "SOME"
  | I_unit -> fprintf fmt "UNIT"
  | I_pair -> fprintf fmt "PAIR"
  | I_car -> fprintf fmt "CAR"
  | I_cdr -> fprintf fmt "CDR"
  | I_cons -> fprintf fmt "CONS"
  | I_size -> fprintf fmt "SIZE"
  | I_mem -> fprintf fmt "MEM"
  | I_get -> fprintf fmt "GET"
  | I_update -> fprintf fmt "UPDATE"
  | I_exec -> fprintf fmt "EXEC"
  | I_failwith -> fprintf fmt "FAILWITH"
  | I_cast t -> fprintf fmt "CAST %a" typ t
  | I_rename -> fprintf fmt "RENAME"
  | I_concat -> fprintf fmt "CONCAT"
  | I_slice -> fprintf fmt "SLICE"
  | I_pack -> fprintf fmt "PACK"
  | I_add -> fprintf fmt "ADD"
  | I_sub -> fprintf fmt "SUB"
  | I_mul -> fprintf fmt "MUL"
  | I_ediv -> fprintf fmt "EDIV"
  | I_isnat -> fprintf fmt "ISNAT"
  | I_int -> fprintf fmt "INT"
  | I_neg -> fprintf fmt "NEG"
  | I_lsl -> fprintf fmt "LSL"
  | I_lsr -> fprintf fmt "LSR"
  | I_or -> fprintf fmt "OR"
  | I_and -> fprintf fmt "AND"
  | I_xor -> fprintf fmt "XOR"
  | I_not -> fprintf fmt "NOT"
  | I_compare -> fprintf fmt "COMPARE"
  | I_eq -> fprintf fmt "EQ"
  | I_neq -> fprintf fmt "NEQ"
  | I_lt -> fprintf fmt "LT"
  | I_gt -> fprintf fmt "GT"
  | I_le -> fprintf fmt "LE"
  | I_ge -> fprintf fmt "GE"
  | I_self -> fprintf fmt "SELF"
  | I_transfer_tokens -> fprintf fmt "TRANSFER_TOKENS"
  | I_set_delegate -> fprintf fmt "SET_DELEGATE"
  | I_create_account -> fprintf fmt "CREATE_ACCOUNT"
  | I_implicit_account -> fprintf fmt "IMPLICIT_ACCOUNT"
  | I_now -> fprintf fmt "NOW"
  | I_amount -> fprintf fmt "AMOUNT"
  | I_balance -> fprintf fmt "BALANCE"
  | I_check_signature -> fprintf fmt "CHECK_SIGNATURE"
  | I_blake2b -> fprintf fmt "BLAKE2B"
  | I_sha256 -> fprintf fmt "SHA256"
  | I_sha512 -> fprintf fmt "SHA512"
  | I_hash_key -> fprintf fmt "HASH_KEY"
  | I_steps_to_quota -> fprintf fmt "STEPS_TO_QUOTA"
  | I_source -> fprintf fmt "SOURCE"
  | I_sender -> fprintf fmt "SENDER"
  | I_address -> fprintf fmt "ADDRESS"
  | I_chain_id -> fprintf fmt "CHAIN_ID"
  | I_noop -> fprintf fmt ""
  | I_unpair -> fprintf fmt "UNPAIR"
  | I_seq (i_1, i_2) -> fprintf fmt "%a; %a" inst i_1 inst i_2
  | I_drop_n n when n = Z.one -> fprintf fmt "DROP"
  | I_drop_n n -> fprintf fmt "DROP %a" Z.pp_print n
  | I_dig n -> fprintf fmt "DIG %a" Z.pp_print n
  | I_dug n -> fprintf fmt "DUG %a" Z.pp_print n
  | I_push (t, d) -> fprintf fmt "PUSH %a %a" typ t data d
  | I_none t -> fprintf fmt "NONE %a" typ t
  | I_if_none (i_1, i_2) ->
      fprintf fmt "IF_NONE { %a } { %a }" inst i_1 inst i_2
  | I_if_some (i_1, i_2) ->
      fprintf fmt "IF_SOME { %a } { %a }" inst i_1 inst i_2
  | I_left t -> fprintf fmt "LEFT %a" typ t
  | I_right t -> fprintf fmt "RIGHT %a" typ t
  | I_if_left (i_1, i_2) ->
      fprintf fmt "IF_LEFT { %a } { %a }" inst i_1 inst i_2
  | I_if_right (i_1, i_2) ->
      fprintf fmt "IF_RIGHT { %a } { %a }" inst i_1 inst i_2
  | I_nil t -> fprintf fmt "NIL %a" typ t
  | I_if_cons (i_1, i_2) ->
      fprintf fmt "IF_CONS { %a } { %a }" inst i_1 inst i_2
  | I_empty_set t -> fprintf fmt "EMPTY_SET %a" typ t
  | I_empty_map (t_1, t_2) -> fprintf fmt "EMPTY_MAP %a %a" typ t_1 typ t_2
  | I_empty_big_map (t_1, t_2) ->
      fprintf fmt "EMPTY_BIG_MAP %a %a" typ t_1 typ t_2
  | I_map i -> fprintf fmt "MAP { %a }" inst i
  | I_iter i -> fprintf fmt "ITER { %a }" inst i
  | I_if (i_1, i_2) -> fprintf fmt "IF { %a } { %a }" inst i_1 inst i_2
  | I_loop i -> fprintf fmt "LOOP { %a }" inst i
  | I_loop_left i -> fprintf fmt "LOOP_LEFT { %a }" inst i
  | I_lambda (t_1, t_2, i) ->
      fprintf fmt "LAMBDA %a %a { %a }" typ t_1 typ t_2 inst i
  | I_dip i -> fprintf fmt "DIP { %a }" inst i
  | I_dip_n (n, i) -> fprintf fmt "DIP %a { %a }" Z.pp_print n inst i
  | I_unpack t -> fprintf fmt "UNPACK %a" typ t
  | I_contract t -> fprintf fmt "CONTRACT %a" typ t
  | I_create_contract p -> fprintf fmt "CREATE_CONTRACT { %a }" program p

and program fmt { code; param; storage } =
  let () = fprintf fmt "parameter %a;\n" typ param in
  let () = fprintf fmt "storage %a;\n" typ storage in
  fprintf fmt "code { %a }\n" inst code
