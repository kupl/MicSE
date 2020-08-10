open Format
open Adt

let typ = Michelson.Pp.typ

let data = Michelson.Pp.data

let rec operation ppf = function
  | O_create_account (e_1, e_2, e_3, e_4) ->
      fprintf ppf "CREATE_ACCOUNT %s %s %s %s" e_1 e_2 e_3 e_4
  | O_create_contract (_, e_1, e_2, e_3) ->
      fprintf ppf "CREATE_CONTRACT {...} %s %s %s" e_1 e_2 e_3
  | O_set_delegate e -> fprintf ppf "SET_DELEGATE %s" e
  | O_transfer_tokens (e_1, e_2, e_3) ->
      fprintf ppf "TRANSFER_TOKENS %s %s %s" e_1 e_2 e_3

and expr ppf = function
  | E_push (d, t) -> fprintf ppf "PUSH %a %a" typ t data d
  | E_car e -> fprintf ppf "CAR %s" e
  | E_cdr e -> fprintf ppf "CDR %s" e
  | E_abs e -> fprintf ppf "ABS %s" e
  | E_neg e -> fprintf ppf "NEG %s" e
  | E_not e -> fprintf ppf "NOT %s" e
  | E_eq e -> fprintf ppf "EQ %s" e
  | E_neq e -> fprintf ppf "NEQ %s" e
  | E_lt e -> fprintf ppf "LT %s" e
  | E_gt e -> fprintf ppf "GT %s" e
  | E_leq e -> fprintf ppf "LEQ %s" e
  | E_geq e -> fprintf ppf "GEQ %s" e
  | E_left (e, t) -> fprintf ppf "LEFT %a %s" typ t e
  | E_right (e, t) -> fprintf ppf "RIGHT %a %s" typ t e
  | E_some e -> fprintf ppf "SOME %s" e
  | E_cast e -> fprintf ppf "CAST %s" e
  | E_pack e -> fprintf ppf "PACK %s" e
  | E_contract_of_address e -> fprintf ppf "CONTRACT %s" e
  | E_implicit_account e -> fprintf ppf "IMPLICIT_ACCOUNT %s" e
  | E_blake2b e -> fprintf ppf "BLAKE2B %s" e
  | E_sha256 e -> fprintf ppf "SHA256 %s" e
  | E_sha512 e -> fprintf ppf "SHA512 %s" e
  | E_hash_key e -> fprintf ppf "HASH_KEY %s" e
  | E_unit -> fprintf ppf "UNIT"
  | E_none t -> fprintf ppf "NONE %a" typ t
  | E_add (e_1, e_2) -> fprintf ppf "ADD %s %s" e_1 e_2
  | E_sub (e_1, e_2) -> fprintf ppf "SUB %s %s" e_1 e_2
  | E_mul (e_1, e_2) -> fprintf ppf "MUL %s %s" e_1 e_2
  | E_ediv (e_1, e_2) -> fprintf ppf "EDIV %s %s" e_1 e_2   
  | E_div (e_1, e_2) -> fprintf ppf "EDIV %s %s" e_1 e_2    (* Original Tezla print E_div as EDIV, so I don't want to change it. *)
  | E_mod (e_1, e_2) -> fprintf ppf "mod %s %s" e_1 e_2
  | E_shiftL (e_1, e_2) -> fprintf ppf "LSL %s %s" e_1 e_2
  | E_shiftR (e_1, e_2) -> fprintf ppf "LSR %s %s" e_1 e_2
  | E_and (e_1, e_2) -> fprintf ppf "AND %s %s" e_1 e_2
  | E_or (e_1, e_2) -> fprintf ppf "OR %s %s" e_1 e_2
  | E_xor (e_1, e_2) -> fprintf ppf "XOR %s %s" e_1 e_2
  | E_compare (e_1, e_2) -> fprintf ppf "COMPARE %s %s" e_1 e_2
  | E_cons (e_1, e_2) -> fprintf ppf "CONS %s %s" e_1 e_2
  | E_pair (e_1, e_2) -> fprintf ppf "PAIR %s %s" e_1 e_2
  | E_mem (e_1, e_2) -> fprintf ppf "MEM %s %s" e_1 e_2
  | E_get (e_1, e_2) -> fprintf ppf "GET %s %s" e_1 e_2
  | E_concat (e_1, e_2) -> fprintf ppf "CONCAT %s %s" e_1 e_2
  | E_concat_list e -> fprintf ppf "CONCAT %s" e
  | E_update (e_1, e_2, e_3) -> fprintf ppf "UPDATE %s %s %s" e_1 e_2 e_3
  | E_slice (e_1, e_2, e_3) -> fprintf ppf "SLICE %s %s %s" e_1 e_2 e_3
  | E_check_signature (e_1, e_2, e_3) ->
      fprintf ppf "CHECK_SIGNATURE %s %s %s" e_1 e_2 e_3
  | E_unpack (t, e) -> fprintf ppf "UNPACK %a %s" typ t e
  | E_self -> fprintf ppf "SELF"
  | E_now -> fprintf ppf "NOW"
  | E_amount -> fprintf ppf "AMOUNT"
  | E_balance -> fprintf ppf "BALANCE"
  | E_steps_to_quota -> fprintf ppf "STEPS_TO_QUOTA"
  | E_source -> fprintf ppf "SOURCE"
  | E_sender -> fprintf ppf "SENDER"
  | E_address_of_contract e -> fprintf ppf "ADDRESS %s" e
  | E_size e -> fprintf ppf "SIZE %s" e
  | E_unlift_option e -> fprintf ppf "unlift_option %s" e
  | E_unlift_or e -> fprintf ppf "unlift_or %s" e
  | E_unlift_left e -> fprintf ppf "unlift_left %s" e
  | E_unlift_right e -> fprintf ppf "unlift_right %s" e
  | E_hd e -> fprintf ppf "hd %s" e
  | E_tl e -> fprintf ppf "tl %s" e
  | E_isnat e -> fprintf ppf "ISNAT %s" e
  | E_int_of_nat e -> fprintf ppf "INT %s" e
  | E_chain_id -> fprintf ppf "CHAIN_ID"
  | E_lambda (t_1, t_2, _) -> fprintf ppf "LAMBDA %a %a {...}" typ t_1 typ t_2
  | E_lambda_id i -> fprintf ppf "LAMBDA_ID %d" i
  | E_exec (e_1, e_2) -> fprintf ppf "EXEC %s %s" e_1 e_2
  | E_create_contract_address _ -> (* TODO: *) fprintf ppf ""
  | E_create_account_address _ -> (* TODO: *) fprintf ppf ""
  | E_operation o -> operation ppf o
  | E_dup s -> fprintf ppf "DUP %s" s
  | E_nil t -> fprintf ppf "NIL %a" typ t
  | E_empty_set t -> fprintf ppf "EMPTY_SET %a" typ t
  | E_empty_map (t_k, t_v) -> fprintf ppf "EMPTY_MAP %a %a" typ t_k typ t_v
  | E_empty_big_map (t_k, t_v) ->
      fprintf ppf "EMPTY_BIG_MAP %a %a" typ t_k typ t_v
  | E_append (v_1, v_2) -> fprintf ppf "append(%s, %s)" v_1 v_2
  | E_phi (v_1, v_2) -> fprintf ppf "phi(%s, %s)" v_1 v_2
  | E_special_nil_list -> fprintf ppf "[]"
  | E_itself v_1 -> fprintf ppf "%s" v_1

let rec stmt i ppf n =
  match n.stm with
  | S_seq ({ id = _; stm = S_skip }, s) | S_seq (s, { id = _; stm = S_skip }) ->
      stmt i ppf s
  | S_seq (s_1, s_2) -> fprintf ppf "%a;\n%a" (stmt i) s_1 (stmt i) s_2
  | S_assign (s, e) -> fprintf ppf "%s := %a" s expr e
  | S_skip -> fprintf ppf ""
  | S_drop l ->
      let print_list ppf =
        let pp_sep ppf _ = fprintf ppf "," in
        let pp_v = pp_print_text in
        pp_print_list ~pp_sep pp_v ppf
      in
      fprintf ppf "DROP %a" print_list l
  | S_swap -> fprintf ppf "SWAP"
  | S_dig -> fprintf ppf "DIG"
  | S_dug -> fprintf ppf "DUG"
  | S_if (s, s_1, s_2) ->
      let i' = i + 1 in
      fprintf ppf "IF %s\n{\n%a\n}\n{\n%a\n}" s (stmt i') s_1 (stmt i') s_2
  | S_if_none (s, s_1, s_2) ->
      let i' = i + 1 in
      fprintf ppf "IF_NONE %s\n{\n%a\n}\n{\n%a\n}" s (stmt i') s_1 (stmt i') s_2
  | S_if_left (s, s_1, s_2) ->
      let i' = i + 1 in
      fprintf ppf "IF_LEFT %s\n{\n%a\n}\n{\n%a\n}" s (stmt i') s_1 (stmt i') s_2
  | S_if_cons (s, s_1, s_2) ->
      let i' = i + 1 in
      fprintf ppf "IF_CONS %s\n{\n%a\n}\n{\n%a\n}" s (stmt i') s_1 (stmt i') s_2
  | S_loop (s, (v_1, v_2), b) ->
      let i' = i + 1 in
      fprintf ppf "LOOP %s := phi(%s, %s)\n{\n%a\n}" s v_1 v_2 (stmt i') b
  | S_loop_left (s, (v_1, v_2), b) ->
      let i' = i + 1 in
      fprintf ppf "LOOP_LEFT %s := phi(%s, %s)\n{\n%a\n}" s v_1 v_2 (stmt i') b
  | S_map ((c, (c_1, c_2)), (r, (r_1, r_2)), b) ->
      let i' = i + 1 in
      fprintf ppf "MAP %s := phi(%s, %s) with %s := phi(%s, %s)\n{\n%a\n}" c c_1
        c_2 r r_1 r_2 (stmt i') b
  | S_iter (s, (v_1, v_2), b) ->
      let i' = i + 1 in
      fprintf ppf "ITER %s := phi(%s, %s)\n{\n%a\n}" s v_1 v_2 (stmt i') b
  | S_failwith s -> fprintf ppf "FAILWITH %s" s

let func ppf (b, v) = fprintf ppf "@[<1>%s => {\n%a\n}" v (stmt 2) b

let program ppf (_, _, b) = func ppf b
