(* Mich : Michelson Frontend Target Datatype. MicSE's Michelson representation
   is defined in Tz module *)

(*****************************************************************************)
(*****************************************************************************)
(* Location in Code                                                          *)
(*****************************************************************************)
(*****************************************************************************)

type pos = {
  col : int;
  lin : int;
}

type loc =
  | Unknown
  | Pos     of pos * pos

(*****************************************************************************)
(*****************************************************************************)
(* Michelson Code                                                            *)
(*****************************************************************************)
(*****************************************************************************)

type annot =
  | A_typ of string (* :type_annot   *)
  | A_var of string (* @var_annot    *)
  | A_fld of string
(* %field_annot *)

type typ =
  | T_key
  | T_unit
  | T_signature
  | T_option    of typ t
  | T_list      of typ t
  | T_set       of typ t
  | T_operation
  | T_contract  of typ t
  | T_pair      of typ t * typ t
  | T_or        of typ t * typ t
  | T_lambda    of typ t * typ t
  | T_map       of typ t * typ t
  | T_big_map   of typ t * typ t
  | T_chain_id
  | T_int
  | T_nat
  | T_string
  | T_bytes
  | T_mutez
  | T_bool
  | T_key_hash
  | T_timestamp
  | T_address

and inst =
  (* Standard Instructions : Michelson-Defined *)
  | I_seq              of inst t * inst t
  | I_drop
  | I_drop_n           of Z.t
  | I_dup
  | I_swap
  | I_dig              of Z.t
  | I_dug              of Z.t
  | I_push             of typ t * data t
  | I_some
  | I_none             of typ t
  | I_unit
  | I_if_none          of inst t * inst t
  | I_pair
  | I_car
  | I_cdr
  | I_left             of typ t
  | I_right            of typ t
  | I_if_left          of inst t * inst t
  | I_nil              of typ t
  | I_cons
  | I_if_cons          of inst t * inst t
  | I_size
  | I_empty_set        of typ t
  | I_empty_map        of typ t * typ t
  | I_empty_big_map    of typ t * typ t
  | I_map              of inst t
  | I_iter             of inst t
  | I_mem
  | I_get
  | I_update
  | I_if               of inst t * inst t
  | I_loop             of inst t
  | I_loop_left        of inst t
  | I_lambda           of typ t * typ t * inst t
  | I_exec
  | I_apply
  | I_dip              of inst t
  | I_dip_n            of Z.t * inst t
  | I_failwith
  | I_cast             of typ t
  | I_rename
  | I_concat
  | I_slice
  | I_pack
  | I_unpack           of typ t
  | I_add
  | I_sub
  | I_mul
  | I_ediv
  | I_abs
  | I_isnat
  | I_int
  | I_neg
  | I_lsl
  | I_lsr
  | I_or
  | I_and
  | I_xor
  | I_not
  | I_compare
  | I_eq
  | I_neq
  | I_lt
  | I_gt
  | I_le
  | I_ge
  | I_self
  | I_contract         of typ t
  | I_transfer_tokens
  | I_set_delegate
  | I_create_account
  | I_create_contract  of program
  | I_implicit_account
  | I_now
  | I_amount
  | I_balance
  | I_check_signature
  | I_blake2b
  | I_sha256
  | I_sha512
  | I_hash_key
  | I_steps_to_quota
  | I_source
  | I_sender
  | I_address
  | I_chain_id
  | I_unpair
  (* Standard Macros *)
  | M_plain            of string (* Macros with no following argument. e.g. FAIL *)
  | M_num              of string * Z.t (* Macros with one number argument. e.g. DUP n *)
  | M_code             of string * inst t (* Macros with one code argument. e.g. MAP_CAR *)
  | M_code2            of string * inst t * inst t (* Macros with two code arguments. e.g. IFCMPEQ *)
  (* Non-Standard Instruction : Introduced to resolve parsing issue *)
  | I_noop
  (* Non-Standard Instruction : Special Comment : MicSE user defined safety property *)
  | I_micse_check      of inst t
(* WARNING: I_check instruction is not in Michelson standard. It is for MicSE formatted-comment *)

and data =
  | D_int    of Z.t
  | D_string of string
  | D_bytes  of string
  | D_unit
  | D_bool   of bool
  | D_pair   of data t * data t
  | D_left   of data t
  | D_right  of data t
  | D_some   of data t
  | D_none
  | D_list   of data t list
  | D_elt    of data t * data t
  | D_lambda of inst t

and 'a t = {
  pos : loc;
  ann : annot list;
  d : 'a;
}

and program = {
  param : typ t;
  storage : typ t;
  code : inst t;
}

(*****************************************************************************)
(*****************************************************************************)
(* To String                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

let _soi = Stdlib.string_of_int

let string_of_pos : pos -> string = (fun p -> _soi p.lin ^ ":" ^ _soi p.col)

let string_of_loc : loc -> string = function
| Unknown      -> "N/A"
| Pos (p1, p2) -> string_of_pos p1 ^ "-" ^ string_of_pos p2

let string_of_annot : annot -> string = function
| A_typ s -> ":" ^ s
| A_var s -> "@" ^ s
| A_fld s -> "%" ^ s

let string_of_annots : annot list -> string =
  (fun al -> al |> List.map string_of_annot |> String.concat " ")

let rec string_of_typt_inner : bool -> typ t -> string =
  fun paren_flag tt ->
  let annotsstr = if tt.ann = [] then "" else " " ^ string_of_annots tt.ann in
  let bodystr =
     match tt.d with
     | T_key              -> "key" ^ annotsstr
     | T_unit             -> "unit" ^ annotsstr
     | T_signature        -> "signature" ^ annotsstr
     | T_option t         ->
       "option" ^ annotsstr ^ " " ^ string_of_typt_inner true t
     | T_list t           ->
       "list" ^ annotsstr ^ " " ^ string_of_typt_inner true t
     | T_set t            ->
       "set" ^ annotsstr ^ " " ^ string_of_typt_inner true t
     | T_operation        -> "operation" ^ annotsstr
     | T_contract t       ->
       "contract" ^ annotsstr ^ " " ^ string_of_typt_inner true t
     | T_pair (t1, t2)    ->
       "pair"
       ^ annotsstr
       ^ " "
       ^ string_of_typt_inner true t1
       ^ " "
       ^ string_of_typt_inner true t2
     | T_or (t1, t2)      ->
       "or"
       ^ annotsstr
       ^ " "
       ^ string_of_typt_inner true t1
       ^ " "
       ^ string_of_typt_inner true t2
     | T_lambda (t1, t2)  ->
       "lambda"
       ^ annotsstr
       ^ " "
       ^ string_of_typt_inner true t1
       ^ " "
       ^ string_of_typt_inner true t2
     | T_map (t1, t2)     ->
       "map"
       ^ annotsstr
       ^ " "
       ^ string_of_typt_inner true t1
       ^ " "
       ^ string_of_typt_inner true t2
     | T_big_map (t1, t2) ->
       "big_map"
       ^ annotsstr
       ^ " "
       ^ string_of_typt_inner true t1
       ^ " "
       ^ string_of_typt_inner true t2
     | T_chain_id         -> "chain_id" ^ annotsstr
     | T_int              -> "int" ^ annotsstr
     | T_nat              -> "nat" ^ annotsstr
     | T_string           -> "string" ^ annotsstr
     | T_bytes            -> "bytes" ^ annotsstr
     | T_mutez            -> "mutez" ^ annotsstr
     | T_bool             -> "bool" ^ annotsstr
     | T_key_hash         -> "key_hash" ^ annotsstr
     | T_timestamp        -> "timestamp" ^ annotsstr
     | T_address          -> "address" ^ annotsstr
  in
  match tt.d with
  | T_key
  | T_unit
  | T_signature
  | T_operation
  | T_chain_id
  | T_int
  | T_nat
  | T_string
  | T_bytes
  | T_mutez
  | T_bool
  | T_key_hash
  | T_timestamp
  | T_address ->
    bodystr
  | T_option _
  | T_list _
  | T_set _
  | T_contract _
  | T_pair _
  | T_or _
  | T_lambda _
  | T_map _
  | T_big_map _ ->
    if paren_flag then "(" ^ bodystr ^ ")" else bodystr

let string_of_typt : typ t -> string = string_of_typt_inner false

(* 'ol' for one-line, means that there are no newline character. - Except for the string of the "I_micse_check" instruction. *)
let rec string_of_datat_ol_inner : bool -> data t -> string =
  fun paren_flag dd ->
  let annotstr = if dd.ann = [] then "" else " " ^ string_of_annots dd.ann in
  let bodystr =
     match dd.d with
     | D_int zn        -> Z.to_string zn ^ annotstr
     | D_string s      -> "\\\"" ^ s ^ "\\\"" ^ annotstr
     | D_bytes s       -> "0x" ^ s ^ annotstr
     | D_unit          -> "Unit" ^ annotstr
     | D_bool b        -> (if b then "True" else "False") ^ annotstr
     | D_pair (d1, d2) ->
       "Pair"
       ^ annotstr
       ^ " "
       ^ string_of_datat_ol_inner true d1
       ^ " "
       ^ string_of_datat_ol_inner true d2
     | D_left d        ->
       "Left" ^ annotstr ^ " " ^ string_of_datat_ol_inner true d
     | D_right d       ->
       "Right" ^ annotstr ^ " " ^ string_of_datat_ol_inner true d
     | D_some d        ->
       "Some" ^ annotstr ^ " " ^ string_of_datat_ol_inner true d
     | D_none          -> "None" ^ annotstr
     | D_list dlst     ->
       "{"
       ^ (dlst
         |> List.map (fun x -> string_of_datat_ol_inner true x)
         |> String.concat "; "
         )
       ^ "}"
       ^ annotstr
     | D_elt (d1, d2)  ->
       "{Elt "
       ^ string_of_datat_ol_inner true d1
       ^ " "
       ^ string_of_datat_ol_inner true d2
       ^ "}"
       ^ annotstr
     | D_lambda c      -> "Lambda " ^ annotstr ^ " " ^ string_of_instt_ol c
  in
  match dd.d with
  | D_pair _
  | D_left _
  | D_right _
  | D_some _ ->
    if paren_flag then "(" ^ bodystr ^ ")" else bodystr
  | _ -> bodystr

and string_of_datat_ol : data t -> string =
  (fun d -> string_of_datat_ol_inner false d)

and string_of_seq_ol : inst t -> string =
  fun c ->
  let bodystr =
     match c.d with
     | I_seq (c1, c2) -> string_of_seq_ol c1 ^ "; " ^ string_of_seq_ol c2
     | _              -> string_of_instt_ol c
  in
  if c.ann <> [] then "{" ^ bodystr ^ "} " ^ string_of_annots c.ann else bodystr

and string_of_instt_ol : inst t -> string =
   let sos = string_of_seq_ol in
   let sos_br i = " {" ^ sos i ^ "}" in
   let sos_br2 i1 i2 = sos_br i1 ^ sos_br i2 in
   let sot t = " " ^ string_of_typt t in
   let sod d = " " ^ string_of_datat_ol d in
   (* FUNCTION BEGIN *)
   fun ii ->
   let annotstr = if ii.ann = [] then "" else " " ^ string_of_annots ii.ann in
   let bodystr =
      match ii.d with
      | I_seq _ -> sos ii
      | I_drop -> "DROP" ^ annotstr
      | I_drop_n zn -> "DROP" ^ " " ^ Z.to_string zn ^ annotstr
      | I_dup -> "DUP" ^ annotstr
      | I_swap -> "SWAP" ^ annotstr
      | I_dig zn -> "DIG" ^ " " ^ Z.to_string zn ^ annotstr
      | I_dug zn -> "DUG" ^ " " ^ Z.to_string zn ^ annotstr
      | I_push (t, d) -> "PUSH" ^ annotstr ^ sot t ^ sod d
      | I_some -> "SOME" ^ annotstr
      | I_none t -> "NONE" ^ sot t ^ annotstr
      | I_unit -> "UNIT" ^ annotstr
      | I_if_none (i1, i2) -> "IF_NONE" ^ annotstr ^ sos_br2 i1 i2
      | I_pair -> "PAIR" ^ annotstr
      | I_car -> "CAR" ^ annotstr
      | I_cdr -> "CDR" ^ annotstr
      | I_left t -> "LEFT" ^ annotstr ^ sot t
      | I_right t -> "RIGHT" ^ annotstr ^ sot t
      | I_if_left (i1, i2) -> "IF_LEFT" ^ annotstr ^ sos_br2 i1 i2
      | I_nil t -> "NIL" ^ annotstr ^ sot t
      | I_cons -> "CONS" ^ annotstr
      | I_if_cons (i1, i2) -> "IF_CONS" ^ annotstr ^ sos_br2 i1 i2
      | I_size -> "SIZE" ^ annotstr
      | I_empty_set t -> "EMPTY_SET" ^ annotstr ^ sot t
      | I_empty_map (t1, t2) -> "EMPTY_MAP" ^ annotstr ^ sot t1 ^ sot t2
      | I_empty_big_map (t1, t2) -> "EMPTY_BIG_MAP" ^ annotstr ^ sot t1 ^ sot t2
      | I_map i -> "MAP" ^ annotstr ^ sos_br i
      | I_iter i -> "ITER" ^ annotstr ^ sos_br i
      | I_mem -> "MEM" ^ annotstr
      | I_get -> "GET" ^ annotstr
      | I_update -> "UPDATE" ^ annotstr
      | I_if (i1, i2) -> "IF" ^ annotstr ^ sos_br2 i1 i2
      | I_loop i -> "LOOP" ^ annotstr ^ sos_br i
      | I_loop_left i -> "LOOP_LEFT" ^ annotstr ^ sos_br i
      | I_lambda (t1, t2, i) -> "LAMBDA" ^ annotstr ^ sot t1 ^ sot t2 ^ sos_br i
      | I_exec -> "EXEC" ^ annotstr
      | I_apply -> "APPLY" ^ annotstr
      | I_dip i -> "DIP" ^ annotstr ^ sos_br i
      | I_dip_n (zn, i) -> "DIP" ^ " " ^ Z.to_string zn ^ sos_br i
      | I_failwith -> "FAILWITH" ^ annotstr
      | I_cast t -> "CAST" ^ annotstr ^ sot t
      | I_rename -> "RENAME" ^ annotstr
      | I_concat -> "CONCAT" ^ annotstr
      | I_slice -> "SLICE" ^ annotstr
      | I_pack -> "PACK" ^ annotstr
      | I_unpack t -> "UNPACK" ^ annotstr ^ sot t
      | I_add -> "ADD" ^ annotstr
      | I_sub -> "SUB" ^ annotstr
      | I_mul -> "MUL" ^ annotstr
      | I_ediv -> "EDIV" ^ annotstr
      | I_abs -> "ABS" ^ annotstr
      | I_isnat -> "ISNAT" ^ annotstr
      | I_int -> "INT" ^ annotstr
      | I_neg -> "NEG" ^ annotstr
      | I_lsl -> "LSL" ^ annotstr
      | I_lsr -> "LSR" ^ annotstr
      | I_or -> "OR" ^ annotstr
      | I_and -> "AND" ^ annotstr
      | I_xor -> "XOR" ^ annotstr
      | I_not -> "NOT" ^ annotstr
      | I_compare -> "COMPARE" ^ annotstr
      | I_eq -> "EQ" ^ annotstr
      | I_neq -> "NEQ" ^ annotstr
      | I_lt -> "LT" ^ annotstr
      | I_gt -> "GT" ^ annotstr
      | I_le -> "LE" ^ annotstr
      | I_ge -> "GE" ^ annotstr
      | I_self -> "SELF" ^ annotstr
      | I_contract t -> "CONTRACT" ^ annotstr ^ sot t
      | I_transfer_tokens -> "TRANSFER_TOKENS" ^ annotstr
      | I_set_delegate -> "SET_DELEGATE" ^ annotstr
      | I_create_account -> "CREATE_ACCOUNT" ^ annotstr
      | I_create_contract p -> "CREATE_CONTRACT" ^ annotstr ^ string_of_pgm_ol p
      | I_implicit_account -> "IMPLICIT_ACCOUNT" ^ annotstr
      | I_now -> "NOW" ^ annotstr
      | I_amount -> "AMOUNT" ^ annotstr
      | I_balance -> "BALANCE" ^ annotstr
      | I_check_signature -> "CHECK_SIGNATURE" ^ annotstr
      | I_blake2b -> "BLAKE2B" ^ annotstr
      | I_sha256 -> "SHA256" ^ annotstr
      | I_sha512 -> "SHA512" ^ annotstr
      | I_hash_key -> "HASH_KEY" ^ annotstr
      | I_steps_to_quota -> "STEPS_TO_QUOTA" ^ annotstr
      | I_source -> "SOURCE" ^ annotstr
      | I_sender -> "SENDER" ^ annotstr
      | I_address -> "ADDRESS" ^ annotstr
      | I_chain_id -> "CHAIN_ID" ^ annotstr
      | I_unpair -> "UNPAIR" ^ annotstr
      (* Standard Macros *)
      | M_plain s -> s ^ annotstr
      | M_num (s, zn) -> s ^ " " ^ Z.to_string zn ^ annotstr
      | M_code (s, i) -> s ^ annotstr ^ sos_br i
      | M_code2 (s, i1, i2) -> s ^ annotstr ^ sos_br2 i1 i2
      (* Non-Standard Instruction : Introduced to resolve parsing issue *)
      | I_noop -> "noop"
      (* Non-Standard Instruction : Special Comment : MicSE user defined safety property *)
      | I_micse_check i -> "#__MICSE_CHECK" ^ sos_br i ^ "\n"
   in
   bodystr

and string_of_pgm_ol : program -> string =
  fun p ->
  "{\nparam "
  ^ string_of_typt p.param
  ^ ";\nstorage "
  ^ string_of_typt p.storage
  ^ ";\ncode {"
  ^ string_of_seq_ol p.code
  ^ "} \n}"

(*****************************************************************************)
(*****************************************************************************)
(* Utility Functions                                                         *)
(*****************************************************************************)
(*****************************************************************************)

let get_d : 'a t -> 'a = (fun x -> x.d)

let gen_t : 'a -> 'a t = (fun x -> { pos = Unknown; ann = []; d = x })

let gen_t_a : annot list -> 'a -> 'a t =
  (fun a x -> { pos = Unknown; ann = a; d = x })

let gen_t_p : loc -> 'a -> 'a t = (fun p x -> { pos = p; ann = []; d = x })

let gen_t_pa : loc -> annot list -> 'a -> 'a t =
  (fun p a x -> { pos = p; ann = a; d = x })

let copy_info : 'a t -> 'b -> 'b t =
  (fun x y -> { pos = x.pos; ann = x.ann; d = y })

let copy_info_t : 'a t -> 'b t -> 'b t =
  (fun x y -> { pos = x.pos; ann = x.ann; d = y.d })

let get_min_pos : pos -> pos -> pos =
  fun { col = c1; lin = l1 } { col = c2; lin = l2 } ->
  match (l1 < l2, l1 = l2, c1 < c2) with
  | (true, _, _)         -> { col = c1; lin = l1 }
  | (false, true, true)  -> { col = c1; lin = l1 }
  | (false, true, false) -> { col = c2; lin = l2 }
  | (false, false, _)    -> { col = c2; lin = l2 }

let get_max_pos : pos -> pos -> pos =
  fun { col = c1; lin = l1 } { col = c2; lin = l2 } ->
  match (l1 < l2, l1 = l2, c1 < c2) with
  | (true, _, _)         -> { col = c2; lin = l2 }
  | (false, true, true)  -> { col = c2; lin = l2 }
  | (false, true, false) -> { col = c1; lin = l1 }
  | (false, false, _)    -> { col = c1; lin = l1 }

let join_pos : loc -> loc -> loc =
  fun loc_1 loc_2 ->
  match (loc_1, loc_2) with
  | (Pos (p11, p12), Pos (p21, p22)) ->
    Pos (get_min_pos p11 p21, get_max_pos p12 p22)
  | (Pos _, Unknown) -> loc_1
  | (Unknown, Pos _) -> loc_2
  | (Unknown, Unknown) -> Unknown

let join_pos_lst : loc list -> loc =
  (fun loclst -> List.fold_left join_pos Unknown loclst)

let join_instt_seq : annot list -> inst t -> inst t -> inst t =
  (fun ann i1 i2 -> { pos = join_pos i1.pos i2.pos; ann; d = I_seq (i1, i2) })

let empty_instt = { pos = Unknown; ann = []; d = I_noop }

let gen_instseq : annot list -> inst list -> inst t =
  fun annlst instlst ->
  let i_t =
     List.fold_left
       (fun acc i -> join_instt_seq [] acc { pos = Unknown; ann = []; d = i })
       empty_instt instlst
  in
  { i_t with ann = annlst }

let gen_insttseq : annot list -> inst t list -> inst t =
  fun annlst insttlst ->
  let i_t =
     List.fold_left (fun acc i -> join_instt_seq [] acc i) empty_instt insttlst
  in
  { i_t with ann = annlst }
(* [A; B; C] -> I_seq ( I_seq (a, b), c ) *)

(*****************************************************************************)
(*****************************************************************************)
(* Standard Macros                                                           *)
(*****************************************************************************)
(*****************************************************************************)

exception Not_Macro of string

let nm_fail s = raise (Not_Macro s)

let str_fst s n : string = String.sub s 0 n
(* the first-n characters in the given string-s. *)

let str_lst s n : string = String.sub s (String.length s - n) n
(* the last-n characters in the given string-s. *)

let str_mid s n1 n2 : string = String.sub s n1 (String.length s - (n1 + n2))
(* exclude first-n1 & last-n2 characters in the given string-s. *)

let m_if : inst -> inst -> inst = (fun i1 i2 -> I_if (gen_t i1, gen_t i2))

(*****************************************************************************)
(* Macros - Plain Macros                                                     *)
(*****************************************************************************)

(* PLAIN MACRO LIST
   - FAIL
   - CMPOP
   - ASSERT
   - ASSERTOP
   - ASSERTCMPOP
   - ASSERT_NONE
   - ASSERT_SOME
   - ASSERT_LEFT
   - ASSERT_RIGHT
   - DUUP
   - CADR
   - SET_CADR
   - PAIR
   - UNPAIR
   - DIIP
*)

let m_fail : inst t = gen_instseq [] [ I_unit; I_failwith ]

let m_fail_d : inst = get_d m_fail

let m_cmpop : inst -> inst t = (fun i -> gen_instseq [] [ I_compare; i ])

let m_assert : inst t = gen_t (m_if I_noop m_fail_d)

let m_assertop : inst -> inst t =
  (fun i -> gen_insttseq [] [ gen_t i; m_assert ])

let m_assertcmpop : inst -> inst t =
  (fun i -> gen_insttseq [] [ m_cmpop i; m_assert ])

let parse_duup : string -> bool * inst t option =
  fun s ->
  if String.length s > 2 && str_fst s 1 = "D" && str_lst s 1 = "P"
  then (
    let (b, n) : bool * int =
       Core.String.foldi (str_mid s 1 1) ~init:(true, 0)
         ~f:(fun _ (acc_b, acc_n) c ->
           if acc_b && c = 'U' then (true, acc_n + 1) else (false, 0)
       )
    in
    let iopt : inst t option =
       match (b, n) with
       | (false, _) -> None
       | (true, 1)  -> gen_t I_dup |> Option.some
       | (true, 2)  ->
         gen_instseq [] [ I_dip (gen_t I_dup); I_swap ] |> Option.some
       | (true, n)  ->
         gen_instseq []
           [ I_dip_n (Z.of_int (n - 1), gen_t I_dup); I_dig (Z.of_int n) ]
         |> Option.some
    in
    (b, iopt)
  )
  else (false, None)

let parse_ad : string -> bool * bool list =
  fun s ->
  if (* IMPLICIT MAPPING: 'A' -> true, 'D' -> false *)
     (* This function considers that the given string-s has one or more characters. *)
     (* If there are any other character except 'A' and 'D' in the given string-s, it will return (false, []) *)
     String.length s > 0
  then (
    let (b, bl_rev) : bool * bool list =
       Core.String.foldi s ~init:(true, []) ~f:(fun _ (acc_b, acc_bl_rev) c ->
           if acc_b
           then (
             match c with
             | 'A' -> (true, true :: acc_bl_rev)
             | 'D' -> (true, false :: acc_bl_rev)
             | _   -> (false, [])
           )
           else (false, [])
       )
    in
    (b, List.rev bl_rev)
  )
  else (false, [])

let parse_cadr : string -> bool * inst t option =
  fun s ->
  if String.length s > 2 && str_fst s 1 = "C" && str_lst s 1 = "R"
  then (
    let (b, bl) = parse_ad (str_mid s 1 1) in
    let il : inst list = List.map (fun x -> if x then I_car else I_cdr) bl in
    if b then (true, Some (gen_instseq [] il)) else (false, None)
  )
  else (false, None)

let parse_set_cadr : string -> bool * inst t option =
  fun s ->
  if str_fst s 5 = "SET_C" && str_lst s 1 = "R" && String.length s > 6
  then (
    let (b, bl) = parse_ad (str_mid s 5 1) in
    let rec f : bool list -> inst t =
      fun bl ->
      match bl with
      | []          -> gen_t I_noop
      | [ true ]    -> gen_instseq [] [ I_cdr; I_swap; I_pair ]
      | [ false ]   -> gen_instseq [] [ I_car; I_pair ]
      | true :: tl  ->
        let in_dip : inst t = gen_insttseq [] [ gen_t I_car; f tl ] in
        gen_instseq [] [ I_dup; I_dip in_dip; I_cdr; I_swap; I_pair ]
      | false :: tl ->
        let in_dip : inst t = gen_insttseq [] [ gen_t I_cdr; f tl ] in
        gen_instseq [] [ I_dup; I_dip in_dip; I_car; I_pair ]
    in
    if b then (true, Some (f bl)) else (false, None)
  )
  else (false, None)

type pair_leaf =
  | PT_A
  | PT_I

type pair_tree =
  | PT_P of pair_tree * pair_tree
  | PT_L of pair_leaf

let rec construct_pair_tree :
    string -> int * pair_tree -> pair_leaf -> int * pair_tree =
  fun s (i, pt) lf ->
  if (* This function considers that the given string-s has only 'P', 'A', and 'I' characters. 'R' should be omitted before invocation. *)
     String.length s <= i
  then (i, pt)
  else (
    match (String.get s i, lf) with
    | ('P', _) ->
      let (j, lpt) = construct_pair_tree s (i + 1, pt) PT_A in
      let (k, rpt) = construct_pair_tree s (j, pt) PT_I in
      (k, PT_P (lpt, rpt))
    | ('A', PT_A) -> (i + 1, PT_L PT_A)
    | ('I', PT_I) -> (i + 1, PT_L PT_I)
    | ('A', _)
    | ('I', _) ->
      nm_fail
        "construct_pair_tree : Expected character but inappropriate situation. "
    | _ -> nm_fail "construct_pair_tree : Unexpected character"
  )

let rec decode_pair_tree : pair_tree -> inst list = function
| PT_P (PT_L PT_A, PT_L PT_I) -> [ I_pair ]
| PT_P (PT_L PT_A, PT_P (y1, y2)) ->
  [ I_dip (gen_instseq [] (decode_pair_tree (PT_P (y1, y2)))); I_pair ]
| PT_P (PT_P (x1, x2), PT_L PT_I) ->
  decode_pair_tree (PT_P (x1, x2)) @ [ I_pair ]
| PT_P (PT_P (x1, x2), PT_P (y1, y2)) ->
  decode_pair_tree (PT_P (x1, x2))
  @ [ I_dip (gen_instseq [] (decode_pair_tree (PT_P (y1, y2)))); I_pair ]
| _ -> nm_fail "decode_pair_tree : Unexpected Tree Form."

let parse_pair : string -> bool * inst t option =
  fun s ->
  if String.length s > 3 && str_lst s 1 = "R"
  then (
    try
      let itopt : inst t option =
         construct_pair_tree (str_mid s 0 1) (0, PT_L PT_A) PT_A
         |> Stdlib.snd
         |> decode_pair_tree
         |> gen_instseq []
         |> Option.some
      in
      (true, itopt)
    with
    | Not_Macro _ -> (false, None)
  )
  else (false, None)

let rec decode_unpair_tree : pair_tree -> inst list =
   let unpairseq : inst list = [ I_dup; I_car; I_dip (gen_t I_cdr) ] in
   function
   | PT_P (PT_L PT_A, PT_L PT_I) -> unpairseq
   | PT_P (PT_L PT_A, PT_P (y1, y2)) ->
     unpairseq @ [ I_dip (gen_instseq [] (decode_unpair_tree (PT_P (y1, y2)))) ]
   | PT_P (PT_P (x1, x2), PT_L PT_I) ->
     unpairseq @ decode_unpair_tree (PT_P (x1, x2))
   | PT_P (PT_P (x1, x2), PT_P (y1, y2)) ->
     unpairseq
     @ [ I_dip (gen_instseq [] (decode_unpair_tree (PT_P (y1, y2)))) ]
     @ decode_unpair_tree (PT_P (x1, x2))
   | _ -> nm_fail "decode_pair_tree : Unexpected Tree Form."

let parse_unpair : string -> bool * inst t option =
  fun s ->
  if String.length s > 5 && str_fst s 2 = "UN" && str_lst s 1 = "R"
  then (
    try
      let itopt : inst t option =
         construct_pair_tree (str_mid s 2 1) (0, PT_L PT_A) PT_A
         |> Stdlib.snd
         |> decode_unpair_tree
         |> gen_instseq []
         |> Option.some
      in
      (true, itopt)
    with
    | Not_Macro _ -> (false, None)
  )
  else (false, None)

let resolve_plain_macro : inst t -> string -> inst t =
  fun i s ->
  let ann = i.ann in
  (* Search Fixed-name macros first *)
  match s with
  | "FAIL"          -> m_fail
  | "SELF_ADDRESS"  -> gen_instseq ann [ I_self; I_address ]
  | "ASSERT"        -> m_assert
  | "ASSERT_NONE"   -> gen_t (I_if_none (gen_t I_noop, m_fail))
  | "ASSERT_SOME"   -> gen_t (I_if_none (m_fail, gen_t_a ann I_rename))
  | "ASSERT_LEFT"   -> gen_t (I_if_left (gen_t_a ann I_rename, m_fail))
  | "ASSERT_RIGHT"  -> gen_t (I_if_left (m_fail, gen_t_a ann I_rename))
  | "CMPEQ"         -> m_cmpop I_eq
  | "ASSERT_EQ"     -> m_assertop I_eq
  | "ASSERT_CMPEQ"  -> m_assertcmpop I_eq
  | "CMPNEQ"        -> m_cmpop I_neq
  | "ASSERT_NEQ"    -> m_assertop I_neq
  | "ASSERT_CMPNEQ" -> m_assertcmpop I_neq
  | "CMPLT"         -> m_cmpop I_lt
  | "ASSERT_LT"     -> m_assertop I_lt
  | "ASSERT_CMPLT"  -> m_assertcmpop I_lt
  | "CMPLE"         -> m_cmpop I_le
  | "ASSERT_LE"     -> m_assertop I_le
  | "ASSERT_CMPLE"  -> m_assertcmpop I_le
  | "CMPGT"         -> m_cmpop I_gt
  | "ASSERT_GT"     -> m_assertop I_gt
  | "ASSERT_CMPGT"  -> m_assertcmpop I_gt
  | "CMPGE"         -> m_cmpop I_ge
  | "ASSERT_GE"     -> m_assertop I_ge
  | "ASSERT_CMPGE"  ->
    m_assertcmpop I_ge (* Other Cases : DUUP, CADR, SET_CADR, PAIR, UNPAIR *)
  | _               ->
    let (duup_bool, duup_it) = parse_duup s in
    if duup_bool
    then Option.get duup_it
    else (
      let (cadr_bool, cadr_it) = parse_cadr s in
      if cadr_bool
      then Option.get cadr_it
      else (
        let (set_cadr_bool, set_cadr_it) = parse_set_cadr s in
        if set_cadr_bool
        then Option.get set_cadr_it
        else (
          let (pair_bool, pair_it) = parse_pair s in
          if pair_bool
          then Option.get pair_it
          else (
            let (unpair_bool, unpair_it) = parse_unpair s in
            if unpair_bool
            then Option.get unpair_it
            else nm_fail ("resolve_plain_macro : every match failed : " ^ s)
          )
        )
      )
    )

(*****************************************************************************)
(* Macros - Macro with Number                                                *)
(*****************************************************************************)

(* NUMBER MACRO LIST
   - DUP
   - GET
   - UPDATE
   - PAIR
   - UNPAIR
*)

let construct_duup : int -> string = (fun n -> "D" ^ String.make n 'U' ^ "P")

let parse_get_n : int -> inst t =
  fun n ->
  let last_il = if n mod 2 = 0 then [] else [ I_car ] in
  gen_instseq [] (List.init (n / 2) (fun _ -> I_cdr) @ last_il)

let rec update_n_body : int -> inst t =
  fun n ->
  if n < 0
  then Stdlib.failwith "Mich.ml : update_n_body : n < 0"
  else (
    match n with
    | 0 -> gen_t I_drop
    | 1 -> gen_instseq [] [ I_unpair; I_drop; I_swap; I_pair ]
    | _ -> gen_instseq [] [ I_unpair; I_dip (update_n_body (n - 2)); I_pair ]
  )

let parse_update_n : int -> inst t =
  (fun n -> gen_t (I_seq (gen_t I_swap, update_n_body n)))

let rec parse_pair_n : int -> inst t =
  fun n ->
  if n < 2
  then Stdlib.failwith "Mich.ml : parse_pair_n : n < 1"
  else (
    match n with
    | 2 -> gen_t I_pair
    | _ -> gen_instseq [] [ I_dip (parse_pair_n (n - 1)); I_pair ]
  )

let rec parse_unpair_n : int -> inst t =
  fun n ->
  if n < 2
  then Stdlib.failwith "Mich.ml : parse_unpair_n : n < 1"
  else (
    match n with
    | 2 -> gen_t I_unpair
    | _ -> gen_instseq [] [ I_unpair; I_dip (parse_unpair_n (n - 1)) ]
  )

let resolve_num_macro : string -> Z.t -> inst t =
  fun s zn ->
  match s with
  | "DUP"    ->
    construct_duup (Z.to_int zn) |> parse_duup |> Stdlib.snd |> Option.get
  | "GET"    -> parse_get_n (Z.to_int zn)
  | "UPDATE" -> parse_update_n (Z.to_int zn)
  | "PAIR"   -> parse_pair_n (Z.to_int zn)
  | "UNPAIR" -> parse_unpair_n (Z.to_int zn)
  | _        -> nm_fail ("resolve_num_macro : every match failed : " ^ s)

(*****************************************************************************)
(* Macros - Macro with a code                                                *)
(*****************************************************************************)

(* CODE MACRO LIST
   - MAP_CADR
*)

let parse_map_cadr : string -> inst t -> bool * inst t option =
  fun s c ->
  if String.length s > 6 && str_fst s 5 = "MAP_C" && str_lst s 1 = "R"
  then (
    let (b, bl) = parse_ad (str_mid s 5 1) in
    let rec f : bool list -> inst t =
      fun bl ->
      match bl with
      | []          -> gen_t I_noop
      | [ true ]    ->
        let in_dip : inst t = gen_insttseq [] [ gen_t I_car; c ] in
        gen_instseq [] [ I_dup; I_cdr; I_dip in_dip; I_swap; I_pair ]
      | [ false ]   ->
        let (hd, tl) =
           ( gen_instseq [] [ I_dup; I_cdr ],
             gen_instseq [] [ I_swap; I_car; I_pair ]
           )
        in
        gen_insttseq [] [ hd; c; tl ]
      | true :: tl  ->
        let in_dip : inst t = gen_insttseq [] [ gen_t I_car; f tl ] in
        gen_instseq [] [ I_dup; I_dip in_dip; I_cdr; I_swap; I_pair ]
      | false :: tl ->
        let in_dip : inst t = gen_insttseq [] [ gen_t I_cdr; f tl ] in
        gen_instseq [] [ I_dup; I_dip in_dip; I_car; I_pair ]
    in
    if b then (true, Some (f bl)) else (false, None)
  )
  else (false, None)

let parse_diip : string -> inst t -> bool * inst t option =
  fun s c ->
  if String.length s > 2
     && str_fst s 1 = "D"
     && str_lst s 1 = "P"
     && str_mid s 1 1 = String.make (String.length s - 2) 'I'
  then (true, Some (gen_t (I_dip_n (Z.of_int (String.length s - 2), c))))
  else (false, None)

let resolve_code_macro : string -> inst t -> inst t =
  fun s c ->
  match s with
  | _ ->
    let (map_cadr_bool, map_cadr_it) = parse_map_cadr s c in
    if map_cadr_bool
    then Option.get map_cadr_it
    else (
      let (diip_bool, diip_it) = parse_diip s c in
      if diip_bool
      then Option.get diip_it
      else nm_fail ("resolve_code_macro : every match failed : " ^ s)
    )

(*****************************************************************************)
(* Macros - Macro with two codes                                             *)
(*****************************************************************************)

(* CODE2 MACRO LIST
   - IFOP
   - IFCMPOP
*)

let m_ifop : inst -> inst t -> inst t -> inst t =
  (fun op i1 i2 -> gen_instseq [] [ op; I_if (i1, i2) ])

let m_ifcmpop : inst -> inst t -> inst t -> inst t =
  (fun op i1 i2 -> gen_insttseq [] [ gen_t I_compare; m_ifop op i1 i2 ])

let resolve_code2_macro : string -> inst t -> inst t -> inst t =
  fun s c1 c2 ->
  match s with
  | "IFEQ"     -> m_ifop I_eq c1 c2
  | "IFCMPEQ"  -> m_ifcmpop I_eq c1 c2
  | "IFNEQ"    -> m_ifop I_neq c1 c2
  | "IFCMPNEQ" -> m_ifcmpop I_neq c1 c2
  | "IFLT"     -> m_ifop I_lt c1 c2
  | "IFCMPLT"  -> m_ifcmpop I_lt c1 c2
  | "IFLE"     -> m_ifop I_le c1 c2
  | "IFCMPLE"  -> m_ifcmpop I_le c1 c2
  | "IFGT"     -> m_ifop I_gt c1 c2
  | "IFCMPGT"  -> m_ifcmpop I_gt c1 c2
  | "IFGE"     -> m_ifop I_ge c1 c2
  | "IFCMPGE"  -> m_ifcmpop I_ge c1 c2
  | "IF_SOME"  -> gen_t (I_if_none (c2, c1))
  | "IF_RIGHT" -> gen_t (I_if_left (c2, c1))
  | _          -> nm_fail ("resolve_code2_macro : every match failed : " ^ s)

(*****************************************************************************)
(* Standard Macros - Overall                                                 *)
(*****************************************************************************)

let subst_standard_macro : inst t -> inst t =
  fun i ->
  match get_d i with
  | M_plain s           -> copy_info_t i (resolve_plain_macro i s)
  | M_num (s, n)        -> copy_info_t i (resolve_num_macro s n)
  | M_code (s, c)       -> copy_info_t i (resolve_code_macro s c)
  | M_code2 (s, c1, c2) -> copy_info_t i (resolve_code2_macro s c1 c2)
  | _                   ->
    nm_fail
      "subst_standard_macro : Given argument is not standard macro instruction."

let rec subst_standard_macro_all : inst t -> inst t =
   let scma = subst_standard_macro_all in
   fun c ->
   match get_d c with
   (* Standard Instructions : Michelson-Defined *)
   | I_seq (i1, i2) -> I_seq (scma i1, scma i2) |> copy_info c
   | I_push (t, d) -> I_push (t, subst_standard_macro_all_data d) |> copy_info c
   | I_if_none (i1, i2) -> I_if_none (scma i1, scma i2) |> copy_info c
   | I_if_left (i1, i2) -> I_if_left (scma i1, scma i2) |> copy_info c
   | I_if_cons (i1, i2) -> I_if_cons (scma i1, scma i2) |> copy_info c
   | I_map i -> I_map (scma i) |> copy_info c
   | I_iter i -> I_iter (scma i) |> copy_info c
   | I_if (i1, i2) -> I_if (scma i1, scma i2) |> copy_info c
   | I_loop i -> I_loop (scma i) |> copy_info c
   | I_loop_left i -> I_loop_left (scma i) |> copy_info c
   | I_lambda (t1, t2, i) -> I_lambda (t1, t2, scma i) |> copy_info c
   | I_dip i -> I_dip (scma i) |> copy_info c
   | I_dip_n (zn, i) -> I_dip_n (zn, scma i) |> copy_info c
   | I_create_contract { param = t1; storage = t2; code = i } ->
     I_create_contract { param = t1; storage = t2; code = scma i }
     |> copy_info c
   | I_drop
   | I_drop_n _
   | I_dup
   | I_swap
   | I_dig _
   | I_dug _
   | I_some
   | I_none _
   | I_unit
   | I_pair
   | I_car
   | I_cdr
   | I_left _
   | I_right _
   | I_nil _
   | I_cons
   | I_size
   | I_empty_set _
   | I_empty_map _
   | I_empty_big_map _
   | I_mem
   | I_get
   | I_update
   | I_exec
   | I_apply
   | I_failwith
   | I_cast _
   | I_rename
   | I_concat
   | I_slice
   | I_pack
   | I_unpack _
   | I_add
   | I_sub
   | I_mul
   | I_ediv
   | I_abs
   | I_isnat
   | I_int
   | I_neg
   | I_lsl
   | I_lsr
   | I_or
   | I_and
   | I_xor
   | I_not
   | I_compare
   | I_eq
   | I_neq
   | I_lt
   | I_gt
   | I_le
   | I_ge
   | I_self
   | I_contract _
   | I_transfer_tokens
   | I_set_delegate
   | I_create_account
   | I_implicit_account
   | I_now
   | I_amount
   | I_balance
   | I_check_signature
   | I_blake2b
   | I_sha256
   | I_sha512
   | I_hash_key
   | I_steps_to_quota
   | I_source
   | I_sender
   | I_address
   | I_chain_id
   | I_unpair ->
     c
   (* Standard Macros *)
   | M_plain _
   | M_num _ ->
     subst_standard_macro c
   | M_code (s, i) -> subst_standard_macro { c with d = M_code (s, scma i) }
   | M_code2 (s, i_1, i_2) ->
     subst_standard_macro { c with d = M_code2 (s, scma i_1, scma i_2) }
   (* Non-Standard Instruction : Introduced to resolve parsing issue *)
   | I_noop -> c
   (* Non-Standard Instruction : Special Comment : MicSE user defined safety property *)
   | I_micse_check i -> I_micse_check (scma i) |> copy_info c
(* WARNING: I_check instruction is not in Michelson standard. It is for MicSE formatted-comment *)

and subst_standard_macro_all_data : data t -> data t =
  fun d ->
  match get_d d with
  | D_lambda i -> D_lambda (subst_standard_macro_all i) |> copy_info d
  | _          -> d

let subst_standard_macro_all_pgm : program -> program =
  (fun pgm -> { pgm with code = subst_standard_macro_all pgm.code })

(*****************************************************************************)
(*****************************************************************************)
(* Fill Unknown Position Informations                                        *)
(*****************************************************************************)
(*****************************************************************************)

let fill_position_all ?(update_loc_flag = false) :
    (?update_loc:bool -> loc -> 'a -> loc * 'a) -> loc -> 'a t -> loc * 'a t =
  fun f l t ->
  match (l, t.pos) with
  | (Unknown, Unknown) ->
    let (rloc, d') = f ~update_loc:update_loc_flag Unknown t.d in
    (rloc, { t with pos = rloc; d = d' })
  | (Unknown, Pos _)   ->
    let (rloc, d') = f ~update_loc:update_loc_flag t.pos t.d in
    let cloc = if update_loc_flag then join_pos t.pos rloc else t.pos in
    (cloc, { t with pos = cloc; d = d' })
  | (Pos _, Unknown)   ->
    let (rloc, d') = f ~update_loc:update_loc_flag l t.d in
    let joined_pos = join_pos l rloc in
    (joined_pos, { t with pos = joined_pos; d = d' })
  | (Pos _, Pos _)     ->
    let (rloc, d') = f ~update_loc:update_loc_flag t.pos t.d in
    let joined_pos = join_pos l rloc in
    let cloc = if update_loc_flag then joined_pos else rloc in
    (joined_pos, { t with pos = cloc; d = d' })

let rec fill_position_all_typ ?(update_loc = false) : loc -> typ -> loc * typ =
  fun l t ->
  let fltyt = fill_position_all_typt ~update_loc l in
  match t with
  | T_option t1 ->
    let (loc_t, t1') = fltyt t1 in
    (loc_t, T_option t1')
  | T_list t1 ->
    let (loc_t, t1') = fltyt t1 in
    (loc_t, T_list t1')
  | T_set t1 ->
    let (loc_t, t1') = fltyt t1 in
    (loc_t, T_set t1')
  | T_contract t1 ->
    let (loc_t, t1') = fltyt t1 in
    (loc_t, T_contract t1')
  | T_pair (t1, t2) ->
    let ((loc_t1, t1'), (loc_t2, t2')) = (fltyt t1, fltyt t2) in
    (join_pos loc_t1 loc_t2, T_pair (t1', t2'))
  | T_or (t1, t2) ->
    let ((loc_t1, t1'), (loc_t2, t2')) = (fltyt t1, fltyt t2) in
    (join_pos loc_t1 loc_t2, T_or (t1', t2'))
  | T_lambda (t1, t2) ->
    let ((loc_t1, t1'), (loc_t2, t2')) = (fltyt t1, fltyt t2) in
    (join_pos loc_t1 loc_t2, T_lambda (t1', t2'))
  | T_map (t1, t2) ->
    let ((loc_t1, t1'), (loc_t2, t2')) = (fltyt t1, fltyt t2) in
    (join_pos loc_t1 loc_t2, T_map (t1', t2'))
  | T_big_map (t1, t2) ->
    let ((loc_t1, t1'), (loc_t2, t2')) = (fltyt t1, fltyt t2) in
    (join_pos loc_t1 loc_t2, T_big_map (t1', t2'))
  | T_key
  | T_unit
  | T_signature
  | T_operation
  | T_chain_id
  | T_int
  | T_nat
  | T_string
  | T_bytes
  | T_mutez
  | T_bool
  | T_key_hash
  | T_timestamp
  | T_address ->
    (l, t)

and fill_position_all_data ?(update_loc = false) : loc -> data -> loc * data =
  fun l d ->
  let fldtt = fill_position_all_datat ~update_loc l in
  match d with
  | D_pair (d1, d2) ->
    let ((loc_d1, d1'), (loc_d2, d2')) = (fldtt d1, fldtt d2) in
    (join_pos loc_d1 loc_d2, D_pair (d1', d2'))
  | D_left d1 ->
    let (loc_d1, d1') = fldtt d1 in
    (loc_d1, D_left d1')
  | D_right d1 ->
    let (loc_d1, d1') = fldtt d1 in
    (loc_d1, D_right d1')
  | D_some d1 ->
    let (loc_d1, d1') = fldtt d1 in
    (loc_d1, D_some d1')
  | D_list dlst ->
    let (locs, ds) = dlst |> List.map (fun x -> fldtt x) |> List.split in
    (join_pos_lst locs, D_list ds)
  | D_elt (d1, d2) ->
    let ((loc_d1, d1'), (loc_d2, d2')) = (fldtt d1, fldtt d2) in
    (join_pos loc_d1 loc_d2, D_elt (d1', d2'))
  | D_lambda c ->
    let (loc_c, c') = fill_position_all_instt ~update_loc l c in
    (loc_c, D_lambda c')
  | D_bool _
  | D_int _
  | D_string _
  | D_bytes _
  | D_unit
  | D_none ->
    (l, d)

and fill_position_all_inst ?(update_loc = false) : loc -> inst -> loc * inst =
  fun l ii ->
  let flitt = fill_position_all_instt ~update_loc l in
  let fldtt = fill_position_all_datat ~update_loc l in
  let fltyt = fill_position_all_typt ~update_loc l in
  match ii with
  (* Nested Instruction *)
  | I_seq (i1, i2) ->
    let ((loc_1, i1'), (loc_2, i2')) = (flitt i1, flitt i2) in
    (join_pos loc_1 loc_2, I_seq (i1', i2'))
  | I_push (t, d) ->
    let ((loc_t, t'), (loc_d, d')) = (fltyt t, fldtt d) in
    (join_pos loc_t loc_d, I_push (t', d'))
  | I_if_none (i1, i2) ->
    let ((loc_1, i1'), (loc_2, i2')) = (flitt i1, flitt i2) in
    (join_pos loc_1 loc_2, I_if_none (i1', i2'))
  | I_if_left (i1, i2) ->
    let ((loc_1, i1'), (loc_2, i2')) = (flitt i1, flitt i2) in
    (join_pos loc_1 loc_2, I_if_left (i1', i2'))
  | I_if_cons (i1, i2) ->
    let ((loc_1, i1'), (loc_2, i2')) = (flitt i1, flitt i2) in
    (join_pos loc_1 loc_2, I_if_cons (i1', i2'))
  | I_map i ->
    let (loc_1, i') = flitt i in
    (loc_1, I_map i')
  | I_iter i ->
    let (loc_1, i') = flitt i in
    (loc_1, I_iter i')
  | I_if (i1, i2) ->
    let ((loc_1, i1'), (loc_2, i2')) = (flitt i1, flitt i2) in
    (join_pos loc_1 loc_2, I_if (i1', i2'))
  | I_loop i ->
    let (loc_1, i') = flitt i in
    (loc_1, I_loop i')
  | I_loop_left i ->
    let (loc_1, i') = flitt i in
    (loc_1, I_loop_left i')
  | I_lambda (t1, t2, i) ->
    let ((loc_t1, t1'), (loc_t2, t2'), (loc_i, i')) =
       (fltyt t1, fltyt t2, flitt i)
    in
    (join_pos_lst [ loc_t1; loc_t2; loc_i ], I_lambda (t1', t2', i'))
  | I_dip i ->
    let (loc_1, i') = flitt i in
    (loc_1, I_dip i')
  | I_dip_n (zn, i) ->
    let (loc_1, i') = flitt i in
    (loc_1, I_dip_n (zn, i'))
  | I_create_contract { param = t1; storage = t2; code = i } ->
    let ((loc_t1, t1'), (loc_t2, t2'), (loc_i, i')) =
       (fltyt t1, fltyt t2, flitt i)
    in
    ( join_pos_lst [ loc_t1; loc_t2; loc_i ],
      I_create_contract { param = t1'; storage = t2'; code = i' }
    )
  (* Nested Type *)
  | I_none t ->
    let (loc_t, t') = fltyt t in
    (loc_t, I_none t')
  | I_left t ->
    let (loc_t, t') = fltyt t in
    (loc_t, I_left t')
  | I_right t ->
    let (loc_t, t') = fltyt t in
    (loc_t, I_right t')
  | I_nil t ->
    let (loc_t, t') = fltyt t in
    (loc_t, I_nil t')
  | I_empty_set t ->
    let (loc_t, t') = fltyt t in
    (loc_t, I_empty_set t')
  | I_empty_map (t1, t2) ->
    let ((loc_t1, t1'), (loc_t2, t2')) = (fltyt t1, fltyt t2) in
    (join_pos loc_t1 loc_t2, I_empty_map (t1', t2'))
  | I_empty_big_map (t1, t2) ->
    let ((loc_t1, t1'), (loc_t2, t2')) = (fltyt t1, fltyt t2) in
    (join_pos loc_t1 loc_t2, I_empty_big_map (t1', t2'))
  | I_cast t ->
    let (loc_t, t') = fltyt t in
    (loc_t, I_cast t')
  | I_unpack t ->
    let (loc_t, t') = fltyt t in
    (loc_t, I_unpack t')
  | I_contract t ->
    let (loc_t, t') = fltyt t in
    (loc_t, I_contract t')
  (* plain instructions *)
  | I_drop
  | I_drop_n _
  | I_dup
  | I_swap
  | I_dig _
  | I_dug _
  | I_some
  | I_unit
  | I_pair
  | I_car
  | I_cdr
  | I_cons
  | I_size
  | I_mem
  | I_get
  | I_update
  | I_exec
  | I_apply
  | I_failwith
  | I_rename
  | I_concat
  | I_slice
  | I_pack
  | I_add
  | I_sub
  | I_mul
  | I_ediv
  | I_abs
  | I_isnat
  | I_int
  | I_neg
  | I_lsl
  | I_lsr
  | I_or
  | I_and
  | I_xor
  | I_not
  | I_compare
  | I_eq
  | I_neq
  | I_lt
  | I_gt
  | I_le
  | I_ge
  | I_self
  | I_transfer_tokens
  | I_set_delegate
  | I_create_account
  | I_implicit_account
  | I_now
  | I_amount
  | I_balance
  | I_check_signature
  | I_blake2b
  | I_sha256
  | I_sha512
  | I_hash_key
  | I_steps_to_quota
  | I_source
  | I_sender
  | I_address
  | I_chain_id
  | I_unpair ->
    (l, ii)
  (* Standard Macros *)
  | M_plain _
  | M_num _ ->
    (l, ii)
  | M_code (s, i) ->
    let (loc_i, i') = flitt i in
    (loc_i, M_code (s, i'))
  | M_code2 (s, i1, i2) ->
    let ((loc_1, i1'), (loc_2, i2')) = (flitt i1, flitt i2) in
    (join_pos loc_1 loc_2, M_code2 (s, i1', i2'))
  (* Non-Standard Instruction : Introduced to resolve parsing issue *)
  | I_noop -> (l, ii)
  (* Non-Standard Instruction : Special Comment : MicSE user defined safety property *)
  | I_micse_check i ->
    let (loc_1, i') = flitt i in
    (loc_1, I_micse_check i')

and fill_position_all_typt ?(update_loc = false) : loc -> typ t -> loc * typ t =
   fill_position_all ~update_loc_flag:update_loc fill_position_all_typ

and fill_position_all_datat ?(update_loc = false) :
    loc -> data t -> loc * data t =
   fill_position_all ~update_loc_flag:update_loc fill_position_all_data

and fill_position_all_instt ?(update_loc = false) :
    loc -> inst t -> loc * inst t =
   fill_position_all ~update_loc_flag:update_loc fill_position_all_inst

let fill_position_all_pgm ?(update_loc = false) : program -> program =
  fun pgm ->
  {
    param =
      fill_position_all_typt ~update_loc pgm.param.pos pgm.param |> Stdlib.snd;
    storage =
      fill_position_all_typt ~update_loc pgm.storage.pos pgm.storage
      |> Stdlib.snd;
    code =
      fill_position_all_instt ~update_loc pgm.code.pos pgm.code |> Stdlib.snd;
  }

(*****************************************************************************)
(*****************************************************************************)
(* Optimization                                                              *)
(*****************************************************************************)
(*****************************************************************************)

let rec optm_remove_noop_in_seq : inst t -> inst t =
   let optm = optm_remove_noop_in_seq in
   fun c ->
   match get_d c with
   | I_seq (i1, { d = I_noop; _ }) -> optm i1
   | I_seq ({ d = I_noop; _ }, i2) -> optm i2
   | I_seq (i1, i2) -> I_seq (optm i1, optm i2) |> copy_info c
   | I_if_none (i1, i2) -> I_if_none (optm i1, optm i2) |> copy_info c
   | I_if_left (i1, i2) -> I_if_left (optm i1, optm i2) |> copy_info c
   | I_if_cons (i1, i2) -> I_if_cons (optm i1, optm i2) |> copy_info c
   | I_map i -> I_map (optm i) |> copy_info c
   | I_iter i -> I_iter (optm i) |> copy_info c
   | I_if (i1, i2) -> I_if (optm i1, optm i2) |> copy_info c
   | I_loop i -> I_loop (optm i) |> copy_info c
   | I_loop_left i -> I_loop_left (optm i) |> copy_info c
   | I_lambda (t1, t2, i) -> I_lambda (t1, t2, optm i) |> copy_info c
   | I_dip i -> I_dip (optm i) |> copy_info c
   | I_dip_n (zn, i) -> I_dip_n (zn, optm i) |> copy_info c
   | I_create_contract { param = t1; storage = t2; code = i } ->
     I_create_contract { param = t1; storage = t2; code = optm i }
     |> copy_info c
   | I_push (t, d) -> I_push (t, subst_standard_macro_all_data d) |> copy_info c
   | I_drop
   | I_drop_n _
   | I_dup
   | I_swap
   | I_dig _
   | I_dug _
   | I_some
   | I_none _
   | I_unit
   | I_pair
   | I_car
   | I_cdr
   | I_left _
   | I_right _
   | I_nil _
   | I_cons
   | I_size
   | I_empty_set _
   | I_empty_map _
   | I_empty_big_map _
   | I_mem
   | I_get
   | I_update
   | I_exec
   | I_apply
   | I_failwith
   | I_cast _
   | I_rename
   | I_concat
   | I_slice
   | I_pack
   | I_unpack _
   | I_add
   | I_sub
   | I_mul
   | I_ediv
   | I_abs
   | I_isnat
   | I_int
   | I_neg
   | I_lsl
   | I_lsr
   | I_or
   | I_and
   | I_xor
   | I_not
   | I_compare
   | I_eq
   | I_neq
   | I_lt
   | I_gt
   | I_le
   | I_ge
   | I_self
   | I_contract _
   | I_transfer_tokens
   | I_set_delegate
   | I_create_account
   | I_implicit_account
   | I_now
   | I_amount
   | I_balance
   | I_check_signature
   | I_blake2b
   | I_sha256
   | I_sha512
   | I_hash_key
   | I_steps_to_quota
   | I_source
   | I_sender
   | I_address
   | I_chain_id
   | I_unpair ->
     c
   (* Standard Macros *)
   | M_plain _
   | M_num _ ->
     c
   | M_code (s, i) -> { c with d = M_code (s, optm i) }
   | M_code2 (s, i_1, i_2) -> { c with d = M_code2 (s, optm i_1, optm i_2) }
   (* Non-Standard Instruction : Introduced to resolve parsing issue *)
   | I_noop -> c
   (* Non-Standard Instruction : Special Comment : MicSE user defined safety property *)
   | I_micse_check i -> I_micse_check (optm i) |> copy_info c
(* WARNING: I_check instruction is not in Michelson standard. It is for MicSE formatted-comment *)

let optm_all_pgm : program -> program =
  (fun pgm -> { pgm with code = optm_remove_noop_in_seq pgm.code })

(*****************************************************************************)
(* Counter - Instruction Counter                                             *)
(*****************************************************************************)

let rec count_inst_pgm : program -> int = (fun pgm -> count_inst_pgm_i pgm.code)

and count_inst_pgm_i : inst t -> int =
  fun i ->
  match i.d with
  (* Standard Instructions : Michelson-Defined *)
  | I_seq (i1, i2) -> count_inst_pgm_i i1 + count_inst_pgm_i i2
  | I_if_none (i1, i2)
  | I_if_left (i1, i2)
  | I_if_cons (i1, i2)
  | I_if (i1, i2) ->
    count_inst_pgm_i i1 + count_inst_pgm_i i2 + 1
  | I_map i1
  | I_iter i1
  | I_loop i1
  | I_loop_left i1
  | I_lambda (_, _, i1)
  | I_dip i1
  | I_dip_n (_, i1) ->
    count_inst_pgm_i i1 + 1
  | I_create_contract p1 -> count_inst_pgm p1 + 1
  | I_drop
  | I_drop_n _
  | I_dup
  | I_swap
  | I_dig _
  | I_dug _
  | I_push (_, _)
  | I_some
  | I_none _
  | I_unit
  | I_pair
  | I_car
  | I_cdr
  | I_left _
  | I_right _
  | I_nil _
  | I_cons
  | I_size
  | I_empty_set _
  | I_empty_map (_, _)
  | I_empty_big_map (_, _)
  | I_mem
  | I_get
  | I_update
  | I_exec
  | I_apply
  | I_failwith
  | I_cast _
  | I_rename
  | I_concat
  | I_slice
  | I_pack
  | I_unpack _
  | I_add
  | I_sub
  | I_mul
  | I_ediv
  | I_abs
  | I_isnat
  | I_int
  | I_neg
  | I_lsl
  | I_lsr
  | I_or
  | I_and
  | I_xor
  | I_not
  | I_compare
  | I_eq
  | I_neq
  | I_lt
  | I_gt
  | I_le
  | I_ge
  | I_self
  | I_contract _
  | I_transfer_tokens
  | I_set_delegate
  | I_create_account
  | I_implicit_account
  | I_now
  | I_amount
  | I_balance
  | I_check_signature
  | I_blake2b
  | I_sha256
  | I_sha512
  | I_hash_key
  | I_steps_to_quota
  | I_source
  | I_sender
  | I_address
  | I_chain_id
  | I_unpair ->
    1
  (* Non-Standard Instruction : Introduced to resolve parsing issue *)
  | I_noop
  (* Non-Standard Instruction : Special Comment : MicSE user defined safety property *)
  | I_micse_check _ ->
    0
  (* Standard Macros *)
  | M_plain _
  | M_num (_, _)
  | M_code (_, _)
  | M_code2 (_, _, _) ->
    Stdlib.failwith "count_inst_pgm_i: macro instruction should be substituted"
