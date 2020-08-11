(*****************************************************************************)
(*****************************************************************************)
(* Location in Code                                                          *)
(*****************************************************************************)
(*****************************************************************************)

type pos = { col : int; lin : int; }

type loc = Unknown | Pos of pos * pos


(*****************************************************************************)
(*****************************************************************************)
(* Michelson Code                                                            *)
(*****************************************************************************)
(*****************************************************************************)

type annot = 
  | A_typ of string (* :type_annot   *)
  | A_var of string (* @var_annot    *)
  | A_fld of string (* %field_annot *)

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
  | I_seq           of inst t * inst t
  | I_drop
  | I_drop_n        of Z.t
  | I_dup
  | I_swap
  | I_dig           of Z.t
  | I_dug           of Z.t
  | I_push          of typ t * data t
  | I_some
  | I_none          of typ t
  | I_unit
  | I_if_none       of inst t * inst t
  | I_pair
  | I_car
  | I_cdr
  | I_left          of typ t
  | I_right         of typ t
  | I_if_left       of inst t * inst t
  | I_nil           of typ t
  | I_cons
  | I_if_cons       of inst t * inst t
  | I_size
  | I_empty_set     of typ t
  | I_empty_map     of typ t * typ t
  | I_empty_big_map of typ t * typ t
  | I_map           of inst t
  | I_iter          of inst t
  | I_mem
  | I_get
  | I_update
  | I_if            of inst t * inst t
  | I_loop          of inst t
  | I_loop_left     of inst t
  | I_lambda        of typ t * typ t * inst t
  | I_exec
  | I_dip           of inst t
  | I_dip_n         of Z.t * inst t
  | I_failwith
  | I_cast          of typ t
  | I_rename
  | I_concat
  | I_slice
  | I_pack
  | I_unpack        of typ t
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
  | I_contract      of typ t
  | I_transfer_tokens
  | I_set_delegate
  | I_create_account
  | I_create_contract of program
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
  | M_plain of string             (* Macros with no following argument. e.g. FAIL *)
  | M_num   of string * Z.t       (* Macros with one number argument. e.g. DUP n *)
  | M_code  of string * (inst t)  (* Macros with one code argument. e.g. MAP_CAR *)
  | M_code2 of string * (inst t) * (inst t) (* Macros with two code arguments. e.g. IFCMPEQ *)
  (* Non-Standard Instruction : Introduced to resolve parsing issue *)
  | I_noop
  (* Non-Standard Instruction : Special Comment : MicSE user defined safety property *)
  | I_micse_check of inst t   (* WARNING: I_check instruction is not in Michelson standard. It is for MicSE formatted-comment *)

and data =
  | D_int     of Z.t
  | D_string  of string
  | D_bytes   of string
  | D_unit
  | D_bool    of bool
  | D_pair    of data t * data t
  | D_left    of data t
  | D_right   of data t
  | D_some    of data t
  | D_none
  | D_list    of data t list
  | D_elt     of data t * data t
  | D_lambda  of inst t

and 'a t = { 
  pos     : loc; 
  ann     : annot list; 
  d       : 'a; 
}

and program = { param : typ t; storage : typ t; code : inst t; }


(*****************************************************************************)
(*****************************************************************************)
(* Utility Functions                                                         *)
(*****************************************************************************)
(*****************************************************************************)

let get_d       : 'a t -> 'a  = fun x -> x.d

let gen_t       :                      'a -> 'a t   = fun x     -> {pos=Unknown; ann=[]; d=x;}
let gen_t_a     :        annot list -> 'a -> 'a t   = fun a x   -> {pos=Unknown; ann=a; d=x;}     
let gen_t_p     : loc               -> 'a -> 'a t   = fun p x   -> {pos=p; ann=[]; d=x;}          
let gen_t_pa    : loc -> annot list -> 'a -> 'a t   = fun p a x -> {pos=p; ann=a; d=x;}           

let copy_info   : 'a t -> 'b   -> 'b t  = fun x y -> {pos=x.pos; ann=x.ann; d=y;}
let copy_info_t : 'a t -> 'b t -> 'b t  = fun x y -> {pos=x.pos; ann=x.ann; d=y.d;}

let get_min_pos : pos -> pos -> pos = fun {col=c1; lin=l1;} {col=c2; lin=l2;} -> begin
  match (l1 < l2), (l1 = l2), (c1 < c2) with
  | true, _, _          -> {col=c1; lin=l1;}
  | false, true, true   -> {col=c1; lin=l1;}
  | false, true, false  -> {col=c2; lin=l2;}
  | false, false, _     -> {col=c2; lin=l2;}
end
let get_max_pos : pos -> pos -> pos = fun {col=c1; lin=l1;} {col=c2; lin=l2;} -> begin
  match (l1 < l2), (l1 = l2), (c1 < c2) with
  | true, _, _          -> {col=c2; lin=l2;}
  | false, true, true   -> {col=c2; lin=l2;}
  | false, true, false  -> {col=c1; lin=l1;}
  | false, false, _     -> {col=c1; lin=l1;}
end
let join_pos : loc -> loc -> loc
=fun loc_1 loc_2 -> begin
  match loc_1, loc_2 with
  | Pos (p11, p12), Pos (p21, p22)  -> Pos ((get_min_pos p11 p21), (get_max_pos p12 p22))
  | Pos _, Unknown                  -> loc_1
  | Unknown, Pos _                  -> loc_2
  | Unknown, Unknown                -> Unknown
end

let join_instt_seq : annot list -> inst t -> inst t -> inst t
=fun ann i1 i2 -> {pos=join_pos i1.pos i2.pos; ann=ann; d=I_seq(i1,i2);}

let empty_instt = {pos=Unknown; ann=[]; d=I_noop;}

let gen_instseq : (annot list) -> inst list -> (inst t)
=fun annlst instlst -> let i_t = List.fold_left (fun acc i -> join_instt_seq [] acc {pos=Unknown; ann=[]; d=i;}) empty_instt instlst in {i_t with ann=annlst;}
let gen_insttseq : (annot list) -> (inst t) list -> (inst t)
=fun annlst insttlst -> let i_t = List.fold_left (fun acc i -> join_instt_seq [] acc i) empty_instt insttlst in {i_t with ann=annlst;}  (* [A; B; C] -> I_seq ( I_seq (a, b), c ) *)


(*****************************************************************************)
(*****************************************************************************)
(* Standard Macros                                                           *)
(*****************************************************************************)
(*****************************************************************************)

exception Not_Macro of string
let nm_fail s = raise (Not_Macro s)

let str_fst s n : string = String.sub s 0 n                     (* the first-n characters in the given string-s. *)
let str_lst s n : string = String.sub s (String.length s - n) n   (* the last-n characters in the given string-s. *)
let str_mid s n1 n2 : string = String.sub s n1 (String.length s - (n1+n2))  (* exclude first-n1 & last-n2 characters in the given string-s. *)

let m_if : inst -> inst -> inst = fun i1 i2 -> I_if (gen_t i1, gen_t i2)


(*****************************************************************************)
(* Macros - Plain Macros                                                     *)
(*****************************************************************************)

(*  PLAIN MACRO LIST
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
*)

let m_fail : inst t = gen_instseq [] [I_unit; I_failwith;]
let m_fail_d : inst = get_d m_fail

let m_cmpop : inst -> inst t = fun i -> gen_instseq [] [I_compare; i;]

let m_assert : inst t = gen_t (m_if I_noop m_fail_d)
let m_assertop : inst -> inst t = fun i -> gen_insttseq [] [gen_t i; m_assert;]
let m_assertcmpop : inst -> inst t = fun i -> gen_insttseq [] [m_cmpop i; m_assert;]

let parse_duup : string -> bool * (inst t option)
=fun s -> begin
  if str_fst s 1 = "D" && str_lst s 1 = "P" && (String.length s > 2)
  then (
    let (b, n) : bool * int = 
      Core.String.foldi (str_mid s 1 1) ~init:(true, 0) 
        ~f:(fun _ (acc_b, acc_n) c -> if acc_b && c = 'U' then (true, acc_n+1) else (false, 0)) in
    let iopt : inst t option = 
      match (b, n) with
      | false, _ -> None
      | true, 1 -> gen_t I_dup |> Option.some
      | true, 2 -> gen_instseq [] [I_dip (gen_t I_dup); I_swap;] |> Option.some
      | true, n -> gen_instseq [] [I_dip_n (Z.of_int (n-1), gen_t I_dup); I_dig (Z.of_int n);] |> Option.some in
    (b, iopt)
  )
  else (false, None)
end

let parse_ad : string -> bool * bool list
=fun s -> begin
  (* IMPLICIT MAPPING: 'A' -> true, 'D' -> false *)
  (* This function considers that the given string-s has one or more characters. *)
  (* If there are any other character except 'A' and 'D' in the given string-s, it will return (false, []) *)
  if String.length s > 0 then (
    let (b, bl_rev) : bool * bool list =
      Core.String.foldi s ~init:(true, [])
        ~f:(fun _ (acc_b, acc_bl_rev) c -> if acc_b then (match c with | 'A' -> (true, true::acc_bl_rev) | 'D' -> (true, false::acc_bl_rev) | _ -> (false, [])) else (false, [])) in
    (b, List.rev bl_rev)
  )
  else (false, [])
end

let parse_cadr : string -> bool * (inst t option)
=fun s -> begin
  if str_fst s 1 = "C" && str_lst s 1 = "R" && (String.length s > 2)
  then (
    let (b, bl) = parse_ad (str_mid s 1 1) in
    let il : inst list = List.map (fun x -> if x then I_car else I_cdr) bl in
    if b then (true, Some (gen_instseq [] il)) else (false, None)
  )
  else (false, None)
end

let parse_set_cadr : string -> bool * (inst t option)
=fun s -> begin
  if str_fst s 5 = "SET_C" && str_lst s 1 = "R" && (String.length s > 6)
  then (
    let (b, bl) = parse_ad (str_mid s 5 1) in
    let rec f : bool list -> inst t
    =fun bl -> begin
      match bl with
      | [] -> gen_t I_noop
      | [true] -> gen_instseq [] [I_cdr; I_swap; I_pair;]
      | [false] -> gen_instseq [] [I_car; I_pair;]
      | true :: tl -> let in_dip : inst t = gen_insttseq [] [gen_t I_car; f tl;] in  gen_instseq [] [I_dup; I_dip in_dip; I_cdr; I_swap; I_pair;]
      | false :: tl -> let in_dip : inst t = gen_insttseq [] [gen_t I_cdr; f tl;] in gen_instseq [] [I_dup; I_dip in_dip; I_car; I_pair;]
    end in
    if b then (true, Some (f bl)) else (false, None)
  )
  else (false, None)
end

type pair_leaf = PT_A | PT_I
type pair_tree = PT_P of pair_tree * pair_tree | PT_L of pair_leaf
let rec construct_pair_tree : string -> (int * pair_tree) -> pair_leaf -> (int * pair_tree)
=fun s (i, pt) lf -> begin
  (* This function considers that the given string-s has only 'P', 'A', and 'I' characters. 'R' should be omitted before invocation. *)
  if (String.length s <= i) then (i, pt)
  else (
    match (String.get s i), lf with
    | 'P', _ -> 
      let (j, lpt) = construct_pair_tree s (i+1, pt) PT_A in
      let (k, rpt) = construct_pair_tree s (j, pt) PT_I in
      (k, PT_P (lpt, rpt))
    | 'A', PT_A -> (i+1, PT_L PT_A)
    | 'I', PT_I -> (i+1, PT_L PT_I)
    | 'A', _ | 'I', _ -> nm_fail "construct_pair_tree : Expected character but inappropriate situation. "
    | _ -> nm_fail "construct_pair_tree : Unexpected character"
  )
end

let rec decode_pair_tree : pair_tree -> inst list = begin function
  | PT_P (PT_L PT_A, PT_L PT_I) -> [I_pair;]
  | PT_P (PT_L PT_A, PT_P (y1, y2)) -> [I_dip (gen_instseq [] (decode_pair_tree (PT_P (y1,y2)))); I_pair;]
  | PT_P (PT_P (x1,x2), PT_L PT_I) -> (decode_pair_tree (PT_P (x1,x2))) @ [I_pair;]
  | PT_P (PT_P (x1,x2), PT_P (y1,y2)) -> (decode_pair_tree (PT_P (x1,x2))) @ [I_dip (gen_instseq [] (decode_pair_tree (PT_P (y1,y2)))); I_pair;]
  | _ -> nm_fail "decode_pair_tree : Unexpected Tree Form."
end
let parse_pair : string -> bool * (inst t option)
=fun s -> begin
  if (str_lst s 1 = "R" && String.length s > 3)
  then ( 
    try
      let itopt : inst t option = 
        construct_pair_tree (str_mid s 0 1) (0, PT_L PT_A) PT_A
        |> Stdlib.snd |> decode_pair_tree |> gen_instseq [] |> Option.some
      in (true, itopt)
    with | Not_Macro _ -> (false, None)
  )
  else (false, None)
end

let rec decode_unpair_tree : pair_tree -> inst list = 
  let unpairseq : inst list = [I_dup; I_car; I_dip (gen_t I_cdr);] in
  begin function
    | PT_P (PT_L PT_A, PT_L PT_I) -> unpairseq
    | PT_P (PT_L PT_A, PT_P (y1,y2)) -> unpairseq @ [I_dip (gen_instseq [] (decode_unpair_tree (PT_P (y1,y2))));]
    | PT_P (PT_P (x1,x2), PT_L PT_I) -> unpairseq @ (decode_unpair_tree (PT_P (x1,x2)))
    | PT_P (PT_P (x1,x2), PT_P (y1,y2)) -> unpairseq @ [I_dip (gen_instseq [] (decode_unpair_tree (PT_P (y1,y2))));] @ (decode_unpair_tree (PT_P (x1,x2)))
    | _ -> nm_fail "decode_pair_tree : Unexpected Tree Form."
  end
let parse_unpair : string -> bool * (inst t option)
=fun s -> begin
  if (str_fst s 2 = "UN" && str_lst s 1 = "R" && String.length s > 5)
  then (
    try
      let itopt : inst t option = 
        construct_pair_tree (str_mid s 2 1) (0, PT_L PT_A) PT_A
        |> Stdlib.snd |> decode_unpair_tree |> gen_instseq [] |> Option.some
      in (true, itopt)
    with | Not_Macro _ -> (false, None)
  ) 
  else (false, None)
end


let resolve_plain_macro : inst t -> string -> inst t
=fun i s -> begin
  let ann = i.ann in
  (* Search Fixed-name macros first *)
  match s with
  | "FAIL" -> m_fail
  | "ASSERT" -> m_assert
  | "ASSERT_NONE"  -> gen_t (I_if_none (gen_t I_noop, m_fail))
  | "ASSERT_SOME"  -> gen_t (I_if_none (m_fail, gen_t_a ann I_rename))
  | "ASSERT_LEFT"  -> gen_t (I_if_left (gen_t_a ann I_rename, m_fail))
  | "ASSERT_RIGHT" -> gen_t (I_if_left (m_fail, gen_t_a ann I_rename))
  | "CMPEQ"  -> m_cmpop I_eq   | "ASSERTEQ"  -> m_assertop I_eq   | "ASSERTCMPEQ"  -> m_assertcmpop I_eq  
  | "CMPNEQ" -> m_cmpop I_neq  | "ASSERTNEQ" -> m_assertop I_neq  | "ASSERTCMPNEQ" -> m_assertcmpop I_neq
  | "CMPLT"  -> m_cmpop I_lt   | "ASSERTLT"  -> m_assertop I_lt   | "ASSERTCMPLT"  -> m_assertcmpop I_lt  
  | "CMPLE"  -> m_cmpop I_le   | "ASSERTLE"  -> m_assertop I_le   | "ASSERTCMPLE"  -> m_assertcmpop I_le  
  | "CMPGT"  -> m_cmpop I_gt   | "ASSERTGT"  -> m_assertop I_gt   | "ASSERTCMPGT"  -> m_assertcmpop I_gt  
  | "CMPGE"  -> m_cmpop I_ge   | "ASSERTGE"  -> m_assertop I_ge   | "ASSERTCMPGE"  -> m_assertcmpop I_ge  
    (* Other Cases : DUUP, CADR, SET_CADR, PAIR, UNPAIR *)
  | _ -> (
      let (duup_bool, duup_it) = parse_duup s in
      if duup_bool then Option.get duup_it else
      let (cadr_bool, cadr_it) = parse_cadr s in
      if cadr_bool then Option.get cadr_it else
      let (set_cadr_bool, set_cadr_it) = parse_set_cadr s in
      if set_cadr_bool then Option.get set_cadr_it else
      let (pair_bool, pair_it) = parse_pair s in
      if pair_bool then Option.get pair_it else
      let (unpair_bool, unpair_it) = parse_unpair s in
      if unpair_bool then Option.get unpair_it
      else nm_fail "resolve_plain_macro : every match failed"
    )
end


(*****************************************************************************)
(* Macros - Macro with Number                                                *)
(*****************************************************************************)

(*  NUMBER MACRO LIST
    - DUP
*)

let construct_duup : int -> string = fun n -> "D" ^ (String.make n 'U') ^ "P"

let resolve_num_macro : string -> Z.t -> inst t
=fun s zn -> begin
  match s with
  | "DUP" -> construct_duup (Z.to_int zn) |> parse_duup |> Stdlib.snd |> Option.get
  | _ -> nm_fail "resolve_num_macro : every match failed"
end


(*****************************************************************************)
(* Macros - Macro with a code                                                *)
(*****************************************************************************)

(*  CODE MACRO LIST
    - MAP_CADR
*)

let parse_map_cadr : string -> inst t -> bool * (inst t option)
=fun s c -> begin
  if str_fst s 5 = "MAP_C" && str_lst s 1 = "R" && (String.length s > 6)
  then (
    let (b, bl) = parse_ad (str_mid s 5 1) in
    let rec f : bool list -> inst t
    =fun bl -> begin
      match bl with
      | [] -> gen_t I_noop
      | [true] -> let in_dip : inst t = gen_insttseq [] [gen_t I_car; c] in gen_instseq [] [I_dup; I_cdr; I_dip in_dip; I_swap; I_pair;]
      | [false] -> let (hd, tl) = gen_instseq [] [I_dup; I_cdr;], gen_instseq [] [I_swap; I_car; I_pair;] in gen_insttseq [] [hd; c; tl]
      | true :: tl -> let in_dip : inst t = gen_insttseq [] [gen_t I_car; f tl;] in  gen_instseq [] [I_dup; I_dip in_dip; I_cdr; I_swap; I_pair;]
      | false :: tl -> let in_dip : inst t = gen_insttseq [] [gen_t I_cdr; f tl;] in gen_instseq [] [I_dup; I_dip in_dip; I_car; I_pair;]
    end in
    if b then (true, Some (f bl)) else (false, None)
  )
  else (false, None)
end

let resolve_code_macro : string -> inst t -> inst t
=fun s c -> begin
  match s with
  | _ -> (
    let (map_cadr_bool, map_cadr_it) = parse_map_cadr s c in
    if map_cadr_bool then Option.get map_cadr_it
    else nm_fail "resolve_code_macro : every match failed"
  )
end


(*****************************************************************************)
(* Macros - Macro with two codes                                             *)
(*****************************************************************************)

(*  CODE2 MACRO LIST
    - IFOP
    - IFCMPOP
*)

let m_ifop : inst -> inst t -> inst t -> inst t = fun op i1 i2 -> gen_instseq [] [op; I_if (i1, i2);]
let m_ifcmpop : inst -> inst t -> inst t -> inst t = fun op i1 i2 -> gen_insttseq [] [gen_t I_compare; m_ifop op i1 i2;]

let resolve_code2_macro : string -> inst t -> inst t -> inst t
=fun s c1 c2 -> begin
  match s with
  | "IFEQ"  -> m_ifop I_eq  c1 c2  | "IFCMPEQ"  -> m_ifcmpop I_eq  c1 c2
  | "IFNEQ" -> m_ifop I_neq c1 c2  | "IFCMPNEQ" -> m_ifcmpop I_neq c1 c2
  | "IFLT"  -> m_ifop I_lt  c1 c2  | "IFCMPLT"  -> m_ifcmpop I_lt  c1 c2
  | "IFLE"  -> m_ifop I_le  c1 c2  | "IFCMPLE"  -> m_ifcmpop I_le  c1 c2
  | "IFGT"  -> m_ifop I_gt  c1 c2  | "IFCMPGT"  -> m_ifcmpop I_gt  c1 c2
  | "IFGE"  -> m_ifop I_ge  c1 c2  | "IFCMPGE"  -> m_ifcmpop I_ge  c1 c2
  | _ -> nm_fail "resolve_code2_macro : every match failed"
end


(*****************************************************************************)
(* Standard Macros - Overall                                                 *)
(*****************************************************************************)

let subst_standard_macro : inst t -> inst t
=fun i -> begin
  match get_d i with
  | M_plain s     -> copy_info_t i (resolve_plain_macro i s)
  | M_num (s, n)  -> copy_info_t i (resolve_num_macro s n)
  | M_code (s, c) -> copy_info_t i (resolve_code_macro s c)
  | M_code2 (s, c1, c2) -> copy_info_t i (resolve_code2_macro s c1 c2)
  | _ -> nm_fail "subst_standard_macro : Given argument is not standard macro instruction."
end

let rec subst_standard_macro_all : inst t -> inst t =
  let scma = subst_standard_macro_all in
  fun c ->
  (match get_d c with
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
  | I_create_contract {param=t1; storage=t2; code=i;} -> I_create_contract {param=t1; storage=t2; code=(scma i);} |> copy_info c
  | I_drop | I_drop_n _ | I_dup | I_swap | I_dig _ | I_dug _
  | I_some | I_none _ | I_unit
  | I_pair | I_car | I_cdr | I_left _ | I_right _
  | I_nil _ | I_cons
  | I_size | I_empty_set _ | I_empty_map _ | I_empty_big_map _
  | I_mem | I_get | I_update
  | I_exec
  | I_failwith | I_cast _ | I_rename | I_concat | I_slice
  | I_pack | I_unpack _ | I_add | I_sub | I_mul
  | I_ediv | I_abs | I_isnat | I_int | I_neg
  | I_lsl | I_lsr | I_or | I_and | I_xor
  | I_not | I_compare | I_eq | I_neq | I_lt
  | I_gt | I_le | I_ge | I_self | I_contract _
  | I_transfer_tokens | I_set_delegate | I_create_account
  | I_implicit_account | I_now | I_amount | I_balance | I_check_signature
  | I_blake2b | I_sha256 | I_sha512 | I_hash_key | I_steps_to_quota
  | I_source | I_sender | I_address | I_chain_id | I_unpair                   -> c
  (* Standard Macros *)
  | M_plain _
  | M_num   _
  | M_code  _
  | M_code2 _ -> subst_standard_macro c
  (* Non-Standard Instruction : Introduced to resolve parsing issue *)
  | I_noop -> c
  (* Non-Standard Instruction : Special Comment : MicSE user defined safety property *)
  | I_micse_check i -> I_micse_check (scma i) |> copy_info c   (* WARNING: I_check instruction is not in Michelson standard. It is for MicSE formatted-comment *)
  )

and subst_standard_macro_all_data : data t -> data t =
  fun d ->
  (match get_d d with
  | D_lambda i -> D_lambda (subst_standard_macro_all i) |> copy_info d
  | _ -> d
  )
