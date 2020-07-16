(*****************************************************************************)
(*****************************************************************************)
(* Verification Language                                                     *)
(*****************************************************************************)
(*****************************************************************************)

type typ = Adt.typ
  (*
    type typ =
    | T_key
    | T_unit
    | T_signature
    | T_option of typ t
    | T_list of typ t
    | T_set of typ t
    | T_operation
    | T_contract of typ t
    | T_pair of typ t * typ t
    | T_or of typ t * typ t
    | T_lambda of typ t * typ t
    | T_map of typ t * typ t
    | T_big_map of typ t * typ t
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
  *)

and data = Adt.data
  (*
    and data =
    | D_int of Z.t
    | D_string of string
    | D_bytes of string
    | D_unit
    | D_bool of bool
    | D_pair of data t * data t
    | D_left of data t
    | D_right of data t
    | D_some of data t
    | D_none
    | D_elt of data t * data t
    | D_list of data t list
  *)

and operation = Adt.operation
  (*
    type operation =
    | O_create_contract of Michelson.Adt.program * string * string * string
    | O_transfer_tokens of string * string * string
    | O_set_delegate of string
    | O_create_account of string * string * string * string
  *)

type var = Cfg.ident
  (*
    type indent = string
  *)

and exp = Cfg.expr
  (*
    type expr =
    | E_push of data * typ
    | E_car of string
    | E_cdr of string
    | E_abs of string
    | E_neg of string
    | E_not of string
    | E_add of string * string
    | E_sub of string * string
    | E_mul of string * string
    | E_div of string * string
    | E_mod of string * string
    | E_shiftL of string * string
    | E_shiftR of string * string
    | E_and of string * string
    | E_or of string * string
    | E_xor of string * string
    | E_eq of string
    | E_neq of string
    | E_lt of string
    | E_gt of string
    | E_leq of string
    | E_geq of string
    | E_compare of string * string
    | E_cons of string * string
    | E_operation of operation
    | E_unit
    | E_pair of string * string
    | E_left of string * typ
    | E_right of string * typ
    | E_some of string
    | E_none of typ
    | E_mem of string * string
    | E_get of string * string
    | E_update of string * string * string
    | E_cast of string
    | E_concat of string * string
    | E_concat_list of string
    | E_slice of string * string * string
    | E_pack of string
    | E_unpack of typ * string
    | E_self
    | E_contract_of_address of string
    | E_implicit_account of string
    | E_now
    | E_amount
    | E_balance
    | E_check_signature of string * string * string
    | E_blake2b of string
    | E_sha256 of string
    | E_sha512 of string
    | E_hash_key of string
    | E_steps_to_quota
    | E_source
    | E_sender
    | E_address_of_contract of string
    | E_create_contract_address of operation
    | E_unlift_option of string
    | E_unlift_or of string
    | E_hd of string
    | E_tl of string
    | E_size of string
    | E_isnat of string
    | E_int_of_nat of string
    | E_chain_id
    | E_create_account_address of operation
    | E_lambda of typ * typ * func
    | E_exec of string * string
    | E_dup of string
    | E_nil of typ
    | E_empty_set of typ
    | E_empty_map of typ * typ
    | E_empty_big_map of typ * typ
    | E_append of string * string
    | E_special_nil_list
    | E_phi of string * string
  *)

type t = v_formula

and v_formula =
  | VF_true  | VF_false
  | VF_not of v_formula
  | VF_and of v_formula * v_formula
  | VF_or of v_formula * v_formula
  | VF_uni_rel of v_uni_rel * v_exp
  | VF_eq of v_exp * v_exp
  | VF_imply of v_formula * v_formula
  | VF_iff of v_formula * v_formula

and v_uni_rel =
  | VF_is_true  | VF_is_none  | VF_is_left  | VF_is_cons

and v_exp =
  | VE_int of Z.t
  | VE_string of string
  | VE_bool of v_formula
  | VE_unit
  | VE_none
  | VE_uni_cont of v_uni_cont * v_exp
  | VE_bin_cont of v_bin_cont * v_exp * v_exp
  | VE_list of v_exp list
  | VE_var of var
  | VE_read of v_exp * v_exp (* A[i] in RHS *)
  | VE_write of v_exp * v_exp * v_exp (* A[i] = v *)
  | VE_nul_op of v_nul_op
  | VE_uni_op of v_uni_op * v_exp
  | VE_bin_op of v_bin_op * v_exp * v_exp
  | VE_ter_op of v_tri_op * v_exp * v_exp * v_exp
  | VE_lambda
  | VE_operation of v_operation

  and v_uni_cont =
  | VE_left     | VE_right    | VE_some

and v_bin_cont =
  | VE_pair     | VE_elt

and v_nul_op =
  | VE_self     | VE_now      | VE_amount   | VE_balance  | VE_steps_to_quota
  | VE_source   | VE_sender   | VE_chain_id

and v_uni_op =
  | VE_car      | VE_cdr      | VE_abs      | VE_neg      | VE_not
  | VE_eq       | VE_neq      | VE_lt       | VE_gt       | VE_leq
  | VE_geq      | VE_cast     | VE_concat   | VE_pack     | VE_unpack
  | VE_contract | VE_account  | VE_blake2b  | VE_sha256   | VE_sha512
  | VE_hash_key | VE_address  | VE_un_opt   | VE_un_or    | VE_hd
  | VE_tl       | VE_size     | VE_isnat    | VE_int
  
and v_bin_op =
  | VE_add      | VE_sub      | VE_mul      | VE_div      | VE_mod
  | VE_lsl      | VE_lsr      | VE_and      | VE_or       | VE_xor
  | VE_cmp      | VE_cons     | VE_concat   | VE_exec     | VE_append

and v_tri_op =
  | VE_slice    | VE_check_signature

and v_operation =
  | VE_transaction
  | VE_origination
  | VE_delegation

val create_exp_var : var -> v_exp

val create_formula_not : v_formula -> v_formula

val create_formula_and : v_formula -> v_formula -> v_formula

val create_formula_or : v_formula -> v_formula -> v_formula

val create_formula_uni_rel : v_uni_rel -> v_exp -> v_formula

val create_formula_is_true : v_exp -> v_formula

val create_formula_is_none : v_exp -> v_formula

val create_formula_is_left : v_exp -> v_formula

val create_formula_is_cons : v_exp -> v_formula

val create_formula_eq : v_exp -> v_exp -> v_formula

val create_formula_imply : v_formula -> v_formula -> v_formula

val create_formula_iff : v_formula -> v_formula -> v_formula

val string_of_formula : v_formula -> string
