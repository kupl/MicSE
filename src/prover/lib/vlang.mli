(*****************************************************************************)
(*****************************************************************************)
(* Verification Language                                                     *)
(*****************************************************************************)
(*****************************************************************************)

type typ = Pre.Lib.Adt.typ
and data = Pre.Lib.Adt.data
and operation = Pre.Lib.Cfg.operation

type var = Pre.Lib.Cfg.ident
and exp = Pre.Lib.Cfg.expr

type t = v_formula

and v_formula =
  | VF_true  | VF_false
  | VF_not of v_formula
  | VF_and of v_formula list
  | VF_or of v_formula list
  | VF_uni_rel of v_uni_rel * v_obj
  | VF_bin_rel of v_bin_rel * v_obj * v_obj
  | VF_imply of v_formula * v_formula
  | VF_iff of v_formula * v_formula
  | VF_forall of v_obj list * v_formula
  (* Customized formula *)
  | VF_sigma_equal of v_obj * v_obj (* VF_sigma_equal a b = VF_bin_rel VF_eq Sigma(a) b *)

and v_uni_rel =
  | VF_is_true  | VF_is_none  | VF_is_left  | VF_is_cons

and v_bin_rel =
  | VF_eq       | VF_neq      | VF_lt       | VF_le
  | VF_gt       | VF_ge

and v_obj = {
  exp: v_exp;
  typ: typ;
}

and v_exp =
  | VE_int of Z.t
  | VE_string of string
  | VE_bool of v_formula
  | VE_unit
  | VE_none
  | VE_uni_cont of v_uni_cont * v_obj
  | VE_bin_cont of v_bin_cont * v_obj * v_obj
  | VE_list of v_obj list
  | VE_var of var
  | VE_nul_op of v_nul_op
  | VE_uni_op of v_uni_op * v_obj
  | VE_bin_op of v_bin_op * v_obj * v_obj
  | VE_ter_op of v_ter_op * v_obj * v_obj * v_obj
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
  | VE_geq      | VE_cast     | VE_pack     | VE_unpack   | VE_list_concat
  | VE_contract | VE_account  | VE_blake2b  | VE_sha256   | VE_sha512
  | VE_hash_key | VE_address  | VE_un_opt   | VE_un_left  | VE_un_right
  | VE_hd       | VE_tl       | VE_size     | VE_isnat    | VE_to_int
  
and v_bin_op =
  | VE_add      | VE_sub      | VE_mul      | VE_ediv     | VE_div
  | VE_mod      | VE_lsl      | VE_lsr      | VE_and      | VE_or
  | VE_xor      | VE_cmp      | VE_cons     | VE_concat   | VE_exec
  | VE_append   | VE_get      | VE_mem

and v_ter_op =
  | VE_slice    | VE_check_signature        | VE_update

and v_operation =
  | VE_transaction
  | VE_origination
  | VE_delegation


(*****************************************************************************)
(*****************************************************************************)
(* Verification Formula                                                      *)
(*****************************************************************************)
(*****************************************************************************)

val create_formula_true : v_formula

val create_formula_false : v_formula

val create_formula_not : v_formula -> v_formula

val create_formula_and : v_formula list -> v_formula

val create_formula_or : v_formula list -> v_formula

val create_formula_uni_rel : rel:v_uni_rel -> o1:v_obj -> v_formula

val create_formula_is_true : v_obj -> v_formula

val create_formula_is_none : v_obj -> v_formula

val create_formula_is_some : v_obj -> v_formula

val create_formula_is_left : v_obj -> v_formula

val create_formula_is_right : v_obj -> v_formula

val create_formula_is_cons : v_obj -> v_formula

val create_formula_is_nil : v_obj -> v_formula

val create_formula_bin_rel : rel:v_bin_rel -> o1:v_obj -> o2:v_obj -> v_formula

val create_formula_eq : v_obj -> v_obj -> v_formula

val create_formula_neq : v_obj -> v_obj -> v_formula

val create_formula_lt : v_obj -> v_obj -> v_formula

val create_formula_le : v_obj -> v_obj -> v_formula

val create_formula_gt : v_obj -> v_obj -> v_formula

val create_formula_ge : v_obj -> v_obj -> v_formula

val create_formula_imply : v_formula -> v_formula -> v_formula

val create_formula_iff : v_formula -> v_formula -> v_formula

val create_formula_forall : bnd:v_obj list -> formula:v_formula -> v_formula

(* Customized formula *)

val create_formula_no_overflow : v_obj -> v_formula

val create_formula_no_underflow : v_obj -> v_formula

val create_formula_sigma_equal : map:v_obj -> mutez:v_obj -> v_formula

(*****************************************************************************)
(*****************************************************************************)
(* Verification object                                                       *)
(*****************************************************************************)
(*****************************************************************************)

val create_obj_of_exp : exp:v_exp -> typ:typ -> v_obj


(*****************************************************************************)
(*****************************************************************************)
(* Verification Expression                                                   *)
(*****************************************************************************)
(*****************************************************************************)

val create_exp_int : Z.t -> v_exp

val create_exp_int_of_small_int : int -> v_exp

val create_exp_int_of_string : string -> v_exp

val create_exp_string : string -> v_exp

val create_exp_bool : v_formula -> v_exp

val create_exp_bool_true : v_exp

val create_exp_bool_false : v_exp

val create_exp_unit : v_exp

val create_exp_none : v_exp

val create_exp_uni_cont : cont:v_uni_cont -> o1:v_obj -> v_exp

val create_exp_uni_cont_left : v_obj -> v_exp

val create_exp_uni_cont_right : v_obj -> v_exp

val create_exp_uni_cont_some : v_obj -> v_exp

val create_exp_bin_cont : cont:v_bin_cont -> o1:v_obj -> o2:v_obj -> v_exp

val create_exp_bin_cont_pair : v_obj -> v_obj -> v_exp

val create_exp_bin_cont_elt : v_obj -> v_obj -> v_exp

val create_exp_list : v_obj list -> v_exp

val create_exp_var : var -> v_exp

val create_exp_nul_op : op:v_nul_op -> v_exp

val create_exp_nul_op_self : v_exp

val create_exp_nul_op_now : v_exp

val create_exp_nul_op_amount : v_exp

val create_exp_nul_op_balance : v_exp

val create_exp_nul_op_steps_to_quota : v_exp

val create_exp_nul_op_source : v_exp

val create_exp_nul_op_sender : v_exp

val create_exp_nul_op_chain_id : v_exp

val create_exp_uni_op : op:v_uni_op -> o1:v_obj -> v_exp

val create_exp_uni_op_car : v_obj -> v_exp

val create_exp_uni_op_cdr : v_obj -> v_exp

val create_exp_uni_op_abs : v_obj -> v_exp

val create_exp_uni_op_neg : v_obj -> v_exp

val create_exp_uni_op_not : v_obj -> v_exp

val create_exp_uni_op_eq : v_obj -> v_exp

val create_exp_uni_op_neq : v_obj -> v_exp

val create_exp_uni_op_lt : v_obj -> v_exp

val create_exp_uni_op_gt : v_obj -> v_exp

val create_exp_uni_op_leq : v_obj -> v_exp

val create_exp_uni_op_geq : v_obj -> v_exp

val create_exp_uni_op_cast : v_obj -> v_exp

val create_exp_uni_op_concat : v_obj -> v_exp

val create_exp_uni_op_pack : v_obj -> v_exp

val create_exp_uni_op_unpack : v_obj -> v_exp

val create_exp_uni_op_contract : v_obj -> v_exp

val create_exp_uni_op_account : v_obj -> v_exp

val create_exp_uni_op_blake2b : v_obj -> v_exp

val create_exp_uni_op_sha256 : v_obj -> v_exp

val create_exp_uni_op_sha512 : v_obj -> v_exp

val create_exp_uni_op_hash_key : v_obj -> v_exp

val create_exp_uni_op_address : v_obj -> v_exp

val create_exp_uni_op_un_opt : v_obj -> v_exp

val create_exp_uni_op_un_left : v_obj -> v_exp

val create_exp_uni_op_un_right : v_obj -> v_exp

val create_exp_uni_op_hd : v_obj -> v_exp

val create_exp_uni_op_tl : v_obj -> v_exp

val create_exp_uni_op_size : v_obj -> v_exp

val create_exp_uni_op_isnat : v_obj -> v_exp

val create_exp_uni_op_int : v_obj -> v_exp

val create_exp_bin_op : op:v_bin_op -> o1:v_obj -> o2:v_obj -> v_exp

val create_exp_bin_op_add : v_obj -> v_obj -> v_exp

val create_exp_bin_op_sub : v_obj -> v_obj -> v_exp

val create_exp_bin_op_mul : v_obj -> v_obj -> v_exp

val create_exp_bin_op_ediv : v_obj -> v_obj -> v_exp

val create_exp_bin_op_div : v_obj -> v_obj -> v_exp

val create_exp_bin_op_mod : v_obj -> v_obj -> v_exp

val create_exp_bin_op_lsl : v_obj -> v_obj -> v_exp

val create_exp_bin_op_lsr : v_obj -> v_obj -> v_exp

val create_exp_bin_op_and : v_obj -> v_obj -> v_exp

val create_exp_bin_op_or : v_obj -> v_obj -> v_exp

val create_exp_bin_op_xor : v_obj -> v_obj -> v_exp

val create_exp_bin_op_cmp : v_obj -> v_obj -> v_exp

val create_exp_bin_op_cons : v_obj -> v_obj -> v_exp

val create_exp_bin_op_concat : v_obj -> v_obj -> v_exp

val create_exp_bin_op_exec : v_obj -> v_obj -> v_exp

val create_exp_bin_op_append : v_obj -> v_obj -> v_exp

val create_exp_bin_op_get : v_obj -> v_obj -> v_exp

val create_exp_bin_op_mem : v_obj -> v_obj -> v_exp

val create_exp_ter_op : op:v_ter_op -> o1:v_obj -> o2:v_obj -> o3:v_obj -> v_exp

val create_exp_ter_op_slice : v_obj -> v_obj -> v_obj -> v_exp

val create_exp_ter_op_check_signature : v_obj -> v_obj -> v_obj -> v_exp

val create_exp_ter_op_update : v_obj -> v_obj -> v_obj -> v_exp

val create_exp_lambda : v_exp

val create_exp_operation : v_operation -> v_exp

val create_exp_operation_transaction : v_exp

val create_exp_operation_origination : v_exp

val create_exp_operation_delegation : v_exp


(*****************************************************************************)
(*****************************************************************************)
(* Stringify Modules                                                         *)
(*****************************************************************************)
(*****************************************************************************)

val string_of_formula : v_formula -> string

val string_of_obj : v_obj -> string

val string_of_exp : v_exp -> string

val string_of_vlang : t -> string