(*****************************************************************************)
(*****************************************************************************)
(* Verification Language                                                     *)
(*****************************************************************************)
(*****************************************************************************)

type typ = Adt.typ
and data = Adt.data
and operation = Adt.operation

type var = Cfg.ident
and exp = Cfg.expr

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
  | VE_none of typ
  | VE_uni_cont of v_uni_cont * v_exp * typ
  | VE_bin_cont of v_bin_cont * v_exp * v_exp * typ
  | VE_list of v_exp list * typ
  | VE_var of var * typ
  | VE_read of v_exp * v_exp (* (i, A) : A[i] in RHS *)
  | VE_write of v_exp * v_exp * v_exp (* (i, v, A) : A[i] = v *)
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
  | VE_add      | VE_sub      | VE_mul      | VE_ediv     | VE_div
  | VE_mod      | VE_lsl      | VE_lsr      | VE_and      | VE_or
  | VE_xor      | VE_cmp      | VE_cons     | VE_concat   | VE_exec
  | VE_append

and v_tri_op =
  | VE_slice    | VE_check_signature

and v_operation =
  | VE_transaction
  | VE_origination
  | VE_delegation


let create_exp_var : var -> typ -> v_exp
=fun v t -> VE_var (v, t)

let create_formula_not : v_formula -> v_formula
=fun f -> VF_not f

let create_formula_and : v_formula -> v_formula -> v_formula
=fun f1 f2 -> VF_and (f1, f2)

let create_formula_or : v_formula -> v_formula -> v_formula
=fun f1 f2 -> VF_or (f1, f2)

let create_formula_uni_rel : v_uni_rel -> v_exp -> v_formula
=fun r e -> VF_uni_rel (r, e)

let create_formula_is_true : v_exp -> v_formula
=fun e -> create_formula_uni_rel VF_is_true e

let create_formula_is_none : v_exp -> v_formula
=fun e -> create_formula_uni_rel VF_is_none e

let create_formula_is_left : v_exp -> v_formula
=fun e -> create_formula_uni_rel VF_is_left e

let create_formula_is_cons : v_exp -> v_formula
=fun e -> create_formula_uni_rel VF_is_cons e

let create_formula_eq : v_exp -> v_exp -> v_formula
=fun e1 e2 -> VF_eq (e1, e2)

let create_formula_imply : v_formula -> v_formula -> v_formula
=fun f1 f2 -> VF_imply (f1, f2)

let create_formula_iff : v_formula -> v_formula -> v_formula
=fun f1 f2 -> VF_iff (f1, f2)

let string_of_formula : v_formula -> string
=fun f -> begin
  ""
end
