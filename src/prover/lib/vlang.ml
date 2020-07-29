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
  | VF_and of v_formula list
  | VF_or of v_formula list
  | VF_uni_rel of v_uni_rel * v_exp
  | VF_bin_rel of v_bin_rel * v_exp * v_exp
  | VF_imply of v_formula * v_formula
  | VF_iff of v_formula * v_formula

and v_uni_rel =
  | VF_is_true  | VF_is_none  | VF_is_left  | VF_is_cons

and v_bin_rel =
  | VF_eq       | VF_neq      | VF_lt       | VF_le
  | VF_gt       | VF_ge

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
  | VE_nul_op of v_nul_op * typ
  | VE_uni_op of v_uni_op * v_exp * typ
  | VE_bin_op of v_bin_op * v_exp * v_exp * typ
  | VE_ter_op of v_ter_op * v_exp * v_exp * v_exp * typ
  | VE_lambda of typ
  | VE_operation of v_operation * typ

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

and v_ter_op =
  | VE_slice    | VE_check_signature

and v_operation =
  | VE_transaction
  | VE_origination
  | VE_delegation


(*****************************************************************************)
(*****************************************************************************)
(* Verification Formula                                                      *)
(*****************************************************************************)
(*****************************************************************************)

let create_formula_true : v_formula
=VF_true

let create_formula_false : v_formula
=VF_false

let create_formula_not : v_formula -> v_formula
=fun f -> VF_not f

let create_formula_and : v_formula list -> v_formula
=fun fl -> VF_and fl

let create_formula_or : v_formula list -> v_formula
=fun fl -> VF_or fl

let create_formula_uni_rel : v_uni_rel -> v_exp -> v_formula
=fun r e -> VF_uni_rel (r, e)

let create_formula_is_true : v_exp -> v_formula
=fun e -> create_formula_uni_rel VF_is_true e

let create_formula_is_none : v_exp -> v_formula
=fun e -> create_formula_uni_rel VF_is_none e

let create_formula_is_some : v_exp -> v_formula
=fun e -> create_formula_not (create_formula_uni_rel VF_is_none e)

let create_formula_is_left : v_exp -> v_formula
=fun e -> create_formula_uni_rel VF_is_left e

let create_formula_is_right : v_exp -> v_formula
=fun e -> create_formula_not (create_formula_uni_rel VF_is_left e)

let create_formula_is_cons : v_exp -> v_formula
=fun e -> create_formula_uni_rel VF_is_cons e

let create_formula_is_nil : v_exp -> v_formula
=fun e -> create_formula_not (create_formula_uni_rel VF_is_cons e)

let create_formula_bin_rel : v_bin_rel -> v_exp -> v_exp -> v_formula
=fun r e1 e2 -> VF_bin_rel (r, e1, e2)

let create_formula_eq : v_exp -> v_exp -> v_formula
=fun e1 e2 -> create_formula_bin_rel VF_eq e1 e2

let create_formula_neq : v_exp -> v_exp -> v_formula
=fun e1 e2 -> create_formula_bin_rel VF_neq e1 e2

let create_formula_lt : v_exp -> v_exp -> v_formula
=fun e1 e2 -> create_formula_bin_rel VF_lt e1 e2

let create_formula_le : v_exp -> v_exp -> v_formula
=fun e1 e2 -> create_formula_bin_rel VF_le e1 e2

let create_formula_gt : v_exp -> v_exp -> v_formula
=fun e1 e2 -> create_formula_bin_rel VF_gt e1 e2

let create_formula_ge : v_exp -> v_exp -> v_formula
=fun e1 e2 -> create_formula_bin_rel VF_ge e1 e2

let create_formula_imply : v_formula -> v_formula -> v_formula
=fun f1 f2 -> VF_imply (f1, f2)

let create_formula_iff : v_formula -> v_formula -> v_formula
=fun f1 f2 -> VF_iff (f1, f2)

let string_of_formula : v_formula -> string
=fun f -> begin
  ""
end


(*****************************************************************************)
(*****************************************************************************)
(* Verification Expression                                                   *)
(*****************************************************************************)
(*****************************************************************************)

let create_exp_int : Z.t -> v_exp
=fun n -> VE_int n

let create_exp_int_of_small_int : int -> v_exp
=fun i -> create_exp_int (Z.of_int i)

let create_exp_int_of_string : string -> v_exp
=fun s -> create_exp_int (Z.of_string s)

let create_exp_string : string -> v_exp
=fun s -> VE_string s

let create_exp_bool : v_formula -> v_exp
=fun f -> VE_bool f

let create_exp_bool_true : v_exp
=create_exp_bool create_formula_true

let create_exp_bool_false : v_exp
=create_exp_bool create_formula_false

let create_exp_unit : v_exp
=VE_unit

let create_exp_none : typ -> v_exp
=fun t -> VE_none t

let create_exp_uni_cont : v_uni_cont -> v_exp -> typ -> v_exp
=fun vuc e1 t -> VE_uni_cont (vuc, e1, t)

let create_exp_uni_cont_left : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_cont VE_left e1 t

let create_exp_uni_cont_right : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_cont VE_right e1 t

let create_exp_uni_cont_some : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_cont VE_some e1 t

let create_exp_bin_cont : v_bin_cont -> v_exp -> v_exp -> typ -> v_exp
=fun vbc e1 e2 t -> VE_bin_cont (vbc, e1, e2, t)

let create_exp_bin_cont_pair : v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 t -> create_exp_bin_cont VE_pair e1 e2 t

let create_exp_bin_cont_elt : v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 t -> create_exp_bin_cont VE_elt e1 e2 t

let create_exp_list : v_exp list -> typ -> v_exp
=fun el t -> VE_list (el, t)

let create_exp_var : var -> typ -> v_exp
=fun v t -> VE_var (v, t)

let create_exp_read : v_exp -> v_exp -> v_exp
=fun ie ae -> VE_read (ie, ae)

let create_exp_write : v_exp -> v_exp -> v_exp -> v_exp
=fun ie ve ae -> VE_write (ie, ve, ae)

let create_exp_nul_op : v_nul_op -> typ -> v_exp
=fun vno t -> VE_nul_op (vno, t)

let create_exp_nul_op_self : typ -> v_exp
=fun t -> create_exp_nul_op VE_self t

let create_exp_nul_op_now : typ -> v_exp
=fun t -> create_exp_nul_op VE_now t

let create_exp_nul_op_amount : typ -> v_exp
=fun t -> create_exp_nul_op VE_amount t

let create_exp_nul_op_balance : typ -> v_exp
=fun t -> create_exp_nul_op VE_balance t

let create_exp_nul_op_steps_to_quota : typ -> v_exp
=fun t -> create_exp_nul_op VE_steps_to_quota t

let create_exp_nul_op_source : typ -> v_exp
=fun t -> create_exp_nul_op VE_source t

let create_exp_nul_op_sender : typ -> v_exp
=fun t -> create_exp_nul_op VE_sender t

let create_exp_nul_op_chain_id : typ -> v_exp
=fun t -> create_exp_nul_op VE_chain_id t

let create_exp_uni_op : v_uni_op -> v_exp -> typ -> v_exp
=fun vuo e1 t -> VE_uni_op (vuo, e1, t)

let create_exp_uni_op_car : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_car e1 t

let create_exp_uni_op_cdr : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_cdr e1 t

let create_exp_uni_op_abs : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_abs e1 t

let create_exp_uni_op_neg : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_neg e1 t

let create_exp_uni_op_not : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_not e1 t

let create_exp_uni_op_eq : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_eq e1 t

let create_exp_uni_op_neq : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_neq e1 t

let create_exp_uni_op_lt : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_lt e1 t

let create_exp_uni_op_gt : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_gt e1 t

let create_exp_uni_op_leq : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_leq e1 t

let create_exp_uni_op_geq : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_geq e1 t

let create_exp_uni_op_cast : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_cast e1 t

let create_exp_uni_op_concat : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_concat e1 t

let create_exp_uni_op_pack : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_pack e1 t

let create_exp_uni_op_unpack : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_unpack e1 t

let create_exp_uni_op_contract : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_contract e1 t

let create_exp_uni_op_account : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_account e1 t

let create_exp_uni_op_blake2b : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_blake2b e1 t

let create_exp_uni_op_sha256 : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_sha256 e1 t

let create_exp_uni_op_sha512 : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_sha512 e1 t

let create_exp_uni_op_hash_key : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_hash_key e1 t

let create_exp_uni_op_address : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_address e1 t

let create_exp_uni_op_un_opt : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_un_opt e1 t

let create_exp_uni_op_un_or : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_un_or e1 t

let create_exp_uni_op_hd : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_hd e1 t

let create_exp_uni_op_tl : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_tl e1 t

let create_exp_uni_op_size : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_size e1 t

let create_exp_uni_op_isnat : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_isnat e1 t

let create_exp_uni_op_int : v_exp -> typ -> v_exp
=fun e1 t -> create_exp_uni_op VE_int e1 t

let create_exp_bin_op : v_bin_op -> v_exp -> v_exp -> typ -> v_exp
=fun vbo e1 e2 t -> VE_bin_op (vbo, e1, e2, t)

let create_exp_bin_op_add : v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 t -> create_exp_bin_op VE_add e1 e2 t

let create_exp_bin_op_sub : v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 t -> create_exp_bin_op VE_sub e1 e2 t

let create_exp_bin_op_mul : v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 t -> create_exp_bin_op VE_mul e1 e2 t

let create_exp_bin_op_ediv : v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 t -> create_exp_bin_op VE_ediv e1 e2 t

let create_exp_bin_op_div : v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 t -> create_exp_bin_op VE_div e1 e2 t

let create_exp_bin_op_mod : v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 t -> create_exp_bin_op VE_mod e1 e2 t

let create_exp_bin_op_lsl : v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 t -> create_exp_bin_op VE_lsl e1 e2 t

let create_exp_bin_op_lsr : v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 t -> create_exp_bin_op VE_lsr e1 e2 t

let create_exp_bin_op_and : v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 t -> create_exp_bin_op VE_and e1 e2 t

let create_exp_bin_op_or : v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 t -> create_exp_bin_op VE_or e1 e2 t

let create_exp_bin_op_xor : v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 t -> create_exp_bin_op VE_xor e1 e2 t

let create_exp_bin_op_cmp : v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 t -> create_exp_bin_op VE_cmp e1 e2 t

let create_exp_bin_op_cons : v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 t -> create_exp_bin_op VE_cons e1 e2 t

let create_exp_bin_op_concat : v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 t -> create_exp_bin_op VE_concat e1 e2 t

let create_exp_bin_op_exec : v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 t -> create_exp_bin_op VE_exec e1 e2 t

let create_exp_bin_op_append : v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 t -> create_exp_bin_op VE_append e1 e2 t

let create_exp_ter_op : v_ter_op -> v_exp -> v_exp -> v_exp -> typ -> v_exp
=fun vto e1 e2 e3 t -> VE_ter_op (vto, e1, e2, e3, t)

let create_exp_ter_op_slice : v_exp -> v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 e3 t -> create_exp_ter_op VE_slice e1 e2 e3 t

let create_exp_ter_op_check_signature : v_exp -> v_exp -> v_exp -> typ -> v_exp
=fun e1 e2 e3 t -> create_exp_ter_op VE_check_signature e1 e2 e3 t

let create_exp_lambda : typ -> v_exp
=fun t -> VE_lambda t

let create_exp_operation : v_operation -> typ -> v_exp
=fun vop t -> VE_operation (vop, t)

let create_exp_operation_transaction : typ -> v_exp
=fun t -> create_exp_operation VE_transaction t

let create_exp_operation_origination : typ -> v_exp
=fun t -> create_exp_operation VE_origination t

let create_exp_operation_delegation : typ -> v_exp
=fun t -> create_exp_operation VE_delegation t