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
  | VF_bin_rel of v_bin_rel * v_exp * v_exp
  | VF_imply of v_formula * v_formula
  | VF_iff of v_formula * v_formula

and v_uni_rel =
  | VF_is_true  | VF_is_none  | VF_is_left  | VF_is_cons

and v_bin_rel =
  | VF_eq       | VF_neq
  | VF_gt       | VF_lt
  | VF_ge       | VF_le

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

let create_rename_var : var -> var
=fun v -> "#" ^ v

let create_exp_var : var -> v_exp
=fun v -> VE_var (v)

let create_formula_not : v_formula -> v_formula
=fun f -> VF_not f

let create_formula_and : v_formula -> v_formula -> v_formula
=fun f1 f2 -> VF_and (f1, f2)

let create_formula_is_true : v_exp -> v_formula
=fun e -> VF_uni_rel (VF_is_true, e)

let create_formula_is_none : v_exp -> v_formula
=fun e -> VF_uni_rel (VF_is_none, e)

let create_formula_is_left : v_exp -> v_formula
=fun e -> VF_uni_rel (VF_is_left, e)

let create_formula_is_cons : v_exp -> v_formula
=fun e -> VF_uni_rel (VF_is_cons, e)

let rec create_rewrite_formula : var -> var -> v_formula -> v_formula
=fun v v' f -> begin
  let nested_rewrite vf = create_rewrite_formula v v' vf in
  let exp_rewrite ve = create_rewrite_exp v v' ve in
  match f with
  | VF_true | VF_false -> f
  | VF_not vf -> VF_not (nested_rewrite vf)
  | VF_and (vf1, vf2) -> VF_and ((nested_rewrite vf1), (nested_rewrite vf2))
  | VF_or (vf1, vf2) -> VF_or ((nested_rewrite vf1), (nested_rewrite vf2))
  | VF_uni_rel (vr, ve) -> VF_uni_rel (vr, (exp_rewrite ve))
  | VF_bin_rel (vr, ve1, ve2) -> VF_bin_rel (vr, (exp_rewrite ve1), (exp_rewrite ve2))
  | VF_imply (vf1, vf2) -> VF_imply ((nested_rewrite vf1), (nested_rewrite vf2))
  | VF_iff (vf1, vf2) -> VF_iff ((nested_rewrite vf1), (nested_rewrite vf2))
end

and create_rewrite_exp : var -> var -> v_exp -> v_exp
=fun v v' e -> begin
  let nested_rewrite ve = create_rewrite_exp v v' ve in
  let formula_rewrite vf = create_rewrite_formula v v' vf in
  match e with
  | VE_int _ | VE_string _ | VE_bool _ | VE_unit | VE_none
  | VE_uni_cont (_, _) | VE_bin_cont (_, _, _) | VE_list _
  | VE_nul_op _ | VE_lambda -> e
  | VE_var vv -> begin
      if vv = v then create_exp_var v'
      else e
    end
  | VE_read (ve1, ve2) -> VE_read ((nested_rewrite ve1), (nested_rewrite ve2))
  | VE_write (ve1, ve2, ve3) -> VE_write ((nested_rewrite ve1), (nested_rewrite ve2), (nested_rewrite ve3))
  | VE_uni_op (vo, ve) -> VE_uni_op (vo, (nested_rewrite ve))
  | VE_bin_op (vo, ve1, ve2) -> VE_bin_op (vo, (nested_rewrite ve1), (nested_rewrite ve2))
  | VE_ter_op (vo, ve1, ve2, ve3) -> VE_ter_op (vo, (nested_rewrite ve1), (nested_rewrite ve2), (nested_rewrite ve3))
end

let rec create_convert_data : data -> v_exp
=fun d -> begin
  match d.d with
  | D_int x -> VE_int x 
  | D_string x -> VE_string x
  | D_bytes x -> VE_string x
  | D_unit -> VE_unit
  | D_bool b -> begin
      if b then VE_bool (VF_true)
      else VE_bool (VF_false)
    end
  | D_pair (c1, c2) -> VE_bin_cont (VE_pair, (create_convert_data c1), (create_convert_data c2))
  | D_left c -> VE_uni_cont (VE_left, (create_convert_data c))
  | D_right c -> VE_uni_cont (VE_right, (create_convert_data c))
  | D_some c -> VE_uni_cont (VE_some, (create_convert_data c))
  | D_none -> VE_none
  | D_elt (c1, c2) -> VE_bin_cont (VE_elt, (create_convert_data c1), (create_convert_data c2))
  | D_list cl -> VE_list (Core.List.map cl ~f:(fun c -> (create_convert_data c)))
end

let create_convert_exp : exp -> v_exp
=fun e -> begin
  match e with
  | E_push (d, _) -> create_convert_data d
  | E_car v -> VE_uni_op (VE_car, (create_exp_var v))
  | E_cdr v -> VE_uni_op (VE_cdr, (create_exp_var v))
  | E_abs v -> VE_uni_op (VE_abs, (create_exp_var v))
  | E_neg v -> VE_uni_op (VE_neg, (create_exp_var v))
  | E_not v -> VE_uni_op (VE_not, (create_exp_var v))
  | E_add (v1, v2) -> VE_bin_op (VE_add, (create_exp_var v1), (create_exp_var v2))
  | E_sub (v1, v2) -> VE_bin_op (VE_sub, (create_exp_var v1), (create_exp_var v2))
  | E_mul (v1, v2) -> VE_bin_op (VE_mul, (create_exp_var v1), (create_exp_var v2))
  | E_div (v1, v2) -> VE_bin_op (VE_div, (create_exp_var v1), (create_exp_var v2))
  | E_mod (v1, v2) -> VE_bin_op (VE_mod, (create_exp_var v1), (create_exp_var v2))
  | E_shiftL (v1, v2) -> VE_bin_op (VE_lsl, (create_exp_var v1), (create_exp_var v2))
  | E_shiftR (v1, v2) -> VE_bin_op (VE_lsr, (create_exp_var v1), (create_exp_var v2))
  | E_and (v1, v2) -> VE_bin_op (VE_and, (create_exp_var v1), (create_exp_var v2))
  | E_or (v1, v2) -> VE_bin_op (VE_or, (create_exp_var v1), (create_exp_var v2))
  | E_xor (v1, v2) -> VE_bin_op (VE_xor, (create_exp_var v1), (create_exp_var v2))
  | E_eq v -> VE_uni_op (VE_eq, (create_exp_var v))
  | E_neq v -> VE_uni_op (VE_neq, (create_exp_var v))
  | E_lt v -> VE_uni_op (VE_lt, (create_exp_var v))
  | E_gt v -> VE_uni_op (VE_gt, (create_exp_var v))
  | E_leq v -> VE_uni_op (VE_leq, (create_exp_var v))
  | E_geq v -> VE_uni_op (VE_geq, (create_exp_var v))
  | E_compare (v1, v2) -> VE_bin_op (VE_cmp, (create_exp_var v1), (create_exp_var v2))
  | E_cons (v1, v2) -> VE_bin_op (VE_cons, (create_exp_var v1), (create_exp_var v2))
  | E_operation o -> begin
      match o with
      | O_create_contract (_, _, _, _) -> VE_operation (VE_origination)
      | O_transfer_tokens (_, _, _)-> VE_operation (VE_transaction)
      | O_set_delegate _ -> VE_operation (VE_delegation)
      | O_create_account (_, _, _, _) -> VE_operation (VE_origination)
    end
  | E_unit -> VE_unit
  | E_pair (v1, v2) -> VE_bin_cont (VE_pair, (create_exp_var v1), (create_exp_var v2))
  | E_left (v, _) -> VE_uni_cont (VE_left, (create_exp_var v))
  | E_right (v, _) -> VE_uni_cont (VE_right, (create_exp_var v))
  | E_some v -> VE_uni_cont (VE_some, (create_exp_var v))
  | E_none _ -> VE_none
  | E_mem (v1, v2) -> VE_bool (VF_not (VF_uni_rel (VF_is_none, VE_read ((create_exp_var v1), (create_exp_var v2)))))
  | E_get (v1, v2) -> VE_read ((create_exp_var v1), (create_exp_var v2))
  | E_update (v1, v2, v3) -> VE_write ((create_exp_var v1), (create_exp_var v2), (create_exp_var v3))
  | E_cast v -> VE_uni_op (VE_cast, (create_exp_var v))
  | E_concat (v1, v2) -> VE_bin_op (VE_concat, (create_exp_var v1), (create_exp_var v2))
  | E_concat_list v -> VE_uni_op (VE_concat, (create_exp_var v))
  | E_slice (v1, v2, v3) -> VE_ter_op (VE_slice, (create_exp_var v1), (create_exp_var v2), (create_exp_var v3))
  | E_pack v -> VE_uni_op (VE_pack, (create_exp_var v))
  | E_unpack (_, v) -> VE_uni_op (VE_unpack, (create_exp_var v))
  | E_self -> VE_nul_op (VE_self)
  | E_contract_of_address v -> VE_uni_op (VE_contract, (create_exp_var v))
  | E_implicit_account v -> VE_uni_op (VE_account, (create_exp_var v))
  | E_now -> VE_nul_op (VE_now)
  | E_amount -> VE_nul_op (VE_amount)
  | E_balance -> VE_nul_op (VE_balance)
  | E_check_signature (v1, v2, v3) -> VE_ter_op (VE_check_signature, (create_exp_var v1), (create_exp_var v2), (create_exp_var v3))
  | E_blake2b v -> VE_uni_op (VE_blake2b, (create_exp_var v))
  | E_sha256 v -> VE_uni_op (VE_sha256, (create_exp_var v))
  | E_sha512 v -> VE_uni_op (VE_sha512, (create_exp_var v))
  | E_hash_key v -> VE_uni_op (VE_hash_key, (create_exp_var v))
  | E_steps_to_quota -> VE_nul_op (VE_steps_to_quota)
  | E_source -> VE_nul_op (VE_source)
  | E_sender -> VE_nul_op (VE_sender)
  | E_address_of_contract v -> VE_uni_op (VE_address, (create_exp_var v))
  | E_create_contract_address _ -> VE_operation (VE_origination)
  | E_unlift_option v -> VE_uni_op (VE_un_opt, (create_exp_var v))
  | E_unlift_or v -> VE_uni_op (VE_un_or, (create_exp_var v))
  | E_hd v -> VE_uni_op (VE_hd, (create_exp_var v))
  | E_tl v -> VE_uni_op (VE_tl, (create_exp_var v))
  | E_size v -> VE_uni_op (VE_size, (create_exp_var v))
  | E_isnat v -> VE_uni_op (VE_isnat, (create_exp_var v))
  | E_int_of_nat v -> VE_uni_op (VE_int, (create_exp_var v))
  | E_chain_id -> VE_nul_op (VE_chain_id)
  | E_create_account_address _ -> VE_operation (VE_origination)
  | E_lambda (_, _, _) -> VE_lambda
  | E_exec (v1, v2) -> VE_bin_op (VE_exec, (create_exp_var v1), (create_exp_var v2))
  | E_dup v -> (create_exp_var v)
  | E_nil _ -> VE_list []
  | E_empty_set _ -> VE_list []
  | E_empty_map _ -> VE_list []
  | E_empty_big_map _ -> VE_list []
  | E_append (v1, v2) -> VE_bin_op (VE_append, (create_exp_var v1), (create_exp_var v2))
  | E_special_nil_list -> VE_list []
  | E_phi (v1, v2) -> raise (Failure "Phi Function")
end

let create_convert_exp : exp -> v_exp
=fun e -> VE_unit

let string_of_formula : v_formula -> string
=fun f -> begin
  ""
end
