open ProverLib

(************************************************)
(************************************************)

let rec convert : Bp.t -> Vlang.t
=fun bp -> begin
  let f, g = ((Option.get bp.pre.formula), (Option.get bp.post.formula)) in
  let f', g' = Core.List.fold_left bp.body ~init:(f, g) ~f:sp in
  let vc = Vlang.create_formula_imply f' g' in
  vc
end

and sp : (Vlang.t * Vlang.t) -> Bp.inst -> (Vlang.t * Vlang.t)
=fun (f, g) s -> begin
  match s with
  | BI_assume c -> begin
      let f' = Vlang.create_formula_and c f in
      (f', g)
    end
  | BI_assign (v, e) -> begin
      let v' = create_rename_var v in
      let f' = create_rewrite_formula v v' f in
      let e' = create_rewrite_exp v v' (create_convert_exp e) in
      let f'' = Vlang.create_formula_and f' (Vlang.create_formula_eq (Vlang.create_exp_var v) e') in
      (f'', g)
    end
  | BI_skip -> (f, g)
end

and create_rename_var : Vlang.var -> Vlang.var
=fun v -> "#" ^ v

and create_rewrite_formula : Vlang.var -> Vlang.var -> Vlang.v_formula -> Vlang.v_formula
=fun v v' f -> begin
  let nested_rewrite vf = create_rewrite_formula v v' vf in
  let exp_rewrite ve = create_rewrite_exp v v' ve in
  match f with
  | VF_true | VF_false -> f
  | VF_not vf -> Vlang.create_formula_not (nested_rewrite vf)
  | VF_and (vf1, vf2) -> Vlang.create_formula_and (nested_rewrite vf1) (nested_rewrite vf2)
  | VF_or (vf1, vf2) -> Vlang.create_formula_or (nested_rewrite vf1) (nested_rewrite vf2)
  | VF_uni_rel (vr, ve) -> Vlang.create_formula_uni_rel vr (exp_rewrite ve)
  | VF_bin_rel (vr, ve1, ve2) -> Vlang.create_formula_bin_rel vr (exp_rewrite ve1) (exp_rewrite ve2)
  | VF_imply (vf1, vf2) -> Vlang.create_formula_imply (nested_rewrite vf1) (nested_rewrite vf2)
  | VF_iff (vf1, vf2) -> Vlang.create_formula_iff (nested_rewrite vf1) (nested_rewrite vf2)
end

and create_rewrite_exp : Vlang.var -> Vlang.var -> Vlang.v_exp -> Vlang.v_exp
=fun v v' e -> begin
  let nested_rewrite ve = create_rewrite_exp v v' ve in
  let formula_rewrite vf = create_rewrite_formula v v' vf in
  match e with
  | VE_int _ | VE_string _ | VE_unit | VE_none
  | VE_uni_cont (_, _) | VE_bin_cont (_, _, _) | VE_list _
  | VE_nul_op _ | VE_lambda | VE_operation _ -> e
  | VE_bool f -> VE_bool (formula_rewrite f)
  | VE_var vv -> begin
      if vv = v then Vlang.create_exp_var v'
      else e
    end
  | VE_read (ve1, ve2) -> VE_read ((nested_rewrite ve1), (nested_rewrite ve2))
  | VE_write (ve1, ve2, ve3) -> VE_write ((nested_rewrite ve1), (nested_rewrite ve2), (nested_rewrite ve3))
  | VE_uni_op (vo, ve) -> VE_uni_op (vo, (nested_rewrite ve))
  | VE_bin_op (vo, ve1, ve2) -> VE_bin_op (vo, (nested_rewrite ve1), (nested_rewrite ve2))
  | VE_ter_op (vo, ve1, ve2, ve3) -> VE_ter_op (vo, (nested_rewrite ve1), (nested_rewrite ve2), (nested_rewrite ve3))
end

and create_convert_data : Vlang.data -> Vlang.v_exp
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

and create_convert_exp : Vlang.exp -> Vlang.v_exp
=fun e -> begin
  match e with
  | E_push (d, _) -> create_convert_data d
  | E_car v -> VE_uni_op (VE_car, (Vlang.create_exp_var v))
  | E_cdr v -> VE_uni_op (VE_cdr, (Vlang.create_exp_var v))
  | E_abs v -> VE_uni_op (VE_abs, (Vlang.create_exp_var v))
  | E_neg v -> VE_uni_op (VE_neg, (Vlang.create_exp_var v))
  | E_not v -> VE_uni_op (VE_not, (Vlang.create_exp_var v))
  | E_add (v1, v2) -> VE_bin_op (VE_add, (Vlang.create_exp_var v1), (Vlang.create_exp_var v2))
  | E_sub (v1, v2) -> VE_bin_op (VE_sub, (Vlang.create_exp_var v1), (Vlang.create_exp_var v2))
  | E_mul (v1, v2) -> VE_bin_op (VE_mul, (Vlang.create_exp_var v1), (Vlang.create_exp_var v2))
  | E_div (v1, v2) -> VE_bin_op (VE_div, (Vlang.create_exp_var v1), (Vlang.create_exp_var v2))
  | E_mod (v1, v2) -> VE_bin_op (VE_mod, (Vlang.create_exp_var v1), (Vlang.create_exp_var v2))
  | E_shiftL (v1, v2) -> VE_bin_op (VE_lsl, (Vlang.create_exp_var v1), (Vlang.create_exp_var v2))
  | E_shiftR (v1, v2) -> VE_bin_op (VE_lsr, (Vlang.create_exp_var v1), (Vlang.create_exp_var v2))
  | E_and (v1, v2) -> VE_bin_op (VE_and, (Vlang.create_exp_var v1), (Vlang.create_exp_var v2))
  | E_or (v1, v2) -> VE_bin_op (VE_or, (Vlang.create_exp_var v1), (Vlang.create_exp_var v2))
  | E_xor (v1, v2) -> VE_bin_op (VE_xor, (Vlang.create_exp_var v1), (Vlang.create_exp_var v2))
  | E_eq v -> VE_uni_op (VE_eq, (Vlang.create_exp_var v))
  | E_neq v -> VE_uni_op (VE_neq, (Vlang.create_exp_var v))
  | E_lt v -> VE_uni_op (VE_lt, (Vlang.create_exp_var v))
  | E_gt v -> VE_uni_op (VE_gt, (Vlang.create_exp_var v))
  | E_leq v -> VE_uni_op (VE_leq, (Vlang.create_exp_var v))
  | E_geq v -> VE_uni_op (VE_geq, (Vlang.create_exp_var v))
  | E_compare (v1, v2) -> VE_bin_op (VE_cmp, (Vlang.create_exp_var v1), (Vlang.create_exp_var v2))
  | E_cons (v1, v2) -> VE_bin_op (VE_cons, (Vlang.create_exp_var v1), (Vlang.create_exp_var v2))
  | E_operation o -> begin
      match o with
      | O_create_contract (_, _, _, _) -> VE_operation (VE_origination)
      | O_transfer_tokens (_, _, _)-> VE_operation (VE_transaction)
      | O_set_delegate _ -> VE_operation (VE_delegation)
      | O_create_account (_, _, _, _) -> VE_operation (VE_origination)
    end
  | E_unit -> VE_unit
  | E_pair (v1, v2) -> VE_bin_cont (VE_pair, (Vlang.create_exp_var v1), (Vlang.create_exp_var v2))
  | E_left (v, _) -> VE_uni_cont (VE_left, (Vlang.create_exp_var v))
  | E_right (v, _) -> VE_uni_cont (VE_right, (Vlang.create_exp_var v))
  | E_some v -> VE_uni_cont (VE_some, (Vlang.create_exp_var v))
  | E_none _ -> VE_none
  | E_mem (v1, v2) -> VE_bool (VF_not (VF_uni_rel (VF_is_none, VE_read ((Vlang.create_exp_var v1), (Vlang.create_exp_var v2)))))
  | E_get (v1, v2) -> VE_read ((Vlang.create_exp_var v1), (Vlang.create_exp_var v2))
  | E_update (v1, v2, v3) -> VE_write ((Vlang.create_exp_var v1), (Vlang.create_exp_var v2), (Vlang.create_exp_var v3))
  | E_cast v -> VE_uni_op (VE_cast, (Vlang.create_exp_var v))
  | E_concat (v1, v2) -> VE_bin_op (VE_concat, (Vlang.create_exp_var v1), (Vlang.create_exp_var v2))
  | E_concat_list v -> VE_uni_op (VE_concat, (Vlang.create_exp_var v))
  | E_slice (v1, v2, v3) -> VE_ter_op (VE_slice, (Vlang.create_exp_var v1), (Vlang.create_exp_var v2), (Vlang.create_exp_var v3))
  | E_pack v -> VE_uni_op (VE_pack, (Vlang.create_exp_var v))
  | E_unpack (_, v) -> VE_uni_op (VE_unpack, (Vlang.create_exp_var v))
  | E_self -> VE_nul_op (VE_self)
  | E_contract_of_address v -> VE_uni_op (VE_contract, (Vlang.create_exp_var v))
  | E_implicit_account v -> VE_uni_op (VE_account, (Vlang.create_exp_var v))
  | E_now -> VE_nul_op (VE_now)
  | E_amount -> VE_nul_op (VE_amount)
  | E_balance -> VE_nul_op (VE_balance)
  | E_check_signature (v1, v2, v3) -> VE_ter_op (VE_check_signature, (Vlang.create_exp_var v1), (Vlang.create_exp_var v2), (Vlang.create_exp_var v3))
  | E_blake2b v -> VE_uni_op (VE_blake2b, (Vlang.create_exp_var v))
  | E_sha256 v -> VE_uni_op (VE_sha256, (Vlang.create_exp_var v))
  | E_sha512 v -> VE_uni_op (VE_sha512, (Vlang.create_exp_var v))
  | E_hash_key v -> VE_uni_op (VE_hash_key, (Vlang.create_exp_var v))
  | E_steps_to_quota -> VE_nul_op (VE_steps_to_quota)
  | E_source -> VE_nul_op (VE_source)
  | E_sender -> VE_nul_op (VE_sender)
  | E_address_of_contract v -> VE_uni_op (VE_address, (Vlang.create_exp_var v))
  | E_create_contract_address _ -> VE_operation (VE_origination)
  | E_unlift_option v -> VE_uni_op (VE_un_opt, (Vlang.create_exp_var v))
  | E_unlift_or v -> VE_uni_op (VE_un_or, (Vlang.create_exp_var v))
  | E_hd v -> VE_uni_op (VE_hd, (Vlang.create_exp_var v))
  | E_tl v -> VE_uni_op (VE_tl, (Vlang.create_exp_var v))
  | E_size v -> VE_uni_op (VE_size, (Vlang.create_exp_var v))
  | E_isnat v -> VE_uni_op (VE_isnat, (Vlang.create_exp_var v))
  | E_int_of_nat v -> VE_uni_op (VE_int, (Vlang.create_exp_var v))
  | E_chain_id -> VE_nul_op (VE_chain_id)
  | E_create_account_address _ -> VE_operation (VE_origination)
  | E_lambda (_, _, _) -> VE_lambda
  | E_exec (v1, v2) -> VE_bin_op (VE_exec, (Vlang.create_exp_var v1), (Vlang.create_exp_var v2))
  | E_dup v -> (Vlang.create_exp_var v)
  | E_nil _ -> VE_list []
  | E_empty_set _ -> VE_list []
  | E_empty_map _ -> VE_list []
  | E_empty_big_map _ -> VE_list []
  | E_append (v1, v2) -> VE_bin_op (VE_append, (Vlang.create_exp_var v1), (Vlang.create_exp_var v2))
  | E_special_nil_list -> VE_list []
  | E_phi (_, _) -> raise (Failure "Phi Function")
end