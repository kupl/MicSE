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
  | BI_assign (v, e, t) -> begin
      let v' = create_rename_var v in
      let f' = create_rewrite_formula v v' f in
      let e' = create_rewrite_exp v v' (create_convert_exp e t) in
      let f'' = Vlang.create_formula_and f' (Vlang.create_formula_eq (Vlang.create_exp_var v t) e') in
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
  | VF_eq (e1, e2) -> Vlang.create_formula_eq (exp_rewrite e1) (exp_rewrite e2)
  | VF_imply (vf1, vf2) -> Vlang.create_formula_imply (nested_rewrite vf1) (nested_rewrite vf2)
  | VF_iff (vf1, vf2) -> Vlang.create_formula_iff (nested_rewrite vf1) (nested_rewrite vf2)
end

and create_rewrite_exp : Vlang.var -> Vlang.var -> Vlang.v_exp -> Vlang.v_exp
=fun v v' e -> begin
  let nested_rewrite ve = create_rewrite_exp v v' ve in
  let formula_rewrite vf = create_rewrite_formula v v' vf in
  match e with
  | VE_int _ | VE_string _ | VE_unit | VE_none _
  | VE_uni_cont (_, _, _) | VE_bin_cont (_, _, _, _) | VE_list _
  | VE_nul_op (_, _) | VE_lambda _ | VE_operation (_, _) -> e
  | VE_bool f -> VE_bool (formula_rewrite f)
  | VE_var (vv, t) -> begin
      if vv = v then Vlang.create_exp_var v' t
      else e
    end
  | VE_read (ve1, ve2) -> VE_read ((nested_rewrite ve1), (nested_rewrite ve2))
  | VE_write (ve1, ve2, ve3) -> VE_write ((nested_rewrite ve1), (nested_rewrite ve2), (nested_rewrite ve3))
  | VE_uni_op (vo, ve, t) -> VE_uni_op (vo, (nested_rewrite ve), t)
  | VE_bin_op (vo, ve1, ve2, t) -> VE_bin_op (vo, (nested_rewrite ve1), (nested_rewrite ve2), t)
  | VE_ter_op (vo, ve1, ve2, ve3, t) -> VE_ter_op (vo, (nested_rewrite ve1), (nested_rewrite ve2), (nested_rewrite ve3), t)
end

and create_convert_data : Vlang.data -> Vlang.typ -> Vlang.v_exp
=fun d t -> begin
  match (d.d, t.d) with
  | D_int x, (T_int | T_nat | T_mutez) -> VE_int x 
  | D_string x, (T_string | T_key | T_key_hash | T_signature | T_address | T_timestamp) -> VE_string x
  | D_bytes x, T_bytes -> VE_string x
  | D_unit, T_unit -> VE_unit
  | D_bool b, T_bool -> begin
      if b then VE_bool (VF_true)
      else VE_bool (VF_false)
    end
  | D_pair (c1, c2), T_pair (t1, t2) -> begin
      VE_bin_cont (VE_pair, (create_convert_data c1 t1), (create_convert_data c2 t2), t)
    end
  | D_left c, T_or(t', _) -> VE_uni_cont (VE_left, (create_convert_data c t'), t)
  | D_right c, T_or(_, t') -> VE_uni_cont (VE_right, (create_convert_data c t'), t)
  | D_some c, T_option t' -> VE_uni_cont (VE_some, (create_convert_data c t'), t)
  | D_none, T_option t' -> VE_none t'
  | D_elt (c1, c2), T_map (t1, t2) -> VE_bin_cont (VE_elt, (create_convert_data c1 t1), (create_convert_data c2 t2), t)
  | D_list cl, T_list t' -> VE_list (Core.List.map cl ~f:(fun c -> (create_convert_data c t')), t)
  | _ -> raise (Failure "Wrong data and type")
end

and create_convert_exp : Vlang.exp -> Vlang.typ -> Vlang.v_exp
=fun e t -> begin
  match e with
  | E_itself v -> Vlang.create_exp_var v t
  | E_push (d, t') -> create_convert_data d t'
  | E_car v -> VE_uni_op (VE_car, (Vlang.create_exp_var v t), t)
  | E_cdr v -> VE_uni_op (VE_cdr, (Vlang.create_exp_var v t), t)
  | E_abs v -> VE_uni_op (VE_abs, (Vlang.create_exp_var v t), t)
  | E_neg v -> VE_uni_op (VE_neg, (Vlang.create_exp_var v t), t)
  | E_not v -> VE_uni_op (VE_not, (Vlang.create_exp_var v t), t)
  | E_add (v1, v2) -> VE_bin_op (VE_add, (Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), t)
  | E_sub (v1, v2) -> VE_bin_op (VE_sub, (Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), t)
  | E_mul (v1, v2) -> VE_bin_op (VE_mul, (Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), t)
  | E_ediv (v1, v2) -> VE_bin_op (VE_ediv, (Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), t)
  | E_div (v1, v2) -> VE_bin_op (VE_div, (Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), t)
  | E_mod (v1, v2) -> VE_bin_op (VE_mod, (Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), t)
  | E_shiftL (v1, v2) -> VE_bin_op (VE_lsl, (Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), t)
  | E_shiftR (v1, v2) -> VE_bin_op (VE_lsr, (Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), t)
  | E_and (v1, v2) -> VE_bin_op (VE_and, (Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), t)
  | E_or (v1, v2) -> VE_bin_op (VE_or, (Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), t)
  | E_xor (v1, v2) -> VE_bin_op (VE_xor, (Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), t)
  | E_eq v -> VE_uni_op (VE_eq, (Vlang.create_exp_var v t), t)
  | E_neq v -> VE_uni_op (VE_neq, (Vlang.create_exp_var v t), t)
  | E_lt v -> VE_uni_op (VE_lt, (Vlang.create_exp_var v t), t)
  | E_gt v -> VE_uni_op (VE_gt, (Vlang.create_exp_var v t), t)
  | E_leq v -> VE_uni_op (VE_leq, (Vlang.create_exp_var v t), t)
  | E_geq v -> VE_uni_op (VE_geq, (Vlang.create_exp_var v t), t)
  | E_compare (v1, v2) -> VE_bin_op (VE_cmp, (Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), t)
  | E_cons (v1, v2) -> VE_bin_op (VE_cons, (Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), t)
  | E_operation o -> begin
      match o with
      | O_create_contract (_, _, _, _) -> VE_operation (VE_origination, t)
      | O_transfer_tokens (_, _, _)-> VE_operation (VE_transaction, t)
      | O_set_delegate _ -> VE_operation (VE_delegation, t)
      | O_create_account (_, _, _, _) -> VE_operation (VE_origination, t)
    end
  | E_unit -> VE_unit
  | E_pair (v1, v2) -> VE_bin_cont (VE_pair, (Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), t)
  | E_left (v, t') -> VE_uni_cont (VE_left, (Vlang.create_exp_var v t), t')
  | E_right (v, t') -> VE_uni_cont (VE_right, (Vlang.create_exp_var v t), t')
  | E_some v -> VE_uni_cont (VE_some, (Vlang.create_exp_var v t), t)
  | E_none t' -> VE_none t'
  | E_mem (v1, v2) -> VE_bool (VF_not (VF_uni_rel (VF_is_none, VE_read ((Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t)))))
  | E_get (v1, v2) -> VE_read ((Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t))
  | E_update (v1, v2, v3) -> VE_write ((Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), (Vlang.create_exp_var v3 t))
  | E_cast v -> VE_uni_op (VE_cast, (Vlang.create_exp_var v t), t)
  | E_concat (v1, v2) -> VE_bin_op (VE_concat, (Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), t)
  | E_concat_list v -> VE_uni_op (VE_concat, (Vlang.create_exp_var v t), t)
  | E_slice (v1, v2, v3) -> VE_ter_op (VE_slice, (Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), (Vlang.create_exp_var v3 t), t)
  | E_pack v -> VE_uni_op (VE_pack, (Vlang.create_exp_var v t), t)
  | E_unpack (_, v) -> VE_uni_op (VE_unpack, (Vlang.create_exp_var v t), t)
  | E_self -> VE_nul_op (VE_self, t)
  | E_contract_of_address v -> VE_uni_op (VE_contract, (Vlang.create_exp_var v t), t)
  | E_implicit_account v -> VE_uni_op (VE_account, (Vlang.create_exp_var v t), t)
  | E_now -> VE_nul_op (VE_now, t)
  | E_amount -> VE_nul_op (VE_amount, t)
  | E_balance -> VE_nul_op (VE_balance, t)
  | E_check_signature (v1, v2, v3) -> VE_ter_op (VE_check_signature, (Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), (Vlang.create_exp_var v3 t), t)
  | E_blake2b v -> VE_uni_op (VE_blake2b, (Vlang.create_exp_var v t), t)
  | E_sha256 v -> VE_uni_op (VE_sha256, (Vlang.create_exp_var v t), t)
  | E_sha512 v -> VE_uni_op (VE_sha512, (Vlang.create_exp_var v t), t)
  | E_hash_key v -> VE_uni_op (VE_hash_key, (Vlang.create_exp_var v t), t)
  | E_steps_to_quota -> VE_nul_op (VE_steps_to_quota, t)
  | E_source -> VE_nul_op (VE_source, t)
  | E_sender -> VE_nul_op (VE_sender, t)
  | E_address_of_contract v -> VE_uni_op (VE_address, (Vlang.create_exp_var v t), t)
  | E_create_contract_address _ -> VE_operation (VE_origination, t)
  | E_unlift_option v -> VE_uni_op (VE_un_opt, (Vlang.create_exp_var v t), t)
  | E_unlift_or v -> VE_uni_op (VE_un_or, (Vlang.create_exp_var v t), t)
  | E_hd v -> VE_uni_op (VE_hd, (Vlang.create_exp_var v t), t)
  | E_tl v -> VE_uni_op (VE_tl, (Vlang.create_exp_var v t), t)
  | E_size v -> VE_uni_op (VE_size, (Vlang.create_exp_var v t), t)
  | E_isnat v -> VE_uni_op (VE_isnat, (Vlang.create_exp_var v t), t)
  | E_int_of_nat v -> VE_uni_op (VE_int, (Vlang.create_exp_var v t), t)
  | E_chain_id -> VE_nul_op (VE_chain_id, t)
  | E_create_account_address _ -> VE_operation (VE_origination, t)
  | E_lambda (_, _, _) -> VE_lambda t
  | E_exec (v1, v2) -> VE_bin_op (VE_exec, (Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), t)
  | E_dup v -> (Vlang.create_exp_var v t)
  | E_nil _ -> VE_list ([], t)
  | E_empty_set _ -> VE_list ([], t)
  | E_empty_map _ -> VE_list ([], t)
  | E_empty_big_map _ -> VE_list ([], t)
  | E_append (v1, v2) -> VE_bin_op (VE_append, (Vlang.create_exp_var v1 t), (Vlang.create_exp_var v2 t), t)
  | E_special_nil_list -> VE_list ([], t)
  | E_phi (_, _) -> raise (Failure "Phi Function")
end