open ProverLib

type object_typ =
  | Mutez

(************************************************)
(************************************************)

let type_map = ref Cfg.CPMap.empty


let rec convert : Bp.t -> Cfg.t -> Vlang.t
=fun bp cfg -> begin
  let _ = type_map := cfg.type_info in
  let precond = create_precond_from_param_storage () in
  let f, g = ((Vlang.create_formula_and ((Option.get bp.pre.formula)::precond)), (Option.get bp.post.formula)) in
  let f', g' = Core.List.fold_left bp.body ~init:(f, g) ~f:sp in
  let vc = Vlang.create_formula_imply f' g' in
  vc
end

and sp : (Vlang.t * Vlang.t) -> Bp.inst -> (Vlang.t * Vlang.t)
=fun (f, g) s -> begin
  match s with
  | BI_assume c -> begin
      let f' = Vlang.create_formula_and [(create_convert_cond c); f] in
      (f', g)
    end
  | BI_assert c -> begin
      let g' = Vlang.create_formula_imply f (create_convert_cond c) in
      (f, g')
    end
  | BI_assign (v, e) -> begin
      let v' = create_rename_var v in
      let f' = create_rewrite_formula v v' f in
      let e' = create_rewrite_exp v v' (create_convert_exp e (read_type v)) in
      let f'' = Vlang.create_formula_and [f'; (Vlang.create_formula_eq (create_var v) e')] in
      (f'', g)
    end
  | BI_skip -> (f, g)
end

and read_type : Vlang.var -> Vlang.typ
=fun v -> Cfg.CPMap.find_exn !type_map v

and update_type : Vlang.var -> Vlang.typ -> unit
=fun v t -> begin
  let _ = type_map := Cfg.CPMap.add_exn !type_map ~key:v ~data:t in
  ()
end

and create_var : Vlang.var -> Vlang.v_exp
=fun v -> Vlang.create_exp_var v (read_type v)

and create_rename_var : Vlang.var -> Vlang.var
=fun v -> begin
  let rec rename_label l = if Cfg.CPMap.mem !type_map (l ^ v) then rename_label ("#" ^ l) else l in
  let v' = (rename_label "#") ^ v in
  let _ = update_type v' (read_type v) in
  v'
end

and create_rewrite_formula : Vlang.var -> Vlang.var -> Vlang.v_formula -> Vlang.v_formula
=fun v v' f -> begin
  let nested_rewrite vf = create_rewrite_formula v v' vf in
  let exp_rewrite ve = create_rewrite_exp v v' ve in
  match f with
  | VF_true | VF_false -> f
  | VF_not vf -> Vlang.create_formula_not (nested_rewrite vf)
  | VF_and fl -> Vlang.create_formula_and (Core.List.map fl ~f:nested_rewrite)
  | VF_or fl -> Vlang.create_formula_or (Core.List.map fl ~f:nested_rewrite)
  | VF_uni_rel (vr, ve) -> Vlang.create_formula_uni_rel vr (exp_rewrite ve)
  | VF_bin_rel (vr, e1, e2) -> Vlang.create_formula_bin_rel vr (exp_rewrite e1) (exp_rewrite e2)
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
  | VE_bool f -> Vlang.create_exp_bool (formula_rewrite f)
  | VE_var (vv, _) -> begin
      if vv = v then create_var v'
      else e
    end
  | VE_read (ve1, ve2) -> Vlang.create_exp_read (nested_rewrite ve1) (nested_rewrite ve2)
  | VE_write (ve1, ve2, ve3) -> Vlang.create_exp_write (nested_rewrite ve1) (nested_rewrite ve2) (nested_rewrite ve3)
  | VE_uni_op (vo, ve, t) -> Vlang.create_exp_uni_op vo (nested_rewrite ve) t
  | VE_bin_op (vo, ve1, ve2, t) -> Vlang.create_exp_bin_op vo (nested_rewrite ve1) (nested_rewrite ve2) t
  | VE_ter_op (vo, ve1, ve2, ve3, t) -> Vlang.create_exp_ter_op vo (nested_rewrite ve1) (nested_rewrite ve2) (nested_rewrite ve3) t
end

and create_convert_data : Vlang.data -> Vlang.typ -> Vlang.v_exp
=fun d t -> begin
  match (d.d, t.d) with
  | D_int x, (T_int | T_nat | T_mutez) -> Vlang.create_exp_int x 
  | D_string x, (T_string | T_key | T_key_hash | T_signature | T_address | T_timestamp) -> Vlang.create_exp_string x
  | D_bytes x, T_bytes -> Vlang.create_exp_string x
  | D_unit, T_unit -> Vlang.create_exp_unit
  | D_bool b, T_bool -> begin
      if b then Vlang.create_exp_bool_true
      else Vlang.create_exp_bool_false
    end
  | D_pair (c1, c2), T_pair (t1, t2) -> Vlang.create_exp_bin_cont_pair  (create_convert_data c1 t1) (create_convert_data c2 t2) t
  | D_left c, T_or(t', _) -> Vlang.create_exp_uni_cont_left (create_convert_data c t') t
  | D_right c, T_or(_, t') -> Vlang.create_exp_uni_cont_right (create_convert_data c t') t
  | D_some c, T_option t' -> Vlang.create_exp_uni_cont_some (create_convert_data c t') t
  | D_none, T_option t' -> Vlang.create_exp_none t'
  | D_elt (c1, c2), T_map (t1, t2) -> Vlang.create_exp_bin_cont_elt (create_convert_data c1 t1) (create_convert_data c2 t2) t
  | D_list cl, T_list t' -> Vlang.create_exp_list (Core.List.map cl ~f:(fun c -> (create_convert_data c t'))) t
  | _ -> raise (Failure "Converter.create_convert_data: Wrong data and type")
end

and create_convert_cond : Bp.cond -> Vlang.v_formula
=fun c -> begin
  match c with
  | BC_is_true v -> Vlang.create_formula_is_true (create_var v)
  | BC_is_none v -> Vlang.create_formula_is_none (create_var v) 
  | BC_is_left v -> Vlang.create_formula_is_left (create_var v)
  | BC_is_cons v -> Vlang.create_formula_is_cons (create_var v)
  | BC_not c -> Vlang.create_formula_not (create_convert_cond c)
end

and create_convert_exp : Vlang.exp -> Vlang.typ -> Vlang.v_exp
=fun e t -> begin
  match e with
  | E_itself v -> create_var v
  | E_push (d, t') -> create_convert_data d t'
  | E_car v -> Vlang.create_exp_uni_op_car (create_var v) t
  | E_cdr v -> Vlang.create_exp_uni_op_cdr (create_var v) t
  | E_abs v -> Vlang.create_exp_uni_op_abs (create_var v) t
  | E_neg v -> Vlang.create_exp_uni_op_neg (create_var v) t
  | E_not v -> Vlang.create_exp_uni_op_not (create_var v) t
  | E_add (v1, v2) -> Vlang.create_exp_bin_op_add (create_var v1) (create_var v2) t
  | E_sub (v1, v2) -> Vlang.create_exp_bin_op_sub (create_var v1) (create_var v2) t
  | E_mul (v1, v2) -> Vlang.create_exp_bin_op_mul (create_var v1) (create_var v2) t
  | E_ediv (v1, v2) -> Vlang.create_exp_bin_op_ediv (create_var v1) (create_var v2) t
  | E_div (v1, v2) -> Vlang.create_exp_bin_op_div (create_var v1) (create_var v2) t
  | E_mod (v1, v2) -> Vlang.create_exp_bin_op_mod (create_var v1) (create_var v2) t
  | E_shiftL (v1, v2) -> Vlang.create_exp_bin_op_lsl (create_var v1) (create_var v2) t
  | E_shiftR (v1, v2) -> Vlang.create_exp_bin_op_lsr (create_var v1) (create_var v2) t
  | E_and (v1, v2) -> Vlang.create_exp_bin_op_and (create_var v1) (create_var v2) t
  | E_or (v1, v2) -> Vlang.create_exp_bin_op_or (create_var v1) (create_var v2) t
  | E_xor (v1, v2) -> Vlang.create_exp_bin_op_xor (create_var v1) (create_var v2) t
  | E_eq v -> Vlang.create_exp_uni_op_eq (create_var v) t
  | E_neq v -> Vlang.create_exp_uni_op_neq (create_var v) t
  | E_lt v -> Vlang.create_exp_uni_op_lt (create_var v) t
  | E_gt v -> Vlang.create_exp_uni_op_gt (create_var v) t
  | E_leq v -> Vlang.create_exp_uni_op_leq (create_var v) t
  | E_geq v -> Vlang.create_exp_uni_op_geq (create_var v) t
  | E_compare (v1, v2) -> Vlang.create_exp_bin_op_cmp (create_var v1) (create_var v2) t
  | E_cons (v1, v2) -> Vlang.create_exp_bin_op_cons (create_var v1) (create_var v2) t
  | E_operation o -> begin
      match o with
      | O_create_contract (_, _, _, _) -> Vlang.create_exp_operation_origination t
      | O_transfer_tokens (_, _, _)-> Vlang.create_exp_operation_transaction t
      | O_set_delegate _ -> Vlang.create_exp_operation_delegation t
      | O_create_account (_, _, _, _) -> Vlang.create_exp_operation_origination t
    end
  | E_unit -> Vlang.create_exp_unit
  | E_pair (v1, v2) -> Vlang.create_exp_bin_cont_pair (create_var v1) (create_var v2) t
  | E_left (v, t') -> Vlang.create_exp_uni_cont_left (create_var v) t'
  | E_right (v, t') -> Vlang.create_exp_uni_cont_right (create_var v) t'
  | E_some v -> Vlang.create_exp_uni_cont_some (create_var v) t
  | E_none t' -> Vlang.create_exp_none t'
  | E_mem (v1, v2) -> Vlang.create_exp_bool (Vlang.create_formula_not (Vlang.create_formula_is_none (Vlang.create_exp_read (create_var v1) (create_var v2))))
  | E_get (v1, v2) -> Vlang.create_exp_read (create_var v1) (create_var v2)
  | E_update (v1, v2, v3) -> Vlang.create_exp_write (create_var v1) (create_var v2) (create_var v3)
  | E_cast v -> Vlang.create_exp_uni_op_cast (create_var v) t
  | E_concat (v1, v2) -> Vlang.create_exp_bin_op_concat (create_var v1) (create_var v2) t
  | E_concat_list v -> Vlang.create_exp_uni_op_concat (create_var v) t
  | E_slice (v1, v2, v3) -> Vlang.create_exp_ter_op_slice (create_var v1) (create_var v2) (create_var v3) t
  | E_pack v -> Vlang.create_exp_uni_op_pack (create_var v) t
  | E_unpack (_, v) -> Vlang.create_exp_uni_op_unpack (create_var v) t
  | E_self -> Vlang.create_exp_nul_op_self t
  | E_contract_of_address v -> Vlang.create_exp_uni_op_contract (create_var v) t
  | E_implicit_account v -> Vlang.create_exp_uni_op_account (create_var v) t
  | E_now -> Vlang.create_exp_nul_op_now t
  | E_amount -> Vlang.create_exp_nul_op_amount t
  | E_balance -> Vlang.create_exp_nul_op_balance t
  | E_check_signature (v1, v2, v3) -> Vlang.create_exp_ter_op_check_signature (create_var v1) (create_var v2) (create_var v3) t
  | E_blake2b v -> Vlang.create_exp_uni_op_blake2b (create_var v) t
  | E_sha256 v -> Vlang.create_exp_uni_op_sha256 (create_var v) t
  | E_sha512 v -> Vlang.create_exp_uni_op_sha512 (create_var v) t
  | E_hash_key v -> Vlang.create_exp_uni_op_hash_key (create_var v) t
  | E_steps_to_quota -> Vlang.create_exp_nul_op_steps_to_quota t
  | E_source -> Vlang.create_exp_nul_op_source t
  | E_sender -> Vlang.create_exp_nul_op_sender t
  | E_address_of_contract v -> Vlang.create_exp_uni_op_address (create_var v) t
  | E_create_contract_address _ -> Vlang.create_exp_operation_origination t
  | E_unlift_option v -> Vlang.create_exp_uni_op_un_opt (create_var v) t
  | E_unlift_or v -> Vlang.create_exp_uni_op_un_or (create_var v) t
  | E_hd v -> Vlang.create_exp_uni_op_hd (create_var v) t
  | E_tl v -> Vlang.create_exp_uni_op_tl (create_var v) t
  | E_size v -> Vlang.create_exp_uni_op_size (create_var v) t
  | E_isnat v -> Vlang.create_exp_uni_op_isnat (create_var v) t
  | E_int_of_nat v -> Vlang.create_exp_uni_op_int (create_var v) t
  | E_chain_id -> Vlang.create_exp_nul_op_chain_id t
  | E_create_account_address _ -> Vlang.create_exp_operation_origination t
  | E_lambda (_, _, _) -> Vlang.create_exp_lambda t
  | E_lambda_id id -> Vlang.create_exp_int_of_small_int id
  | E_exec (v1, v2) -> Vlang.create_exp_bin_op_exec (create_var v1) (create_var v2) t
  | E_dup v -> create_var v
  | E_nil _ -> Vlang.create_exp_list [] t
  | E_empty_set _ -> Vlang.create_exp_list [] t
  | E_empty_map _ -> Vlang.create_exp_list [] t
  | E_empty_big_map _ -> Vlang.create_exp_list [] t
  | E_append (v1, v2) -> Vlang.create_exp_bin_op_append (create_var v1) (create_var v2) t
  | E_special_nil_list -> Vlang.create_exp_list [] t
  | E_phi (_, _) -> raise (Failure "Converter.create_convert_exp: Phi Function")
end

and create_precond_from_param_storage : unit -> Vlang.v_formula list
=fun () -> begin
  let param_storage_var = "param_storage" in
  let param_storage = create_var param_storage_var in
  let param_storage_typ = read_type param_storage_var in
  let mutez_formula e = begin
    let mutez_upper_bound = Vlang.create_exp_int_of_string "9223372036854775808" in
    let mutez_lower_bound = Vlang.create_exp_int_of_string "0" in
    let lower_formula = Vlang.create_formula_le mutez_lower_bound e in
    let upper_formula = Vlang.create_formula_lt e mutez_upper_bound in
    Vlang.create_formula_and [lower_formula; upper_formula]
  end in
  let rec read_nested_param_storage : Vlang.typ -> Vlang.v_exp -> (object_typ * (Vlang.v_formula -> Vlang.v_formula) list * Vlang.v_exp) list
  =fun ty e -> begin
    match ty.d with
    | T_option ty' -> begin
        let e' = Vlang.create_exp_uni_op_un_opt e ty in
        Core.List.map (read_nested_param_storage ty' e') ~f:(fun (obj, fl, exp) -> (
          (obj, (Vlang.create_formula_imply (Vlang.create_formula_is_some e))::fl, exp)
        ))
      end
    | T_pair (ty1', ty2') -> begin
        let e1' = Vlang.create_exp_uni_op_car e ty in
        let fst = Core.List.map (read_nested_param_storage ty1' e1') ~f:(fun (obj, fl, exp) -> (
          (obj, fl, exp)
        )) in
        let e2' = Vlang.create_exp_uni_op_cdr e ty in
        let snd = Core.List.map (read_nested_param_storage ty2' e2') ~f:(fun (obj, fl, exp) -> (
          (obj, fl, exp)
        )) in
        fst @ snd
      end
    | T_or (ty1', ty2') -> begin
        let e' = Vlang.create_exp_uni_op_un_or e ty in
        let left = Core.List.map (read_nested_param_storage ty1' e') ~f:(fun (obj, fl, exp) -> (
          (obj, (Vlang.create_formula_imply (Vlang.create_formula_is_left e))::fl, exp)
        )) in
        let right = Core.List.map (read_nested_param_storage ty2' e') ~f:(fun (obj, fl, exp) -> (
          (obj, (Vlang.create_formula_imply (Vlang.create_formula_is_right e))::fl, exp)
        )) in
        left @ right
      end
    | T_mutez -> [(Mutez, [], e)]
    | _ -> []
  end in
  let create_formula_from_param_storage (obj, fl, exp) = begin
    match obj with
    | Mutez -> Core.List.fold_right fl ~f:(fun func formula -> func formula) ~init:(mutez_formula exp)
  end in
  Core.List.map (read_nested_param_storage param_storage_typ param_storage) ~f:create_formula_from_param_storage
end