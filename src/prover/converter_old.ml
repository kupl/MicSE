(*
open ProverLib

type object_typ =
  | Mutez_Map
  | Mutez

(************************************************)
(************************************************)

let type_map = ref Pre.Lib.Cfg.CPMap.empty

let rec convert : Bp.t -> Pre.Lib.Cfg.t -> (Vlang.t * Query.t list)
=fun bp cfg -> begin
  let _ = type_map := cfg.type_info in
  let (f, g) = (Inv.T.read_formula (bp.pre), Inv.T.read_formula (bp.post)) in
  let f', qs = Core.List.fold_left bp.body ~init:(f, []) ~f:sp in
  let inductive = Vlang.create_formula_imply f' g in
  (inductive, qs)
end

and sp : (Vlang.t * Query.t list) -> (Bp.vertex * Bp.inst) -> (Vlang.t * Query.t list)
=fun (f, qs) (_, s) -> begin
  match s with
  | BI_assume c -> begin
      let f' = Vlang.create_formula_and [(create_convert_cond c); f] in
      (f', qs)
    end
  | BI_assert (c, loc, ctg) -> begin
      let formula = Vlang.create_formula_imply f (create_convert_cond c) in
      let query = Query.create_new_query formula loc ctg in
      (f, (query::qs))
    end
  | BI_assign (v, e) -> begin
      let v' = create_rename_var v in
      let f' = create_rewrite_formula v v' f in
      let o' = create_rewrite_obj v v' (create_convert_obj e (read_type v)) in
      let f'' = Vlang.create_formula_and [f'; (Vlang.create_formula_eq (create_var v) o')] in
      (f'', qs)
    end
  | BI_skip -> (f, qs)
end

and read_type : Vlang.var -> Vlang.typ
=fun v -> Pre.Lib.Cfg.CPMap.find_exn !type_map v

and update_type : Vlang.var -> Vlang.typ -> unit
=fun v t -> begin
  let _ = type_map := Pre.Lib.Cfg.CPMap.add_exn !type_map ~key:v ~data:t in
  ()
end

and create_var : Vlang.var -> Vlang.v_obj
=fun v -> Vlang.create_obj_of_exp ~exp:(Vlang.create_exp_var v) ~typ:(read_type v)

and create_rename_var : Vlang.var -> Vlang.var
=fun v -> begin
  let rec rename_label l = if Pre.Lib.Cfg.CPMap.mem !type_map (l ^ v) then rename_label ("#" ^ l) else l in
  let v' = (rename_label "#") ^ v in
  let _ = update_type v' (read_type v) in
  v'
end

and create_rewrite_formula : Vlang.var -> Vlang.var -> Vlang.v_formula -> Vlang.v_formula
=fun v v' f -> begin
  let nested_rewrite vf = create_rewrite_formula v v' vf in
  let obj_rewrite vo = create_rewrite_obj v v' vo in
  match f with
  | VF_true | VF_false -> f
  | VF_not vf -> Vlang.create_formula_not (nested_rewrite vf)
  | VF_and fl -> Vlang.create_formula_and (Core.List.map fl ~f:nested_rewrite)
  | VF_or fl -> Vlang.create_formula_or (Core.List.map fl ~f:nested_rewrite)
  | VF_uni_rel (vr, vo) -> Vlang.create_formula_uni_rel ~rel:vr ~o1:(obj_rewrite vo)
  | VF_bin_rel (vr, o1, o2) -> Vlang.create_formula_bin_rel ~rel:vr ~o1:(obj_rewrite o1) ~o2:(obj_rewrite o2)
  | VF_imply (vf1, vf2) -> Vlang.create_formula_imply (nested_rewrite vf1) (nested_rewrite vf2)
  | VF_iff (vf1, vf2) -> Vlang.create_formula_iff (nested_rewrite vf1) (nested_rewrite vf2)
  | VF_forall (vol, vf) -> Vlang.create_formula_forall ~bnd:(Core.List.map vol ~f:obj_rewrite) ~formula:(nested_rewrite vf)
  | VF_sigma_equal (o1, o2) -> Vlang.create_formula_sigma_equal ~map:(obj_rewrite o1) ~mutez:(obj_rewrite o2) 
end

and create_rewrite_exp : Vlang.var -> Vlang.var -> Vlang.v_exp -> Vlang.v_exp
=fun v v' e -> begin
  let nested_rewrite vobj = create_rewrite_obj v v' vobj in
  let formula_rewrite vf = create_rewrite_formula v v' vf in
  match e with
  | VE_int _ | VE_string _ | VE_unit | VE_none
  | VE_uni_cont (_, _) | VE_bin_cont (_, _, _) | VE_list _
  | VE_nul_op _ | VE_lambda | VE_operation _ -> e
  | VE_bool f -> Vlang.create_exp_bool (formula_rewrite f)
  | VE_var vv -> begin
      if vv = v then Vlang.create_exp_var v'
      else e
    end
  | VE_uni_op (vop, vobj1) -> Vlang.create_exp_uni_op ~op:vop ~o1:(nested_rewrite vobj1)
  | VE_bin_op (vop, vobj1, vobj2) -> Vlang.create_exp_bin_op ~op:vop ~o1:(nested_rewrite vobj1) ~o2:(nested_rewrite vobj2)
  | VE_ter_op (vop, vobj1, vobj2, vobj3) -> Vlang.create_exp_ter_op ~op:vop ~o1:(nested_rewrite vobj1) ~o2:(nested_rewrite vobj2) ~o3:(nested_rewrite vobj3)
end

and create_rewrite_obj : Vlang.var -> Vlang.var -> Vlang.v_obj -> Vlang.v_obj
=fun v v' o -> { o with exp=(create_rewrite_exp v v' o.exp) }

and create_convert_data : Vlang.data -> Vlang.typ -> Vlang.v_obj
=fun d t -> begin
  let make_obj e = Vlang.create_obj_of_exp ~exp:e ~typ:t in
  match (d.d, t.d) with
  | D_int x, (T_int | T_nat | T_mutez) -> make_obj (Vlang.create_exp_int x)
  | D_string x, (T_string | T_key | T_key_hash | T_signature | T_address | T_timestamp) -> make_obj (Vlang.create_exp_string x)
  | D_bytes x, T_bytes -> make_obj (Vlang.create_exp_string x)
  | D_unit, T_unit -> make_obj (Vlang.create_exp_unit)
  | D_bool b, T_bool -> begin
      if b then make_obj (Vlang.create_exp_bool_true)
      else make_obj (Vlang.create_exp_bool_false)
    end
  | D_pair (c1, c2), T_pair (t1, t2) -> make_obj (Vlang.create_exp_bin_cont_pair (create_convert_data c1 t1) (create_convert_data c2 t2))
  | D_left c, T_or(t', _) -> make_obj (Vlang.create_exp_uni_cont_left (create_convert_data c t'))
  | D_right c, T_or(_, t') -> make_obj (Vlang.create_exp_uni_cont_right (create_convert_data c t'))
  | D_some c, T_option t' -> make_obj (Vlang.create_exp_uni_cont_some (create_convert_data c t'))
  | D_none, T_option _ -> make_obj (Vlang.create_exp_none)
  | D_elt (c1, c2), T_big_map (t1, t2) | D_elt (c1, c2), T_map (t1, t2) -> make_obj (Vlang.create_exp_bin_cont_elt (create_convert_data c1 t1) (create_convert_data c2 t2))
  | D_list cl, T_map _ -> make_obj (Vlang.create_exp_list (Core.List.map cl ~f:(fun c -> (create_convert_data c t))))
  | D_list cl, T_list t' -> make_obj (Vlang.create_exp_list (Core.List.map cl ~f:(fun c -> (create_convert_data c t'))))
  | _ -> raise (Failure ("Converter.create_convert_data: Wrong data (" ^ (Pre.Lib.Mich.string_of_datat_ol d) ^ ") and type (" ^ (Pre.Lib.Mich.string_of_typt t) ^ ")"))
end

and create_convert_cond : Bp.cond -> Vlang.v_formula
=fun c -> begin
  match c with
  | BC_is_true v -> Vlang.create_formula_is_true (create_var v)
  | BC_is_none v -> Vlang.create_formula_is_none (create_var v) 
  | BC_is_left v -> Vlang.create_formula_is_left (create_var v)
  | BC_is_cons v -> Vlang.create_formula_is_cons (create_var v)
  | BC_no_overflow (e, t) -> Vlang.create_formula_no_overflow (create_convert_obj e t)
  | BC_no_underflow (e, t) -> Vlang.create_formula_no_underflow (create_convert_obj e t)
  | BC_not c -> Vlang.create_formula_not (create_convert_cond c)
end

and create_convert_obj : Vlang.exp -> Vlang.typ -> Vlang.v_obj
=fun e t -> begin
  let make_obj e = Vlang.create_obj_of_exp ~exp:e ~typ:t in
  match e with
  | E_itself v -> create_var v
  | E_push (d, t') -> create_convert_data d t'
  | E_car v -> make_obj (Vlang.create_exp_uni_op_car (create_var v))
  | E_cdr v -> make_obj (Vlang.create_exp_uni_op_cdr (create_var v))
  | E_abs v -> make_obj (Vlang.create_exp_uni_op_abs (create_var v))
  | E_neg v -> make_obj (Vlang.create_exp_uni_op_neg (create_var v))
  | E_not v -> make_obj (Vlang.create_exp_uni_op_not (create_var v))
  | E_add (v1, v2) -> make_obj (Vlang.create_exp_bin_op_add (create_var v1) (create_var v2))
  | E_sub (v1, v2) -> make_obj (Vlang.create_exp_bin_op_sub (create_var v1) (create_var v2))
  | E_mul (v1, v2) -> make_obj (Vlang.create_exp_bin_op_mul (create_var v1) (create_var v2))
  | E_ediv (v1, v2) -> make_obj (Vlang.create_exp_bin_op_ediv (create_var v1) (create_var v2))
  | E_div (v1, v2) -> make_obj (Vlang.create_exp_bin_op_div (create_var v1) (create_var v2))
  | E_mod (v1, v2) -> make_obj (Vlang.create_exp_bin_op_mod (create_var v1) (create_var v2))
  | E_shiftL (v1, v2) -> make_obj (Vlang.create_exp_bin_op_lsl (create_var v1) (create_var v2))
  | E_shiftR (v1, v2) -> make_obj (Vlang.create_exp_bin_op_lsr (create_var v1) (create_var v2))
  | E_and (v1, v2) -> make_obj (Vlang.create_exp_bin_op_and (create_var v1) (create_var v2))
  | E_or (v1, v2) -> make_obj (Vlang.create_exp_bin_op_or (create_var v1) (create_var v2))
  | E_xor (v1, v2) -> make_obj (Vlang.create_exp_bin_op_xor (create_var v1) (create_var v2))
  | E_eq v -> make_obj (Vlang.create_exp_uni_op_eq (create_var v))
  | E_neq v -> make_obj (Vlang.create_exp_uni_op_neq (create_var v))
  | E_lt v -> make_obj (Vlang.create_exp_uni_op_lt (create_var v))
  | E_gt v -> make_obj (Vlang.create_exp_uni_op_gt (create_var v))
  | E_leq v -> make_obj (Vlang.create_exp_uni_op_leq (create_var v))
  | E_geq v -> make_obj (Vlang.create_exp_uni_op_geq (create_var v))
  | E_compare (v1, v2) -> make_obj (Vlang.create_exp_bin_op_cmp (create_var v1) (create_var v2))
  | E_cons (v1, v2) -> make_obj (Vlang.create_exp_bin_op_cons (create_var v1) (create_var v2))
  | E_operation o -> begin
      match o with
      | O_create_contract (_, _, _, _) -> make_obj (Vlang.create_exp_operation_origination)
      | O_transfer_tokens (_, _, _) -> make_obj (Vlang.create_exp_operation_transaction)
      | O_set_delegate _ -> make_obj (Vlang.create_exp_operation_delegation)
      | O_create_account (_, _, _, _) -> make_obj (Vlang.create_exp_operation_origination)
    end
  | E_unit -> make_obj (Vlang.create_exp_unit)
  | E_pair (v1, v2) -> make_obj (Vlang.create_exp_bin_cont_pair (create_var v1) (create_var v2))
  | E_left (v, _) -> make_obj (Vlang.create_exp_uni_cont_left (create_var v))
  | E_right (v, _) -> make_obj (Vlang.create_exp_uni_cont_right (create_var v))
  | E_some v -> make_obj (Vlang.create_exp_uni_cont_some (create_var v))
  | E_none _ -> make_obj (Vlang.create_exp_none)
  | E_mem (v1, v2) -> make_obj (Vlang.create_exp_bin_op_mem (create_var v1) (create_var v2))
  | E_get (v1, v2) -> make_obj (Vlang.create_exp_bin_op_get (create_var v1) (create_var v2))
  | E_update (v1, v2, v3) -> make_obj (Vlang.create_exp_ter_op_update (create_var v1) (create_var v2) (create_var v3))
  | E_cast v -> make_obj (Vlang.create_exp_uni_op_cast (create_var v))
  | E_concat (v1, v2) -> make_obj (Vlang.create_exp_bin_op_concat (create_var v1) (create_var v2))
  | E_concat_list v -> make_obj (Vlang.create_exp_uni_op_concat (create_var v))
  | E_slice (v1, v2, v3) -> make_obj (Vlang.create_exp_ter_op_slice (create_var v1) (create_var v2) (create_var v3))
  | E_pack v -> make_obj (Vlang.create_exp_uni_op_pack (create_var v))
  | E_unpack (_, v) -> make_obj (Vlang.create_exp_uni_op_unpack (create_var v))
  | E_self -> make_obj (Vlang.create_exp_nul_op_self)
  | E_contract_of_address (_, v) -> make_obj (Vlang.create_exp_uni_op_contract (create_var v))
  | E_implicit_account v -> make_obj (Vlang.create_exp_uni_op_account (create_var v))
  | E_now -> make_obj (Vlang.create_exp_nul_op_now)
  | E_amount -> make_obj (Vlang.create_exp_nul_op_amount)
  | E_balance -> make_obj (Vlang.create_exp_nul_op_balance)
  | E_check_signature (v1, v2, v3) -> make_obj (Vlang.create_exp_ter_op_check_signature (create_var v1) (create_var v2) (create_var v3))
  | E_blake2b v -> make_obj (Vlang.create_exp_uni_op_blake2b (create_var v))
  | E_sha256 v -> make_obj (Vlang.create_exp_uni_op_sha256 (create_var v))
  | E_sha512 v -> make_obj (Vlang.create_exp_uni_op_sha512 (create_var v))
  | E_hash_key v -> make_obj (Vlang.create_exp_uni_op_hash_key (create_var v))
  | E_steps_to_quota -> make_obj (Vlang.create_exp_nul_op_steps_to_quota)
  | E_source -> make_obj (Vlang.create_exp_nul_op_source)
  | E_sender -> make_obj (Vlang.create_exp_nul_op_sender)
  | E_address_of_contract v -> make_obj (Vlang.create_exp_uni_op_address (create_var v))
  | E_create_contract_address _ -> make_obj (Vlang.create_exp_operation_origination)
  | E_unlift_option v -> make_obj (Vlang.create_exp_uni_op_un_opt (create_var v))
  | E_unlift_left v -> make_obj (Vlang.create_exp_uni_op_un_left (create_var v))
  | E_unlift_right v -> make_obj (Vlang.create_exp_uni_op_un_right (create_var v))
  | E_hd v -> make_obj (Vlang.create_exp_uni_op_hd (create_var v))
  | E_tl v -> make_obj (Vlang.create_exp_uni_op_tl (create_var v))
  | E_size v -> make_obj (Vlang.create_exp_uni_op_size (create_var v))
  | E_isnat v -> make_obj (Vlang.create_exp_uni_op_isnat (create_var v))
  | E_int_of_nat v -> make_obj (Vlang.create_exp_uni_op_int (create_var v))
  | E_chain_id -> make_obj (Vlang.create_exp_nul_op_chain_id)
  | E_create_account_address _ -> make_obj (Vlang.create_exp_operation_origination)
  | E_lambda (_, _, _) -> make_obj (Vlang.create_exp_lambda)
  | E_lambda_id id -> make_obj (Vlang.create_exp_int_of_small_int id)
  | E_exec (v1, v2) -> make_obj (Vlang.create_exp_bin_op_exec (create_var v1) (create_var v2))
  | E_dup v -> create_var v
  | E_nil _ -> make_obj (Vlang.create_exp_list [])
  | E_empty_set _ -> make_obj (Vlang.create_exp_list [])
  | E_empty_map _ -> make_obj (Vlang.create_exp_list [])
  | E_empty_big_map _ -> make_obj (Vlang.create_exp_list [])
  | E_append (v1, v2) -> make_obj (Vlang.create_exp_bin_op_append (create_var v1) (create_var v2))
  | E_special_nil_list -> make_obj (Vlang.create_exp_list [])
  | E_phi (_, _) -> raise (Failure "Converter.create_convert_exp: Deprecated Phi Function")
  | E_unlift_or _ -> raise (Failure "Converter.create_convert_exp: Deprecated Unlift or")
end
*)