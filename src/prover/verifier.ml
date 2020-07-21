open ProverLib

(************************************************)
(************************************************)

let rec verify : Vlang.t -> bool
=fun vc -> begin
  let zexp_of_vc = create_convert_vformula vc in
  let solver = Vc.create_solver () in
  let _ = Vc.update_solver_add solver [zexp_of_vc] in
  let result, _ = Vc.create_check solver in
  result
end

and sort_of_typt : Vc.typ -> Vc.z_sort
=fun typ -> begin
  match typ.d with
  | T_key -> Vc.create_string_sort
  | T_unit -> Vc.create_unit_sort
  | T_signature -> Vc.create_string_sort
  | T_option t -> Vc.create_option_sort (sort_of_typt t)
  | T_list t -> Vc.create_list_sort (sort_of_typt t)
  | T_set t -> Vc.create_list_sort (sort_of_typt t)
  | T_operation -> Vc.create_operation_sort
  | T_contract _ -> Vc.create_contract_sort
  | T_pair (t1, t2) -> Vc.create_pair_sort (sort_of_typt t1) (sort_of_typt t2)
  | T_or (t1, t2) -> Vc.create_or_sort (sort_of_typt t1) (sort_of_typt t2)
  | T_lambda (_, _) -> Vc.create_lambda_sort
  | T_map (t1, t2) -> Vc.create_map_sort (sort_of_typt t1) (sort_of_typt t2)
  | T_big_map (t1, t2) -> Vc.create_map_sort (sort_of_typt t1) (sort_of_typt t2)
  | T_chain_id -> Vc.create_string_sort
  | T_int -> Vc.create_int_sort
  | T_nat -> Vc.create_int_sort
  | T_string -> Vc.create_string_sort
  | T_bytes -> Vc.create_string_sort
  | T_mutez -> Vc.create_int_sort
  | T_bool -> Vc.create_bool_sort
  | T_key_hash -> Vc.create_string_sort
  | T_timestamp -> Vc.create_string_sort
  | T_address -> Vc.create_string_sort
end

and sort_of_inner_type : Vc.typ -> Vc.z_sort list
=fun typ -> begin
  match typ.d with
  | T_option t -> [(sort_of_typt t)]
  | T_list t -> [(sort_of_typt t)]
  | T_set t -> [(sort_of_typt t)]
  | T_pair (t1, t2) -> [(sort_of_typt t1); (sort_of_typt t2)]
  | T_or (t1, t2) -> [(sort_of_typt t1); (sort_of_typt t2)]
  | T_lambda (t1, t2) -> [(sort_of_typt t1); (sort_of_typt t2)]
  | T_map (t1, t2) -> [(sort_of_typt t1); (sort_of_typt t2)]
  | T_big_map (t1, t2) -> [(sort_of_typt t1); (sort_of_typt t2)]
  | _ -> raise (Failure "Cannot get inner type on this type")
end

and create_convert_vformula : Vlang.v_formula -> Vc.z_expr
=fun vf -> begin
  match vf with
  | VF_true -> Vc.create_bool_true
  | VF_false -> Vc.create_bool_false
  | VF_not f -> Vc.create_bool_not (create_convert_vformula f)
  | VF_and (f1, f2) -> Vc.create_bool_and [(create_convert_vformula f1); (create_convert_vformula f2)]
  | VF_or (f1, f2) -> Vc.create_bool_or [(create_convert_vformula f1); (create_convert_vformula f2)]
  | VF_uni_rel (vur, e) -> begin
      let e' = create_convert_vexp e in
      match vur with
      | VF_is_true -> Vc.create_bool_eq e' (Vc.create_bool_true)
      | VF_is_none -> Vc.create_bool_eq (Vc.read_option_exist e') (Vc.create_option_enum_none)
      | VF_is_left -> Vc.create_bool_eq (Vc.read_or_location e') (Vc.create_or_enum_left)
      | VF_is_cons -> Vc.create_bool_list_is_cons e'
    end
  | VF_eq (e1, e2) -> Vc.create_bool_eq (create_convert_vexp e1) (create_convert_vexp e2)
  | VF_imply (f1, f2) -> Vc.create_bool_imply (create_convert_vformula f1) (create_convert_vformula f2)
  | VF_iff (f1, f2) -> Vc.create_bool_iff (create_convert_vformula f1) (create_convert_vformula f2)
end

and create_convert_vexp : Vlang.v_exp -> Vc.z_expr
=fun ve -> begin
  let get_nth = Core.List.nth_exn in
  match ve with
  | VE_int n -> Vc.create_int_from_zarith n
  | VE_string s -> Vc.create_string s
  | VE_bool f -> create_convert_vformula f
  | VE_unit -> Vc.create_unit
  | VE_none t -> Vc.create_option (get_nth (sort_of_inner_type t) 0) None
  | VE_uni_cont (vuc, e1, t) -> begin
      let inner_sorts = sort_of_inner_type t in
      match vuc with
      | VE_left -> begin
          let right_dummy_sort = get_nth inner_sorts 1 in
          Vc.create_or right_dummy_sort (Left (create_convert_vexp e1))
        end
      | VE_right -> begin
          let left_dummy_sort = get_nth inner_sorts 0 in
          Vc.create_or left_dummy_sort (Right (create_convert_vexp e1))
        end
      | VE_some -> begin
          let item_sort = get_nth inner_sorts 0 in
          Vc.create_option item_sort (Some (create_convert_vexp e1))
        end
    end
  | VE_bin_cont (vbc, e1, e2, t) -> begin
      match vbc with
      | VE_pair -> Vc.create_pair (create_convert_vexp e1) (create_convert_vexp e2)
      | VE_elt -> begin
          let map = Vc.create_map (sort_of_typt t) in
          Vc.update_map map (create_convert_vexp e1) (create_convert_vexp e2)
        end
    end
  | VE_list (vel, t) -> begin
      let nil = Vc.create_list (sort_of_typt t) in
      Core.List.fold_right vel ~f:(fun e l -> Vc.update_list_cons (create_convert_vexp e) l) ~init:nil
    end
  | VE_var (v, t) -> Vc.read_var (Vc.create_symbol v) (sort_of_typt t)
  | VE_read (e1, e2) -> begin
      let key, map = ((create_convert_vexp e1), (create_convert_vexp e2)) in
      let item = Vc.read_map map key in
      let sort_of_item = Vc.read_sort_of_expr item in
      let default_item = Vc.read_default_term map in
      Vc.create_ite (Vc.create_bool_eq item default_item) (Vc.create_option sort_of_item None) (Vc.create_option sort_of_item (Some item))
    end
  | VE_write (e1, e2, e3) -> Vc.update_map (create_convert_vexp e3) (create_convert_vexp e1) (create_convert_vexp e2)
  | VE_nul_op (vno, t) -> begin
      match vno with
      | VE_self -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_now -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_amount -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_balance -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_steps_to_quota -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_source -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_sender -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_chain_id -> Vc.create_dummy_expr (sort_of_typt t)
    end
  | VE_uni_op (vuo, e1, t) -> begin
      let zero = Vc.create_int 0 in
      match vuo with
      | VE_car -> Vc.read_pair_fst (create_convert_vexp e1)
      | VE_cdr -> Vc.read_pair_snd (create_convert_vexp e1)
      | VE_abs -> begin
          let ze1 = create_convert_vexp e1 in
          Vc.create_ite (Vc.create_bool_int_ge ze1 zero) ze1 (Vc.create_int_neg ze1)
        end
      | VE_neg -> Vc.create_int_neg (create_convert_vexp e1)
      | VE_not -> Vc.create_bool_not (create_convert_vexp e1)
      | VE_eq -> Vc.create_bool_eq (create_convert_vexp e1) zero
      | VE_neq -> Vc.create_bool_not (Vc.create_bool_eq (create_convert_vexp e1) zero)
      | VE_lt -> Vc.create_bool_int_lt (create_convert_vexp e1) zero
      | VE_gt -> Vc.create_bool_int_gt (create_convert_vexp e1) zero
      | VE_leq -> Vc.create_bool_int_le (create_convert_vexp e1) zero
      | VE_geq -> Vc.create_bool_int_ge (create_convert_vexp e1) zero
      | VE_cast -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_concat -> Vc.create_string_concat [(create_convert_vexp e1)]
      | VE_pack -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_unpack -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_contract -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_account -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_blake2b -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_sha256 -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_sha512 -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_hash_key -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_address -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_un_opt -> Vc.read_option_content (create_convert_vexp e1)
      | VE_un_or -> Vc.read_or_content (create_convert_vexp e1)
      | VE_hd -> Vc.read_list_head (create_convert_vexp e1)
      | VE_tl -> Vc.read_list_tail (create_convert_vexp e1)
      | VE_size -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_isnat -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_int -> Vc.create_dummy_expr (sort_of_typt t)
    end
  | VE_bin_op (vbo, e1, e2, t) -> begin
      let zero = Vc.create_int 0 in
      let two = Vc.create_int 2 in
      match vbo with
      | VE_add -> Vc.create_int_add [(create_convert_vexp e1); (create_convert_vexp e2)]
      | VE_sub -> Vc.create_int_sub [(create_convert_vexp e1); (create_convert_vexp e2)]
      | VE_mul -> Vc.create_int_mul [(create_convert_vexp e1); (create_convert_vexp e2)]
      | VE_ediv -> begin
          let dividend, divisor = ((create_convert_vexp e1), (create_convert_vexp e2)) in
          let x = Vc.create_pair (Vc.create_int_div dividend divisor) (Vc.create_int_mod dividend divisor) in
          let sort_of_x = Vc.read_sort_of_expr x in
          Vc.create_ite (Vc.create_bool_eq divisor zero) (Vc.create_option sort_of_x None) (Vc.create_option sort_of_x (Some x))
        end
      | VE_div -> Vc.create_int_div (create_convert_vexp e1) (create_convert_vexp e2)
      | VE_mod -> Vc.create_int_mod (create_convert_vexp e1) (create_convert_vexp e2)
      | VE_lsl -> begin
          let bin_exp = Vc.create_int_power two (create_convert_vexp e2) in
          Vc.create_int_mul [(create_convert_vexp e1); bin_exp]
        end
      | VE_lsr -> begin
          let bin_exp = Vc.create_int_power two (create_convert_vexp e2) in
          Vc.create_int_div (create_convert_vexp e1) bin_exp
        end
      | VE_and -> Vc.create_bool_and [(create_convert_vexp e1); (create_convert_vexp e2)]
      | VE_or -> Vc.create_bool_or [(create_convert_vexp e1); (create_convert_vexp e2)]
      | VE_xor -> Vc.create_bool_xor (create_convert_vexp e1) (create_convert_vexp e2)
      | VE_cmp -> Vc.create_cmp (create_convert_vexp e1) (create_convert_vexp e2)
      | VE_cons -> Vc.update_list_cons (create_convert_vexp e1) (create_convert_vexp e2)
      | VE_concat -> Vc.create_string_concat [(create_convert_vexp e1); (create_convert_vexp e2)]
      | VE_exec -> Vc.create_dummy_expr (sort_of_typt t)
      | VE_append -> Vc.create_dummy_expr (sort_of_typt t)
    end
  | VE_ter_op (vto, e1, e2, e3, t) -> begin
      match vto with
      | VE_slice -> begin
          let offset, length, s = ((create_convert_vexp e1), (create_convert_vexp e2), (create_convert_vexp e3)) in
          Vc.create_string_slice s offset (Vc.create_int_add [offset; length])
        end
      | VE_check_signature -> Vc.create_dummy_expr (sort_of_typt t)
    end
  | VE_lambda t -> Vc.create_dummy_expr (sort_of_typt t)
  | VE_operation (_, t) -> Vc.create_dummy_expr (sort_of_typt t)
end