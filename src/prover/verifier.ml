open ProverLib

(************************************************)
(************************************************)

let rec verify : Vlang.t -> Pre.Lib.Cfg.t -> bool * (Smt.z_expr * Smt.z_expr) option
=fun vc cfg -> begin
  let zexp_of_vc = create_convert_vformula (Vlang.create_formula_not vc) in
  let solver = Smt.create_solver () in
  let _ = Smt.update_solver_add solver [zexp_of_vc] in
  let result, model_opt = Smt.create_check solver in
  let param_storage_opt = match model_opt with None -> None | Some model -> create_param_storage_from_model model cfg in
  result, param_storage_opt
end

and sort_of_typt : Smt.typ -> Smt.z_sort
=fun typ -> begin
  match typ.d with
  | T_key -> Smt.create_string_sort
  | T_unit -> Smt.create_unit_sort
  | T_signature -> Smt.create_string_sort
  | T_option t -> Smt.create_option_sort (sort_of_typt t)
  | T_list t -> Smt.create_list_sort (sort_of_typt t)
  | T_set t -> Smt.create_list_sort (sort_of_typt t)
  | T_operation -> Smt.create_operation_sort
  | T_contract _ -> Smt.create_contract_sort
  | T_pair (t1, t2) -> Smt.create_pair_sort (sort_of_typt t1) (sort_of_typt t2)
  | T_or (t1, t2) -> Smt.create_or_sort (sort_of_typt t1) (sort_of_typt t2)
  | T_lambda (_, _) -> Smt.create_lambda_sort
  | T_map (t1, t2) -> Smt.create_map_sort (sort_of_typt t1) (sort_of_typt t2)
  | T_big_map (t1, t2) -> Smt.create_map_sort (sort_of_typt t1) (sort_of_typt t2)
  | T_chain_id -> Smt.create_string_sort
  | T_int -> Smt.create_int_sort
  | T_nat -> Smt.create_int_sort
  | T_string -> Smt.create_string_sort
  | T_bytes -> Smt.create_string_sort
  | T_mutez -> Smt.create_int_sort
  | T_bool -> Smt.create_bool_sort
  | T_key_hash -> Smt.create_string_sort
  | T_timestamp -> Smt.create_string_sort
  | T_address -> Smt.create_string_sort
end

and sort_of_inner_type : Smt.typ -> Smt.z_sort list
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

and create_convert_vformula : Vlang.v_formula -> Smt.z_expr
=fun vf -> begin
  try
    match vf with
    | VF_true -> Smt.create_bool_true
    | VF_false -> Smt.create_bool_false
    | VF_not f -> Smt.create_bool_not (create_convert_vformula f)
    | VF_and fl -> begin
        let formulas = Core.List.map fl ~f:create_convert_vformula in
        Smt.create_bool_and formulas
      end
    | VF_or fl -> begin
        let formulas = Core.List.map fl ~f:create_convert_vformula in
        Smt.create_bool_or formulas
      end
    | VF_uni_rel (vur, e) -> begin
        let e' = create_convert_vexp e in
        match vur with
        | VF_is_true -> Smt.create_bool_eq e' (Smt.create_bool_true)
        | VF_is_none -> Smt.create_bool_option_is_none e'
        | VF_is_left -> Smt.create_bool_option_is_left e'
        | VF_is_cons -> Smt.create_bool_list_is_cons e'
      end
    | VF_bin_rel (vbr, e1, e2) -> begin
        let e1', e2' = ((create_convert_vexp e1), (create_convert_vexp e2)) in
        match vbr with
        | VF_eq -> Smt.create_bool_eq e1' e2'
        | VF_neq -> Smt.create_bool_not (Smt.create_bool_eq e1' e2')
        | VF_lt -> Smt.create_bool_int_lt e1' e2'
        | VF_le -> Smt.create_bool_int_le e1' e2'
        | VF_gt -> Smt.create_bool_int_gt e1' e2'
        | VF_ge -> Smt.create_bool_int_ge e1' e2'
      end
    | VF_imply (f1, f2) -> Smt.create_bool_imply (create_convert_vformula f1) (create_convert_vformula f2)
    | VF_iff (f1, f2) -> Smt.create_bool_iff (create_convert_vformula f1) (create_convert_vformula f2)
    | VF_forall (vl, f) -> begin
        let vl' = Core.List.map vl ~f:(fun (v, t) -> Smt.read_var (Smt.create_symbol v) (sort_of_typt t)) in
        let f' = create_convert_vformula f in
        Smt.create_forall vl' f'
      end
  with
  | Smt.Z3Error s -> raise (Failure ("Verifier.create_convert_vformula (" ^ (Vlang.string_of_formula vf) ^ "): " ^ s))
  | err -> raise err
end

and create_convert_vexp : Vlang.v_exp -> Smt.z_expr
=fun ve -> begin
  try
    let get_nth = Core.List.nth_exn in
    match ve with
    | VE_int n -> Smt.create_int_from_zarith n
    | VE_string s -> Smt.create_string s
    | VE_bool f -> create_convert_vformula f
    | VE_unit -> Smt.create_unit
    | VE_none t -> Smt.create_option (get_nth (sort_of_inner_type t) 0) None
    | VE_uni_cont (vuc, e1, t) -> begin
        let inner_sorts = sort_of_inner_type t in
        match vuc with
        | VE_left -> begin
            let right_dummy_sort = get_nth inner_sorts 1 in
            Smt.create_or right_dummy_sort (Left (create_convert_vexp e1))
          end
        | VE_right -> begin
            let left_dummy_sort = get_nth inner_sorts 0 in
            Smt.create_or left_dummy_sort (Right (create_convert_vexp e1))
          end
        | VE_some -> begin
            let item_sort = get_nth inner_sorts 0 in
            Smt.create_option item_sort (Some (create_convert_vexp e1))
          end
      end
    | VE_bin_cont (vbc, e1, e2, t) -> begin
        match vbc with
        | VE_pair -> Smt.create_pair (create_convert_vexp e1) (create_convert_vexp e2)
        | VE_elt -> begin
            let map = Smt.create_map (sort_of_typt t) in
            Smt.update_map map (create_convert_vexp e1) (create_convert_vexp e2)
          end
      end
    | VE_list (vel, t) -> begin
        let nil = Smt.create_list (get_nth (sort_of_inner_type t) 0) in
        Core.List.fold_right vel ~f:(fun e l -> Smt.update_list_cons (create_convert_vexp e) l) ~init:nil
      end
    | VE_var (v, t) -> Smt.read_var (Smt.create_symbol v) (sort_of_typt t)
    | VE_read (e1, e2) -> Smt.read_map (create_convert_vexp e2) (create_convert_vexp e1)
    | VE_write (e1, e2, e3) -> Smt.update_map (create_convert_vexp e3) (create_convert_vexp e1) (create_convert_vexp e2)
    | VE_nul_op (vno, t) -> begin
        match vno with
        | VE_self -> Smt.read_var (Smt.create_symbol "self") (sort_of_typt t)
        | VE_now -> Smt.read_var (Smt.create_symbol "now") (sort_of_typt t)
        | VE_amount -> Smt.read_var (Smt.create_symbol "amount") (sort_of_typt t)
        | VE_balance -> Smt.read_var (Smt.create_symbol "balance") (sort_of_typt t)
        | VE_steps_to_quota -> Smt.read_var (Smt.create_symbol "steps_to_quota") (sort_of_typt t)
        | VE_source -> Smt.read_var (Smt.create_symbol "source") (sort_of_typt t)
        | VE_sender -> Smt.read_var (Smt.create_symbol "sender") (sort_of_typt t)
        | VE_chain_id -> Smt.read_var (Smt.create_symbol "chain_id") (sort_of_typt t)
      end
    | VE_uni_op (vuo, e1, t) -> begin
        let zero = Smt.create_int 0 in
        match vuo with
        | VE_car -> Smt.read_pair_fst (create_convert_vexp e1)
        | VE_cdr -> Smt.read_pair_snd (create_convert_vexp e1)
        | VE_abs -> begin
            let ze1 = create_convert_vexp e1 in
            Smt.create_ite (Smt.create_bool_int_ge ze1 zero) ze1 (Smt.create_int_neg ze1)
          end
        | VE_neg -> Smt.create_int_neg (create_convert_vexp e1)
        | VE_not -> Smt.create_bool_not (create_convert_vexp e1)
        | VE_eq -> Smt.create_bool_eq (create_convert_vexp e1) zero
        | VE_neq -> Smt.create_bool_not (Smt.create_bool_eq (create_convert_vexp e1) zero)
        | VE_lt -> Smt.create_bool_int_lt (create_convert_vexp e1) zero
        | VE_gt -> Smt.create_bool_int_gt (create_convert_vexp e1) zero
        | VE_leq -> Smt.create_bool_int_le (create_convert_vexp e1) zero
        | VE_geq -> Smt.create_bool_int_ge (create_convert_vexp e1) zero
        | VE_cast -> Smt.create_dummy_expr (sort_of_typt t)
        | VE_list_concat -> Smt.create_string_concat [(create_convert_vexp e1)]
        | VE_pack -> Smt.create_dummy_expr (sort_of_typt t)
        | VE_unpack -> Smt.create_dummy_expr (sort_of_typt t)
        | VE_contract -> Smt.create_dummy_expr (sort_of_typt t)
        | VE_account -> Smt.create_dummy_expr (sort_of_typt t)
        | VE_blake2b -> Smt.create_dummy_expr (sort_of_typt t)
        | VE_sha256 -> Smt.create_dummy_expr (sort_of_typt t)
        | VE_sha512 -> Smt.create_dummy_expr (sort_of_typt t)
        | VE_hash_key -> Smt.create_dummy_expr (sort_of_typt t)
        | VE_address -> Smt.create_dummy_expr (sort_of_typt t)
        | VE_un_opt -> Smt.read_option_content (create_convert_vexp e1)
        | VE_un_left -> Smt.read_or_left_content (create_convert_vexp e1)
        | VE_un_right -> Smt.read_or_right_content (create_convert_vexp e1)
        | VE_hd -> Smt.read_list_head (create_convert_vexp e1)
        | VE_tl -> Smt.read_list_tail (create_convert_vexp e1)
        | VE_size -> Smt.create_dummy_expr (sort_of_typt t)
        | VE_isnat -> Smt.create_dummy_expr (sort_of_typt t)
        | VE_to_int -> Smt.create_dummy_expr (sort_of_typt t)
      end
    | VE_bin_op (vbo, e1, e2, t) -> begin
        let zero = Smt.create_int 0 in
        let two = Smt.create_int 2 in
        match vbo with
        | VE_add -> Smt.create_int_add [(create_convert_vexp e1); (create_convert_vexp e2)]
        | VE_sub -> Smt.create_int_sub [(create_convert_vexp e1); (create_convert_vexp e2)]
        | VE_mul -> Smt.create_int_mul [(create_convert_vexp e1); (create_convert_vexp e2)]
        | VE_ediv -> begin
            let dividend, divisor = ((create_convert_vexp e1), (create_convert_vexp e2)) in
            let x = Smt.create_pair (Smt.create_int_div dividend divisor) (Smt.create_int_mod dividend divisor) in
            let sort_of_x = Smt.read_sort_of_expr x in
            Smt.create_ite (Smt.create_bool_eq divisor zero) (Smt.create_option sort_of_x None) (Smt.create_option sort_of_x (Some x))
          end
        | VE_div -> Smt.create_int_div (create_convert_vexp e1) (create_convert_vexp e2)
        | VE_mod -> Smt.create_int_mod (create_convert_vexp e1) (create_convert_vexp e2)
        | VE_lsl -> begin
            let bin_exp = Smt.create_int_power two (create_convert_vexp e2) in
            Smt.create_int_mul [(create_convert_vexp e1); bin_exp]
          end
        | VE_lsr -> begin
            let bin_exp = Smt.create_int_power two (create_convert_vexp e2) in
            Smt.create_int_div (create_convert_vexp e1) bin_exp
          end
        | VE_and -> Smt.create_bool_and [(create_convert_vexp e1); (create_convert_vexp e2)]
        | VE_or -> Smt.create_bool_or [(create_convert_vexp e1); (create_convert_vexp e2)]
        | VE_xor -> Smt.create_bool_xor (create_convert_vexp e1) (create_convert_vexp e2)
        | VE_cmp -> Smt.create_cmp (create_convert_vexp e1) (create_convert_vexp e2)
        | VE_cons -> Smt.update_list_cons (create_convert_vexp e1) (create_convert_vexp e2)
        | VE_concat -> Smt.create_string_concat [(create_convert_vexp e1); (create_convert_vexp e2)]
        | VE_exec -> Smt.create_dummy_expr (sort_of_typt t)
        | VE_append -> Smt.create_dummy_expr (sort_of_typt t)
        | VE_get -> begin
            let key, map = ((create_convert_vexp e1), (create_convert_vexp e2)) in
            let item = Smt.read_map map key in
            let sort_of_item = Smt.read_sort_of_expr item in
            let default_item = Smt.read_default_term map in
            Smt.create_ite (Smt.create_bool_eq item default_item) (Smt.create_option sort_of_item None) (Smt.create_option sort_of_item (Some item))
          end
        | VE_mem -> begin
            let key, map = ((create_convert_vexp e1), (create_convert_vexp e2)) in
            let item = Smt.read_map map key in
            let default_item = Smt.read_default_term map in
            Smt.create_bool_eq item default_item
          end
      end
    | VE_ter_op (vto, e1, e2, e3, t) -> begin
        match vto with
        | VE_slice -> begin
            let offset, length, s = ((create_convert_vexp e1), (create_convert_vexp e2), (create_convert_vexp e3)) in
            Smt.create_string_slice s offset (Smt.create_int_add [offset; length])
          end
        | VE_check_signature -> Smt.create_dummy_expr (sort_of_typt t)
        | VE_update -> begin
            let key, value, map = ((create_convert_vexp e1), (create_convert_vexp e2), (create_convert_vexp e3)) in
            let default_item = Smt.read_default_term map in
            Smt.create_ite (Smt.create_bool_option_is_none value) (Smt.update_map map key default_item) (Smt.update_map map key (Smt.read_option_content value))
          end
      end
    | VE_lambda t -> Smt.create_dummy_expr (sort_of_typt t)
    | VE_operation (_, t) -> Smt.create_dummy_expr (sort_of_typt t)
  with
  | Smt.Z3Error s -> raise (Failure ("Verifier.create_convert_vexp (" ^ (Vlang.string_of_exp ve) ^ "): " ^ s))
  | err -> print_endline (Vlang.string_of_exp ve); raise err
end

and create_param_storage_from_model : Smt.model -> Pre.Lib.Cfg.t -> (Smt.z_expr * Smt.z_expr) option
=fun m cfg -> begin
  let param_storage_var = "param_storage" in
  let param_storage_sort = sort_of_typt (Pre.Lib.Cfg.CPMap.find_exn cfg.type_info param_storage_var) in
  let param_storage = Smt.read_var (Smt.create_symbol param_storage_var) param_storage_sort in
  let param, storage = ((Smt.read_pair_fst param_storage), (Smt.read_pair_snd param_storage)) in
  let param_expr_opt, storage_expr_opt = ((Smt.create_evaluation m param), (Smt.create_evaluation m storage)) in
  match (param_expr_opt, storage_expr_opt) with
  | Some param_expr, Some storage_expr -> Some (param_expr, storage_expr)
  | _, _ -> None
end