(*
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
  | T_map (t1, t2) -> Smt.create_map_sort ~elt_sort:(Smt.create_elt_sort ~key_sort:(sort_of_typt t1) ~value_sort:(sort_of_typt t2))
  | T_big_map (t1, t2) -> Smt.create_map_sort ~elt_sort:(Smt.create_elt_sort ~key_sort:(sort_of_typt t1) ~value_sort:(sort_of_typt t2))
  | T_chain_id -> Smt.create_string_sort
  | T_int -> Smt.create_int_sort
  | T_nat -> Smt.create_int_sort
  | T_string -> Smt.create_string_sort
  | T_bytes -> Smt.create_string_sort
  | T_mutez -> Smt.create_mutez_sort
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
    | VF_uni_rel (vur, o1) -> begin
        let ze1 = create_convert_vobj o1 in
        match vur with
        | VF_is_true -> Smt.create_bool_eq ze1 (Smt.create_bool_true)
        | VF_is_none -> Smt.create_bool_option_is_none ze1
        | VF_is_left -> Smt.create_bool_option_is_left ze1
        | VF_is_cons -> Smt.create_bool_list_is_cons ze1
      end
    | VF_bin_rel (vbr, o1, o2) -> begin
        let ze1, ze2 = ((create_convert_vobj o1), (create_convert_vobj o2)) in
        match o1.typ.d, o2.typ.d with
        | T_int, T_int
        | T_nat, T_nat -> begin
            match vbr with
            | VF_eq -> Smt.create_bool_eq ze1 ze2
            | VF_neq -> Smt.create_bool_not (Smt.create_bool_eq ze1 ze2)
            | VF_lt -> Smt.create_bool_int_lt ze1 ze2
            | VF_le -> Smt.create_bool_int_le ze1 ze2
            | VF_gt -> Smt.create_bool_int_gt ze1 ze2
            | VF_ge -> Smt.create_bool_int_ge ze1 ze2
          end
        | T_mutez, T_mutez -> begin
            match vbr with
            | VF_eq -> Smt.create_bool_eq ze1 ze2
            | VF_neq -> Smt.create_bool_not (Smt.create_bool_eq ze1 ze2)
            | VF_lt -> Smt.create_bool_mutez_lt ~v1:ze1 ~v2:ze2
            | VF_le -> Smt.create_bool_mutez_le ~v1:ze1 ~v2:ze2
            | VF_gt -> Smt.create_bool_mutez_gt ~v1:ze1 ~v2:ze2
            | VF_ge -> Smt.create_bool_mutez_ge ~v1:ze1 ~v2:ze2
          end
        | _, _ -> begin
            match vbr with
            | VF_eq -> Smt.create_bool_eq ze1 ze2
            | VF_neq -> Smt.create_bool_not (Smt.create_bool_eq ze1 ze2)
            | _ -> raise (Failure "Verifier.create_convert_vformula: binary relation error")
          end
      end
    | VF_imply (f1, f2) -> Smt.create_bool_imply (create_convert_vformula f1) (create_convert_vformula f2)
    | VF_iff (f1, f2) -> Smt.create_bool_iff (create_convert_vformula f1) (create_convert_vformula f2)
    | VF_forall (ol, f) -> begin
        let el = Core.List.map ol ~f:(fun o -> (
          match o.exp with
          | VE_var _ -> (create_convert_vobj o)
          | _ -> raise (Failure "Verifier.create_convert_vformula: forall error")
        )) in
        let f' = create_convert_vformula f in
        Smt.create_forall el f'
      end
    | VF_sigma_equal (o1, o2) -> begin
        let ze1, ze2 = ((create_convert_vobj o1), (create_convert_vobj o2)) in
        let sigma = Smt.read_map_sigma ~map:ze1 in
        Smt.create_bool_eq sigma ze2
      end
  with
  | Smt.Z3Error s -> raise (Failure ("Verifier.create_convert_vformula (" ^ (Vlang.string_of_formula vf) ^ "): " ^ s))
  | err -> raise err
end

and create_convert_vobj : Vlang.v_obj -> Smt.z_expr
=fun vo -> begin
  try
    let get_nth = Core.List.nth_exn in
    match vo.exp with
    | VE_int n -> begin
        match vo.typ.d with
        | T_mutez -> Smt.create_mutez_from_zarith ~value:n
        | _ -> Smt.create_int_from_zarith n
      end
    | VE_string s -> Smt.create_string s
    | VE_bool f -> create_convert_vformula f
    | VE_unit -> Smt.create_unit
    | VE_none -> Smt.create_option (get_nth (sort_of_inner_type vo.typ) 0) None
    | VE_uni_cont (vuc, o1) -> begin
        let inner_sorts = sort_of_inner_type vo.typ in
        match vuc with
        | VE_left -> begin
            let right_dummy_sort = get_nth inner_sorts 1 in
            Smt.create_or right_dummy_sort (Left (create_convert_vobj o1))
          end
        | VE_right -> begin
            let left_dummy_sort = get_nth inner_sorts 0 in
            Smt.create_or left_dummy_sort (Right (create_convert_vobj o1))
          end
        | VE_some -> begin
            let item_sort = get_nth inner_sorts 0 in
            Smt.create_option item_sort (Some (create_convert_vobj o1))
          end
      end
    | VE_bin_cont (vbc, o1, o2) -> begin
        match vbc with
        | VE_pair -> Smt.create_pair (create_convert_vobj o1) (create_convert_vobj o2)
        | VE_elt -> Smt.create_elt ~key:(create_convert_vobj o1) ~value:(create_convert_vobj o2)
      end
    | VE_list vol -> begin
        match vo.typ.d with
        | T_list t' -> begin
            let nil = Smt.create_list (sort_of_typt t') in
            Core.List.fold_right vol ~f:(fun o l -> Smt.update_list_cons (create_convert_vobj o) l) ~init:nil
          end
        | T_map (t1', t2') | T_big_map (t1', t2') -> begin
            let key_sort, value_sort = ((sort_of_typt t1'), (sort_of_typt t2')) in
            let nim = Smt.create_map ~key_sort:key_sort ~value_sort:value_sort in
            Core.List.fold_right vol ~f:(fun o m -> begin
              let elt = create_convert_vobj o in
              let key, value = ((Smt.read_elt_key ~elt:elt), (Smt.read_elt_value ~elt:elt)) in
              let value_opt = Smt.create_option value_sort (Some value) in
              Smt.update_map ~key:key ~value_opt:value_opt ~map:m
            end) ~init:nim
          end
        | _ -> raise (Failure ("Verifier.create_convert_vobj: Type error in list creation"))
      end
    | VE_var v -> Smt.read_var (Smt.create_symbol v) (sort_of_typt vo.typ)
    | VE_nul_op vno -> begin
        match vno with
        | VE_self -> Smt.read_var (Smt.create_symbol "self") (sort_of_typt vo.typ)
        | VE_now -> Smt.read_var (Smt.create_symbol "now") (sort_of_typt vo.typ)
        | VE_amount -> Smt.read_var (Smt.create_symbol "amount") (sort_of_typt vo.typ)
        | VE_balance -> Smt.read_var (Smt.create_symbol "balance") (sort_of_typt vo.typ)
        | VE_steps_to_quota -> Smt.read_var (Smt.create_symbol "steps_to_quota") (sort_of_typt vo.typ)
        | VE_source -> Smt.read_var (Smt.create_symbol "source") (sort_of_typt vo.typ)
        | VE_sender -> Smt.read_var (Smt.create_symbol "sender") (sort_of_typt vo.typ)
        | VE_chain_id -> Smt.read_var (Smt.create_symbol "chain_id") (sort_of_typt vo.typ)
      end
    | VE_uni_op (vuo, o1) -> begin
        let zero = Smt.create_int 0 in
        match vuo with
        | VE_car -> Smt.read_pair_fst (create_convert_vobj o1)
        | VE_cdr -> Smt.read_pair_snd (create_convert_vobj o1)
        | VE_abs -> begin
            let ze1 = create_convert_vobj o1 in
            Smt.create_ite (Smt.create_bool_int_ge ze1 zero) ze1 (Smt.create_int_neg ze1)
          end
        | VE_neg -> Smt.create_int_neg (create_convert_vobj o1)
        | VE_not -> Smt.create_bool_not (create_convert_vobj o1)
        | VE_eq -> Smt.create_bool_eq (create_convert_vobj o1) zero
        | VE_neq -> Smt.create_bool_not (Smt.create_bool_eq (create_convert_vobj o1) zero)
        | VE_lt -> Smt.create_bool_int_lt (create_convert_vobj o1) zero
        | VE_gt -> Smt.create_bool_int_gt (create_convert_vobj o1) zero
        | VE_leq -> Smt.create_bool_int_le (create_convert_vobj o1) zero
        | VE_geq -> Smt.create_bool_int_ge (create_convert_vobj o1) zero
        | VE_cast -> Smt.create_dummy_expr (sort_of_typt vo.typ)
        | VE_list_concat -> Smt.create_string_concat [(create_convert_vobj o1)]
        | VE_pack -> Smt.create_dummy_expr (sort_of_typt vo.typ)
        | VE_unpack -> Smt.create_dummy_expr (sort_of_typt vo.typ)
        | VE_contract -> Smt.create_dummy_expr (sort_of_typt vo.typ)
        | VE_account -> Smt.create_dummy_expr (sort_of_typt vo.typ)
        | VE_blake2b -> Smt.create_dummy_expr (sort_of_typt vo.typ)
        | VE_sha256 -> Smt.create_dummy_expr (sort_of_typt vo.typ)
        | VE_sha512 -> Smt.create_dummy_expr (sort_of_typt vo.typ)
        | VE_hash_key -> Smt.create_dummy_expr (sort_of_typt vo.typ)
        | VE_address -> Smt.create_dummy_expr (sort_of_typt vo.typ)
        | VE_un_opt -> Smt.read_option_content (create_convert_vobj o1)
        | VE_un_left -> Smt.read_or_left_content (create_convert_vobj o1)
        | VE_un_right -> Smt.read_or_right_content (create_convert_vobj o1)
        | VE_hd -> Smt.read_list_head (create_convert_vobj o1)
        | VE_tl -> Smt.read_list_tail (create_convert_vobj o1)
        | VE_size -> Smt.create_dummy_expr (sort_of_typt vo.typ)
        | VE_isnat -> Smt.create_dummy_expr (sort_of_typt vo.typ)
        | VE_to_int -> Smt.create_dummy_expr (sort_of_typt vo.typ)
      end
    | VE_bin_op (vbo, o1, o2) -> begin
        let zero = Smt.create_int 0 in
        let two = Smt.create_int 2 in
        match vbo with
        | VE_add -> begin
            match vo.typ.d with
            | T_mutez -> Smt.create_mutez_add ~v1:(create_convert_vobj o1) ~v2:(create_convert_vobj o2)
            | T_nat | T_int -> Smt.create_int_add [(create_convert_vobj o1); (create_convert_vobj o2)]
            | _ -> raise (Failure "Wrong Type")
          end
        | VE_sub -> begin
            match vo.typ.d with
            | T_mutez -> Smt.create_mutez_sub ~v1:(create_convert_vobj o1) ~v2:(create_convert_vobj o2)
            | T_nat | T_int -> Smt.create_int_sub [(create_convert_vobj o1); (create_convert_vobj o2)]
            | _ -> raise (Failure "Wrong Type")
          end
        | VE_mul -> begin
            match vo.typ.d with
            | T_mutez -> Smt.create_mutez_mul ~v1:(create_convert_vobj o1) ~v2:(create_convert_vobj o2)
            | T_nat | T_int -> Smt.create_int_mul [(create_convert_vobj o1); (create_convert_vobj o2)]
            | _ -> raise (Failure "Wrong Type")
          end
        | VE_ediv -> begin
            let dividend, divisor = ((create_convert_vobj o1), (create_convert_vobj o2)) in
            let x = Smt.create_pair (Smt.create_int_div dividend divisor) (Smt.create_int_mod dividend divisor) in
            let sort_of_x = Smt.read_sort_of_expr x in
            Smt.create_ite (Smt.create_bool_eq divisor zero) (Smt.create_option sort_of_x None) (Smt.create_option sort_of_x (Some x))
          end
        | VE_div -> begin
            match vo.typ.d with
            | T_mutez -> Smt.create_mutez_div ~v1:(create_convert_vobj o1) ~v2:(create_convert_vobj o2)
            | T_nat | T_int -> Smt.create_int_div (create_convert_vobj o1) (create_convert_vobj o2)
            | _ -> raise (Failure "Wrong Type")
          end
        | VE_mod -> begin
            match vo.typ.d with
            | T_mutez -> Smt.create_mutez_div ~v1:(create_convert_vobj o1) ~v2:(create_convert_vobj o2)
            | T_nat | T_int -> Smt.create_int_mod (create_convert_vobj o1) (create_convert_vobj o2)
            | _ -> raise (Failure "Wrong Type")
          end
        | VE_lsl -> begin
            let bin_exp = Smt.create_int_power two (create_convert_vobj o2) in
            Smt.create_int_mul [(create_convert_vobj o1); bin_exp]
          end
        | VE_lsr -> begin
            let bin_exp = Smt.create_int_power two (create_convert_vobj o2) in
            Smt.create_int_div (create_convert_vobj o1) bin_exp
          end
        | VE_and -> Smt.create_bool_and [(create_convert_vobj o1); (create_convert_vobj o2)]
        | VE_or -> Smt.create_bool_or [(create_convert_vobj o1); (create_convert_vobj o2)]
        | VE_xor -> Smt.create_bool_xor (create_convert_vobj o1) (create_convert_vobj o2)
        | VE_cmp -> begin
            match o1.typ.d, o2.typ.d with
            | T_int, T_int | T_nat, T_nat -> Smt.create_int_cmp ~v1:(create_convert_vobj o1) ~v2:(create_convert_vobj o2)
            | T_mutez, T_mutez -> Smt.create_mutez_cmp ~v1:(create_convert_vobj o1) ~v2:(create_convert_vobj o2)
            | T_string, T_string | T_address, T_address -> Smt.create_string_cmp ~v1:(create_convert_vobj o1) ~v2:(create_convert_vobj o2)
            | _, _ -> raise (Failure ("Verifier.create_convert_vobj (" ^ (Vlang.string_of_exp vo.exp) ^ "): (" ^ (Pre.Lib.Mich.string_of_typt o1.typ) ^ ") & (" ^ (Pre.Lib.Mich.string_of_typt o2.typ) ^ ") is not supported."))
          end
        | VE_cons -> Smt.update_list_cons (create_convert_vobj o1) (create_convert_vobj o2)
        | VE_concat -> Smt.create_string_concat [(create_convert_vobj o1); (create_convert_vobj o2)]
        | VE_exec -> Smt.create_dummy_expr (sort_of_typt vo.typ)
        | VE_append -> Smt.create_dummy_expr (sort_of_typt vo.typ)
        | VE_get -> Smt.read_map_elt_content ~key:(create_convert_vobj o1) ~map:(create_convert_vobj o2)
        | VE_mem -> Smt.read_map_elt_exists ~key:(create_convert_vobj o1) ~map:(create_convert_vobj o2)
      end
    | VE_ter_op (vto, o1, o2, o3) -> begin
        match vto with
        | VE_slice -> begin
            let offset, length, s = ((create_convert_vobj o1), (create_convert_vobj o2), (create_convert_vobj o3)) in
            Smt.create_string_slice s offset (Smt.create_int_add [offset; length])
          end
        | VE_check_signature -> Smt.create_dummy_expr (sort_of_typt vo.typ)
        | VE_update -> Smt.update_map ~key:(create_convert_vobj o1) ~value_opt:(create_convert_vobj o2) ~map:(create_convert_vobj o3)
      end
    | VE_lambda -> Smt.create_dummy_expr (sort_of_typt vo.typ)
    | VE_operation _ -> Smt.create_dummy_expr (sort_of_typt vo.typ)
  with
  | Smt.Z3Error s -> begin
      raise (Failure ("Verifier.create_convert_vobj:\n" ^
                      "  Expr: (" ^ (Vlang.string_of_exp vo.exp) ^ ") \n" ^
                      "  Type: (" ^ (Pre.Lib.Mich.string_of_typt vo.typ) ^ ") \n" ^
                      "  Msg: " ^ s))
    end
  | err -> print_endline (Vlang.string_of_exp vo.exp); raise err
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
*)