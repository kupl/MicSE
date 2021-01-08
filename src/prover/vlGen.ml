(* Vlang generator (from Cfg Stmt & Cfg Expression). 
    This module does not consider any variable renaming issues *)

exception InvalidConversion_Expr of PreLib.Cfg.expr

let read_type_cfgvar : PreLib.Cfg.t -> PreLib.Cfg.ident -> ProverLib.Vlang.typ
=fun cfg v -> begin
  try
    let mich_ty = PreLib.Cfg.t_map_find ~errtrace:("VlGen.read_type_cfgvar : " ^ v) cfg.type_info v in
    ProverLib.Vlang.TypeUtil.ty_of_mty mich_ty
  with 
  | Stdlib.Failure _ -> (
      (* check if the variable is global-variable *)
      let ps_typ = PreLib.Cfg.t_map_find ~errtrace:("VlGen.read_type_cfgvar : pstyp : " ^ v) cfg.type_info PreLib.Cfg.param_storage_name |> ProverLib.Vlang.TypeUtil.ty_of_mty in
      let (p_typ, s_typ) = ProverLib.Vlang.TypeUtil.get_innertyp2 ps_typ in
      if ProverLib.GlVar.is_param_var v then p_typ else
      if ProverLib.GlVar.is_storage_var v then s_typ else
      if ProverLib.GlVar.is_amount_var v || ProverLib.GlVar.is_balance_var v then ProverLib.Vlang.Ty.T_mutez else
      if ProverLib.GlVar.is_sender_var v || ProverLib.GlVar.is_source_var v then ProverLib.Vlang.Ty.T_address else
      Stdlib.failwith ("VlGen.read_type_cfgvar : all-match-failed with variable : " ^ v)
    )
end

let create_var_of_cfgvar : ProverLib.GlVar.Env.t ref -> PreLib.Cfg.t -> PreLib.Cfg.ident -> ProverLib.Vlang.Expr.t
= let open ProverLib.Vlang.Expr in
  fun glenv_ref cfg v -> 
  if v = PreLib.Cfg.param_storage_name then (
    (* "param_storage" variable treated specially. *)
    let (p_typ, s_typ) = ProverLib.Vlang.TypeUtil.get_innertyp2 (read_type_cfgvar cfg v) in
    V_pair (V_var (p_typ, !glenv_ref.gv_param), V_var (s_typ, !glenv_ref.gv_storage))
  )
  else V_var (read_type_cfgvar cfg v, v)

let rec create_expr_of_michdata_i : PreLib.Mich.data -> ProverLib.Vlang.typ -> ProverLib.Vlang.Expr.t = 
  let open PreLib.Mich in
  let open ProverLib.Vlang.Ty in
  let open ProverLib.Vlang.Expr in
  let cem = create_expr_of_michdata in (* syntax sugar *)
  (fun michdata vtyp ->
    match (vtyp, michdata) with 
    | T_int, D_int zn -> V_lit_int zn
    | T_nat, D_int zn -> V_lit_nat zn
    | T_mutez, D_int zn -> V_lit_mutez zn
    | T_timestamp, D_int zn -> V_lit_timestamp_sec zn
    | T_string, D_string s -> V_lit_string s
    | T_key_hash, D_string s -> V_lit_key_hash s
    | T_timestamp, D_string s -> V_lit_timestamp_str s
    | T_address, D_string s -> V_lit_address (V_lit_key_hash s)
    | T_key, D_string s -> V_lit_key s
    | T_bytes, D_bytes s -> V_lit_bytes s
    | T_chain_id, D_bytes s -> V_lit_chain_id s
    | T_unit, D_unit -> V_unit
    | T_bool, D_bool b -> V_lit_bool b
    | T_pair (t1, t2), D_pair (d1, d2) -> V_pair (cem d1 t1, cem d2 t2)
    | T_or (t1, _), D_left d -> V_left (vtyp, cem d t1)
    | T_or (_, t2), D_right d -> V_right (vtyp, cem d t2)
    | T_option t, D_some d -> V_some (cem d t)
    | T_option t, D_none -> V_none t
    | T_list elt, D_list dlist -> V_lit_list (elt, List.map (fun x -> cem x elt) dlist)
    | T_set elt, D_list dlist -> V_lit_set (elt, List.fold_left (fun acc x -> Core.Set.Poly.add acc (cem x elt)) Core.Set.Poly.empty dlist)
    | T_map (kt, vt), D_list dlist -> 
      let errmsg : string = "Prover.Converter.create_expr_of_michdata_i : (T_map, D_list)" in
      V_lit_map (kt, vt, (List.fold_left (fun acc x -> (match (PreLib.Mich.get_d x) with | D_elt (k, v) -> (Core.Map.Poly.add acc ~key:(cem k kt) ~data:(cem v vt) |> (function | `Ok m -> m | `Duplicate -> acc)) | _ -> Stdlib.failwith errmsg)) Core.Map.Poly.empty dlist))
    | T_big_map (kt, vt), D_list dlist -> 
      let errmsg : string = "Prover.Converter.create_expr_of_michdata_i : (T_map, D_list)" in
      V_lit_big_map (kt, vt, (List.fold_left (fun acc x -> (match (PreLib.Mich.get_d x) with | D_elt (k, v) -> (Core.Map.Poly.add acc ~key:(cem k kt) ~data:(cem v vt) |> (function | `Ok m -> m | `Duplicate -> acc)) | _ -> Stdlib.failwith errmsg)) Core.Map.Poly.empty dlist))
    | _, D_elt _ -> Stdlib.failwith "Prover.Converter.create_expr_of_michdata_i : (_, D_elt)"
    | T_lambda (t1, t2), D_lambda it -> V_lit_lambda (t1, t2, it)
    | T_contract t, D_string s -> V_contract_of_address (t, (V_lit_address (V_lit_key_hash s)))
    | _ -> Stdlib.failwith ("Prover.Converter.create_expr_of_michdata_i : match failed between type [" ^ (vtyp |> ProverLib.Vlang.Ty.to_string) ^ "] and [" ^ (PreLib.Mich.gen_t michdata |> Pre.Lib.Mich.string_of_datat_ol) ^ "]")
  )
and create_expr_of_michdata : PreLib.Mich.data PreLib.Mich.t -> ProverLib.Vlang.typ -> ProverLib.Vlang.Expr.t
= fun michdata_t vtyp -> begin
  create_expr_of_michdata_i (PreLib.Mich.get_d michdata_t) vtyp
end (* function create_expr_of_michdata end *)

let expr_of_cfgexpr : ProverLib.GlVar.Env.t ref -> PreLib.Cfg.t -> PreLib.Cfg.expr -> ProverLib.Vlang.Expr.t
= let open PreLib.Cfg in
  let open ProverLib.Vlang.Expr in
  let module CPMap = Core.Map.Poly in
  fun glenv_ref cfg cfgexpr -> begin
  let cvt : PreLib.Cfg.ident -> ProverLib.Vlang.typ = read_type_cfgvar cfg in
  let cvf : PreLib.Cfg.ident -> ProverLib.Vlang.Expr.t = create_var_of_cfgvar glenv_ref cfg in
  let err (): 'a = InvalidConversion_Expr cfgexpr |> raise in
  (match cfgexpr with 
  | E_push (d, t) -> create_expr_of_michdata d (ProverLib.Vlang.TypeUtil.ty_of_mty t)
  | E_car v -> (match cvt v with | T_pair _ -> V_car (cvf v) | _ -> err ())
  | E_cdr v -> (match cvt v with | T_pair _ -> V_cdr (cvf v) | _ -> err ())
  | E_abs v -> (match cvt v with | T_int -> V_abs_in (cvf v) | _ -> err ())
  | E_neg v -> (match cvt v with | T_nat -> V_neg_ni (cvf v) | T_int -> V_neg_ii (cvf v) | _ -> err ())
  | E_not v -> (match cvt v with | T_nat -> V_not_ni (cvf v) | T_int -> V_not_ii (cvf v) | T_bool -> V_not_bb (cvf v) | _ -> err ())
  | E_add (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in
      match cvt v1, cvt v2 with 
      | T_nat, T_int -> V_add_nii (vv1, vv2)
      | T_int, T_nat -> V_add_ini (vv1, vv2)
      | T_int, T_int -> V_add_iii (vv1, vv2)
      | T_nat, T_nat -> V_add_nnn (vv1, vv2)
      | T_mutez, T_mutez -> V_add_mmm (vv1, vv2)
      | T_timestamp, T_int -> V_add_tit (vv1, vv2)
      | T_int, T_timestamp -> V_add_itt (vv1, vv2)
      | _ -> err ()
    )
  | E_sub (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in
      match cvt v1, cvt v2 with
      | T_nat, T_nat -> V_sub_nni (vv1, vv2)
      | T_nat, T_int -> V_sub_nii (vv1, vv2)
      | T_int, T_nat -> V_sub_ini (vv1, vv2)
      | T_int, T_int -> V_sub_iii (vv1, vv2)
      | T_timestamp, T_timestamp -> V_sub_tti (vv1, vv2)
      | T_mutez, T_mutez -> V_sub_mmm (vv1, vv2)
      | T_timestamp, T_int -> V_sub_tit (vv1, vv2)
      | _ -> err ()
    )
  | E_mul (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in
      match cvt v1, cvt v2 with
      | T_nat, T_int -> V_mul_nii (vv1, vv2)
      | T_int, T_nat -> V_mul_ini (vv1, vv2)
      | T_int, T_int -> V_mul_iii (vv1, vv2)
      | T_nat, T_nat -> V_mul_nnn (vv1, vv2)
      | T_mutez, T_nat -> V_mul_mnm (vv1, vv2)
      | T_nat, T_mutez -> V_mul_nmm (vv1, vv2)
      | _ -> err ()
    )
  | E_ediv (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in
      match cvt v1, cvt v2 with
      | T_nat, T_nat -> V_ediv_nnnn (vv1, vv2)
      | T_nat, T_int -> V_ediv_niin (vv1, vv2)
      | T_int, T_nat -> V_ediv_inin (vv1, vv2)
      | T_int, T_int -> V_ediv_iiin (vv1, vv2)
      | T_mutez, T_nat -> V_ediv_mnmm (vv1, vv2)
      | T_mutez, T_mutez -> V_ediv_mmnm (vv1, vv2)
      | _ -> err ()
    )
  | E_shiftL (v1, v2) -> (match cvt v1, cvt v2 with | T_nat, T_nat -> V_shiftL_nnn (cvf v1, cvf v2) | _ -> err ())
  | E_shiftR (v1, v2) -> (match cvt v1, cvt v2 with | T_nat, T_nat -> V_shiftR_nnn (cvf v1, cvf v2) | _ -> err ())
  | E_and (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in
      match cvt v1, cvt v2 with
      | T_nat, T_nat -> V_and_nnn (vv1, vv2)
      | T_int, T_nat -> V_and_inn (vv1, vv2)
      | T_bool, T_bool -> V_and_bbb (vv1, vv2)
      | _ -> err ()
    )
  | E_or (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in
      match cvt v1, cvt v2 with
      | T_nat, T_nat -> V_or_nnn (vv1, vv2)
      | T_bool, T_bool -> V_or_bbb (vv1, vv2)
      | _ -> err ()
    )
  | E_xor (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in
      match cvt v1, cvt v2 with
      | T_nat, T_nat -> V_xor_nnn (vv1, vv2)
      | T_bool, T_bool -> V_xor_bbb (vv1, vv2)
      | _ -> err ()
    )
  | E_eq v -> (match cvt v with | T_int -> V_eq_ib (cvf v) | _ -> err ())
  | E_neq v -> (match cvt v with | T_int -> V_neq_ib (cvf v) | _ -> err ())
  | E_lt v -> (match cvt v with | T_int -> V_lt_ib (cvf v) | _ -> err ())
  | E_gt v -> (match cvt v with | T_int -> V_gt_ib (cvf v) | _ -> err ())
  | E_leq v -> (match cvt v with | T_int -> V_leq_ib (cvf v) | _ -> err ())
  | E_geq v -> (match cvt v with | T_int -> V_geq_ib (cvf v) | _ -> err ())
  | E_compare (v1, v2) -> (if cvt v1 = cvt v2 then V_compare (cvf v1, cvf v2) else err ())
  | E_cons (v1, v2) -> (match cvt v1, cvt v2 with | elt1, T_list elt2 when elt1 = elt2 -> V_cons (cvf v1, cvf v2) | _ -> err ())
  | E_operation op -> (
      (* TODO : check variable types! *)
      match op with
      | O_create_contract (p, v1, v2, v3) -> V_create_contract ((ProverLib.Vlang.TypeUtil.ty_of_mty p.PreLib.Mich.param), (ProverLib.Vlang.TypeUtil.ty_of_mty p.PreLib.Mich.storage), V_lit_program (p), cvf v1, cvf v2, cvf v3)
      | O_transfer_tokens (v1, v2, v3) -> V_transfer_tokens (cvf v1, cvf v2, cvf v3)
      | O_set_delegate v -> V_set_delegate (cvf v)
      | O_create_account _ -> err () (* deprecated operation *)
    )
  | E_unit -> V_unit
  | E_pair (v1, v2) -> V_pair (cvf v1, cvf v2)
  | E_left (v, t) -> let tt = ProverLib.Vlang.TypeUtil.ty_of_mty t in (match tt, cvt v with | T_or (lt1, _), lt2 when lt1 = lt2 -> V_left (tt, cvf v) | _ -> err ())
  | E_right (v, t) -> let tt = ProverLib.Vlang.TypeUtil.ty_of_mty t in (match tt, cvt v with | T_or (_, lt1), lt2 when lt1 = lt2 -> V_right (tt, cvf v) | _ -> err ())
  | E_some v -> V_some (cvf v)
  | E_none t -> V_none (ProverLib.Vlang.TypeUtil.ty_of_mty t)
  | E_mem (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in
      match cvt v1, cvt v2 with
      | elt1, T_set elt2 when elt1 = elt2 -> V_mem_xsb (vv1, vv2)
      | kt1, T_map (kt2, _) when kt1 = kt2 -> V_mem_xmb (vv1, vv2)
      | kt1, T_big_map (kt2, _) when kt1 = kt2 -> V_mem_xbmb (vv1, vv2)
      | _ -> err ()
    )
  | E_get (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in
      match cvt v1, cvt v2 with
      | kt1, T_map (kt2, _) when kt1 = kt2 -> V_get_xmoy (vv1, vv2)
      | kt1, T_big_map (kt2, _) when kt1 = kt2 -> V_get_xbmo (vv1, vv2)
      | _ -> err ()
    )
  | E_update (v1, v2, v3) -> (
      let vv1, vv2, vv3 = cvf v1, cvf v2, cvf v3 in
      match cvt v1, cvt v2, cvt v3 with
      | t1, T_bool, T_set t2 when t1 = t2 -> V_update_xbss (vv1, vv2, vv3)
      | kt1, T_option vt1, T_map (kt2, vt2) when kt1 = kt2 && vt1 = vt2 -> V_update_xomm (vv1, vv2, vv3)
      | kt1, T_option vt1, T_big_map (kt2, vt2) when kt1 = kt2 && vt1 = vt2 -> V_update_xobmbm (vv1, vv2, vv3)
      | _ -> err ()
    )
  
  | E_concat (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in 
      match cvt v1, cvt v2 with 
      | T_string, T_string -> V_concat_sss (vv1, vv2)
      | T_bytes, T_bytes -> V_concat_bbb (vv1, vv2)
      | _ -> err ()
    )
  | E_concat_list v -> (match cvt v with | T_list T_string -> V_concat_list_s (cvf v) | T_list T_bytes -> V_concat_list_b (cvf v) | _ -> err ())
  | E_slice (v1, v2, v3) -> (
      let vv1, vv2, vv3 = cvf v1, cvf v2, cvf v3 in
      match cvt v1, cvt v2, cvt v3 with
      | T_nat, T_nat, T_string -> V_slice_nnso (vv1, vv2, vv3)
      | T_nat, T_nat, T_bytes -> V_slice_nnbo (vv1, vv2, vv3)
      | _ -> err ()
    )
  | E_pack v ->  V_pack (cvf v)
  | E_unpack (t, v) -> V_unpack (ProverLib.Vlang.TypeUtil.ty_of_mty t, cvf v)
  | E_self -> (
      let paramstoragetyp = PreLib.Cfg.t_map_find ~errtrace:"Prover.Converter.create_expr_of_cfgexpr : E_self" cfg.type_info PreLib.Cfg.param_storage_name in
      match ProverLib.Vlang.TypeUtil.ty_of_mty paramstoragetyp with | T_pair (pt, _) -> V_self pt | _ -> err ()
    )
  | E_contract_of_address (t, v) -> (match cvt v with | T_address -> V_contract_of_address (ProverLib.Vlang.TypeUtil.ty_of_mty t, cvf v) | _ -> err ())
  | E_implicit_account v -> (match cvt v with | T_key_hash -> V_implicit_account (cvf v) | _ -> err ())
  | E_now -> V_now
  | E_amount -> ProverLib.Vlang.Expr.V_var (ProverLib.Vlang.Ty.T_mutez, !glenv_ref.gv_amount)
  | E_balance -> ProverLib.Vlang.Expr.V_var (ProverLib.Vlang.Ty.T_mutez, !glenv_ref.gv_balance)
  | E_check_signature (v1, v2, v3) -> (match cvt v1, cvt v2, cvt v3 with | T_key, T_signature, T_bytes -> V_check_signature (cvf v1, cvf v2, cvf v3) | _ -> err ())
  | E_blake2b v -> (match cvt v with | T_bytes -> V_blake2b (cvf v) | _ -> err ())
  | E_sha256 v -> (match cvt v with | T_bytes -> V_sha256 (cvf v) | _ -> err ())
  | E_sha512 v -> (match cvt v with | T_bytes -> V_sha512 (cvf v) | _ -> err ())
  | E_hash_key v -> (match cvt v with | T_key -> V_hash_key (cvf v) | _ -> err ())
  | E_source -> ProverLib.Vlang.Expr.V_var (ProverLib.Vlang.Ty.T_address, !glenv_ref.gv_source)
  | E_sender -> ProverLib.Vlang.Expr.V_var (ProverLib.Vlang.Ty.T_address, !glenv_ref.gv_sender)
  | E_address_of_contract v -> (match cvt v with | T_contract _ -> V_address_of_contract (cvf v) | _ -> err ())
  | E_unlift_option v -> (match cvt v with | T_option _ -> V_unlift_option (cvf v) | _ -> err ())
  | E_unlift_left v -> (match cvt v with | T_or _ -> V_unlift_left (cvf v) | _ -> err ())
  | E_unlift_right v -> (match cvt v with | T_or _ -> V_unlift_right (cvf v) | _ -> err ())
  | E_hd v -> (
      let vv = cvf v in
      match cvt v with
      | T_list _ -> V_hd_l vv
      | T_set _ -> V_hd_s vv
      | T_map _ -> V_hd_m vv
      | T_big_map _ -> V_hd_bm vv
      | _ -> err ()
    )
  | E_tl v -> (
      let vv = cvf v in
      match cvt v with
      | T_list _ -> V_tl_l vv
      | T_set _ -> V_tl_s vv
      | T_map _ -> V_tl_m vv
      | T_big_map _ -> V_tl_bm vv
      | _ -> err ()
    )
  | E_hdtl v -> (
      let vv = cvf v in
      match cvt v with
      | T_list _ -> V_hdtl_l vv
      | T_set _ -> V_hdtl_s vv
      | T_map _ -> V_hdtl_m vv
      | _ -> err ()
    )
  | E_size v -> (
      let vv = cvf v in
      match cvt v with 
      | T_set _ -> V_size_s vv
      | T_map _ -> V_size_m vv
      | T_list _ -> V_size_l vv
      | T_string -> V_size_str vv
      | T_bytes -> V_size_b vv
      | _ -> err ()
    )
  | E_isnat v -> (match cvt v with | T_int -> V_isnat (cvf v) | _ -> err ())
  | E_int_of_nat v -> (match cvt v with | T_nat -> V_int_of_nat (cvf v) | _ -> err ())
  | E_chain_id -> V_chain_id
  | E_lambda_id n -> (
      let (_, _, pt, rett) = (t_map_find ~errtrace:"Prover.Converter.create_expr_of_cfgexpr" cfg.lambda_id_map n) in
      V_lambda_id (ProverLib.Vlang.TypeUtil.ty_of_mty pt, ProverLib.Vlang.TypeUtil.ty_of_mty rett, n)
    )
  | E_exec (v1, v2) -> (
      match cvt v1, cvt v2 with
      | pt1, T_lambda (pt2, _) when pt1 = pt2 -> V_exec (cvf v1, cvf v2)
      | _ -> err ()
    )
  | E_dup v -> V_dup (cvf v)
  | E_nil t -> V_nil (ProverLib.Vlang.TypeUtil.ty_of_mty t)
  | E_empty_set t -> V_empty_set (ProverLib.Vlang.TypeUtil.ty_of_mty t)
  | E_empty_map (t1, t2) -> V_empty_map (ProverLib.Vlang.TypeUtil.ty_of_mty t1, ProverLib.Vlang.TypeUtil.ty_of_mty t2)
  | E_empty_big_map (t1, t2) -> V_empty_big_map (ProverLib.Vlang.TypeUtil.ty_of_mty t1, ProverLib.Vlang.TypeUtil.ty_of_mty t2)
  | E_itself v -> V_itself (cvf v)
  | E_append (v1, v2) -> (
      match cvt v1, cvt v2 with
      | elt1, T_list elt2 when elt1 = elt2 -> V_append_l (cvf v1, cvf v2)
      | _ -> err ()
    )

  (* TODOs *)
  | E_cast _ -> err () (* TODO : vlang-unimplemented *)
  | E_steps_to_quota -> err () (* deprecated instruction *)
  
  (* Deprecated from Cfg *)
  | E_div _ -> err ()
  | E_mod _ -> err ()
  | E_create_contract_address _ -> err ()
  | E_create_account_address _ -> err ()
  | E_lambda _ -> err ()
  | E_special_nil_list -> err ()
  | E_phi _ -> err ()
  | E_unlift_or _ -> err ()
  )

end (* function "expr_of_cfgexpr" ends *)

