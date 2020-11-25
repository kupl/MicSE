open ProverLib

exception InvalidConversion_Expr of PreLib.Cfg.expr
exception InvalidConversion_Cond of Bp.cond

let newvar_prefix : string = "#"
let gen_nv x : string = newvar_prefix ^ x

type convert_env_body = {
  cfg : PreLib.Cfg.t;
  varname : (string, string) Core.Map.Poly.t; (* original-var-string -> latest-used-var-string *)
}
type convert_env = convert_env_body ref

let get_cur_varname : convert_env -> string -> string 
=fun cenv v -> begin
  let m = !cenv.varname in  (* map *)
  (match Core.Map.Poly.find m v with  (* find v in map *)
  | None -> 
      let nm = Core.Map.Poly.add m ~key:v ~data:v |> (function | `Ok mm -> mm | `Duplicate -> Stdlib.failwith "Prover.Converter.get_cur_varname : duplicate") in
      (cenv := {!cenv with varname=nm}; v) (* update map & apply at convert_env *)
  | Some vv -> vv
  )
end

(* WARNING: "get_new_varname" will change the given convert_env data *)
let get_new_varname : convert_env -> string -> string
=fun cenv v -> begin
  let m = !cenv.varname in (* map *)
  (match Core.Map.Poly.find m v with (* find v in map *)
  | None ->
      let nv = gen_nv v in
      let nm = Core.Map.Poly.add m ~key:v ~data:nv |> (function | `Ok mm -> mm | `Duplicate -> Stdlib.failwith "Prover.Converter.get_new_varname : None : duplicate") in
      (cenv := {!cenv with varname=nm}; nv)
  | Some cv ->
      let nv = gen_nv cv in
      let nm = Core.Map.Poly.add m ~key:v ~data:nv |> (function | `Ok mm -> mm | `Duplicate -> Stdlib.failwith "Prover.Converter.get_new_varname : Some : duplicate") in
      (cenv := {!cenv with varname=nm}; nv)
  )
end

(* "get_original_varname" just removes continuous "newvar_prefix"es in front of the given string *)
let get_original_varname : string -> string
=fun v -> begin
  let idx : int ref = ref 0 in
  let flag : bool ref = ref true in
  let _ : unit = String.iter (fun c -> if !flag && (String.make 1 c = newvar_prefix) then Stdlib.incr idx else flag := false) v in
  String.sub v !idx (String.length v - !idx)
end

let read_type_cfgvar : convert_env -> PreLib.Cfg.ident -> Vlang.typ
=fun cenv v -> begin Pre.Lib.Cfg.CPMap.find !cenv.cfg.type_info v |> (function Some x -> Vlang.TypeUtil.ty_of_mty x | None -> Stdlib.failwith "Prover.Converter.read_type_cfgvar : None") end

(*let read_type : Vlang.Expr.t -> Vlang.typ = Vlang.TypeUtil.ty_of_expr*)
let convert_type : PreLib.Cfg.typ -> Vlang.typ = Vlang.TypeUtil.ty_of_mty

let create_var_of_cfgvar : convert_env -> PreLib.Cfg.ident -> Vlang.Expr.t
= fun cenv v -> Vlang.Expr.V_var ((read_type_cfgvar cenv (get_original_varname v)), v)

let rec create_expr_of_michdata_i : PreLib.Mich.data -> Vlang.typ -> Vlang.Expr.t = 
  let open PreLib.Mich in
  let open Vlang.Ty in
  let open Vlang.Expr in
  let cem = create_expr_of_michdata in (* syntax sugar *)
  (fun michdata vtyp ->
    match (vtyp, michdata) with 
    | T_int, D_int zn -> V_lit_int zn
    | T_nat, D_int zn -> V_lit_int zn
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
    | _ -> Stdlib.failwith "Prover.Converter.create_expr_of_michdata_i : match failed"
  )
and create_expr_of_michdata : PreLib.Mich.data PreLib.Mich.t -> Vlang.typ -> Vlang.Expr.t
= fun michdata_t vtyp -> begin
  create_expr_of_michdata_i (PreLib.Mich.get_d michdata_t) vtyp
end

(* TODO (plan) : make this function transaction-specific / amount, balance, source, sender, ... *)
let create_expr_of_cfgexpr : convert_env -> PreLib.Cfg.expr -> Vlang.Expr.t
= let open PreLib.Cfg in
  let open Vlang.Expr in
  fun cenv cfgexpr -> begin
  let cvt : PreLib.Cfg.ident -> Vlang.typ = read_type_cfgvar cenv in (* syntax sugar *)
  let cvf : PreLib.Cfg.ident -> Vlang.Expr.t = fun v -> create_var_of_cfgvar cenv (get_cur_varname cenv v) in (* syntax sugar *) (* TODO : reduce redundant procedure, if it is bottleneck : cvf calls cvt *)
  let err (): 'a = Stdlib.raise (InvalidConversion_Expr cfgexpr) in
  match cfgexpr with 
  | E_push (d, t) -> create_expr_of_michdata d (convert_type t)
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
      | O_create_contract (p, v1, v2, v3) -> V_create_contract ((convert_type p.PreLib.Mich.param), (convert_type p.PreLib.Mich.storage), V_lit_program (p), cvf v1, cvf v2, cvf v3)
      | O_transfer_tokens (v1, v2, v3) -> V_transfer_tokens (cvf v1, cvf v2, cvf v3)
      | O_set_delegate v -> V_set_delegate (cvf v)
      | O_create_account _ -> err () (* deprecated operation *)
    )
  | E_unit -> V_unit
  | E_pair (v1, v2) -> V_pair (cvf v1, cvf v2)
  | E_left (v, t) -> let tt = convert_type t in (match tt, cvt v with | T_or (lt1, _), lt2 when lt1 = lt2 -> V_left (tt, cvf v) | _ -> err ())
  | E_right (v, t) -> let tt = convert_type t in (match tt, cvt v with | T_or (_, lt1), lt2 when lt1 = lt2 -> V_right (tt, cvf v) | _ -> err ())
  | E_some v -> V_some (cvf v)
  | E_none t -> V_none (convert_type t)
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
      | kt1, T_map (kt2, _) when kt1 = kt2 -> V_get_xbmo (vv1, vv2)
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
  | E_unpack (t, v) -> V_unpack (convert_type t, cvf v)
  | E_self -> (
      let paramstoragetyp = PreLib.Cfg.t_map_find ~errtrace:"Prover.Converter.create_expr_of_cfgexpr : E_self" !cenv.cfg.type_info PreLib.Cfg.param_storage_name in
      match convert_type paramstoragetyp with | T_pair (pt, _) -> V_self pt | _ -> err ()
    )
  | E_contract_of_address (t, v) -> (match cvt v with | T_address -> V_contract_of_address (convert_type t, cvf v) | _ -> err ())
  | E_implicit_account v -> (match cvt v with | T_key_hash -> V_implicit_account (cvf v) | _ -> err ())
  | E_now -> V_now
  | E_amount -> V_amount
  | E_balance -> V_balance
  | E_check_signature (v1, v2, v3) -> (match cvt v1, cvt v2, cvt v3 with | T_key, T_signature, T_bytes -> V_check_signature (cvf v1, cvf v2, cvf v3) | _ -> err ())
  | E_blake2b v -> (match cvt v with | T_bytes -> V_blake2b (cvf v) | _ -> err ())
  | E_sha256 v -> (match cvt v with | T_bytes -> V_sha256 (cvf v) | _ -> err ())
  | E_sha512 v -> (match cvt v with | T_bytes -> V_sha512 (cvf v) | _ -> err ())
  | E_hash_key v -> (match cvt v with | T_key -> V_hash_key (cvf v) | _ -> err ())
  | E_source -> V_source
  | E_sender -> V_sender
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
      let (_, _, pt, rett) = (t_map_find ~errtrace:"Prover.Converter.create_expr_of_cfgexpr" !cenv.cfg.lambda_id_map n) in
      V_lambda_id (convert_type pt, convert_type rett, n)
    )
  | E_exec (v1, v2) -> (
      match cvt v1, cvt v2 with
      | pt1, T_lambda (pt2, _) when pt1 = pt2 -> V_exec (cvf v1, cvf v2)
      | _ -> err ()
    )
  | E_dup v -> V_dup (cvf v)
  | E_nil t -> V_nil (convert_type t)
  | E_empty_set t -> V_empty_set (convert_type t)
  | E_empty_map (t1, t2) -> V_empty_map (convert_type t1, convert_type t2)
  | E_empty_big_map (t1, t2) -> V_empty_big_map (convert_type t1, convert_type t2)
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

end (* function create_expr_of_cfgexpr end *)

let create_formula_no_overflow : Vlang.Expr.t -> Vlang.t
=fun e -> begin
  let two_mutez_ge e1 e2 = Vlang.Formula.VF_not (Vlang.Formula.VF_eq (Vlang.Expr.V_compare (e1, e2), Vlang.Expr.V_lit_int (Z.minus_one))) in (* (e1 >= e2) === (!(cmp(e1, e2) == (-1))) *)
  match e with
  | V_add_mmm (_, e2) -> two_mutez_ge e e2
  | V_mul_mnm (_, _) -> Stdlib.failwith "Prover.Converter.create_formula_no_overflow : no-overflow formula for multiplication not supported yet"
  | _ -> Stdlib.failwith "Prover.Converter.create_formula_no_overflow : match failed"
end

let create_var_in_convert_cond : convert_env -> PreLib.Cfg.ident -> Vlang.Expr.t 
=fun cenv v -> begin
  let tt = PreLib.Cfg.t_map_find ~errtrace:("Prover.Converter.create_var_in_convert_cond : " ^ v) !cenv.cfg.type_info v |> convert_type in
  let vv = get_cur_varname cenv v in
  Vlang.Expr.V_var (tt, vv)
end


let rec convert_cond : convert_env -> Bp.cond -> Vlang.v_formula
= let open Vlang.Formula in
  let open Bp in
  fun cenv c -> begin
  let cvt : PreLib.Cfg.ident -> Vlang.typ = read_type_cfgvar cenv in (* syntax sugar *)
  let cvf : PreLib.Cfg.ident -> Vlang.Expr.t = fun v -> create_var_of_cfgvar cenv (get_cur_varname cenv v) in (* syntax sugar *) (* TODO : reduce redundant procedure, if it is bottleneck : cvf calls cvt *)
  let err (): 'a = Stdlib.raise (InvalidConversion_Cond c) in
  match c with
    | BC_is_true v -> VF_mich_if (create_var_in_convert_cond cenv v)
    | BC_is_none v -> VF_mich_if_none (create_var_in_convert_cond cenv v)
    | BC_is_left v -> VF_mich_if_left (create_var_in_convert_cond cenv v)
    | BC_is_cons v -> VF_mich_if_cons (create_var_in_convert_cond cenv v)
    | BC_no_overflow e -> (
        match e with
        | E_add (v1, v2) -> (
            let vv1, vv2 = cvf v1, cvf v2 in
            match cvt v1, cvt v2 with 
            | T_mutez, T_mutez -> VF_add_mmm_no_overflow (vv1, vv2)
            | _ -> err ()
          )
        | E_mul (v1, v2) -> (
            let vv1, vv2 = cvf v1, cvf v2 in
            match cvt v1, cvt v2 with 
            | T_mutez, T_nat -> VF_mul_mnm_no_overflow (vv1, vv2)
            | T_nat, T_mutez -> VF_mul_nmm_no_overflow (vv1, vv2)
            | _ -> err ()
          )
        | _ -> err () 
      )
    | BC_no_underflow e -> (
        match e with
        | E_sub (v1, v2) -> (
            let vv1, vv2 = cvf v1, cvf v2 in
            match cvt v1, cvt v2 with 
            | T_mutez, T_mutez -> VF_sub_mmm_no_underflow (vv1, vv2)
            | _ -> err ()
          )
        | _ -> err () 
      )
    | BC_not c -> VF_not (convert_cond cenv c)
end


let sp : convert_env -> (Vlang.t * Query.t list) -> (Bp.vertex * Bp.inst) -> (Vlang.t * Query.t list)
=fun cenv (f, qs) (_, s) -> begin
  match s with
  | BI_assume c -> 
      let f' = Vlang.Formula.VF_and [(convert_cond cenv c); f] in
      (f', qs)
  | BI_assert (c, loc, ctg) ->
      let formula = Vlang.Formula.VF_imply (f, (convert_cond cenv c)) in
      let query = Query.create_new_query formula ~loc:loc ~category:ctg in
      (f, (query::qs))
  | BI_assign (v, e) ->
      let e_f = create_expr_of_cfgexpr cenv e in
      let v' = Vlang.Expr.V_var (read_type_cfgvar cenv v, get_new_varname cenv v) in
      let f' = Vlang.Formula.VF_and [Vlang.Formula.VF_eq (v', e_f); f] in
      (f', qs)
  | BI_skip -> (f, qs)
end

let convert : Bp.t -> PreLib.Cfg.t -> (Vlang.t * Query.t list)
=fun bp cfg -> begin
  let cv_env : convert_env = ref {cfg=cfg; varname=Core.Map.Poly.empty} in
  let (f, g) = (Inv.T.read_formula (bp.pre), Inv.T.read_formula (bp.post)) in
  let (f', qs) = Core.List.fold_left bp.body ~init:(f, []) ~f:(sp cv_env) in
  let inductive = Vlang.Formula.VF_imply (f', g) in
  (inductive, qs)
end
