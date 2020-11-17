open ProverLib

exception Not_Implemented_f of Vlang.t
exception Not_Implemented_e of Vlang.Expr.t

let rec sort_of_typt : Smt.typ -> Smt.z_sort
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

let rec smtsort_of_vlangtyp : Vlang.Ty.t -> Smt.z_sort
= let open Vlang.Ty in
  let sot = smtsort_of_vlangtyp in (* syntax sugar *)
  fun vt -> begin
  match vt with
  | T_key -> Smt.create_string_sort
  | T_unit -> Smt.create_unit_sort
  | T_signature -> Smt.create_string_sort
  | T_option t -> Smt.create_option_sort (sot t)
  | T_list t -> Smt.create_list_sort (sot t)
  | T_set t -> Smt.create_list_sort (sot t)
  | T_operation -> Smt.create_operation_sort
  | T_contract _ -> Smt.create_contract_sort
  | T_pair (t1, t2) -> Smt.create_pair_sort (sot t1) (sot t2)
  | T_or (t1, t2) -> Smt.create_or_sort (sot t1) (sot t2)
  | T_lambda (_, _) -> Smt.create_lambda_sort
  | T_map (t1, t2) -> Smt.create_map_sort ~elt_sort:(Smt.create_elt_sort ~key_sort:(sot t1) ~value_sort:(sot t2))
  | T_big_map (t1, t2) -> Smt.create_map_sort ~elt_sort:(Smt.create_elt_sort ~key_sort:(sot t1) ~value_sort:(sot t2))
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
end (* function smttyp_of_vlangtyp end *)


let rec smtexpr_of_compare : Vlang.Expr.t -> Vlang.Expr.t -> Smt.z_expr
= let open Vlang.Ty in
  let soe = smtexpr_of_vlangexpr in (* syntax sugar *)
  (*let soc = smtexpr_of_compare in (* syntax sugar *)*)
  let err e = Stdlib.raise (Not_Implemented_e e) in (* syntax sugar *)
  fun e1 e2 -> begin
    match Vlang.TypeUtil.ty_of_expr e1, Vlang.TypeUtil.ty_of_expr e2 with
    | T_int, T_int -> Smt.create_int_cmp ~v1:(soe e1) ~v2:(soe e2)
    | T_nat, T_nat -> Smt.create_int_cmp ~v1:(soe e1) ~v2:(soe e2)
    | T_string, T_string -> Smt.create_string_cmp ~v1:(soe e1) ~v2:(soe e2)
    | T_bytes, T_bytes -> Smt.create_string_cmp ~v1:(soe e1) ~v2:(soe e2)
    | T_mutez, T_mutez -> Smt.create_mutez_cmp ~v1:(soe e1) ~v2:(soe e2)
    | T_bool, T_bool -> err e1  (* True is larger than False, like OCaml *)
    | T_key_hash, T_key_hash -> err e1
    | T_timestamp, T_timestamp -> err e1
    | T_address, T_address -> err e1
    | T_pair (_, _), T_pair (_, _) -> err e1
    | t1, t2 when t1 = t2 -> Stdlib.failwith ("Prover.Verifier.smtexpr_of_compare : expression like this cannot be compared")
    | _ -> Stdlib.failwith ("Prover.Verifier.smtexpr_of_compare : two expressions have different types")

end (* smtexpr_of_compare end *)

(* Overall, lots of uninterpreted function needed, not dummy expression *)
and smtexpr_of_vlangexpr : Vlang.Expr.t -> Smt.z_expr
= let open Vlang.Expr in
  let soe = smtexpr_of_vlangexpr in (* syntax sugar *)
  let err e = Stdlib.raise (Not_Implemented_e e) in (* syntax sugar *)
  fun ve -> begin
  match ve with
  (*************************************************************************)
  (* Variable & Polymorphic                                                *)
  (*************************************************************************)
  | V_var (t,v) -> Smt.read_var (Smt.create_symbol v) (smtsort_of_vlangtyp t)
  | V_car e -> Smt.read_pair_fst (soe e)
  | V_cdr e -> Smt.read_pair_snd (soe e)
  | V_unlift_option e -> Smt.read_option_content (soe e)
  | V_unlift_left e -> Smt.read_or_left_content (soe e)
  | V_unlift_right e -> Smt.read_or_right_content (soe e)
  | V_hd_l e -> Smt.read_list_head (soe e)
  | V_hd_s _ -> err ve (* get the first element of the set *)
  | V_exec (_, _) -> err ve (* function application & execution *)
  | V_dup e -> soe e
  | V_itself e -> soe e

  (*************************************************************************)
  (* Integer                                                               *)
  (*************************************************************************)
  | V_lit_int zn -> Smt.create_int_from_zarith zn
  | V_neg_ni e -> Smt.create_int_neg (soe e)
  | V_neg_ii e -> Smt.create_int_neg (soe e)
  | V_not_ni _ -> err ve  (* not supported yet *)
  | V_not_ii _ -> err ve  (* not supported *)
  | V_add_nii (e1, e2) -> Smt.create_int_add [soe e1; soe e2;]
  | V_add_ini (e1, e2) -> Smt.create_int_add [soe e1; soe e2;]
  | V_add_iii (e1, e2) -> Smt.create_int_add [soe e1; soe e2;]
  | V_sub_nni (e1, e2) -> Smt.create_int_sub [soe e1; soe e2;]
  | V_sub_nii (e1, e2) -> Smt.create_int_sub [soe e1; soe e2;]
  | V_sub_ini (e1, e2) -> Smt.create_int_sub [soe e1; soe e2;]
  | V_sub_iii (e1, e2) -> Smt.create_int_sub [soe e1; soe e2;]
  | V_sub_tti _ -> err ve (* not supported *)
  | V_mul_nii (e1, e2) -> Smt.create_int_mul [soe e1; soe e2;]
  | V_mul_ini (e1, e2) -> Smt.create_int_mul [soe e1; soe e2;]
  | V_mul_iii (e1, e2) -> Smt.create_int_mul [soe e1; soe e2;]
  | V_compare (e1, e2) -> smtexpr_of_compare e1 e2
  | V_int_of_nat  _ -> err ve (* not supported *)

  (*************************************************************************)
  (* Natural Number                                                        *)
  (*************************************************************************)
  | V_lit_nat zn -> Smt.create_int_from_zarith zn
  | V_abs_in e -> let ze = soe e in Smt.create_ite (Smt.create_bool_int_ge ze (Smt.create_int 0)) ze (Smt.create_int_neg ze)
  | V_add_nnn (e1, e2) -> Smt.create_int_add [soe e1; soe e2;]
  | V_mul_nnn (e1, e2) -> Smt.create_int_mul [soe e1; soe e2;]
  | V_shiftL_nnn (e1, e2) -> Smt.create_int_mul [soe e1; Smt.create_int_power (Smt.create_int 2) (soe e2)]
  | V_shiftR_nnn (e1, e2) -> Smt.create_int_div (soe e1) (Smt.create_int_power (Smt.create_int 2) (soe e2))
  | V_and_nnn (_, _) -> err ve  (* not supported *)
  | V_and_inn (_, _) -> err ve  (* not supported *)
  | V_or_nnn (_, _) -> err ve   (* not supported *)
  | V_xor_nnn (_, _) -> err ve  (* not supported *)
  | V_size_s _ -> err ve        (* not supported *)
  | V_size_m _ -> err ve        (* not supported *)
  | V_size_l _ -> err ve        (* not supported *)
  | V_size_str _ -> err ve      (* not supported *)
  | V_size_b _ -> err ve        (* not supported *)

  (*************************************************************************)
  (* String                                                                *)
  (*************************************************************************)
  | V_lit_string s -> Smt.create_string s
  | V_concat_sss (e1, e2) -> Smt.create_string_concat [soe e1; soe e2]
  | V_concat_list_s _ -> err ve (* cannot find how to convert "z_expr (smt-string smt-list)" to "(z_expr (string)) list" *)

  (*************************************************************************)
  (* Bytes                                                                 *)
  (*************************************************************************)
  | V_lit_bytes s -> Smt.create_string s
  | V_concat_bbb (e1, e2) -> Smt.create_string_concat [soe e1; soe e2]
  | V_concat_list_b _ -> err ve (* same error reason as "V_concat_list_s" *)
  | V_pack _ -> err ve      (* uninterpreted function needed *)
  | V_blake2b _ -> err ve   (* uninterpreted function needed *)
  | V_sha256 _ -> err ve    (* uninterpreted function needed *)
  | V_sha512 _ -> err ve    (* uninterpreted function needed *)

  (*************************************************************************)
  (* Mutez                                                                 *)
  (*************************************************************************)
  | V_lit_mutez zn -> Smt.create_mutez_from_zarith ~value:zn
  | V_amount -> err ve    (* native & uninterpreted symbol needed *)
  | V_balance -> err ve   (* native & uninterpreted symbol needed *)
  | V_add_mmm (e1, e2) -> Smt.create_mutez_add ~v1:(soe e1) ~v2:(soe e2)
  | V_sub_mmm (e1, e2) -> Smt.create_mutez_sub ~v1:(soe e1) ~v2:(soe e2)
  | V_mul_mnm (_, _) -> err ve (* e1 and e2 should be same sort, which is not supported for now *)
  | V_mul_nmm (_, _) -> err ve (* e1 and e2 should be same sort, which is not supported for now *)

  (*************************************************************************)
  (* Bool                                                                  *)
  (*************************************************************************)
  | V_lit_bool b -> if b then Smt.create_bool_true else Smt.create_bool_false
  | V_not_bb e -> Smt.create_bool_not (soe e) 
  | V_and_bbb (e1, e2) -> Smt.create_bool_and [soe e1; soe e2]
  | V_or_bbb (e1, e2) -> Smt.create_bool_or [soe e1; soe e2]
  | V_xor_bbb (e1, e2) -> Smt.create_bool_xor (soe e1) (soe e2)
  | V_eq_ib e -> Smt.create_bool_eq (soe e) (Smt.create_int 0)
  | V_neq_ib e -> Smt.create_bool_not (Smt.create_bool_eq (soe e) (Smt.create_int 0))
  | V_lt_ib e -> Smt.create_bool_int_lt (soe e) (Smt.create_int 0)
  | V_gt_ib e -> Smt.create_bool_int_gt (soe e) (Smt.create_int 0)
  | V_leq_ib e -> Smt.create_bool_int_le (soe e) (Smt.create_int 0)
  | V_geq_ib e -> Smt.create_bool_int_ge (soe e) (Smt.create_int 0)
  | V_mem_xsb _ -> err ve (* not supported *)
  | V_mem_xmb (e1, e2) -> Smt.read_map_elt_exists ~key:(soe e1) ~map:(soe e2)
  | V_mem_xbmb (e1, e2) -> Smt.read_map_elt_exists ~key:(soe e1) ~map:(soe e2)
  | V_check_signature _ -> err ve (* not supported *)

  (*************************************************************************)
  (* Key Hash                                                              *)
  (*************************************************************************)
  | V_lit_key_hash s -> Smt.create_string s
  | V_hash_key _ -> err ve (* not supported *)

  (*************************************************************************)
  (* Timestamp                                                             *)
  (*************************************************************************)
  | V_lit_timestamp_str s -> Smt.create_string s
  | V_lit_timestamp_sec _ -> err ve (* not supproted *)
  | V_now -> err ve (* native & uninterpreted symbol needed *)
  | V_add_tit (_, _) -> err ve (* not supported *)
  | V_add_itt (_, _) -> err ve (* not supported *)
  | V_sub_tit (_, _) -> err ve (* not supported *)

  (*************************************************************************)
  (* Address                                                               *)
  (*************************************************************************)
  | V_lit_address _ -> err ve (* not supported *)
  | V_source -> err ve (* native & uninterpreted symbol needed *)
  | V_sender -> err ve (* native & uninterpreted symbol needed *)
  | V_address_of_contract _ -> err ve (* not supported *)

  (*************************************************************************)
  (* Key                                                                   *)
  (*************************************************************************)
  | V_lit_key s -> Smt.create_string s

  (*************************************************************************)
  (* Unit                                                                  *)
  (*************************************************************************)
  (* | V_lit_unit : t_unit t *) (* V_unit has the same feature. *)
  | V_unit -> Smt.create_unit

  (*************************************************************************)
  (* Signature                                                             *)
  (*************************************************************************)
  | V_lit_signature_str s -> Smt.create_string s
  | V_lit_signature_signed (_, _) -> err ve (* uninterpreted function needed *)

  (*************************************************************************)
  (* Option                                                                *)
  (*************************************************************************)
  (* | V_lit_option : 'a t option -> 'a t_option t *) (* V_some and V_none has the same feature. *)
  | V_some e -> Smt.create_option (Vlang.TypeUtil.ty_of_expr e |> smtsort_of_vlangtyp) (Some (soe e))
  | V_none t -> Smt.create_option (smtsort_of_vlangtyp t) None
  | V_ediv_nnnn (e1, e2) ->
    let dividend, divisor = soe e1, soe e2 in
    let qr = Smt.create_pair (Smt.create_int_div dividend divisor) (Smt.create_int_mod dividend divisor) in
    let div_zero_result = Smt.create_option (Smt.read_sort_of_expr qr) None in
    Smt.create_ite (Smt.create_bool_eq divisor (Smt.create_int 0)) div_zero_result qr
  | V_ediv_niin (e1, e2) ->
    let dividend, divisor = soe e1, soe e2 in
    let qr = Smt.create_pair (Smt.create_int_div dividend divisor) (Smt.create_int_mod dividend divisor) in
    let div_zero_result = Smt.create_option (Smt.read_sort_of_expr qr) None in
    Smt.create_ite (Smt.create_bool_eq divisor (Smt.create_int 0)) div_zero_result qr
  | V_ediv_inin (e1, e2) ->
    let dividend, divisor = soe e1, soe e2 in
    let qr = Smt.create_pair (Smt.create_int_div dividend divisor) (Smt.create_int_mod dividend divisor) in
    let div_zero_result = Smt.create_option (Smt.read_sort_of_expr qr) None in
    Smt.create_ite (Smt.create_bool_eq divisor (Smt.create_int 0)) div_zero_result qr
  | V_ediv_iiin (e1, e2) ->
    let dividend, divisor = soe e1, soe e2 in
    let qr = Smt.create_pair (Smt.create_int_div dividend divisor) (Smt.create_int_mod dividend divisor) in
    let div_zero_result = Smt.create_option (Smt.read_sort_of_expr qr) None in
    Smt.create_ite (Smt.create_bool_eq divisor (Smt.create_int 0)) div_zero_result qr
  | V_ediv_mnmm _ -> err ve
  | V_ediv_mmnm _ -> err ve
  | V_get_xmoy (e1, e2) -> Smt.read_map_elt_content ~key:(soe e1) ~map:(soe e2)
  | V_get_xbmo (e1, e2) -> Smt.read_map_elt_content ~key:(soe e1) ~map:(soe e2)
  | V_slice_nnso (e1, e2, e3) -> 
    let offset, length, s = soe e1, soe e2, soe e3 in
    Smt.create_string_slice s offset (Smt.create_int_add [offset; length])
  | V_slice_nnbo (e1, e2, e3) ->
    let offset, length, s = soe e1, soe e2, soe e3 in
    Smt.create_string_slice s offset (Smt.create_int_add [offset; length])
  | V_unpack _ -> err ve (* similar to pack *)
  | V_contract_of_address _ -> err ve (* encoding is complicated *)
  | V_isnat _ -> err ve (* not supported *)

  (*************************************************************************)
  (* List                                                                  *)
  (*************************************************************************)
  | V_lit_list (t, el) ->
    let nil = Smt.create_list (smtsort_of_vlangtyp t) in
    List.fold_right (fun e acc_l -> Smt.update_list_cons (soe e) acc_l) el nil
  | V_nil t -> Smt.create_list (smtsort_of_vlangtyp t)
  | V_cons (e1, e2) -> Smt.update_list_cons (soe e1) (soe e2)
  | V_tl_l e -> Smt.read_list_tail (soe e)

  (*************************************************************************)
  (* Set                                                                   *)
  (*************************************************************************)
  | V_lit_set _ -> err ve
  | V_empty_set _ -> err ve
  | V_update_xbss _ -> err ve
  | V_tl_s _ -> err ve

  (*************************************************************************)
  (* Operation                                                             *)
  (*************************************************************************)
  (* | V_lit_operation of t_operation t *) (* V_create_contract, V_transfer_tokens, V_set_delegate has the same feature. *)
  | V_create_contract _ -> err ve
  | V_transfer_tokens _ -> err ve
  | V_set_delegate    _ -> err ve

  (*************************************************************************)
  (* Contract                                                              *)
  (*************************************************************************)
  | V_lit_contract _ -> err ve
  | V_self _ -> err ve
  | V_implicit_account _ -> err ve

  (*************************************************************************)
  (* Pair                                                                  *)
  (*************************************************************************)
  (* | V_lit_pair : 'a t * 'b t -> ('a, 'b) t_pair t *) (* V_pair has the same feature *)
  | V_pair (e1, e2) -> Smt.create_pair (soe e1) (soe e2)
  | V_hd_m _ -> err ve  (* previous implementation mixes list and array theories *) 
  | V_hd_bm _ -> err ve

  (*************************************************************************)
  (* Or                                                                    *)
  (*************************************************************************)
  (* | V_lit_or *) (* It cannot construct any value, use V_left or V_right instead *)
  | V_left (t, e) -> Smt.create_or (t |> Vlang.TypeUtil.get_innertyp2 |> Stdlib.snd |> smtsort_of_vlangtyp) (Smt.Left (soe e))
  | V_right (t, e) -> Smt.create_or (t |> Vlang.TypeUtil.get_innertyp2 |> Stdlib.fst |> smtsort_of_vlangtyp) (Smt.Right (soe e))

  (*************************************************************************)
  (* Lambda                                                                *)
  (*************************************************************************)
  | V_lit_program _ -> err ve
  | V_lambda_id _ -> err ve
  | V_lit_lambda _ -> err ve
  | V_lambda_unknown _ -> err ve
  | V_lambda_closure _ -> err ve
  (*************************************************************************)
  (* Map                                                                   *)
  (*************************************************************************)
  | V_lit_map (kt, vt, e) -> 
    let ks, vs = smtsort_of_vlangtyp kt, smtsort_of_vlangtyp vt in
    let nim = Smt.create_map ~key_sort:ks ~value_sort:vs in
    Core.Map.Poly.fold e ~init:nim ~f:(fun ~key ~data acc_zm -> Smt.update_map ~key:(soe key) ~value_opt:(Smt.create_option vs (Some (soe data))) ~map:acc_zm)
  | V_empty_map (kt, vt) -> Smt.create_map ~key_sort:(smtsort_of_vlangtyp kt) ~value_sort:(smtsort_of_vlangtyp vt)
  | V_update_xomm (e1, e2, e3) -> Smt.update_map ~key:(soe e1) ~value_opt:(soe e2) ~map:(soe e3)
  | V_tl_m _ -> err ve  (* not supported *)

  (*************************************************************************)
  (* Big Map                                                               *)
  (*************************************************************************)
  | V_lit_big_map (kt, vt, e) ->
    let ks, vs = smtsort_of_vlangtyp kt, smtsort_of_vlangtyp vt in
    let nim = Smt.create_map ~key_sort:ks ~value_sort:vs in
    Core.Map.Poly.fold e ~init:nim ~f:(fun ~key ~data acc_zm -> Smt.update_map ~key:(soe key) ~value_opt:(Smt.create_option vs (Some (soe data))) ~map:acc_zm)
  | V_empty_big_map (kt, vt) -> Smt.create_map ~key_sort:(smtsort_of_vlangtyp kt) ~value_sort:(smtsort_of_vlangtyp vt)
  | V_update_xobmbm (e1, e2, e3) -> Smt.update_map ~key:(soe e1) ~value_opt:(soe e2) ~map:(soe e3)
  | V_tl_bm _ -> err ve
  
  (*************************************************************************)
  (* Chain Id                                                              *)
  (*************************************************************************)
  | V_lit_chain_id _ -> err ve
  | V_chain_id -> err ve
end (* function smtexpr_of_vlangexpr end *)

let rec smtexpr_of_vlangformula : Vlang.t -> Smt.z_expr
= let open Vlang.Formula in
  let sof = smtexpr_of_vlangformula in  (* syntax sugar *)
  let soe = smtexpr_of_vlangexpr in (* syntax sugar *)
  let err f = Stdlib.raise (Not_Implemented_f f) in (* syntax sugar *)
  fun vf -> begin
  match vf with
  (* logical formula *)
  | VF_true -> Smt.create_bool_true
  | VF_false -> Smt.create_bool_false
  | VF_not f -> Smt.create_bool_not (sof f)
  | VF_and fl -> Smt.create_bool_and (List.map sof fl)
  | VF_or fl -> Smt.create_bool_or (List.map sof fl)
  | VF_eq (e1,e2) -> Smt.create_bool_eq (soe e1) (soe e2)
  | VF_imply (f1, f2) -> Smt.create_bool_imply (sof f1) (sof f2)
  (* micse-cfg specific boolean *)
  | VF_mich_if e -> Smt.create_bool_eq (soe e) (Smt.create_bool_true)
  | VF_mich_if_none e -> Smt.create_bool_option_is_none (soe e)
  | VF_mich_if_left e -> Smt.create_bool_option_is_left (soe e)
  | VF_mich_if_cons e -> Smt.create_bool_list_is_cons (soe e)
  (* NOT USED. belows are not constructed from Prover.converter *)
  | VF_mich_loop e -> Smt.create_bool_eq (soe e) (Smt.create_bool_true)
  | VF_mich_loop_left e -> Smt.create_bool_option_is_left (soe e)
  | VF_mich_map_l e -> Smt.create_bool_option_is_none (soe e)
  | VF_mich_map_m _ -> err vf (* check whether map's size is not 0 *)
  | VF_mich_iter_l e -> Smt.create_bool_option_is_none (soe e)
  | VF_mich_iter_s _ -> err vf (* check whether set's size is not 0 *)
  | VF_mich_iter_m _ -> err vf (* check whether map's size is not 0 *)
  | VF_mich_micse_check_value e -> Smt.create_bool_eq (soe e) (Smt.create_bool_true)
end (* function smtexpr_of_vlangformula end *)
