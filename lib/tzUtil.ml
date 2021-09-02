open Core
open Tz

(******************************************************************************)
(******************************************************************************)
(* InnerFirst Mapping                                                         *)
(******************************************************************************)
(******************************************************************************)

(* WARNING : It does not map any mich_v in functions (e.g., values in LAMBDA) *)
let rec mvcc_map_innerfst : mapf:(mich_v -> mich_v) -> mich_v cc -> mich_v cc =
  fun ~mapf mvcc ->
  let r : mich_v cc -> mich_v cc = mvcc_map_innerfst ~mapf in
  let gcc : 'a -> 'a cc = (fun base x -> { base with cc_v = x }) mvcc in
  let fc : mich_v -> mich_v cc = (fun x -> mapf x |> gcc) in
  let argv = mvcc.cc_v in
  match argv with
  | MV_symbol _ -> fc argv
  | MV_car v -> MV_car (r v) |> fc
  | MV_cdr v -> MV_cdr (r v) |> fc
  | MV_unlift_option v -> MV_unlift_option (r v) |> fc
  | MV_unlift_left v -> MV_unlift_left (r v) |> fc
  | MV_unlift_right v -> MV_unlift_right (r v) |> fc
  | MV_hd_l v -> MV_hd_l (r v) |> fc
  (*************************************************************************)
  (* Integer                                                               *)
  (*************************************************************************)
  | MV_lit_int _ -> fc argv
  | MV_neg_ni v -> MV_neg_ni (r v) |> fc
  | MV_neg_ii v -> MV_neg_ii (r v) |> fc
  | MV_not_ni v -> MV_not_ni (r v) |> fc
  | MV_not_ii v -> MV_not_ii (r v) |> fc
  | MV_add_nii (v1, v2) -> MV_add_nii (r v1, r v2) |> fc
  | MV_add_ini (v1, v2) -> MV_add_ini (r v1, r v2) |> fc
  | MV_add_iii (v1, v2) -> MV_add_iii (r v1, r v2) |> fc
  | MV_sub_nni (v1, v2) -> MV_sub_nni (r v1, r v2) |> fc
  | MV_sub_nii (v1, v2) -> MV_sub_nii (r v1, r v2) |> fc
  | MV_sub_ini (v1, v2) -> MV_sub_ini (r v1, r v2) |> fc
  | MV_sub_iii (v1, v2) -> MV_sub_iii (r v1, r v2) |> fc
  | MV_sub_tti (v1, v2) -> MV_sub_tti (r v1, r v2) |> fc
  | MV_mul_nii (v1, v2) -> MV_mul_nii (r v1, r v2) |> fc
  | MV_mul_ini (v1, v2) -> MV_mul_ini (r v1, r v2) |> fc
  | MV_mul_iii (v1, v2) -> MV_mul_iii (r v1, r v2) |> fc
  | MV_compare (v1, v2) -> MV_compare (r v1, r v2) |> fc
  | MV_int_of_nat v -> MV_int_of_nat (r v) |> fc
  (*************************************************************************)
  (* Natural Number                                                        *)
  (*************************************************************************)
  | MV_lit_nat _ -> fc argv
  | MV_abs_in v -> MV_abs_in (r v) |> fc
  | MV_add_nnn (v1, v2) -> MV_add_nnn (r v1, r v2) |> fc
  | MV_mul_nnn (v1, v2) -> MV_mul_nnn (r v1, r v2) |> fc
  | MV_shiftL_nnn (v1, v2) -> MV_shiftL_nnn (r v1, r v2) |> fc
  | MV_shiftR_nnn (v1, v2) -> MV_shiftR_nnn (r v1, r v2) |> fc
  | MV_and_nnn (v1, v2) -> MV_and_nnn (r v1, r v2) |> fc
  | MV_and_inn (v1, v2) -> MV_and_inn (r v1, r v2) |> fc
  | MV_or_nnn (v1, v2) -> MV_or_nnn (r v1, r v2) |> fc
  | MV_xor_nnn (v1, v2) -> MV_xor_nnn (r v1, r v2) |> fc
  | MV_size_s v -> MV_size_s (r v) |> fc
  | MV_size_m v -> MV_size_m (r v) |> fc
  | MV_size_l v -> MV_size_l (r v) |> fc
  | MV_size_str v -> MV_size_str (r v) |> fc
  | MV_size_b v -> MV_size_b (r v) |> fc
  (*************************************************************************)
  (* String                                                                *)
  (*************************************************************************)
  | MV_lit_string _ -> fc argv
  | MV_concat_sss (v1, v2) -> MV_concat_sss (r v1, r v2) |> fc
  | MV_concat_list_s v -> MV_concat_list_s (r v) |> fc
  (*************************************************************************)
  (* Bytes                                                                 *)
  (*************************************************************************)
  | MV_lit_bytes _ -> fc argv
  | MV_concat_bbb (v1, v2) -> MV_concat_bbb (r v1, r v2) |> fc
  | MV_concat_list_b v -> MV_concat_list_b (r v) |> fc
  | MV_pack v -> MV_pack (r v) |> fc
  | MV_blake2b v -> MV_blake2b (r v) |> fc
  | MV_sha256 v -> MV_sha256 (r v) |> fc
  | MV_sha512 v -> MV_sha256 (r v) |> fc
  (*************************************************************************)
  (* Mutez                                                                 *)
  (*************************************************************************)
  | MV_lit_mutez _ -> fc argv
  | MV_add_mmm (v1, v2) -> MV_add_mmm (r v1, r v2) |> fc
  | MV_sub_mmm (v1, v2) -> MV_sub_mmm (r v1, r v2) |> fc
  | MV_mul_mnm (v1, v2) -> MV_mul_mnm (r v1, r v2) |> fc
  | MV_mul_nmm (v1, v2) -> MV_mul_nmm (r v1, r v2) |> fc
  | MV_mtz_of_op_list v -> MV_mtz_of_op_list (r v) |> fc
  (*************************************************************************)
  (* Bool                                                                  *)
  (*************************************************************************)
  | MV_lit_bool _ -> fc argv
  | MV_not_bb v -> MV_not_bb (r v) |> fc
  | MV_and_bbb (v1, v2) -> MV_and_bbb (r v1, r v2) |> fc
  | MV_or_bbb (v1, v2) -> MV_or_bbb (r v1, r v2) |> fc
  | MV_xor_bbb (v1, v2) -> MV_xor_bbb (r v1, r v2) |> fc
  | MV_eq_ib (v1, v2) -> MV_eq_ib (r v1, r v2) |> fc
  | MV_neq_ib (v1, v2) -> MV_neq_ib (r v1, r v2) |> fc
  | MV_lt_ib (v1, v2) -> MV_lt_ib (r v1, r v2) |> fc
  | MV_gt_ib (v1, v2) -> MV_gt_ib (r v1, r v2) |> fc
  | MV_leq_ib (v1, v2) -> MV_leq_ib (r v1, r v2) |> fc
  | MV_geq_ib (v1, v2) -> MV_geq_ib (r v1, r v2) |> fc
  | MV_mem_xsb (v1, v2) -> MV_mem_xsb (r v1, r v2) |> fc
  | MV_mem_xmb (v1, v2) -> MV_mem_xmb (r v1, r v2) |> fc
  | MV_mem_xbmb (v1, v2) -> MV_mem_xbmb (r v1, r v2) |> fc
  | MV_check_signature (v1, v2, v3) ->
    MV_check_signature (r v1, r v2, r v3) |> fc
  (*************************************************************************)
  (* Key Hash                                                              *)
  (*************************************************************************)
  | MV_lit_key_hash _ -> fc argv
  | MV_hash_key v -> MV_hash_key (r v) |> fc
  (*************************************************************************)
  (* Timestamp                                                             *)
  (*************************************************************************)
  | MV_lit_timestamp_str _ -> fc argv
  | MV_lit_timestamp_sec _ -> fc argv
  | MV_add_tit (v1, v2) -> MV_add_tit (r v1, r v2) |> fc
  | MV_add_itt (v1, v2) -> MV_add_itt (r v1, r v2) |> fc
  | MV_sub_tit (v1, v2) -> MV_sub_tit (r v1, r v2) |> fc
  (*************************************************************************)
  (* Address                                                               *)
  (*************************************************************************)
  | MV_lit_address v -> MV_lit_address (r v) |> fc
  | MV_address_of_contract v -> MV_address_of_contract (r v) |> fc
  (*************************************************************************)
  (* Key                                                                   *)
  (*************************************************************************)
  | MV_lit_key _ -> fc argv
  (*************************************************************************)
  (* Unit                                                                  *)
  (*************************************************************************)
  | MV_unit -> fc argv
  (*************************************************************************)
  (* Signature                                                             *)
  (*************************************************************************)
  | MV_lit_signature_str _ -> fc argv
  | MV_lit_signature_signed (v1, v2) ->
    MV_lit_signature_signed (r v1, r v2) |> fc
  (*************************************************************************)
  (* Option                                                                *)
  (*************************************************************************)
  | MV_some v -> MV_some (r v) |> fc
  | MV_none _ -> fc argv
  | MV_ediv_nnnn (v1, v2) -> MV_ediv_nnnn (r v1, r v2) |> fc
  | MV_ediv_niin (v1, v2) -> MV_ediv_niin (r v1, r v2) |> fc
  | MV_ediv_inin (v1, v2) -> MV_ediv_inin (r v1, r v2) |> fc
  | MV_ediv_iiin (v1, v2) -> MV_ediv_iiin (r v1, r v2) |> fc
  | MV_ediv_mnmm (v1, v2) -> MV_ediv_mnmm (r v1, r v2) |> fc
  | MV_ediv_mmnm (v1, v2) -> MV_ediv_mmnm (r v1, r v2) |> fc
  | MV_get_xmoy (v1, v2) -> MV_get_xmoy (r v1, r v2) |> fc
  | MV_get_xbmo (v1, v2) -> MV_get_xbmo (r v1, r v2) |> fc
  | MV_slice_nnso (v1, v2, v3) -> MV_slice_nnso (r v1, r v2, r v3) |> fc
  | MV_slice_nnbo (v1, v2, v3) -> MV_slice_nnbo (r v1, r v2, r v3) |> fc
  | MV_unpack (t, v) -> MV_unpack (t, r v) |> fc
  | MV_contract_of_address (t, v) -> MV_contract_of_address (t, r v) |> fc
  | MV_isnat v -> MV_isnat (r v) |> fc
  (*************************************************************************)
  (* List                                                                  *)
  (*************************************************************************)
  | MV_lit_list (t, vl) -> MV_lit_list (t, List.map vl ~f:r) |> fc
  | MV_nil _ -> fc argv
  | MV_cons (v1, v2) -> MV_cons (r v1, r v2) |> fc
  | MV_tl_l v -> MV_tl_l (r v) |> fc
  (*************************************************************************)
  (* Set                                                                   *)
  (*************************************************************************)
  | MV_lit_set (t, vl) -> MV_lit_set (t, List.map vl ~f:r) |> fc
  | MV_empty_set _ -> fc argv
  | MV_update_xbss (v1, v2, v3) -> MV_update_xbss (r v1, r v2, r v3) |> fc
  (*************************************************************************)
  (* Operation                                                             *)
  (*************************************************************************)
  | MV_create_contract (t1, t2, v1, v2, v3, v4, v5) ->
    MV_create_contract (t1, t2, r v1, r v2, r v3, r v4, r v5) |> fc
  | MV_transfer_tokens (v1, v2, v3) ->
    MV_transfer_tokens (r v1, r v2, r v3) |> fc
  | MV_set_delegate v -> MV_set_delegate (r v) |> fc
  (*************************************************************************)
  (* Contract                                                              *)
  (*************************************************************************)
  | MV_lit_contract (t, v) -> MV_lit_contract (t, r v) |> fc
  | MV_self _ -> fc argv
  | MV_implicit_account v -> MV_implicit_account (r v) |> fc
  (*************************************************************************)
  (* Pair                                                                  *)
  (*************************************************************************)
  | MV_pair (v1, v2) -> MV_pair (r v1, r v2) |> fc
  (*************************************************************************)
  (* Or                                                                    *)
  (*************************************************************************)
  | MV_left (t, v) -> MV_left (t, r v) |> fc
  | MV_right (t, v) -> MV_right (t, r v) |> fc
  (*************************************************************************)
  (* Lambda                                                                *)
  (*************************************************************************)
  | MV_lit_lambda _ -> fc argv
  (* embedded code with LAMBDA Michelson-instruction should be expressed with V_lambda_id, not V_lit_lambda *)
  | MV_lambda_unknown _ -> fc argv
  | MV_lambda_closure (v1, v2) -> MV_lambda_closure (r v1, r v2) |> fc
  (*************************************************************************)
  (* Map                                                                   *)
  (*************************************************************************)
  | MV_lit_map (t1, t2, vvl) ->
    MV_lit_map (t1, t2, List.map ~f:(fun (x, y) -> (r x, r y)) vvl) |> fc
  | MV_empty_map _ -> fc argv
  | MV_update_xomm (v1, v2, v3) -> MV_update_xomm (r v1, r v2, r v3) |> fc
  (*************************************************************************)
  (* Big Map                                                               *)
  (*************************************************************************)
  | MV_lit_big_map (t1, t2, vvl) ->
    MV_lit_big_map (t1, t2, List.map ~f:(fun (x, y) -> (r x, r y)) vvl) |> fc
  | MV_empty_big_map _ -> fc argv
  | MV_update_xobmbm (v1, v2, v3) -> MV_update_xobmbm (r v1, r v2, r v3) |> fc
  (*************************************************************************)
  (* Chain Id                                                              *)
  (*************************************************************************)
  | MV_lit_chain_id _ -> fc argv
  (*************************************************************************)
  (* Custom Domain Value for Invariant Synthesis                           *)
  (*************************************************************************)
  | MV_ref _ -> fc argv
  | MV_ref_cont _ -> fc argv
  | MV_sigma_tmplm v -> MV_sigma_tmplm (r v) |> fc
(* function mvcc_map_innerfst end *)

let rec mf_map_innerfst : mapf:(mich_f -> mich_f) -> mich_f -> mich_f =
  fun ~mapf mf ->
  let r = mf_map_innerfst ~mapf in
  match mf with
  | MF_true -> mapf mf
  | MF_false -> mapf mf
  | MF_not f -> MF_not (r f) |> mapf
  | MF_and fl -> MF_and (List.map fl ~f:r) |> mapf
  | MF_or fl -> MF_or (List.map fl ~f:r) |> mapf
  | MF_eq _ -> mapf mf
  | MF_imply (f1, f2) -> MF_imply (r f1, mapf f2) |> mapf
  (* MicSE Branch *)
  | MF_is_true _ -> mapf mf
  | MF_is_none _ -> mapf mf
  | MF_is_left _ -> mapf mf
  | MF_is_cons _ -> mapf mf
  (* MicSE Datatype Constraint *)
  | MF_mutez_bound _ -> mapf mf
  | MF_nat_bound _ -> mapf mf
  (* Custom Formula for verifiying *)
  | MF_add_mmm_no_overflow _ -> mapf mf
  | MF_sub_mmm_no_underflow _ -> mapf mf
  | MF_mul_mnm_no_overflow _ -> mapf mf
  | MF_mul_nmm_no_overflow _ -> mapf mf
  | MF_shiftL_nnn_rhs_in_256 _ -> mapf mf
  | MF_shiftR_nnn_rhs_in_256 _ -> mapf mf

(******************************************************************************)
(******************************************************************************)
(* InnerFirst Folding                                                         *)
(******************************************************************************)
(******************************************************************************)

(* Simple process like context-swapping uses mapping only, but since our
   Integer/NaturalNum/Mutez value requires appropriate constraints
   we need to collect some constraints (e.g., MF_mutez_bound) everytime
   new mutez/nat symbols comes out to the surface by optimization process.
   That's why fold function required.
*)

let rec mvcc_fold_innerfst :
    f:('a * mich_v cc -> 'a * mich_v cc) ->
    acc:'a ->
    mich_v cc ->
    'a * mich_v cc =
  fun ~f ~acc mvcc ->
  (* init:'a -> f:(mich_v cc -> 'a -> 'a) -> mich_v cc -> 'a = *)
  (* fun ~init ~f mvcc -> *)
  let r : 'a * mich_v cc -> 'a * mich_v cc =
    (fun (a, m) -> mvcc_fold_innerfst ~f ~acc:a m)
  in
  let fc0 : mich_v cc -> 'a * mich_v cc = (fun x -> f (acc, x)) in
  let fc1 : (mich_v cc -> mich_v) -> mich_v cc -> 'a * mich_v cc =
    fun cstr a1 ->
    let (acc', a1') = r (acc, a1) in
    f (acc', cstr a1' |> (fun x -> { mvcc with cc_v = x }))
  in
  let fc2 :
      (mich_v cc -> mich_v cc -> mich_v) ->
      mich_v cc ->
      mich_v cc ->
      'a * mich_v cc =
    fun cstr a1 a2 ->
    let (acc', a1') = r (acc, a1) in
    let (acc'', a2') = r (acc', a2) in
    f (acc'', cstr a1' a2' |> (fun x -> { mvcc with cc_v = x }))
  in
  let fc3 :
      (mich_v cc -> mich_v cc -> mich_v cc -> mich_v) ->
      mich_v cc ->
      mich_v cc ->
      mich_v cc ->
      'a * mich_v cc =
    fun cstr a1 a2 a3 ->
    let (acc', a1') = r (acc, a1) in
    let (acc'', a2') = r (acc', a2) in
    let (acc''', a3') = r (acc'', a3) in
    f (acc''', cstr a1' a2' a3' |> (fun x -> { mvcc with cc_v = x }))
  in
  let fc5 :
      (mich_v cc -> mich_v cc -> mich_v cc -> mich_v cc -> mich_v cc -> mich_v) ->
      mich_v cc ->
      mich_v cc ->
      mich_v cc ->
      mich_v cc ->
      mich_v cc ->
      'a * mich_v cc =
    fun cstr a1 a2 a3 a4 a5 ->
    let (acc', a1') = r (acc, a1) in
    let (acc'', a2') = r (acc', a2) in
    let (acc''', a3') = r (acc'', a3) in
    let (acc'''', a4') = r (acc''', a4) in
    let (acc''''', a5') = r (acc'''', a5) in
    f (acc''''', cstr a1' a2' a3' a4' a5' |> (fun x -> { mvcc with cc_v = x }))
  in
  (* let argv = mvcc.cc_v in *)
  match mvcc.cc_v with
  | MV_symbol _ -> fc0 mvcc
  | MV_car v -> fc1 (fun x -> MV_car x) v
  | MV_cdr v -> fc1 (fun x -> MV_cdr x) v
  | MV_unlift_option v -> fc1 (fun x -> MV_unlift_option x) v
  | MV_unlift_left v -> fc1 (fun x -> MV_unlift_left x) v
  | MV_unlift_right v -> fc1 (fun x -> MV_unlift_right x) v
  | MV_hd_l v -> fc1 (fun x -> MV_hd_l x) v
  (*************************************************************************)
  (* Integer                                                               *)
  (*************************************************************************)
  | MV_lit_int _ -> fc0 mvcc
  | MV_neg_ni v -> fc1 (fun x -> MV_neg_ni x) v
  | MV_neg_ii v -> fc1 (fun x -> MV_neg_ii x) v
  | MV_not_ni v -> fc1 (fun x -> MV_not_ni x) v
  | MV_not_ii v -> fc1 (fun x -> MV_not_ii x) v
  | MV_add_nii (v1, v2) -> fc2 (fun x y -> MV_add_nii (x, y)) v1 v2
  | MV_add_ini (v1, v2) -> fc2 (fun x y -> MV_add_ini (x, y)) v1 v2
  | MV_add_iii (v1, v2) -> fc2 (fun x y -> MV_add_iii (x, y)) v1 v2
  | MV_sub_nni (v1, v2) -> fc2 (fun x y -> MV_sub_nni (x, y)) v1 v2
  | MV_sub_nii (v1, v2) -> fc2 (fun x y -> MV_sub_nii (x, y)) v1 v2
  | MV_sub_ini (v1, v2) -> fc2 (fun x y -> MV_sub_ini (x, y)) v1 v2
  | MV_sub_iii (v1, v2) -> fc2 (fun x y -> MV_sub_iii (x, y)) v1 v2
  | MV_sub_tti (v1, v2) -> fc2 (fun x y -> MV_sub_tti (x, y)) v1 v2
  | MV_mul_nii (v1, v2) -> fc2 (fun x y -> MV_mul_nii (x, y)) v1 v2
  | MV_mul_ini (v1, v2) -> fc2 (fun x y -> MV_mul_ini (x, y)) v1 v2
  | MV_mul_iii (v1, v2) -> fc2 (fun x y -> MV_mul_iii (x, y)) v1 v2
  | MV_compare (v1, v2) -> fc2 (fun x y -> MV_compare (x, y)) v1 v2
  | MV_int_of_nat v -> fc1 (fun x -> MV_int_of_nat x) v
  (*************************************************************************)
  (* Natural Number                                                        *)
  (*************************************************************************)
  | MV_lit_nat _ -> fc0 mvcc
  | MV_abs_in v -> fc1 (fun x -> MV_abs_in x) v
  | MV_add_nnn (v1, v2) -> fc2 (fun x y -> MV_add_nnn (x, y)) v1 v2
  | MV_mul_nnn (v1, v2) -> fc2 (fun x y -> MV_mul_nnn (x, y)) v1 v2
  | MV_shiftL_nnn (v1, v2) -> fc2 (fun x y -> MV_shiftL_nnn (x, y)) v1 v2
  | MV_shiftR_nnn (v1, v2) -> fc2 (fun x y -> MV_shiftR_nnn (x, y)) v1 v2
  | MV_and_nnn (v1, v2) -> fc2 (fun x y -> MV_and_nnn (x, y)) v1 v2
  | MV_and_inn (v1, v2) -> fc2 (fun x y -> MV_and_inn (x, y)) v1 v2
  | MV_or_nnn (v1, v2) -> fc2 (fun x y -> MV_or_nnn (x, y)) v1 v2
  | MV_xor_nnn (v1, v2) -> fc2 (fun x y -> MV_xor_nnn (x, y)) v1 v2
  | MV_size_s v -> fc1 (fun x -> MV_size_s x) v
  | MV_size_m v -> fc1 (fun x -> MV_size_m x) v
  | MV_size_l v -> fc1 (fun x -> MV_size_l x) v
  | MV_size_str v -> fc1 (fun x -> MV_size_str x) v
  | MV_size_b v -> fc1 (fun x -> MV_size_b x) v
  (*************************************************************************)
  (* String                                                                *)
  (*************************************************************************)
  | MV_lit_string _ -> fc0 mvcc
  | MV_concat_sss (v1, v2) -> fc2 (fun x y -> MV_concat_sss (x, y)) v1 v2
  | MV_concat_list_s v -> fc1 (fun x -> MV_concat_list_s x) v
  (*************************************************************************)
  (* Bytes                                                                 *)
  (*************************************************************************)
  | MV_lit_bytes _ -> fc0 mvcc
  | MV_concat_bbb (v1, v2) -> fc2 (fun x y -> MV_concat_bbb (x, y)) v1 v2
  | MV_concat_list_b v -> fc1 (fun x -> MV_concat_list_b x) v
  | MV_pack v -> fc1 (fun x -> MV_pack x) v
  | MV_blake2b v -> fc1 (fun x -> MV_blake2b x) v
  | MV_sha256 v -> fc1 (fun x -> MV_sha256 x) v
  | MV_sha512 v -> fc1 (fun x -> MV_sha256 x) v
  (*************************************************************************)
  (* Mutez                                                                 *)
  (*************************************************************************)
  | MV_lit_mutez _ -> fc0 mvcc
  | MV_add_mmm (v1, v2) -> fc2 (fun x y -> MV_add_mmm (x, y)) v1 v2
  | MV_sub_mmm (v1, v2) -> fc2 (fun x y -> MV_sub_mmm (x, y)) v1 v2
  | MV_mul_mnm (v1, v2) -> fc2 (fun x y -> MV_mul_mnm (x, y)) v1 v2
  | MV_mul_nmm (v1, v2) -> fc2 (fun x y -> MV_mul_nmm (x, y)) v1 v2
  | MV_mtz_of_op_list v -> fc1 (fun x -> MV_mtz_of_op_list x) v
  (*************************************************************************)
  (* Bool                                                                  *)
  (*************************************************************************)
  | MV_lit_bool _ -> fc0 mvcc
  | MV_not_bb v -> fc1 (fun x -> MV_not_bb x) v
  | MV_and_bbb (v1, v2) -> fc2 (fun x y -> MV_and_bbb (x, y)) v1 v2
  | MV_or_bbb (v1, v2) -> fc2 (fun x y -> MV_or_bbb (x, y)) v1 v2
  | MV_xor_bbb (v1, v2) -> fc2 (fun x y -> MV_xor_bbb (x, y)) v1 v2
  | MV_eq_ib (v1, v2) -> fc2 (fun x y -> MV_eq_ib (x, y)) v1 v2
  | MV_neq_ib (v1, v2) -> fc2 (fun x y -> MV_neq_ib (x, y)) v1 v2
  | MV_lt_ib (v1, v2) -> fc2 (fun x y -> MV_lt_ib (x, y)) v1 v2
  | MV_gt_ib (v1, v2) -> fc2 (fun x y -> MV_gt_ib (x, y)) v1 v2
  | MV_leq_ib (v1, v2) -> fc2 (fun x y -> MV_leq_ib (x, y)) v1 v2
  | MV_geq_ib (v1, v2) -> fc2 (fun x y -> MV_geq_ib (x, y)) v1 v2
  | MV_mem_xsb (v1, v2) -> fc2 (fun x y -> MV_mem_xsb (x, y)) v1 v2
  | MV_mem_xmb (v1, v2) -> fc2 (fun x y -> MV_mem_xmb (x, y)) v1 v2
  | MV_mem_xbmb (v1, v2) -> fc2 (fun x y -> MV_mem_xbmb (x, y)) v1 v2
  | MV_check_signature (v1, v2, v3) ->
    fc3 (fun x y z -> MV_check_signature (x, y, z)) v1 v2 v3
  (*************************************************************************)
  (* Key Hash                                                              *)
  (*************************************************************************)
  | MV_lit_key_hash _ -> fc0 mvcc
  | MV_hash_key v -> fc1 (fun x -> MV_hash_key x) v
  (*************************************************************************)
  (* Timestamp                                                             *)
  (*************************************************************************)
  | MV_lit_timestamp_str _ -> fc0 mvcc
  | MV_lit_timestamp_sec _ -> fc0 mvcc
  | MV_add_tit (v1, v2) -> fc2 (fun x y -> MV_add_tit (x, y)) v1 v2
  | MV_add_itt (v1, v2) -> fc2 (fun x y -> MV_add_itt (x, y)) v1 v2
  | MV_sub_tit (v1, v2) -> fc2 (fun x y -> MV_sub_tit (x, y)) v1 v2
  (*************************************************************************)
  (* Address                                                               *)
  (*************************************************************************)
  | MV_lit_address v -> fc1 (fun x -> MV_lit_address x) v
  | MV_address_of_contract v -> fc1 (fun x -> MV_address_of_contract x) v
  (*************************************************************************)
  (* Key                                                                   *)
  (*************************************************************************)
  | MV_lit_key _ -> fc0 mvcc
  (*************************************************************************)
  (* Unit                                                                  *)
  (*************************************************************************)
  | MV_unit -> fc0 mvcc
  (*************************************************************************)
  (* Signature                                                             *)
  (*************************************************************************)
  | MV_lit_signature_str _ -> fc0 mvcc
  | MV_lit_signature_signed (v1, v2) ->
    fc2 (fun x y -> MV_lit_signature_signed (x, y)) v1 v2
  (*************************************************************************)
  (* Option                                                                *)
  (*************************************************************************)
  | MV_some v -> fc1 (fun x -> MV_some x) v
  | MV_none _ -> fc0 mvcc
  | MV_ediv_nnnn (v1, v2) -> fc2 (fun x y -> MV_ediv_nnnn (x, y)) v1 v2
  | MV_ediv_niin (v1, v2) -> fc2 (fun x y -> MV_ediv_niin (x, y)) v1 v2
  | MV_ediv_inin (v1, v2) -> fc2 (fun x y -> MV_ediv_inin (x, y)) v1 v2
  | MV_ediv_iiin (v1, v2) -> fc2 (fun x y -> MV_ediv_iiin (x, y)) v1 v2
  | MV_ediv_mnmm (v1, v2) -> fc2 (fun x y -> MV_ediv_mnmm (x, y)) v1 v2
  | MV_ediv_mmnm (v1, v2) -> fc2 (fun x y -> MV_ediv_mmnm (x, y)) v1 v2
  | MV_get_xmoy (v1, v2) -> fc2 (fun x y -> MV_get_xmoy (x, y)) v1 v2
  | MV_get_xbmo (v1, v2) -> fc2 (fun x y -> MV_get_xbmo (x, y)) v1 v2
  | MV_slice_nnso (v1, v2, v3) ->
    fc3 (fun x y z -> MV_slice_nnso (x, y, z)) v1 v2 v3
  | MV_slice_nnbo (v1, v2, v3) ->
    fc3 (fun x y z -> MV_slice_nnbo (x, y, z)) v1 v2 v3
  | MV_unpack (t, v) -> fc1 (fun x -> MV_unpack (t, x)) v
  | MV_contract_of_address (t, v) ->
    fc1 (fun x -> MV_contract_of_address (t, x)) v
  | MV_isnat v -> fc1 (fun x -> MV_isnat x) v
  (*************************************************************************)
  (* List                                                                  *)
  (*************************************************************************)
  | MV_lit_list (t, vl) ->
    let (fr_acc, fr_vl) =
       List.fold_right vl ~init:(acc, [])
         ~f:(fun v_in_vl (innerfold_acc, innerfold_acc_vl) ->
           let (vr_acc, vr) = r (innerfold_acc, v_in_vl) in
           (vr_acc, vr :: innerfold_acc_vl)
       )
    in
    f (fr_acc, MV_lit_list (t, fr_vl) |> (fun x -> { mvcc with cc_v = x }))
  | MV_nil _ -> fc0 mvcc
  | MV_cons (v1, v2) -> fc2 (fun x y -> MV_cons (x, y)) v1 v2
  | MV_tl_l v -> fc1 (fun x -> MV_tl_l x) v
  (*************************************************************************)
  (* Set                                                                   *)
  (*************************************************************************)
  | MV_lit_set (t, vl) ->
    let (fr_acc, fr_vl) =
       List.fold_right vl ~init:(acc, [])
         ~f:(fun v_in_vl (innerfold_acc, innerfold_acc_vl) ->
           let (vr_acc, vr) = r (innerfold_acc, v_in_vl) in
           (vr_acc, vr :: innerfold_acc_vl)
       )
    in
    f (fr_acc, MV_lit_set (t, fr_vl) |> (fun x -> { mvcc with cc_v = x }))
  | MV_empty_set _ -> fc0 mvcc
  | MV_update_xbss (v1, v2, v3) ->
    fc3 (fun x y z -> MV_update_xbss (x, y, z)) v1 v2 v3
  (*************************************************************************)
  (* Operation                                                             *)
  (*************************************************************************)
  | MV_create_contract (t1, t2, v1, v2, v3, v4, v5) ->
    fc5
      (fun a b c d e -> MV_create_contract (t1, t2, a, b, c, d, e))
      v1 v2 v3 v4 v5
  | MV_transfer_tokens (v1, v2, v3) ->
    fc3 (fun x y z -> MV_transfer_tokens (x, y, z)) v1 v2 v3
  | MV_set_delegate v -> fc1 (fun x -> MV_set_delegate x) v
  (*************************************************************************)
  (* Contract                                                              *)
  (*************************************************************************)
  | MV_lit_contract (t, v) -> fc1 (fun x -> MV_lit_contract (t, x)) v
  | MV_self _ -> fc0 mvcc
  | MV_implicit_account v -> fc1 (fun x -> MV_implicit_account x) v
  (*************************************************************************)
  (* Pair                                                                  *)
  (*************************************************************************)
  | MV_pair (v1, v2) -> fc2 (fun x y -> MV_pair (x, y)) v1 v2
  (*************************************************************************)
  (* Or                                                                    *)
  (*************************************************************************)
  | MV_left (t, v) -> fc1 (fun x -> MV_left (t, x)) v
  | MV_right (t, v) -> fc1 (fun x -> MV_right (t, x)) v
  (*************************************************************************)
  (* Lambda                                                                *)
  (*************************************************************************)
  | MV_lit_lambda _ -> fc0 mvcc
  (* embedded code with LAMBDA Michelson-instruction should be expressed with V_lambda_id, not V_lit_lambda *)
  | MV_lambda_unknown _ -> fc0 mvcc
  | MV_lambda_closure (v1, v2) ->
    fc2 (fun x y -> MV_lambda_closure (x, y)) v1 v2
  (*************************************************************************)
  (* Map                                                                   *)
  (*************************************************************************)
  | MV_lit_map (t1, t2, vvl) ->
    let (fr_acc, fr_vvl) =
       List.fold_right vvl ~init:(acc, [])
         ~f:(fun (v1_in_vvl, v2_in_vvl) (innerfold_acc, innerfold_acc_vvl) ->
           let (vr_acc_1, vr_1) = r (innerfold_acc, v1_in_vvl) in
           let (vr_acc_2, vr_2) = r (vr_acc_1, v2_in_vvl) in
           (vr_acc_2, (vr_1, vr_2) :: innerfold_acc_vvl)
       )
    in
    f (fr_acc, MV_lit_map (t1, t2, fr_vvl) |> (fun x -> { mvcc with cc_v = x }))
  | MV_empty_map _ -> fc0 mvcc
  | MV_update_xomm (v1, v2, v3) ->
    fc3 (fun x y z -> MV_update_xomm (x, y, z)) v1 v2 v3
  (*************************************************************************)
  (* Big Map                                                               *)
  (*************************************************************************)
  | MV_lit_big_map (t1, t2, vvl) ->
    let (fr_acc, fr_vvl) =
       List.fold_right vvl ~init:(acc, [])
         ~f:(fun (v1_in_vvl, v2_in_vvl) (innerfold_acc, innerfold_acc_vvl) ->
           let (vr_acc_1, vr_1) = r (innerfold_acc, v1_in_vvl) in
           let (vr_acc_2, vr_2) = r (vr_acc_1, v2_in_vvl) in
           (vr_acc_2, (vr_1, vr_2) :: innerfold_acc_vvl)
       )
    in
    f
      ( fr_acc,
        MV_lit_big_map (t1, t2, fr_vvl) |> (fun x -> { mvcc with cc_v = x })
      )
  | MV_empty_big_map _ -> fc0 mvcc
  | MV_update_xobmbm (v1, v2, v3) ->
    fc3 (fun x y z -> MV_update_xobmbm (x, y, z)) v1 v2 v3
  (*************************************************************************)
  (* Chain Id                                                              *)
  (*************************************************************************)
  | MV_lit_chain_id _ -> fc0 mvcc
  (*************************************************************************)
  (* Custom Domain Value for Invariant Synthesis                           *)
  (*************************************************************************)
  | MV_ref _ -> fc0 mvcc
  | MV_ref_cont _ -> fc0 mvcc
  | MV_sigma_tmplm v -> fc1 (fun x -> MV_sigma_tmplm x) v
(* function mvcc_fold_innerfst end *)

(******************************************************************************)
(******************************************************************************)
(* Utility Functions for Tz                                                   *)
(******************************************************************************)
(******************************************************************************)

(******************************************************************************)
(* Code Component                                                             *)
(******************************************************************************)

let gen_dummy_cc : 'a -> 'a cc =
  (fun x -> { cc_loc = CCLOC_Unknown; cc_anl = []; cc_v = x })

let gen_custom_cc : 'ccbase cc -> 'a -> 'a cc =
  (fun base x -> { base with cc_v = x })

(******************************************************************************)
(* MV_symbol context swap                                                     *)
(******************************************************************************)

let symbol_context_swap_i : mich_sym_ctxt -> mich_v -> mich_v =
  fun ctxt mv ->
  match mv with
  | MV_symbol (t, symcat, _) -> MV_symbol (t, symcat, ctxt)
  | _                        ->
    failwith ("Tz.symbol_context_swap_i : " ^ __LOC__)

let symbol_context_swap : mich_sym_ctxt -> mich_v cc -> mich_v cc =
  (fun ctxt x -> symbol_context_swap_i ctxt x.cc_v |> gen_custom_cc x)

let symbol_context_swap_recursive : mich_sym_ctxt -> mich_v cc -> mich_v cc =
  fun ctxt mvcc ->
  mvcc_map_innerfst
    ~mapf:(function
      | MV_symbol (t, symcat, _) -> MV_symbol (t, symcat, ctxt)
      | _ as mv                  -> mv)
    mvcc

let symbol_context_swap_michf_recursive : mich_sym_ctxt -> mich_f -> mich_f =
  fun ctxt mf ->
  let vswap = symbol_context_swap_recursive ctxt in
  mf_map_innerfst
    ~mapf:(function
      (* Logical Formula *)
      | MF_true as mf -> mf
      | MF_false as mf -> mf
      | MF_not _ as mf -> mf
      | MF_and _ as mf -> mf
      | MF_or _ as mf -> mf
      | MF_eq (v1, v2) -> MF_eq (vswap v1, vswap v2)
      | MF_imply _ as mf -> mf
      (* MicSE Branch *)
      | MF_is_true v -> MF_is_true (vswap v)
      | MF_is_none v -> MF_is_none (vswap v)
      | MF_is_left v -> MF_is_left (vswap v)
      | MF_is_cons v -> MF_is_cons (vswap v)
      (* MicSE Datatype Constraint *)
      | MF_mutez_bound v -> MF_mutez_bound (vswap v)
      | MF_nat_bound v -> MF_nat_bound (vswap v)
      (* Custom Formula for verifiying *)
      | MF_add_mmm_no_overflow (v1, v2) ->
        MF_add_mmm_no_overflow (vswap v1, vswap v2)
      | MF_sub_mmm_no_underflow (v1, v2) ->
        MF_sub_mmm_no_underflow (vswap v1, vswap v2)
      | MF_mul_mnm_no_overflow (v1, v2) ->
        MF_mul_mnm_no_overflow (vswap v1, vswap v2)
      | MF_mul_nmm_no_overflow (v1, v2) ->
        MF_mul_nmm_no_overflow (vswap v1, vswap v2)
      | MF_shiftL_nnn_rhs_in_256 (v1, v2) ->
        MF_shiftL_nnn_rhs_in_256 (vswap v1, vswap v2)
      | MF_shiftR_nnn_rhs_in_256 (v1, v2) ->
        MF_shiftR_nnn_rhs_in_256 (vswap v1, vswap v2))
    mf

(* Warning: input trx_image must contain MV_symbol values only. *)
let trx_image_symbol_context_swap : mich_sym_ctxt -> trx_image -> trx_image =
  fun ctxt ti ->
  {
    ti_contract = symbol_context_swap ctxt ti.ti_contract;
    ti_source = symbol_context_swap ctxt ti.ti_source;
    ti_sender = symbol_context_swap ctxt ti.ti_sender;
    ti_param = symbol_context_swap ctxt ti.ti_param;
    ti_amount = symbol_context_swap ctxt ti.ti_amount;
    ti_time = symbol_context_swap ctxt ti.ti_time;
  }

let sym_image_symbol_context_swap : mich_sym_ctxt -> sym_image -> sym_image =
  fun ctxt si ->
  let rswap = symbol_context_swap_recursive ctxt in
  {
    si_mich = List.map si.si_mich ~f:rswap;
    si_dip = List.map si.si_dip ~f:rswap;
    si_map_entry = List.map si.si_map_entry ~f:rswap;
    si_map_exit = List.map si.si_map_exit ~f:rswap;
    si_map_mapkey = List.map si.si_map_mapkey ~f:rswap;
    si_iter = List.map si.si_iter ~f:rswap;
    si_balance = rswap si.si_balance;
    si_bc_balance = rswap si.si_bc_balance;
    si_param = trx_image_symbol_context_swap ctxt si.si_param;
  }

let sym_state_symbol_context_swap : mich_sym_ctxt -> sym_state -> sym_state =
  fun ctxt ss ->
  {
    ss_id = ctxt;
    ss_start_mci = ss.ss_start_mci;
    ss_block_mci = ss.ss_block_mci;
    ss_start_si = sym_image_symbol_context_swap ctxt ss.ss_start_si;
    ss_block_si = sym_image_symbol_context_swap ctxt ss.ss_block_si;
    ss_constraints =
      List.map ss.ss_constraints ~f:(symbol_context_swap_michf_recursive ctxt);
  }

(******************************************************************************)
(* Tezos Type                                                                 *)
(******************************************************************************)

let typ_of_val : mich_v cc -> mich_t cc =
   let rec typ_of_val_i : mich_v cc -> mich_t cc =
     fun v ->
     let err : unit -> 'a =
       fun () ->
       TzError
         ("typ_of_val : typ_of_val_i : "
         (* ^ Sexp.to_string (sexp_of_cc sexp_of_mich_v v) *)
         ^ Sexp.to_string
             (sexp_of_cc sexp_of_mich_v v |> SexpUtil.tz_cc_sexp_form)
         )
       |> Stdlib.raise
     in
     let gen_cc : mich_t -> mich_t cc = (fun t -> gen_custom_cc v t) in
     match v.cc_v with
     (**************************************************************************)
     (* Symbol & Polymorphic                                                   *)
     (**************************************************************************)
     | MV_symbol (t, _, _) -> t
     | MV_car v1 -> (
       (typ_of_val_i v1).cc_v
       |> function
       | MT_pair (t1, _) -> t1
       | _               -> err ()
     )
     | MV_cdr v1 -> (
       (typ_of_val_i v1).cc_v
       |> function
       | MT_pair (_, t2) -> t2
       | _               -> err ()
     )
     | MV_unlift_option v1 -> (
       (typ_of_val_i v1).cc_v
       |> function
       | MT_option t -> t
       | _           -> err ()
     )
     | MV_unlift_left v1 -> (
       (typ_of_val_i v1).cc_v
       |> function
       | MT_or (t1, _) -> t1
       | _             -> err ()
     )
     | MV_unlift_right v1 -> (
       (typ_of_val_i v1).cc_v
       |> function
       | MT_or (_, t2) -> t2
       | _             -> err ()
     )
     | MV_hd_l v1 -> (
       (typ_of_val_i v1).cc_v
       |> function
       | MT_list t -> t
       | _         -> err ()
     )
     (****************************************************************************)
     (* Integer                                                                  *)
     (****************************************************************************)
     | MV_lit_int _
     | MV_neg_ni _
     | MV_neg_ii _
     | MV_not_ni _
     | MV_not_ii _
     | MV_add_nii _
     | MV_add_ini _
     | MV_add_iii _
     | MV_sub_nni _
     | MV_sub_nii _
     | MV_sub_ini _
     | MV_sub_iii _
     | MV_sub_tti _
     | MV_mul_nii _
     | MV_mul_ini _
     | MV_mul_iii _
     | MV_compare _
     | MV_int_of_nat _ ->
       gen_cc MT_int
     (****************************************************************************)
     (* Natural Number                                                           *)
     (****************************************************************************)
     | MV_lit_nat _
     | MV_abs_in _
     | MV_add_nnn _
     | MV_mul_nnn _
     | MV_shiftL_nnn _
     | MV_shiftR_nnn _
     | MV_and_nnn _
     | MV_and_inn _
     | MV_or_nnn _
     | MV_xor_nnn _
     | MV_size_s _
     | MV_size_m _
     | MV_size_l _
     | MV_size_str _
     | MV_size_b _ ->
       gen_cc MT_nat
     (****************************************************************************)
     (* String                                                                   *)
     (****************************************************************************)
     | MV_lit_string _
     | MV_concat_sss _
     | MV_concat_list_s _ ->
       gen_cc MT_string
     (****************************************************************************)
     (* Bytes                                                                    *)
     (****************************************************************************)
     | MV_lit_bytes _
     | MV_concat_bbb _
     | MV_concat_list_b _
     | MV_pack _
     | MV_blake2b _
     | MV_sha256 _
     | MV_sha512 _ ->
       gen_cc MT_bytes
     (****************************************************************************)
     (* Mutez                                                                    *)
     (****************************************************************************)
     | MV_lit_mutez _
     | MV_add_mmm _
     | MV_sub_mmm _
     | MV_mul_mnm _
     | MV_mul_nmm _
     | MV_mtz_of_op_list _ ->
       gen_cc MT_mutez
     (****************************************************************************)
     (* Bool                                                                     *)
     (****************************************************************************)
     | MV_lit_bool _
     | MV_not_bb _
     | MV_and_bbb _
     | MV_or_bbb _
     | MV_xor_bbb _
     | MV_eq_ib _
     | MV_neq_ib _
     | MV_lt_ib _
     | MV_gt_ib _
     | MV_leq_ib _
     | MV_geq_ib _
     | MV_mem_xsb _
     | MV_mem_xmb _
     | MV_mem_xbmb _
     | MV_check_signature _ ->
       gen_cc MT_bool
     (****************************************************************************)
     (* Key Hash                                                                 *)
     (****************************************************************************)
     | MV_lit_key_hash _
     | MV_hash_key _ ->
       gen_cc MT_key_hash
     (****************************************************************************)
     (* Timestamp                                                                *)
     (****************************************************************************)
     | MV_lit_timestamp_str _
     | MV_lit_timestamp_sec _
     | MV_add_tit _
     | MV_add_itt _
     | MV_sub_tit _ ->
       gen_cc MT_timestamp
     (****************************************************************************)
     (* Address                                                                  *)
     (****************************************************************************)
     | MV_lit_address _
     | MV_address_of_contract _ ->
       gen_cc MT_address
     (****************************************************************************)
     (* Key                                                                      *)
     (****************************************************************************)
     | MV_lit_key _ -> gen_cc MT_key
     (****************************************************************************)
     (* Unit                                                                     *)
     (****************************************************************************)
     | MV_unit -> gen_cc MT_unit
     (****************************************************************************)
     (* Signature                                                                *)
     (****************************************************************************)
     | MV_lit_signature_str _
     | MV_lit_signature_signed _ ->
       gen_cc MT_signature
     (****************************************************************************)
     (* Option                                                                   *)
     (****************************************************************************)
     | MV_some v1 -> gen_cc (MT_option (typ_of_val_i v1))
     | MV_none t1 -> gen_cc (MT_option t1)
     | MV_ediv_nnnn _ ->
       gen_cc (MT_option (gen_cc (MT_pair (gen_cc MT_nat, gen_cc MT_nat))))
     | MV_ediv_niin _ ->
       gen_cc (MT_option (gen_cc (MT_pair (gen_cc MT_int, gen_cc MT_nat))))
     | MV_ediv_inin _ ->
       gen_cc (MT_option (gen_cc (MT_pair (gen_cc MT_int, gen_cc MT_nat))))
     | MV_ediv_iiin _ ->
       gen_cc (MT_option (gen_cc (MT_pair (gen_cc MT_int, gen_cc MT_nat))))
     | MV_ediv_mnmm _ ->
       gen_cc (MT_option (gen_cc (MT_pair (gen_cc MT_mutez, gen_cc MT_mutez))))
     | MV_ediv_mmnm _ ->
       gen_cc (MT_option (gen_cc (MT_pair (gen_cc MT_nat, gen_cc MT_mutez))))
     | MV_get_xmoy (_, v2) -> (
       (typ_of_val_i v2).cc_v
       |> function
       | MT_map (_, t2) -> gen_cc (MT_option t2)
       | _              -> err ()
     )
     | MV_get_xbmo (_, v2) -> (
       (typ_of_val_i v2).cc_v
       |> function
       | MT_big_map (_, t2) -> gen_cc (MT_option t2)
       | _                  -> err ()
     )
     | MV_slice_nnso _ -> gen_cc (MT_option (gen_cc MT_string))
     | MV_slice_nnbo _ -> gen_cc (MT_option (gen_cc MT_bytes))
     | MV_unpack (t1, _) -> gen_cc (MT_option t1)
     | MV_contract_of_address (t1, _) ->
       gen_cc (MT_option (gen_cc (MT_contract t1)))
     | MV_isnat _ -> gen_cc (MT_option (gen_cc MT_nat))
     (****************************************************************************)
     (* List                                                                     *)
     (****************************************************************************)
     | MV_lit_list (t1, _) -> gen_cc (MT_list t1)
     | MV_nil t1 -> gen_cc (MT_list t1)
     | MV_cons (v1, _) -> gen_cc (MT_list (typ_of_val_i v1))
     | MV_tl_l v1 -> typ_of_val_i v1
     (****************************************************************************)
     (* Set                                                                      *)
     (****************************************************************************)
     | MV_lit_set (t1, _) -> gen_cc (MT_set t1)
     | MV_empty_set t1 -> gen_cc (MT_set t1)
     | MV_update_xbss (v1, _, _) -> gen_cc (MT_set (typ_of_val_i v1))
     (****************************************************************************)
     (* Operation                                                                *)
     (****************************************************************************)
     | MV_create_contract _
     | MV_transfer_tokens _
     | MV_set_delegate _ ->
       gen_cc MT_operation
     (****************************************************************************)
     (* Contract                                                                 *)
     (****************************************************************************)
     | MV_lit_contract (t1, _) -> gen_cc (MT_contract t1)
     | MV_self t1 -> gen_cc (MT_contract t1)
     | MV_implicit_account _ -> gen_cc (MT_contract (gen_cc MT_unit))
     (****************************************************************************)
     (* Pair                                                                     *)
     (****************************************************************************)
     | MV_pair (v1, v2) -> gen_cc (MT_pair (typ_of_val_i v1, typ_of_val_i v2))
     (****************************************************************************)
     (* Or                                                                       *)
     (****************************************************************************)
     | MV_left (t1, _)
     | MV_right (t1, _) ->
       t1
     (****************************************************************************)
     (* Lambda                                                                   *)
     (****************************************************************************)
     | MV_lit_lambda (t1, t2, _) -> gen_cc (MT_lambda (t1, t2))
     | MV_lambda_unknown (t1, t2) -> gen_cc (MT_lambda (t1, t2))
     | MV_lambda_closure (v1, _) -> (
       (typ_of_val_i v1).cc_v
       |> function
       | MT_lambda (t1, t2) -> (
         match t1.cc_v with
         | MT_pair (_, t12) -> gen_cc (MT_lambda (t12, t2))
         | _                -> err ()
       )
       | _                  -> err ()
     )
     (****************************************************************************)
     (* Map                                                                      *)
     (****************************************************************************)
     | MV_lit_map (t1, t2, _) -> gen_cc (MT_map (t1, t2))
     | MV_empty_map (t1, t2) -> gen_cc (MT_map (t1, t2))
     | MV_update_xomm (v1, v2, _) -> (
       (typ_of_val_i v1, (typ_of_val_i v2).cc_v)
       |> function
       | (t1, MT_option t2) -> gen_cc (MT_map (t1, t2))
       | _                  -> err ()
     )
     (****************************************************************************)
     (* Big Map                                                                  *)
     (****************************************************************************)
     | MV_lit_big_map (t1, t2, _) -> gen_cc (MT_map (t1, t2))
     | MV_empty_big_map (t1, t2) -> gen_cc (MT_map (t1, t2))
     | MV_update_xobmbm (v1, v2, _) -> (
       (typ_of_val_i v1, (typ_of_val_i v2).cc_v)
       |> function
       | (t1, MT_option t2) -> gen_cc (MT_map (t1, t2))
       | _                  -> err ()
     )
     (****************************************************************************)
     (* Chain Id                                                                 *)
     (****************************************************************************)
     | MV_lit_chain_id _ -> gen_cc MT_chain_id
     (****************************************************************************)
     (* Custom Domain Value for Invariant Synthesis                              *)
     (****************************************************************************)
     | MV_ref (t1, _) -> t1
     | MV_ref_cont (t1, _) -> t1
     | MV_sigma_tmplm _ -> gen_cc MT_mutez
     (* inner-function typ_of_val_i end *)
   in
   (fun v -> typ_of_val_i v)
(* function typ_of_val end *)

let get_innertyp : mich_t cc -> mich_t cc =
  fun ttt ->
  match ttt.cc_v with
  | MT_option t
  | MT_list t
  | MT_set t
  | MT_contract t ->
    t
  | _ ->
    TzError ("get_innertyp : " ^ Sexp.to_string (sexp_of_cc sexp_of_mich_t ttt))
    |> Stdlib.raise
(* function get_innertyp end *)

let get_innertyp2 : mich_t cc -> mich_t cc * mich_t cc =
  fun ttt ->
  match ttt.cc_v with
  | MT_pair (t1, t2)
  | MT_or (t1, t2)
  | MT_lambda (t1, t2)
  | MT_map (t1, t2)
  | MT_big_map (t1, t2) ->
    (t1, t2)
  | _ ->
    TzError ("get_innertyp2 : " ^ Sexp.to_string (sexp_of_cc sexp_of_mich_t ttt))
    |> Stdlib.raise
(* function get_innertyp2 end *)

(******************************************************************************)
(* Value & Formula Optimizations                                              *)
(******************************************************************************)

let opt_mf_rules : mich_f -> mich_f =
  fun mf ->
  match mf with
  (* NOT *)
  | MF_not MF_true -> MF_false
  | MF_not MF_false -> MF_true
  | MF_not (MF_not f) -> f
  (* AND *)
  | MF_and fl ->
    (* 1. remove True values *)
    let fl' = List.filter fl ~f:(fun x -> not (equal_mich_f MF_true x)) in
    if equal_list equal_mich_f fl' []
    then MF_true
    else if List.exists fl' ~f:(equal_mich_f MF_false)
    then MF_false
    else MF_and fl'
  (* IMPLY *)
  | MF_imply (MF_true, f) -> f
  | MF_imply (f, MF_false) -> MF_not f
  | _ -> mf

let opt_mf : mich_f -> mich_f = mf_map_innerfst ~mapf:opt_mf_rules

(* Duplicated constraint generator from Se module *)
let mtz_constriant_if_it_is_or_true :
    tv:Tz.mich_t Tz.cc * Tz.mich_v Tz.cc -> Tz.mich_f =
  fun ~tv:(t, v) ->
  if equal_mich_t t.cc_v MT_mutez then MF_mutez_bound v else MF_true

let nat_constriant_if_it_is_or_true :
    tv:Tz.mich_t Tz.cc * Tz.mich_v Tz.cc -> Tz.mich_f =
   let open Tz in
   fun ~tv:(t, v) ->
   if equal_mich_t t.cc_v MT_nat then MF_nat_bound v else MF_true

let opt_mvcc_rules : mich_f list * mich_v cc -> mich_f list * mich_v cc =
  fun (mfl_acc, mvcc) ->
  let v_return : mich_v cc -> mich_f list * mich_v cc =
    fun v ->
    let tv = (typ_of_val v, v) in
    ( [
        mtz_constriant_if_it_is_or_true ~tv; nat_constriant_if_it_is_or_true ~tv;
      ]
      @ mfl_acc,
      v
    )
  in
  let v =
     match mvcc.cc_v with
     (* PAIR, CAR, CDR *)
     | MV_car { cc_v = MV_pair (v, _); _ } -> v
     | MV_cdr { cc_v = MV_pair (_, v); _ } -> v
     | MV_pair ({ cc_v = MV_car v1; _ }, { cc_v = MV_cdr v2; _ })
       when equal_mich_v v1.cc_v v2.cc_v ->
       v1
     (* UNLIFT-OPTION & SOME *)
     | MV_unlift_option { cc_v = MV_some v; _ } -> v
     (* UNLIFT-LEFT & UNLIFT-RIGHT & OR-VALUES *)
     | MV_unlift_left { cc_v = MV_left (_, v); _ } -> v
     | MV_unlift_right { cc_v = MV_right (_, v); _ } -> v
     (* LIST HEAD-TAIL *)
     | MV_hd_l { cc_v = MV_lit_list (_, hd :: _); _ } -> hd
     | MV_tl_l { cc_v = MV_lit_list (t, _ :: tl); _ } ->
       MV_lit_list (t, tl) |> gen_custom_cc mvcc
     | MV_hd_l { cc_v = MV_cons (hd, _); _ } -> hd
     | MV_tl_l { cc_v = MV_cons (_, tl); _ } -> tl
     | MV_cons ({ cc_v = MV_hd_l v1; _ }, { cc_v = MV_tl_l v2; _ })
       when equal_mich_v v1.cc_v v2.cc_v ->
       v1
     (* MAP, BIG-MAP GET & UPDATE *)
     | MV_get_xmoy (k1, { cc_v = MV_update_xomm (k2, v, _); _ })
       when equal_mich_v k1.cc_v k2.cc_v ->
       v
     | MV_get_xbmo (k1, { cc_v = MV_update_xobmbm (k2, v, _); _ })
       when equal_mich_v k1.cc_v k2.cc_v ->
       v
     (* MAP, BIG-MAP UPDATE twice *)
     | MV_update_xomm (k1, v1, { cc_v = MV_update_xomm (k2, _, m2); _ })
       when equal_mich_v k1.cc_v k2.cc_v ->
       MV_update_xomm (k1, v1, m2) |> gen_custom_cc mvcc
     | MV_update_xobmbm (k1, v1, { cc_v = MV_update_xobmbm (k2, _, m2); _ })
       when equal_mich_v k1.cc_v k2.cc_v ->
       MV_update_xobmbm (k1, v1, m2) |> gen_custom_cc mvcc
     (* Others *)
     | _ -> mvcc
  in
  v_return v

let opt_mvcc : mich_v cc -> mich_f list * mich_v cc =
  fun mvcc ->
  mvcc_fold_innerfst ~f:opt_mvcc_rules ~acc:[] mvcc
  |> (fun (fl, v) -> (List.map ~f:opt_mf fl, v))

(******************************************************************************)
(* Michelson Cut Information                                                  *)
(******************************************************************************)

let lb_of_ln_mci : mich_cut_info -> mich_cut_info option =
  fun mci ->
  let (lb_mcc, flag) : mich_cut_category * bool =
     match mci.mci_cutcat with
     | MCC_ln_loop     -> (MCC_lb_loop, true)
     | MCC_ln_loopleft -> (MCC_lb_loopleft, true)
     | MCC_ln_map      -> (MCC_lb_map, true)
     | MCC_ln_iter     -> (MCC_lb_iter, true)
     | _ as m          -> (m, false)
  in
  if flag then Some { mci with mci_cutcat = lb_mcc } else None
(* function lb_of_ln_mci end *)

let lb_of_ln_exn : mich_cut_info -> mich_cut_info =
  fun mci ->
  lb_of_ln_mci mci
  |> function
  | Some bbb -> bbb
  | None     -> TzError "lb_of_ln_exn" |> Stdlib.raise
(* function lb_of_ln_exn end *)

let is_ln_mcc : mich_cut_category -> bool = function
| MCC_trx_entry -> false
| MCC_trx_exit -> false
| MCC_ln_loop
| MCC_ln_loopleft
| MCC_ln_map
| MCC_ln_iter ->
  true
| MCC_lb_loop
| MCC_lb_loopleft
| MCC_lb_map
| MCC_lb_iter ->
  false
| MCC_query _ -> false
(* function is_ln_mcc end *)

let is_ln_mci : mich_cut_info -> bool = (fun mci -> is_ln_mcc mci.mci_cutcat)

let ln_of_lb_mci : mich_cut_info -> mich_cut_info option =
  fun mci ->
  let (ln_mcc, flag) : mich_cut_category * bool =
     match mci.mci_cutcat with
     | MCC_lb_loop     -> (MCC_ln_loop, true)
     | MCC_lb_loopleft -> (MCC_ln_loopleft, true)
     | MCC_lb_map      -> (MCC_ln_map, true)
     | MCC_lb_iter     -> (MCC_ln_iter, true)
     | _ as m          -> (m, false)
  in
  if flag then Some { mci with mci_cutcat = ln_mcc } else None
(* function ln_of_lb_mci end *)

let ln_of_lb_exn : mich_cut_info -> mich_cut_info =
  fun mci ->
  ln_of_lb_mci mci
  |> function
  | Some nnn -> nnn
  | None     -> TzError "ln_of_lb_exn" |> Stdlib.raise
(* function ln_of_lb_exn end *)

let is_lb_mcc : mich_cut_category -> bool = function
| MCC_trx_entry -> false
| MCC_trx_exit -> false
| MCC_ln_loop
| MCC_ln_loopleft
| MCC_ln_map
| MCC_ln_iter ->
  false
| MCC_lb_loop
| MCC_lb_loopleft
| MCC_lb_map
| MCC_lb_iter ->
  true
| MCC_query _ -> false
(* function is_lb_mcc end *)

let is_lb_mci : mich_cut_info -> bool = (fun mci -> is_lb_mcc mci.mci_cutcat)

let exit_of_entry_mci : mich_cut_info -> mich_cut_info option =
  fun mci ->
  let (exit_mcc, flag) : mich_cut_category * bool =
     match mci.mci_cutcat with
     | MCC_trx_entry -> (MCC_trx_exit, true)
     | _ as m        -> (m, false)
  in
  if flag then Some { mci with mci_cutcat = exit_mcc } else None
(* function exit_of_entry_mci end *)

let exit_of_entry_exn : mich_cut_info -> mich_cut_info =
  fun mci ->
  exit_of_entry_mci mci
  |> function
  | Some xxx -> xxx
  | None     -> TzError "exit_of_entry_exn" |> Stdlib.raise
(* function exit_of_entry_exn end *)

let is_exit_mcc : mich_cut_category -> bool = function
| MCC_trx_entry -> false
| MCC_trx_exit -> true
| MCC_ln_loop
| MCC_ln_loopleft
| MCC_ln_map
| MCC_ln_iter ->
  false
| MCC_lb_loop
| MCC_lb_loopleft
| MCC_lb_map
| MCC_lb_iter ->
  false
| MCC_query _ -> false
(* function is_exit_mcc end *)

let is_exit_mci : mich_cut_info -> bool = (fun mci -> is_exit_mcc mci.mci_cutcat)

let entry_of_exit_mci : mich_cut_info -> mich_cut_info option =
  fun mci ->
  let (entry_mcc, flag) : mich_cut_category * bool =
     match mci.mci_cutcat with
     | MCC_trx_exit -> (MCC_trx_entry, true)
     | _ as m       -> (m, false)
  in
  if flag then Some { mci with mci_cutcat = entry_mcc } else None
(* function entry_of_exit_mci end *)

let entry_of_exit_exn : mich_cut_info -> mich_cut_info =
  fun mci ->
  entry_of_exit_mci mci
  |> function
  | Some xxx -> xxx
  | None     -> TzError "entry_of_exit_exn" |> Stdlib.raise
(* function entry_of_exit_exn end *)

let is_entry_mcc : mich_cut_category -> bool = function
| MCC_trx_entry -> true
| MCC_trx_exit -> false
| MCC_ln_loop
| MCC_ln_loopleft
| MCC_ln_map
| MCC_ln_iter ->
  false
| MCC_lb_loop
| MCC_lb_loopleft
| MCC_lb_map
| MCC_lb_iter ->
  false
| MCC_query _ -> false
(* function is_entry_mcc end *)

let is_entry_mci : mich_cut_info -> bool =
  (fun mci -> is_entry_mcc mci.mci_cutcat)

let get_reduced_mcc : mich_cut_category -> r_mich_cut_category = function
| MCC_trx_entry
| MCC_trx_exit ->
  RMCC_trx
| MCC_ln_loop
| MCC_lb_loop ->
  RMCC_loop
| MCC_ln_loopleft
| MCC_lb_loopleft ->
  RMCC_loopleft
| MCC_ln_map
| MCC_lb_map ->
  RMCC_map
| MCC_ln_iter
| MCC_lb_iter ->
  RMCC_iter
| MCC_query qc -> RMCC_query qc
(* function get_reduced_mcc end *)

let get_reduced_mci : mich_cut_info -> r_mich_cut_info =
  fun mci ->
  { rmci_loc = mci.mci_loc; rmci_cutcat = get_reduced_mcc mci.mci_cutcat }

(******************************************************************************)
(******************************************************************************)
(* Michelson to Tz                                                            *)
(******************************************************************************)
(******************************************************************************)

module M2T = struct
  open Mich

  exception Error of string

  let cv_pos : Mich.pos -> ccp_pos =
    (fun { col = c; lin = l } -> { col = c; lin = l })

  let cv_loc : Mich.loc -> ccp_loc = function
  | Unknown      -> CCLOC_Unknown
  | Pos (p1, p2) -> CCLOC_Pos (cv_pos p1, cv_pos p2)

  let cv_annot : Mich.annot -> ccp_annot = function
  | A_typ s -> CCA_typ s
  | A_var s -> CCA_var s
  | A_fld s -> CCA_fld s

  let cv_t : 'a Mich.t -> 'a cc =
    fun { pos; ann; d } ->
    { cc_loc = cv_loc pos; cc_anl = List.map ann ~f:cv_annot; cc_v = d }

  let rec cv_typ : Mich.typ -> mich_t = function
  | T_key              -> MT_key
  | T_unit             -> MT_unit
  | T_signature        -> MT_signature
  | T_option t         -> MT_option (cv_typt t)
  | T_list t           -> MT_list (cv_typt t)
  | T_set t            -> MT_set (cv_typt t)
  | T_operation        -> MT_operation
  | T_contract t       -> MT_contract (cv_typt t)
  | T_pair (t1, t2)    -> MT_pair (cv_typt t1, cv_typt t2)
  | T_or (t1, t2)      -> MT_or (cv_typt t1, cv_typt t2)
  | T_lambda (t1, t2)  -> MT_lambda (cv_typt t1, cv_typt t2)
  | T_map (t1, t2)     -> MT_map (cv_typt t1, cv_typt t2)
  | T_big_map (t1, t2) -> MT_big_map (cv_typt t1, cv_typt t2)
  | T_chain_id         -> MT_chain_id
  | T_int              -> MT_int
  | T_nat              -> MT_nat
  | T_string           -> MT_string
  | T_bytes            -> MT_bytes
  | T_mutez            -> MT_mutez
  | T_bool             -> MT_bool
  | T_key_hash         -> MT_key_hash
  | T_timestamp        -> MT_timestamp
  | T_address          -> MT_address

  and cv_typt : typ t -> mich_t cc = (fun x -> cv_t { x with d = cv_typ x.d })

  let rec cv_inst : inst -> mich_i = function
  | I_seq (i1, i2) -> MI_seq (cv_instt i1, cv_instt i2)
  | I_drop -> MI_drop Bigint.one
  | I_drop_n zn -> MI_drop zn
  | I_dup -> MI_dup Bigint.one
  | I_swap -> MI_swap
  | I_dig zn -> MI_dig zn
  | I_dug zn -> MI_dug zn
  | I_push (t, d) -> MI_push (cv_typt t, cv_datat t d)
  | I_some -> MI_some
  | I_none t -> MI_none (cv_typt t)
  | I_unit -> MI_unit
  | I_if_none (i1, i2) -> MI_if_none (cv_instt i1, cv_instt i2)
  | I_pair -> MI_pair
  | I_car -> MI_car
  | I_cdr -> MI_cdr
  | I_left t -> MI_left (cv_typt t)
  | I_right t -> MI_right (cv_typt t)
  | I_if_left (i1, i2) -> MI_if_left (cv_instt i1, cv_instt i2)
  | I_nil t -> MI_nil (cv_typt t)
  | I_cons -> MI_cons
  | I_if_cons (i1, i2) -> MI_if_cons (cv_instt i1, cv_instt i2)
  | I_size -> MI_size
  | I_empty_set t -> MI_empty_set (cv_typt t)
  | I_empty_map (t1, t2) -> MI_empty_map (cv_typt t1, cv_typt t2)
  | I_empty_big_map (t1, t2) -> MI_empty_big_map (cv_typt t1, cv_typt t2)
  | I_map i -> MI_map (cv_instt i)
  | I_iter i -> MI_iter (cv_instt i)
  | I_mem -> MI_mem
  | I_get -> MI_get
  | I_update -> MI_update
  | I_if (i1, i2) -> MI_if (cv_instt i1, cv_instt i2)
  | I_loop i -> MI_loop (cv_instt i)
  | I_loop_left i -> MI_loop_left (cv_instt i)
  | I_lambda (t1, t2, i) -> MI_lambda (cv_typt t1, cv_typt t2, cv_instt i)
  | I_exec -> MI_exec
  | I_apply -> MI_apply
  | I_dip i -> MI_dip_n (Bigint.one, cv_instt i)
  | I_dip_n (zn, i) -> MI_dip_n (zn, cv_instt i)
  | I_failwith -> MI_failwith
  | I_cast t -> MI_cast (cv_typt t)
  | I_rename -> MI_rename
  | I_concat -> MI_concat
  | I_slice -> MI_slice
  | I_pack -> MI_pack
  | I_unpack t -> MI_unpack (cv_typt t)
  | I_add -> MI_add
  | I_sub -> MI_sub
  | I_mul -> MI_mul
  | I_ediv -> MI_ediv
  | I_abs -> MI_abs
  | I_isnat -> MI_isnat
  | I_int -> MI_int
  | I_neg -> MI_neg
  | I_lsl -> MI_lsl
  | I_lsr -> MI_lsr
  | I_or -> MI_or
  | I_and -> MI_and
  | I_xor -> MI_xor
  | I_not -> MI_not
  | I_compare -> MI_compare
  | I_eq -> MI_eq
  | I_neq -> MI_neq
  | I_lt -> MI_lt
  | I_gt -> MI_gt
  | I_le -> MI_le
  | I_ge -> MI_ge
  | I_self -> MI_self
  | I_contract t -> MI_contract (cv_typt t)
  | I_transfer_tokens -> MI_transfer_tokens
  | I_set_delegate -> MI_set_delegate
  | I_create_account -> MI_create_account
  | I_create_contract p ->
    let (tp, ts, i) = cv_program p in
    MI_create_contract (tp, ts, i)
  | I_implicit_account -> MI_implicit_account
  | I_now -> MI_now
  | I_amount -> MI_amount
  | I_balance -> MI_balance
  | I_check_signature -> MI_check_signature
  | I_blake2b -> MI_blake2b
  | I_sha256 -> MI_sha256
  | I_sha512 -> MI_sha512
  | I_hash_key -> MI_hash_key
  | I_steps_to_quota -> MI_steps_to_quota
  | I_source -> MI_source
  | I_sender -> MI_sender
  | I_address -> MI_address
  | I_chain_id -> MI_chain_id
  | I_unpair -> MI_unpair
  (* Non-Standard Instruction : Special Comment : MicSE user defined safety property *)
  | I_noop -> MI_drop (Bigint.of_int 0)
  | I_micse_check i -> MI_micse_check (cv_instt i)
  (* Error case - Macros & Undefined *)
  (* | _ as inst -> Error ("cv_inst : " ^ (PreLib.Mich.string_of_instt_ol (PreLib.Mich.gen_t inst))) |> raise *)
  | _ -> Error "cv_inst" |> raise

  and cv_instt : inst t -> mich_i cc = (fun x -> cv_t { x with d = cv_inst x.d })

  and cv_data : typ t -> data -> mich_v =
    fun tt dd ->
    match (tt.Mich.d, dd) with
    | (T_key, D_string s) -> MV_lit_key s
    | (T_unit, D_unit) -> MV_unit
    | (T_signature, D_string s) -> MV_lit_signature_str s
    | (T_option t, D_none) -> MV_none (cv_typt t)
    | (T_option t, D_some d) -> MV_some (cv_datat t d)
    | (T_list t, D_list d) -> MV_lit_list (cv_typt t, List.map ~f:(cv_datat t) d)
    | (T_set t, D_list d) -> MV_lit_set (cv_typt t, List.map ~f:(cv_datat t) d)
    | (T_pair (t1, t2), D_pair (d1, d2)) ->
      MV_pair (cv_datat t1 d1, cv_datat t2 d2)
    | (T_pair (t1, t2), D_list (d1 :: d2)) ->
      MV_pair (cv_datat t1 d1, cv_datat t2 (gen_t (D_list d2)))
    | (T_or (t1, _), D_left d) -> MV_left (cv_typt tt, cv_datat t1 d)
    | (T_or (_, t2), D_right d) -> MV_right (cv_typt tt, cv_datat t2 d)
    | (T_lambda (t1, t2), D_lambda i) ->
      MV_lit_lambda (cv_typt t1, cv_typt t2, cv_instt i)
    | (T_map (t1, t2), D_list d) ->
      let m : (mich_v cc, mich_v cc) Core.Map.Poly.t =
         List.fold_left
           ~f:(fun acc ddd ->
             match ddd.d with
             | D_elt (d1, d2) -> (
               Core.Map.Poly.add acc ~key:(cv_datat t1 d1) ~data:(cv_datat t2 d2)
               |> function
               | `Ok m      -> m
               | `Duplicate ->
                 Error "cv_data : (T_map, D_list) : duplicated" |> raise
             )
             | _              ->
               Error "cv_data : (T_map, D_list) : not-Elt" |> raise)
           ~init:Core.Map.Poly.empty d
      in
      MV_lit_map (cv_typt t1, cv_typt t2, Core.Map.Poly.to_alist m)
    | (T_big_map (t1, t2), D_list d) ->
      let m : (mich_v cc, mich_v cc) Core.Map.Poly.t =
         List.fold_left
           ~f:(fun acc ddd ->
             match ddd.d with
             | D_elt (d1, d2) -> (
               Core.Map.Poly.add acc ~key:(cv_datat t1 d1) ~data:(cv_datat t2 d2)
               |> function
               | `Ok m      -> m
               | `Duplicate ->
                 Error "cv_data : (T_big_map, D_list) : duplicated" |> raise
             )
             | _              ->
               Error "cv_data : (T_big_map, D_list) : not-Elt" |> raise)
           ~init:Core.Map.Poly.empty d
      in
      MV_lit_big_map (cv_typt t1, cv_typt t2, Core.Map.Poly.to_alist m)
    | (T_big_map (t1, t2), D_int _) ->
      let _ =
         Utils.Log.warn (fun m ->
             m
               "%s : D_int in this data is interpreted to MV_empty_big_map. If it is not intended situation, please check the data."
               Stdlib.__LOC__
         )
      in
      MV_empty_big_map (cv_typt t1, cv_typt t2)
    | (T_chain_id, D_string s) -> MV_lit_chain_id s
    | (T_int, D_int zn) -> MV_lit_int zn
    | (T_nat, D_int zn) -> MV_lit_nat zn
    | (T_string, D_string s) -> MV_lit_string s
    | (T_bytes, D_bytes s) -> MV_lit_bytes s
    | (T_mutez, D_int zn) -> MV_lit_mutez zn
    | (T_bool, D_bool b) -> MV_lit_bool b
    | (T_key_hash, D_string s) -> MV_lit_key_hash s
    | (T_key_hash, D_bytes s) -> MV_lit_key_hash s
    | (T_timestamp, D_string s) -> MV_lit_timestamp_str s
    | (T_timestamp, D_int zn) -> MV_lit_timestamp_sec zn
    | (T_address, D_string s) ->
      MV_lit_address (MV_lit_key_hash s |> gen_dummy_cc)
    | (T_address, D_bytes s) ->
      MV_lit_address (MV_lit_key_hash s |> gen_dummy_cc)
    | (T_contract t, D_string s) when Mich.equal_typ t.Mich.d Mich.T_unit ->
      MV_implicit_account (MV_lit_key_hash s |> gen_dummy_cc)
    | (T_contract t, D_bytes s) ->
      MV_lit_contract
        ( cv_typt t,
          MV_lit_address (MV_lit_key_hash s |> gen_dummy_cc) |> gen_dummy_cc
        )
    | _ -> Error "cv_data : match failed" |> raise

  (* unused *)
  (* | T_operation,        D_ -> *)
  (* | T_contract t,       D_ -> *)

  and cv_datat : typ t -> data t -> mich_v cc =
    (fun t x -> cv_t { x with d = cv_data t x.d })

  and cv_program : program -> mich_t cc * mich_t cc * mich_i cc =
    fun { param; storage; code } ->
    (cv_typt param, cv_typt storage, cv_instt code)
end
(* module M2T end *)
