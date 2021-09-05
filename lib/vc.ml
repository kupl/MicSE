(* Vc: Verification condition manager *)

exception VcError of string

open! Core

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(******************************************************************************)
(******************************************************************************)
(* Smt Encoder                                                                *)
(******************************************************************************)
(******************************************************************************)

module Encoder = struct
  exception Not_Implemented

  let rec cv_mt : Smt.Ctx.t -> Tz.mich_t -> Smt.Sort.t =
     let open Tz in
     let open TzUtil in
     let open Smt in
     fun ctx typ ->
     let gdc typ = gen_dummy_cc typ in
     let sot typ_cc = cv_mtcc ctx typ_cc in
     (* syntax sugar *)
     match typ with
     | MT_key                  -> ZKey.create_sort ctx
     | MT_unit                 -> ZUnit.create_sort ctx
     | MT_signature            -> ZSig.create_sort ctx
     | MT_option t1cc          ->
       ZOption.create_sort ctx ~typ:(gdc typ) ~content_sort:(sot t1cc)
     | MT_list t1cc            ->
       ZList.create_sort ctx ~typ:(gdc typ) ~content_sort:(sot t1cc)
     | MT_set t1cc             ->
       ZSet.create_sort ctx ~typ:(gdc typ) ~content_sort:(sot t1cc)
     | MT_operation            -> ZOperation.create_sort ctx
     | MT_contract t1cc        ->
       ZContract.create_sort ctx ~typ:(gdc typ) ~content_sort:(sot t1cc)
     | MT_pair (t1cc, t2cc)    ->
       ZPair.create_sort ctx ~typ:(gdc typ) ~content_sort:(sot t1cc, sot t2cc)
     | MT_or (t1cc, t2cc)      ->
       ZOr.create_sort ctx ~typ:(gdc typ) ~content_sort:(sot t1cc, sot t2cc)
     | MT_lambda (t1cc, t2cc)  ->
       ZLambda.create_sort ctx ~typ:(gdc typ) ~domain_sort:(sot t1cc)
         ~range_sort:(sot t2cc)
     | MT_map (t1cc, t2cc)     ->
       ZMap.create_sort ctx ~typ:(gdc typ) ~key_sort:(sot t1cc)
         ~data_body_sort:(sot t2cc)
     | MT_big_map (t1cc, t2cc) ->
       ZMap.create_sort ctx ~typ:(gdc typ) ~key_sort:(sot t1cc)
         ~data_body_sort:(sot t2cc)
     | MT_chain_id             -> ZStr.create_sort ctx
     | MT_int                  -> ZInt.create_sort ctx
     | MT_nat                  -> ZNat.create_sort ctx
     | MT_string               -> ZStr.create_sort ctx
     | MT_bytes                -> ZBytes.create_sort ctx
     | MT_mutez                -> ZMutez.create_sort ctx
     | MT_bool                 -> ZBool.create_sort ctx
     | MT_key_hash             -> ZKeyHash.create_sort ctx
     | MT_timestamp            -> ZTimestamp.create_sort ctx
     | MT_address              -> ZAddr.create_sort ctx
  (* function cv_mt end *)

  and cv_mtcc : Smt.Ctx.t -> Tz.mich_t Tz.cc -> Smt.Sort.t =
    (fun ctx typ_cc -> cv_mt ctx typ_cc.cc_v)
  (* function cv_mtcc end *)

  let rec cv_mv : Smt.Ctx.t -> Tz.mich_v -> Smt.Expr.t =
     let open Tz in
     let open TzUtil in
     let open Smt in
     fun ctx value ->
     let gdc value = gen_dummy_cc value in
     let sot typ_cc = cv_mtcc ctx typ_cc in
     let sov value_cc = cv_mtcc ctx (typ_of_val value_cc) in
     let eov value_cc = cv_mvcc ctx value_cc in
     (* syntax sugar *)
     match value with
     (*************************************************************************)
     (* Symbol & Polymorphic                                                  *)
     (*************************************************************************)
     | MV_symbol (t1, c2, sc3) ->
       Expr.create_var ctx (sot t1)
         ~name:
           (Sexp.to_string (sexp_of_mich_sym_category c2)
           ^ "_"
           ^ Sexp.to_string (sexp_of_mich_sym_ctxt sc3)
           )
     | MV_car v1cc -> ZPair.read_content_fst (eov v1cc)
     | MV_cdr v1cc -> ZPair.read_content_snd (eov v1cc)
     | MV_unlift_option v1cc -> ZOption.read_content (eov v1cc)
     | MV_unlift_left v1cc -> ZOr.read_content_left (eov v1cc)
     | MV_unlift_right v1cc -> ZOr.read_content_right (eov v1cc)
     | MV_hd_l v1cc -> ZList.read_head (eov v1cc)
     (*************************************************************************)
     (* Integer                                                               *)
     (*************************************************************************)
     | MV_lit_int lit1 -> ZInt.create_expr_of_bigint ctx lit1
     | MV_neg_ni v1cc
     | MV_neg_ii v1cc
     | MV_not_ni v1cc
     | MV_not_ii v1cc ->
       ZInt.create_neg ctx (eov v1cc)
     | MV_add_nii (v1cc, v2cc)
     | MV_add_ini (v1cc, v2cc)
     | MV_add_iii (v1cc, v2cc) ->
       ZInt.create_add ctx (eov v1cc) (eov v2cc)
     | MV_sub_nni (v1cc, v2cc)
     | MV_sub_nii (v1cc, v2cc)
     | MV_sub_ini (v1cc, v2cc)
     | MV_sub_iii (v1cc, v2cc)
     | MV_sub_tti (v1cc, v2cc) ->
       ZInt.create_sub ctx (eov v1cc) (eov v2cc)
     | MV_mul_nii (v1cc, v2cc)
     | MV_mul_ini (v1cc, v2cc)
     | MV_mul_iii (v1cc, v2cc) ->
       ZInt.create_mul ctx (eov v1cc) (eov v2cc)
     | MV_compare (v1cc, v2cc) ->
       cv_compare ctx
         ((typ_of_val v1cc).cc_v, eov v1cc)
         ((typ_of_val v2cc).cc_v, eov v2cc)
     | MV_int_of_nat v1cc -> eov v1cc
     (*************************************************************************)
     (* Natural Number                                                        *)
     (*************************************************************************)
     | MV_lit_nat lit1 -> ZNat.create_expr_of_bigint ctx lit1
     | MV_abs_in v1cc -> ZNat.create_abs ctx (eov v1cc)
     | MV_add_nnn (v1cc, v2cc) -> ZNat.create_add ctx (eov v1cc) (eov v2cc)
     | MV_mul_nnn (v1cc, v2cc) -> ZNat.create_mul ctx (eov v1cc) (eov v2cc)
     | MV_shiftL_nnn (v1cc, v2cc) ->
       ZNat.create_shift_l ctx (eov v1cc) (eov v2cc)
     | MV_shiftR_nnn (v1cc, v2cc) ->
       ZNat.create_shift_r ctx (eov v1cc) (eov v2cc)
     | MV_and_nnn (v1cc, v2cc)
     | MV_and_inn (v1cc, v2cc) ->
       ZNat.create_and ctx (eov v1cc) (eov v2cc)
     | MV_or_nnn (v1cc, v2cc) -> ZNat.create_or ctx (eov v1cc) (eov v2cc)
     | MV_xor_nnn (v1cc, v2cc) -> ZNat.create_xor ctx (eov v1cc) (eov v2cc)
     | MV_size_s _ -> Not_Implemented |> raise
     | MV_size_m _ -> Not_Implemented |> raise
     | MV_size_l _ -> Not_Implemented |> raise
     | MV_size_str v1cc -> ZStr.create_size ctx (eov v1cc)
     | MV_size_b _ -> Not_Implemented |> raise
     (*************************************************************************)
     (* String                                                                *)
     (*************************************************************************)
     | MV_lit_string lit1 -> ZStr.create_expr ctx lit1
     | MV_concat_sss (v1cc, v2cc) ->
       ZStr.create_concat ctx [ eov v1cc; eov v2cc ]
     | MV_concat_list_s _ -> Not_Implemented |> raise
     (*************************************************************************)
     (* Bytes                                                                 *)
     (*************************************************************************)
     | MV_lit_bytes lit1 -> ZBytes.create_expr ctx lit1
     | MV_concat_bbb (v1cc, v2cc) ->
       ZBytes.create_concat ctx (eov v1cc) (eov v2cc)
     | MV_concat_list_b _ -> Not_Implemented |> raise
     | MV_pack _ -> Not_Implemented |> raise
     | MV_blake2b v1cc -> ZBytes.create_blake2b ctx (eov v1cc)
     | MV_sha256 v1cc -> ZBytes.create_sha256 ctx (eov v1cc)
     | MV_sha512 v1cc -> ZBytes.create_sha512 ctx (eov v1cc)
     (*************************************************************************)
     (* Mutez                                                                 *)
     (*************************************************************************)
     | MV_lit_mutez lit1 -> ZMutez.create_expr_of_bigint ctx lit1
     | MV_add_mmm (v1cc, v2cc) -> ZMutez.create_add ctx (eov v1cc) (eov v2cc)
     | MV_sub_mmm (v1cc, v2cc) -> ZMutez.create_sub ctx (eov v1cc) (eov v2cc)
     | MV_mul_mnm (v1cc, v2cc)
     | MV_mul_nmm (v1cc, v2cc) ->
       ZMutez.create_mul ctx (eov v1cc) (eov v2cc)
     | MV_mtz_of_op_list _ -> Not_Implemented |> raise
     (****************************************************************************)
     (* Bool                                                                     *)
     (****************************************************************************)
     | MV_lit_bool lit1 -> ZBool.create_expr ctx lit1
     | MV_not_bb v1cc -> ZBool.create_not ctx (eov v1cc)
     | MV_and_bbb (v1cc, v2cc) -> ZBool.create_and ctx (eov v1cc) (eov v2cc)
     | MV_or_bbb (v1cc, v2cc) -> ZBool.create_or ctx (eov v1cc) (eov v2cc)
     | MV_xor_bbb (v1cc, v2cc) -> ZBool.create_xor ctx (eov v1cc) (eov v2cc)
     | MV_eq_ib (v1cc, v2cc) -> ZBool.create_eq ctx (eov v1cc) (eov v2cc)
     | MV_neq_ib (v1cc, v2cc) -> ZBool.create_neq ctx (eov v1cc) (eov v2cc)
     | MV_lt_ib (v1cc, v2cc) -> ZBool.create_int_lt ctx (eov v1cc) (eov v2cc)
     | MV_gt_ib (v1cc, v2cc) -> ZBool.create_int_gt ctx (eov v1cc) (eov v2cc)
     | MV_leq_ib (v1cc, v2cc) -> ZBool.create_int_leq ctx (eov v1cc) (eov v2cc)
     | MV_geq_ib (v1cc, v2cc) -> ZBool.create_int_geq ctx (eov v1cc) (eov v2cc)
     | MV_mem_xsb (v1cc, v2cc) ->
       ZSet.read_mem ctx ~content:(eov v1cc) (eov v2cc)
     | MV_mem_xmb (v1cc, v2cc) -> ZMap.read_mem ctx ~key:(eov v1cc) (eov v2cc)
     | MV_mem_xbmb (v1cc, v2cc) -> ZMap.read_mem ctx ~key:(eov v1cc) (eov v2cc)
     | MV_check_signature _ -> Not_Implemented |> raise
     (*************************************************************************)
     (* Key Hash                                                              *)
     (*************************************************************************)
     | MV_lit_key_hash lit1 -> ZKeyHash.create_expr ctx lit1
     | MV_hash_key v1cc -> ZKeyHash.create_hashkey ctx (eov v1cc)
     (*************************************************************************)
     (* Timestamp                                                             *)
     (*************************************************************************)
     | MV_lit_timestamp_str lit1 -> (
       Ptime.of_rfc3339 ~strict:false ~sub:true ~start:0 lit1
       |> Result.ok
       |> function
       | None            -> (
         try Bigint.of_string lit1 |> ZInt.create_expr_of_bigint ctx with
         | _ ->
           SmtError ("Encoder : cv_mv : MV_lit_timestamp_str : " ^ lit1)
           |> raise
       )
       | Some (pt, _, _) -> (
         Ptime.to_span pt
         |> Ptime.Span.to_int_s
         |> function
         | None      ->
           VcError "Encoder : cv_mv : MV_lit_timestamp_str" |> raise
         | Some int1 -> ZInt.create_expr ctx int1
       )
     )
     | MV_lit_timestamp_sec lit1 -> ZTimestamp.create_expr_of_bigint ctx lit1
     | MV_add_tit (v1cc, v2cc) -> ZTimestamp.create_add ctx (eov v1cc) (eov v2cc)
     | MV_add_itt (v1cc, v2cc) -> ZTimestamp.create_add ctx (eov v1cc) (eov v2cc)
     | MV_sub_tit (v1cc, v2cc) -> ZTimestamp.create_sub ctx (eov v1cc) (eov v2cc)
     (*************************************************************************)
     (* Address                                                               *)
     (*************************************************************************)
     | MV_lit_address v1cc -> ZAddr.create_expr ctx (eov v1cc)
     | MV_address_of_contract v1cc ->
       ZContract.read_keyhash (eov v1cc) |> ZAddr.create_expr ctx
     (*************************************************************************)
     (* Key                                                                   *)
     (*************************************************************************)
     | MV_lit_key lit1 -> ZKey.create_expr ctx lit1
     (*************************************************************************)
     (* Unit                                                                  *)
     (*************************************************************************)
     | MV_unit -> ZUnit.create_expr ctx
     (*************************************************************************)
     (* Signature                                                             *)
     (*************************************************************************)
     | MV_lit_signature_str lit1 -> ZSig.create_expr ctx lit1
     | MV_lit_signature_signed (v1cc, v2cc) ->
       ZSig.create_signed ctx (eov v1cc) (eov v2cc)
     (*************************************************************************)
     (* Option                                                                *)
     (*************************************************************************)
     | MV_some v1cc -> ZOption.create_expr_some (sov v1cc) (eov v1cc)
     | MV_none t1cc -> ZOption.create_expr_none (sot t1cc)
     | MV_ediv_nnnn (v1cc, v2cc)
     | MV_ediv_niin (v1cc, v2cc)
     | MV_ediv_inin (v1cc, v2cc)
     | MV_ediv_iiin (v1cc, v2cc)
     | MV_ediv_mnmm (v1cc, v2cc)
     | MV_ediv_mmnm (v1cc, v2cc) ->
       let ((expr1 : Expr.t), (expr2 : Expr.t)) = (eov v1cc, eov v2cc) in
       let (typ : mich_t cc) = typ_of_val (gdc value) in
       let (sort : Sort.t) = sot typ in
       Formula.if_then_else ctx
         ~if_:(Formula.create_eq ctx expr2 (ZInt.create_expr ctx 0))
         ~then_:(ZOption.create_expr_none sort)
         ~else_:
           (ZOption.create_expr_some sort
              (ZPair.create_expr
                 (typ |> get_innertyp |> sot)
                 ( ZInt.create_div ctx expr1 expr2,
                   ZInt.create_mod ctx expr1 expr2
                 )
              )
           )
     | MV_get_xmoy (v1cc, v2cc)
     | MV_get_xbmo (v1cc, v2cc) ->
       ZMap.read_value ctx ~key:(eov v1cc) (eov v2cc)
     | MV_slice_nnso (v1cc, v2cc, v3cc) ->
       ZStr.create_slice ctx ~offset:(eov v1cc) ~len:(eov v2cc) (eov v3cc)
     | MV_slice_nnbo (v1cc, v2cc, v3cc) ->
       ZBytes.create_slice ctx ~offset:(eov v1cc) ~len:(eov v2cc) (eov v3cc)
     | MV_unpack _ -> Not_Implemented |> raise
     | MV_contract_of_address (t1cc, v2cc) ->
       ZContract.create_expr_of_address (sot t1cc) (eov v2cc)
     | MV_isnat v1cc ->
       let (expr1 : Expr.t) = eov v1cc in
       Formula.if_then_else ctx
         ~if_:(Formula.create_arith_ge ctx expr1 (ZInt.create_expr ctx 0))
         ~then_:(ZOption.create_expr_some (gdc value |> sov) expr1)
         ~else_:(ZOption.create_expr_none (gdc value |> sov))
     (*************************************************************************)
     (* List                                                                  *)
     (*************************************************************************)
     | MV_lit_list (t1cc, vcc_lst) ->
       List.fold_right vcc_lst
         ~f:(fun vcc expr -> ZList.create_cons ~content:(eov vcc) expr)
         ~init:(ZList.create_expr_nil (sot t1cc))
     | MV_nil t1cc -> ZList.create_expr_nil (sot t1cc)
     | MV_cons (v1cc, v2cc) -> ZList.create_cons ~content:(eov v1cc) (eov v2cc)
     | MV_tl_l v1cc -> ZList.read_tail (eov v1cc)
     (*************************************************************************)
     (* Set                                                                   *)
     (*************************************************************************)
     | MV_lit_set (t1cc, vcc_lst) ->
       let (expr_true : Expr.t) = ZBool.create_expr ctx true in
       List.fold vcc_lst
         ~init:(ZSet.create_expr_empty_set ctx (sot t1cc))
         ~f:(fun expr vcc ->
           ZSet.update ctx ~content:(eov vcc) ~flag:expr_true expr)
     | MV_empty_set t1cc -> ZSet.create_expr_empty_set ctx (sot t1cc)
     | MV_update_xbss (v1cc, v2cc, v3cc) ->
       ZSet.update ctx ~content:(eov v1cc) ~flag:(eov v2cc) (eov v3cc)
     (*************************************************************************)
     (* Operation                                                             *)
     (*************************************************************************)
     | MV_create_contract (_, _, _, v4cc, v5cc, _, v7cc) ->
       ZOperation.create_expr_create_contract ctx (eov v4cc) (eov v5cc)
         (eov v7cc)
     | MV_transfer_tokens (_, v2cc, _) ->
       ZOperation.create_expr_transfer_tokens ctx (eov v2cc)
     | MV_set_delegate v1cc -> ZOperation.create_expr_set_delegate ctx (eov v1cc)
     (*************************************************************************)
     (* Contract                                                              *)
     (*************************************************************************)
     | MV_lit_contract (t1cc, v2cc) ->
       ZContract.create_expr_of_address (sot t1cc) (eov v2cc)
     | MV_self _ -> Not_Implemented |> raise
     | MV_implicit_account v1cc -> ZContract.create_expr ctx (eov v1cc)
     (*************************************************************************)
     (* Pair                                                                  *)
     (*************************************************************************)
     | MV_pair (v1cc, v2cc) ->
       ZPair.create_expr (gdc value |> sov) (eov v1cc, eov v2cc)
     (*************************************************************************)
     (* Or                                                                    *)
     (*************************************************************************)
     | MV_left (_, v2cc) -> ZOr.create_expr_left (gdc value |> sov) (eov v2cc)
     | MV_right (_, v2cc) -> ZOr.create_expr_right (gdc value |> sov) (eov v2cc)
     (*************************************************************************)
     (* Lambda                                                                *)
     (*************************************************************************)
     | MV_lit_lambda _ -> Not_Implemented |> raise
     | MV_lambda_unknown (t1cc, t2cc) ->
       let (domain_expr : Expr.t) = ZLambda.create_expr_domain ctx (sot t1cc) in
       ZLambda.create_expr (sot t1cc) domain_expr
         (Expr.create_dummy ctx (sot t2cc))
     | MV_lambda_closure (v1cc, v2cc) ->
       ZLambda.create_apply ctx (eov v2cc) (eov v1cc)
     (*************************************************************************)
     (* Map                                                                   *)
     (*************************************************************************)
     | MV_lit_map (_, t2cc, vcc_pair_lst) ->
       let (sort2 : Sort.t) = MT_option t2cc |> gdc |> sot in
       List.fold vcc_pair_lst
         ~init:(ZMap.create_expr_empty_map ctx (gdc value |> sov))
         ~f:(fun expr (v1cc, v2cc) ->
           ZMap.update ctx ~key:(eov v1cc)
             ~data:(ZOption.create_expr_some sort2 (eov v2cc))
             expr)
     | MV_empty_map _ -> ZMap.create_expr_empty_map ctx (gdc value |> sov)
     | MV_update_xomm (v1cc, v2cc, v3cc) ->
       ZMap.update ctx ~key:(eov v1cc) ~data:(eov v2cc) (eov v3cc)
     (*************************************************************************)
     (* Big Map                                                               *)
     (*************************************************************************)
     | MV_lit_big_map (_, t2cc, vcc_pair_lst) ->
       let (sort2 : Sort.t) = MT_option t2cc |> gdc |> sot in
       List.fold vcc_pair_lst
         ~init:(ZMap.create_expr_empty_map ctx (gdc value |> sov))
         ~f:(fun expr (v1cc, v2cc) ->
           ZMap.update ctx ~key:(eov v1cc)
             ~data:(ZOption.create_expr_some sort2 (eov v2cc))
             expr)
     | MV_empty_big_map _ -> ZMap.create_expr_empty_map ctx (gdc value |> sov)
     | MV_update_xobmbm (v1cc, v2cc, v3cc) ->
       ZMap.update ctx ~key:(eov v1cc) ~data:(eov v2cc) (eov v3cc)
     (****************************************************************************)
     (* Chain Id                                                                 *)
     (****************************************************************************)
     | MV_lit_chain_id lit1 -> ZStr.create_expr ctx lit1
     (****************************************************************************)
     (* Custom Domain Value for Invariant Synthesis                              *)
     (****************************************************************************)
     | MV_ref _ -> Not_Implemented |> raise
     | MV_ref_cont _ -> Not_Implemented |> raise
     | MV_sigma_tmplm _ -> Not_Implemented |> raise
  (* function cv_mv end *)

  and cv_mvcc : Smt.Ctx.t -> Tz.mich_v Tz.cc -> Smt.Expr.t =
    (fun ctx value_cc -> cv_mv ctx value_cc.cc_v)
  (* function cv_mvcc end *)

  and cv_compare :
      Smt.Ctx.t ->
      Tz.mich_t * Smt.Expr.t ->
      Tz.mich_t * Smt.Expr.t ->
      Smt.Expr.t =
     let open Tz in
     let open Smt in
     fun ctx (typ1, expr1) (typ2, expr2) ->
     let (zero : Expr.t) = ZInt.create_expr ctx 0 in
     let (one : Expr.t) = ZInt.create_expr ctx 1 in
     let (minus_one : Expr.t) = ZInt.create_expr ctx (-1) in
     let (dummy : Expr.t) = Expr.create_dummy ctx (ZInt.create_sort ctx) in
     (* syntax sugar *)
     match (typ1, typ2) with
     | (MT_int, MT_int)
     | (MT_nat, MT_nat)
     | (MT_mutez, MT_mutez)
     | (MT_timestamp, MT_timestamp) ->
       Formula.if_then_else ctx
         ~if_:(Formula.create_eq ctx expr1 expr2)
         ~then_:zero
         ~else_:
           (Formula.if_then_else ctx
              ~if_:(Formula.create_arith_lt ctx expr1 expr2)
              ~then_:minus_one ~else_:one
           )
     | (MT_bool, MT_bool) ->
       Formula.if_then_else ctx
         ~if_:(Formula.create_eq ctx expr1 expr2)
         ~then_:zero
         ~else_:
           (Formula.if_then_else ctx
              ~if_:(Formula.create_is_false ctx expr1)
              ~then_:minus_one ~else_:one
           )
     | (MT_string, MT_string)
     | (MT_chain_id, MT_chain_id) ->
       Formula.if_then_else ctx
         ~if_:(Formula.create_eq ctx expr1 expr2)
         ~then_:zero
         ~else_:
           (Formula.if_then_else ctx
              ~if_:(Formula.create_str_lt ctx expr1 expr2)
              ~then_:minus_one ~else_:one
           )
     | (MT_unit, MT_unit) -> zero
     | (MT_key, MT_key) ->
       cv_compare ctx
         (MT_string, ZKey.read_content expr1)
         (MT_string, ZKey.read_content expr2)
     | (MT_key_hash, MT_key_hash) ->
       Formula.if_then_else ctx
         ~if_:(Formula.create_is_keyhash_str expr1)
         ~then_:
           (Formula.if_then_else ctx
              ~if_:(Formula.create_is_keyhash_str expr2)
              ~then_:
                (cv_compare ctx
                   (MT_string, ZKeyHash.read_content_str expr1)
                   (MT_string, ZKeyHash.read_content_str expr2)
                )
              ~else_:dummy
           )
         ~else_:
           (Formula.if_then_else ctx
              ~if_:(Formula.create_is_keyhash_key expr2)
              ~then_:
                (cv_compare ctx
                   (MT_key, ZKeyHash.read_content_key expr1)
                   (MT_key, ZKeyHash.read_content_key expr2)
                )
              ~else_:dummy
           )
     | (MT_option t1cc, MT_option t2cc)
       when Tz.equal_cc Tz.equal_mich_t t1cc t2cc ->
       Formula.if_then_else ctx
         ~if_:(Formula.create_is_option_none expr1)
         ~then_:
           (Formula.if_then_else ctx
              ~if_:(Formula.create_is_option_none expr2)
              ~then_:zero ~else_:minus_one
           )
         ~else_:
           (Formula.if_then_else ctx
              ~if_:(Formula.create_is_option_none expr2)
              ~then_:one
              ~else_:
                (cv_compare ctx
                   (t1cc.cc_v, ZOption.read_content expr1)
                   (t2cc.cc_v, ZOption.read_content expr2)
                )
           )
     | (MT_pair (t11cc, t12cc), MT_pair (t21cc, t22cc))
       when Tz.equal_cc Tz.equal_mich_t t11cc t21cc
            && Tz.equal_cc Tz.equal_mich_t t12cc t22cc ->
       let (cmp_fst : Expr.t) =
          cv_compare ctx
            (t11cc.cc_v, ZPair.read_content_fst expr1)
            (t21cc.cc_v, ZPair.read_content_fst expr2)
       in
       let (cmp_snd : Expr.t) =
          cv_compare ctx
            (t12cc.cc_v, ZPair.read_content_snd expr1)
            (t22cc.cc_v, ZPair.read_content_snd expr2)
       in
       Formula.if_then_else ctx
         ~if_:(Formula.create_neq ctx cmp_fst zero)
         ~then_:cmp_fst ~else_:cmp_snd
     | (MT_bytes, MT_bytes) -> raise Not_Implemented
     | (MT_signature, MT_signature) ->
       Formula.if_then_else ctx
         ~if_:(Formula.create_is_signature_str expr1)
         ~then_:
           (Formula.if_then_else ctx
              ~if_:(Formula.create_is_signature_str expr2)
              ~then_:
                (cv_compare ctx
                   (MT_string, ZSig.read_str expr1)
                   (MT_string, ZSig.read_str expr2)
                )
              ~else_:dummy
           )
         ~else_:
           (Formula.if_then_else ctx
              ~if_:(Formula.create_eq ctx expr1 expr2)
              ~then_:zero ~else_:dummy
           )
     | (MT_address, MT_address) -> raise Not_Implemented
     | (MT_or (t11cc, t12cc), MT_or (t21cc, t22cc))
       when Tz.equal_cc Tz.equal_mich_t t11cc t21cc
            && Tz.equal_cc Tz.equal_mich_t t12cc t22cc ->
       Formula.if_then_else ctx
         ~if_:(Formula.create_is_or_left expr1)
         ~then_:
           (Formula.if_then_else ctx
              ~if_:(Formula.create_is_or_left expr2)
              ~then_:
                (cv_compare ctx
                   (t11cc.cc_v, ZOr.read_content_left expr1)
                   (t21cc.cc_v, ZOr.read_content_left expr2)
                )
              ~else_:minus_one
           )
         ~else_:
           (Formula.if_then_else ctx
              ~if_:(Formula.create_is_or_right expr2)
              ~then_:
                (cv_compare ctx
                   (t12cc.cc_v, ZOr.read_content_right expr1)
                   (t22cc.cc_v, ZOr.read_content_right expr2)
                )
              ~else_:one
           )
     | _ -> VcError "Encoder : cv_compare : wrong comparison" |> raise
  (* function cv_compare end *)

  let rec cv_mf : Smt.Ctx.t -> Tz.mich_f -> Smt.Formula.t =
     let open Tz in
     let open TzUtil in
     let open Smt in
     fun ctx fmla ->
     let eov value_cc = cv_mvcc ctx value_cc in
     let fof fmla = cv_mf ctx fmla in
     (* syntax sugar *)
     match fmla with
     (* Logical Formula *)
     | MF_true -> Formula.create_true ctx
     | MF_false -> Formula.create_false ctx
     | MF_not f1 -> Formula.create_not ctx (fof f1)
     | MF_and fl1 -> Formula.create_and ctx (List.map fl1 ~f:fof)
     | MF_or fl1 -> Formula.create_or ctx (List.map fl1 ~f:fof)
     | MF_eq (v1, v2) -> Formula.create_eq ctx (eov v1) (eov v2)
     | MF_imply (f1, f2) -> Formula.create_imply ctx (fof f1) (fof f2)
     (* MicSE Branch *)
     | MF_is_true v1 -> Formula.create_is_true ctx (eov v1)
     | MF_is_none v1 -> Formula.create_is_option_none (eov v1)
     | MF_is_left v1 -> Formula.create_is_or_left (eov v1)
     | MF_is_cons v1 -> Formula.create_is_list_cons (eov v1)
     (* MicSE Datatype Constraint *)
     | MF_mutez_bound v1 -> Formula.create_mutez_bound ctx (eov v1)
     | MF_nat_bound v1 -> Formula.create_nat_bound ctx (eov v1)
     (* Custom Formula for verifiying *)
     | MF_add_mmm_no_overflow (v1, v2) ->
       Formula.create_add_no_overflow ctx (eov v1) (eov v2)
     | MF_sub_mmm_no_underflow (v1, v2) ->
       Formula.create_sub_no_underflow ctx (eov v1) (eov v2)
     | MF_mul_mnm_no_overflow (v1, v2) ->
       Formula.create_mul_no_overflow ctx (eov v1) (eov v2)
     | MF_mul_nmm_no_overflow (v1, v2) ->
       Formula.create_mul_no_overflow ctx (eov v1) (eov v2)
     | MF_shiftL_nnn_rhs_in_256 _ -> raise Not_Implemented
     | MF_shiftR_nnn_rhs_in_256 _ -> raise Not_Implemented
  (* function cv_mf end *)
end
