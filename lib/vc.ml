(* Vc: Verification condition manager *)

exception VcError of string

open! Core

(******************************************************************************)
(******************************************************************************)
(* Smt Encoder                                                                *)
(******************************************************************************)
(******************************************************************************)

module Encoder = struct
  exception Not_Implemented

  let rec cv_mtcc_i : Smt.Ctx.t -> Tz.mich_t Tz.cc -> Smt.Sort.t =
     let open Tz in
     let open Smt in
     fun ctx typ_cc ->
     let sot typ_cc = cv_mtcc ctx typ_cc in
     (* syntax sugar *)
     match typ_cc.cc_v with
     | MT_key                  -> ZKey.create_sort ctx
     | MT_unit                 -> ZUnit.create_sort ctx
     | MT_signature            -> ZSig.create_sort ctx
     | MT_option t1cc          ->
       ZOption.create_sort ctx ~typ:typ_cc ~content_sort:(sot t1cc)
     | MT_list t1cc            ->
       ZList.create_sort ctx ~typ:typ_cc ~content_sort:(sot t1cc)
     | MT_set t1cc             ->
       ZSet.create_sort ctx ~typ:typ_cc ~content_sort:(sot t1cc)
     | MT_operation            -> ZOperation.create_sort ctx
     | MT_contract t1cc        ->
       ZContract.create_sort ctx ~typ:typ_cc ~content_sort:(sot t1cc)
     | MT_pair (t1cc, t2cc)    ->
       ZPair.create_sort ctx ~typ:typ_cc ~content_sort:(sot t1cc, sot t2cc)
     | MT_or (t1cc, t2cc)      ->
       ZOr.create_sort ctx ~typ:typ_cc ~content_sort:(sot t1cc, sot t2cc)
     | MT_lambda (t1cc, t2cc)  ->
       ZLambda.create_sort ctx ~typ:typ_cc ~domain_sort:(sot t1cc)
         ~range_sort:(sot t2cc)
     | MT_map (t1cc, t2cc)     ->
       ZMap.create_sort ctx ~typ:typ_cc ~key_sort:(sot t1cc)
         ~data_body_sort:(sot t2cc)
     | MT_big_map (t1cc, t2cc) ->
       ZMap.create_sort ctx ~typ:typ_cc ~key_sort:(sot t1cc)
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
     let open Tz in
     fun ctx typ_cc ->
     try cv_mtcc_i ctx typ_cc with
     | Z3.Error s ->
       VcError
         (s
         ^ " : "
         ^ Sexp.to_string
             (sexp_of_cc sexp_of_mich_t typ_cc |> SexpUtil.tz_cc_sexp_form)
         )
       |> raise
  (* function cv_mtcc end *)

  let rec cv_mvcc_i :
      sctx:Tz.mich_sym_ctxt -> Smt.Ctx.t -> Tz.mich_v Tz.cc -> Smt.Expr.t =
     let open Tz in
     let open TzUtil in
     let open Smt in
     fun ~sctx ctx value_cc ->
     let gcc v1 = gen_custom_cc value_cc v1 in
     let sot t1cc = cv_mtcc ctx t1cc in
     let sov v1cc = cv_mtcc ctx (typ_of_val v1cc) in
     let eov v1cc = cv_mvcc ~sctx ctx v1cc in
     (* syntax sugar *)
     let (sort : Sort.t) = sov value_cc in
     let fv arg_lst =
        let (field : string) =
           match sexp_of_mich_v value_cc.cc_v with
           | Sexp.List (Sexp.Atom s :: _) -> s
           | s ->
             VcError ("Encoder : cv_mvcc_i : fv : " ^ (s |> SexpUtil.to_string))
             |> raise
        in
        Expr.create_var ctx sort
          ~name:
            (Sexp.to_string (sexp_of_mich_sym_ctxt sctx)
            ^ field
            ^ (List.map arg_lst ~f:(fun vcc ->
                   sexp_of_mich_v vcc.cc_v |> SexpUtil.to_string
               )
              |> String.concat
              )
            )
        (* inner-function fv end *)
     in
     let eos cont =
        let (zero : Expr.t) =
           match (typ_of_val value_cc).cc_v with
           | MT_mutez -> ZMutez.create_expr ctx 0
           | MT_nat   -> ZNat.create_expr ctx 0
           | MT_int   -> ZInt.create_expr ctx 0
           | _        -> VcError "" |> raise
        in
        let acc elem =
           acc_of_sigma ~sigma:value_cc ~ctx:sctx elem |> snd |> eov
        in
        let sum lst =
           if List.is_empty lst
           then zero
           else
             List.map lst ~f:acc
             |>
             match (typ_of_val value_cc).cc_v with
             | MT_mutez -> ZNat.create_add_lst ctx
             | MT_nat   -> ZNat.create_add_lst ctx
             | MT_int   -> ZNat.create_add_lst ctx
             | _        -> VcError "" |> raise
        in
        match cont.cc_v with
        | MV_nil _ -> zero
        | MV_empty_map _ -> zero
        | MV_empty_big_map _ -> zero
        | MV_lit_list (_, vlst) -> sum vlst
        | MV_lit_map (_, _, kvlst) -> sum (List.map kvlst ~f:snd)
        | MV_lit_big_map (_, _, kvlst) -> sum (List.map kvlst ~f:snd)
        | _ -> fv [ cont |> opt_mvcc ~ctx:sctx |> snd ]
        (* inner-function eos end *)
     in
     match value_cc.cc_v with
     (*************************************************************************)
     (* Symbol & Polymorphic                                                  *)
     (*************************************************************************)
     | MV_symbol (t1, c2) ->
       Expr.create_var ctx (sot t1)
         ~name:
           (Sexp.to_string (sexp_of_mich_sym_ctxt sctx)
           ^ Sexp.to_string (sexp_of_mich_sym_category c2)
           )
     | MV_car v1cc -> (
       match v1cc.cc_v with
       (* Optimization Rules **************************************************)
       | MV_unlift_option v11cc -> (
         match v11cc.cc_v with
         | MV_ediv_nnnn (v111cc, v112cc)
         | MV_ediv_mmnm (v111cc, v112cc) -> (
           match v112cc.cc_v with
           | MV_lit_nat l1 when Bigint.equal l1 (Bigint.of_int 1) -> eov v111cc
           | _ -> ZInt.create_div ctx (eov v111cc) (eov v112cc)
         )
         | MV_ediv_niin (v111cc, v112cc)
         | MV_ediv_inin (v111cc, v112cc)
         | MV_ediv_iiin (v111cc, v112cc) -> (
           match v112cc.cc_v with
           | MV_lit_int l1 when Bigint.equal l1 (Bigint.of_int 1) -> eov v111cc
           | _ -> ZInt.create_div ctx (eov v111cc) (eov v112cc)
         )
         | MV_ediv_mnmm (v111cc, v112cc) -> (
           match v112cc.cc_v with
           | MV_lit_mutez l1 when Bigint.equal l1 (Bigint.of_int 1) ->
             eov v111cc
           | _ -> ZInt.create_div ctx (eov v111cc) (eov v112cc)
         )
         | _ -> ZOption.read_content (eov v11cc) |> ZPair.read_content_fst
       )
       (* Normal Case *********************************************************)
       | _ -> ZPair.read_content_fst (eov v1cc)
     )
     | MV_cdr v1cc -> (
       match v1cc.cc_v with
       (* Optimization Rules **************************************************)
       | MV_unlift_option v11cc -> (
         match v11cc.cc_v with
         | MV_ediv_nnnn (v111cc, v112cc)
         | MV_ediv_mmnm (v111cc, v112cc) -> (
           match v112cc.cc_v with
           | MV_lit_nat l1 when Bigint.equal l1 (Bigint.of_int 1) ->
             ZInt.create_expr ctx 0
           | _ -> ZInt.create_mod ctx (eov v111cc) (eov v112cc)
         )
         | MV_ediv_niin (v111cc, v112cc)
         | MV_ediv_inin (v111cc, v112cc)
         | MV_ediv_iiin (v111cc, v112cc) -> (
           match v112cc.cc_v with
           | MV_lit_int l1 when Bigint.equal l1 (Bigint.of_int 1) ->
             ZInt.create_expr ctx 0
           | _ -> ZInt.create_mod ctx (eov v111cc) (eov v112cc)
         )
         | MV_ediv_mnmm (v111cc, v112cc) -> (
           match v112cc.cc_v with
           | MV_lit_mutez l1 when Bigint.equal l1 (Bigint.of_int 1) ->
             ZInt.create_expr ctx 0
           | _ -> ZInt.create_mod ctx (eov v111cc) (eov v112cc)
         )
         | _ -> ZOption.read_content (eov v11cc) |> ZPair.read_content_snd
       )
       (* Normal Case *********************************************************)
       | _ -> ZPair.read_content_snd (eov v1cc)
     )
     | MV_unlift_option v1cc -> (
       match v1cc.cc_v with
       (* Optimization Rules **************************************************)
       | MV_contract_of_address (_, v2cc) ->
         ZContract.create_expr_of_address sort (eov v2cc)
       (* Normal Case *********************************************************)
       | _ -> ZOption.read_content (eov v1cc)
     )
     | MV_unlift_left v1cc -> ZOr.read_content_left (eov v1cc)
     | MV_unlift_right v1cc -> ZOr.read_content_right (eov v1cc)
     | MV_hd_l v1cc -> ZList.read_head (eov v1cc)
     | MV_exec (v1cc, v2cc) -> ZLambda.create_exec (eov v1cc) (eov v2cc)
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
     | MV_mtz_of_nat_mn v1cc -> eov v1cc
     | MV_add_nnn (v1cc, v2cc)
     | MV_add_mnn (v1cc, v2cc) ->
       ZNat.create_add ctx (eov v1cc) (eov v2cc)
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
     | MV_size_l v1cc -> fv [ v1cc ]
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
     | MV_mtz_of_op_list v1cc ->
       Formula.if_then_else ctx
         ~if_:(Formula.create_is_list_nil (eov v1cc))
         ~then_:(ZMutez.create_expr ctx 0)
         ~else_:(fv [ v1cc ])
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
     | MV_check_signature (v1cc, v2cc, v3cc) -> fv [ v1cc; v2cc; v3cc ]
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
           VcError ("Encoder : cv_mv : MV_lit_timestamp_str : " ^ lit1) |> raise
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
     | MV_some v1cc -> ZOption.create_expr_some sort (eov v1cc)
     | MV_none _ -> ZOption.create_expr_none sort
     | MV_ediv_nnnn (v1cc, v2cc)
     | MV_ediv_niin (v1cc, v2cc)
     | MV_ediv_inin (v1cc, v2cc)
     | MV_ediv_iiin (v1cc, v2cc)
     | MV_ediv_mnmm (v1cc, v2cc)
     | MV_ediv_mmnm (v1cc, v2cc) ->
       let ((expr1 : Expr.t), (expr2 : Expr.t)) = (eov v1cc, eov v2cc) in
       let (typ : mich_t cc) = typ_of_val value_cc in
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
     | MV_contract_of_address _ -> Expr.create_dummy ctx sort
     | MV_isnat v1cc ->
       let (expr1 : Expr.t) = eov v1cc in
       Formula.if_then_else ctx
         ~if_:(Formula.create_arith_ge ctx expr1 (ZInt.create_expr ctx 0))
         ~then_:(ZOption.create_expr_some sort expr1)
         ~else_:(ZOption.create_expr_none sort)
     (*************************************************************************)
     (* List                                                                  *)
     (*************************************************************************)
     | MV_lit_list (_, vcc_lst) ->
       List.fold_right vcc_lst
         ~f:(fun vcc expr -> ZList.create_cons ~content:(eov vcc) expr)
         ~init:(ZList.create_expr_nil sort)
     | MV_nil _ -> ZList.create_expr_nil sort
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
     | MV_empty_set _ -> ZSet.create_expr_empty_set ctx sort
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
     | MV_lit_contract (_, v2cc) ->
       ZContract.create_expr_of_address sort (eov v2cc)
     | MV_self _ -> Not_Implemented |> raise
     | MV_implicit_account v1cc -> ZContract.create_expr sort (eov v1cc)
     (*************************************************************************)
     (* Pair                                                                  *)
     (*************************************************************************)
     | MV_pair (v1cc, v2cc) -> ZPair.create_expr sort (eov v1cc, eov v2cc)
     (*************************************************************************)
     (* Or                                                                    *)
     (*************************************************************************)
     | MV_left (_, v2cc) -> ZOr.create_expr_left sort (eov v2cc)
     | MV_right (_, v2cc) -> ZOr.create_expr_right sort (eov v2cc)
     (*************************************************************************)
     (* Lambda                                                                *)
     (*************************************************************************)
     | MV_lit_lambda (t1cc, t2cc, _) ->
       let (domain_expr : Expr.t) = Expr.create_dummy ctx (sot t1cc) in
       ZLambda.create_expr sort domain_expr (Expr.create_dummy ctx (sot t2cc))
     | MV_lambda_unknown (t1cc, t2cc) ->
       let (domain_expr : Expr.t) = Expr.create_dummy ctx (sot t1cc) in
       ZLambda.create_expr sort domain_expr (Expr.create_dummy ctx (sot t2cc))
     | MV_lambda_closure (v1cc, v2cc) ->
       ZLambda.create_apply ctx (eov v2cc) (eov v1cc)
     (*************************************************************************)
     (* Map                                                                   *)
     (*************************************************************************)
     | MV_lit_map (t1cc, t2cc, vcc_pair_lst) ->
       let (sort2 : Sort.t) = MT_option t2cc |> gcc |> sot in
       List.fold vcc_pair_lst
         ~init:
           (ZMap.create_expr_empty_map ctx ~key_sort:(sot t1cc) ~data_sort:sort2)
         ~f:(fun expr (v1cc, v2cc) ->
           ZMap.update ctx ~key:(eov v1cc)
             ~data:(ZOption.create_expr_some sort2 (eov v2cc))
             expr)
     | MV_empty_map (t1cc, t2cc) ->
       let (sort2 : Sort.t) = MT_option t2cc |> gcc |> sot in
       ZMap.create_expr_empty_map ctx ~key_sort:(sot t1cc) ~data_sort:sort2
     | MV_update_xomm (v1cc, v2cc, v3cc) ->
       ZMap.update ctx ~key:(eov v1cc) ~data:(eov v2cc) (eov v3cc)
     (*************************************************************************)
     (* Big Map                                                               *)
     (*************************************************************************)
     | MV_lit_big_map (t1cc, t2cc, vcc_pair_lst) ->
       let (sort2 : Sort.t) = MT_option t2cc |> gcc |> sot in
       List.fold vcc_pair_lst
         ~init:
           (ZMap.create_expr_empty_map ctx ~key_sort:(sot t1cc) ~data_sort:sort2)
         ~f:(fun expr (v1cc, v2cc) ->
           ZMap.update ctx ~key:(eov v1cc)
             ~data:(ZOption.create_expr_some sort2 (eov v2cc))
             expr)
     | MV_empty_big_map (t1cc, t2cc) ->
       let (sort2 : Sort.t) = MT_option t2cc |> gcc |> sot in
       ZMap.create_expr_empty_map ctx ~key_sort:(sot t1cc) ~data_sort:sort2
     | MV_update_xobmbm (v1cc, v2cc, v3cc) ->
       ZMap.update ctx ~key:(eov v1cc) ~data:(eov v2cc) (eov v3cc)
     (****************************************************************************)
     (* Chain Id                                                                 *)
     (****************************************************************************)
     | MV_lit_chain_id lit1 -> ZStr.create_expr ctx lit1
     (****************************************************************************)
     (* Custom Domain Value for Invariant Synthesis                              *)
     (****************************************************************************)
     | MV_ref (t1cc, c2) ->
       Expr.create_var ctx (sot t1cc)
         ~name:
           (Sexp.to_string (sexp_of_mich_sym_ctxt sctx)
           ^ Sexp.to_string (sexp_of_mich_sym_category c2)
           )
     | MV_ref_cont t1cc ->
       Expr.create_var ctx (sot t1cc)
         ~name:("MV_ref_cont_" ^ Sexp.to_string (sexp_of_mich_sym_ctxt sctx))
     | MV_sigma_tmp_l_m2 v1cc -> eos v1cc
     | MV_sigma_m_l_m1 v1cc -> eos v1cc
     | MV_sigma_mmspnbppnmpnnpp_abm_m1 v1cc -> eos v1cc
     | MV_sigma_mmspnbppnmpnnpp_abm_smspnbppnm2 v1cc -> eos v1cc
     | MV_sigma_mspnbpp_nm_m1 v1cc -> eos v1cc
     | MV_sigma_n_am_n1 v1cc -> eos v1cc
     | MV_sigma_m_am_m1 v1cc -> eos v1cc
  (* function cv_mv end *)

  and cv_mvcc :
      sctx:Tz.mich_sym_ctxt -> Smt.Ctx.t -> Tz.mich_v Tz.cc -> Smt.Expr.t =
     let open Tz in
     fun ~sctx ctx value_cc ->
     try cv_mvcc_i ~sctx ctx value_cc with
     | Not_Implemented ->
       VcError
         ("Not Implemented : "
         ^ Sexp.to_string
             (sexp_of_cc sexp_of_mich_v value_cc |> SexpUtil.tz_cc_sexp_form)
         )
       |> raise
     | Z3.Error s      ->
       VcError
         (s
         ^ " : "
         ^ Sexp.to_string
             (sexp_of_cc sexp_of_mich_v value_cc |> SexpUtil.tz_cc_sexp_form)
         )
       |> raise
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
     | (MT_address, MT_address) ->
       cv_compare ctx
         (MT_key_hash, ZAddr.read_content expr1)
         (MT_key_hash, ZAddr.read_content expr2)
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
     | _ ->
       VcError
         ("Encoder : cv_compare : wrong comparison : "
         ^ (typ1 |> sexp_of_mich_t |> Sexp.to_string)
         ^ " - "
         ^ (typ2 |> sexp_of_mich_t |> Sexp.to_string)
         )
       |> raise
  (* function cv_compare end *)

  let rec cv_mf : Smt.Ctx.t -> Tz.mich_f -> Smt.Formula.t =
     let open Tz in
     let open TzUtil in
     let open Smt in
     fun ctx fmla ->
     let eov { ctx_i; ctx_v } = cv_mvcc ~sctx:ctx_i ctx ctx_v in
     let fof fmla = cv_mf ctx fmla in
     (* syntax sugar *)
     try
       match fmla with
       (* Logical Formula *)
       | MF_true -> Formula.create_true ctx
       | MF_false -> Formula.create_false ctx
       | MF_not f1 -> Formula.create_not ctx (fof f1)
       | MF_and fl1 -> Formula.create_and ctx (List.map fl1 ~f:fof)
       | MF_or fl1 -> Formula.create_or ctx (List.map fl1 ~f:fof)
       | MF_eq (v1, v2) -> (
         match (typ_of_val v1.ctx_v).cc_v with
         | MT_list _
         | MT_map (_, _)
         | MT_big_map (_, _)
         | MT_set _ ->
           let (cont_eq : Formula.t) =
              Formula.create_eq ctx (eov v1) (eov v2)
           in
           let (sigma_eq : Formula.t list) =
              List.map2_exn (sigma_of_cont v1.ctx_v) (sigma_of_cont v2.ctx_v)
                ~f:(fun sigma1 sigma2 ->
                  Formula.create_eq ctx
                    (eov (gen_mich_v_ctx ~ctx:v1.ctx_i sigma1))
                    (eov (gen_mich_v_ctx ~ctx:v2.ctx_i sigma2))
              )
           in
           Formula.create_and ctx (cont_eq :: sigma_eq)
         | _ -> Formula.create_eq ctx (eov v1) (eov v2)
       )
       | MF_imply (f1, f2) -> Formula.create_imply ctx (fof f1) (fof f2)
       (* MicSE Branch *)
       | MF_is_true v1 -> Formula.create_is_true ctx (eov v1)
       | MF_is_none v1 -> Formula.create_is_option_none (eov v1)
       | MF_is_left v1 -> Formula.create_is_or_left (eov v1)
       | MF_is_cons v1 -> Formula.create_is_list_cons (eov v1)
       (* MicSE Datatype Constraint *)
       | MF_mutez_bound v1 -> Formula.create_mutez_bound ctx (eov v1)
       | MF_nat_bound v1 -> Formula.create_nat_bound ctx (eov v1)
       | MF_map_default_value v1 -> Formula.create_map_default_value ctx (eov v1)
       | MF_set_default_value v1 -> Formula.create_set_default_value ctx (eov v1)
       (* Custom Formula for verifiying *)
       | MF_add_mmm_no_overflow (v1, v2) ->
         Formula.create_add_no_overflow ctx (eov v1) (eov v2)
       | MF_sub_mmm_no_underflow (v1, v2) ->
         Formula.create_sub_no_underflow ctx (eov v1) (eov v2)
       | MF_mul_mnm_no_overflow (v1, v2) ->
         Formula.create_mul_no_overflow ctx (eov v1) (eov v2)
       | MF_mul_nmm_no_overflow (v1, v2) ->
         Formula.create_mul_no_overflow ctx (eov v1) (eov v2)
       | MF_shiftL_nnn_rhs_in_256 (_, v2) ->
         Formula.create_shift_l_rhs_in_256 ctx (eov v2)
       | MF_shiftR_nnn_rhs_in_256 (_, v2) ->
         Formula.create_shift_r_rhs_in_256 ctx (eov v2)
       (* Custom Formula for state merging *)
       | MF_time_leq (v1, v2) -> Formula.create_arith_le ctx (eov v1) (eov v2)
       (* Custom Formula for represent property *)
       | MF_all_element_equal_to (v1, v2) -> (
         match (typ_of_val v1.ctx_v).cc_v with
         | MT_map (ktcc, _)
         | MT_big_map (ktcc, _) ->
           let (key : Expr.t) = Expr.create_dummy ctx (cv_mtcc ctx ktcc) in
           let (map : Expr.t) = eov v1 in
           let (value_opt : Expr.t) = ZMap.read_value ctx ~key map in
           let (value : Expr.t) = ZOption.read_content value_opt in
           let (mem_f : Formula.t) =
              Formula.create_is_true ctx (ZMap.read_mem ctx ~key map)
           in
           let (elem_f : Formula.t) = Formula.create_is_option_some value_opt in
           let (prec : Formula.t) = Formula.create_and ctx [ mem_f; elem_f ] in
           Formula.create_forall ctx [ key ]
             (Formula.create_imply ctx prec
                (Formula.create_eq ctx value (eov v2))
             )
         | _ -> VcError "Encoder : cv_mf : MF_all_element_equal_to" |> raise
       )
     with
     | Z3.Error s ->
       VcError
         (s
         ^ " : "
         ^ Sexp.to_string (sexp_of_mich_f fmla |> SexpUtil.tz_cc_sexp_form)
         )
       |> raise
  (* function cv_mf end *)
end

(******************************************************************************)
(******************************************************************************)
(* Utility                                                                    *)
(******************************************************************************)
(******************************************************************************)

let get_hd1 : 'a list -> 'a =
  fun vlst ->
  match vlst with
  | v1 :: _ -> v1
  | _       -> VcError "get_hd1 : wrong mich stack status" |> raise
(* function get_hd1 end *)

let get_hd2 : 'a list -> 'a * 'a =
  fun vlst ->
  match vlst with
  | v1 :: v2 :: _ -> (v1, v2)
  | _             -> VcError "get_hd2 : wrong mich stack status" |> raise
(* function get_hd2 end *)

let property_of_query :
    sctx:Tz.mich_sym_ctxt -> Tz.mich_cut_info -> Tz.sym_image -> Tz.mich_f =
   let open Tz in
   let open TzUtil in
   fun ~sctx mci si ->
   match mci.mci_cutcat with
   | MCC_query qc -> (
     match qc with
     | Q_mutez_add_no_overflow ->
       let ((v1 : mich_v cc), (v2 : mich_v cc)) = get_hd2 si.si_mich in
       MF_add_mmm_no_overflow
         (gen_mich_v_ctx v1 ~ctx:sctx, gen_mich_v_ctx v2 ~ctx:sctx)
     | Q_mutez_sub_no_underflow ->
       let ((v1 : mich_v cc), (v2 : mich_v cc)) = get_hd2 si.si_mich in
       MF_sub_mmm_no_underflow
         (gen_mich_v_ctx v1 ~ctx:sctx, gen_mich_v_ctx v2 ~ctx:sctx)
     | Q_mutez_mul_mnm_no_overflow ->
       let ((v1 : mich_v cc), (v2 : mich_v cc)) = get_hd2 si.si_mich in
       MF_mul_mnm_no_overflow
         (gen_mich_v_ctx v1 ~ctx:sctx, gen_mich_v_ctx v2 ~ctx:sctx)
     | Q_mutez_mul_nmm_no_overflow ->
       let ((v1 : mich_v cc), (v2 : mich_v cc)) = get_hd2 si.si_mich in
       MF_mul_nmm_no_overflow
         (gen_mich_v_ctx v1 ~ctx:sctx, gen_mich_v_ctx v2 ~ctx:sctx)
     | Q_shiftleft_safe ->
       let ((v1 : mich_v cc), (v2 : mich_v cc)) = get_hd2 si.si_mich in
       MF_shiftL_nnn_rhs_in_256
         (gen_mich_v_ctx v1 ~ctx:sctx, gen_mich_v_ctx v2 ~ctx:sctx)
     | Q_shiftright_safe ->
       let ((v1 : mich_v cc), (v2 : mich_v cc)) = get_hd2 si.si_mich in
       MF_shiftR_nnn_rhs_in_256
         (gen_mich_v_ctx v1 ~ctx:sctx, gen_mich_v_ctx v2 ~ctx:sctx)
     | Q_assertion ->
       let (v1 : mich_v cc) = get_hd1 si.si_mich in
       MF_is_true (gen_mich_v_ctx v1 ~ctx:sctx)
   )
   | _            -> VcError "gen_query_property : wrong mci" |> raise
(* function property_of_query end *)

let fmla_for_initial_storage :
    sctx:Tz.mich_sym_ctxt ->
    Tz.mich_cut_info ->
    Tz.sym_image ->
    Tz.mich_v Tz.cc ->
    Tz.mich_f =
   let open Tz in
   let open TzUtil in
   fun ~sctx mci si init_strg ->
   match mci.mci_cutcat with
   | MCC_trx_entry ->
     let (param_strg : mich_v cc) = get_hd1 si.si_mich in
     let (sym_strg : mich_v cc) = gen_dummy_cc (MV_cdr param_strg) in
     MF_eq
       (gen_mich_v_ctx sym_strg ~ctx:sctx, gen_mich_v_ctx init_strg ~ctx:sctx)
   | _             -> VcError "apply_initial_storage : wrong mci" |> raise
(* function fmla_for_initial_storage end *)

let get_from_stack : Tz.mich_v Tz.cc list -> loc:int -> Tz.mich_v Tz.cc =
  (fun stack ~loc -> List.nth_exn stack (List.length stack - loc - 1))
(* function get_from_stack end *)

let apply_inv_at_start :
    sctx:Tz.mich_sym_ctxt ->
    Tz.mich_cut_info ->
    Tz.sym_image ->
    Tz.mich_f ->
    Tz.mich_f =
   let open Tz in
   let open TzUtil in
   fun ~sctx mci si inv ->
   let (mapf : mich_v -> mich_v) = function
   | MV_ref (t, msc2) -> (
     match msc2 with
     | MSC_contract             -> si.si_param.ti_contract.cc_v
     | MSC_source               -> si.si_param.ti_source.cc_v
     | MSC_sender               -> si.si_param.ti_sender.cc_v
     | MSC_param                -> si.si_param.ti_param.cc_v
     | MSC_amount               -> si.si_param.ti_amount.cc_v
     | MSC_time                 -> si.si_param.ti_time.cc_v
     | MSC_balance              -> si.si_balance.cc_v
     | MSC_bc_balance           -> si.si_bc_balance.cc_v
     | MSC_mich_stack loc       -> (get_from_stack si.si_mich ~loc).cc_v
     | MSC_dip_stack loc        -> (get_from_stack si.si_dip ~loc).cc_v
     | MSC_map_entry_stack loc  -> (get_from_stack si.si_map_entry ~loc).cc_v
     | MSC_map_exit_stack loc   -> (get_from_stack si.si_map_exit ~loc).cc_v
     | MSC_map_mapkey_stack loc -> (get_from_stack si.si_map_mapkey ~loc).cc_v
     | MSC_iter_stack loc       -> (get_from_stack si.si_iter ~loc).cc_v
     | _ as msc                 -> MV_symbol (t, msc)
   )
   | MV_ref_cont t1cc -> (
     match mci.mci_cutcat with
     | MCC_ln_map  -> (List.hd_exn si.si_mich).cc_v
     | MCC_ln_iter -> (
       match t1cc.cc_v with
       | MT_list t11cc             -> MV_nil t11cc
       | MT_set t11cc              -> MV_empty_set t11cc
       | MT_map (t11cc, t12cc)     -> MV_empty_map (t11cc, t12cc)
       | MT_big_map (t11cc, t12cc) -> MV_empty_big_map (t11cc, t12cc)
       | _                         ->
         VcError "apply_inv_at_start : MV_ref_cont : MCC_ln_iter : wrong type"
         |> raise
     )
     | MCC_lb_map  -> (List.hd_exn si.si_map_entry).cc_v
     | MCC_lb_iter -> (List.hd_exn si.si_iter).cc_v
     | _           ->
       VcError "apply_inv_at_start : MV_ref_cont : wrong reference" |> raise
   )
   | mv               -> mv
   in
   mvcc_subst_mf inv ~mapf:(fun mvcc_ctx ->
       let (_, (ctx_v : mich_v cc)) =
          mvcc_map_innerfst ~mapf mvcc_ctx.ctx_v |> opt_mvcc ~ctx:sctx
       in
       gen_mich_v_ctx ~ctx:sctx ctx_v
   )
   |> opt_mf
(* function apply_inv_at_start end *)

let apply_inv_at_block :
    sctx:Tz.mich_sym_ctxt ->
    Tz.mich_cut_info ->
    Tz.sym_image ->
    Tz.mich_f ->
    Tz.mich_f =
   let open Tz in
   let open TzUtil in
   fun ~sctx mci si inv ->
   let (mapf : mich_v -> mich_v) = function
   | MV_ref (t, msc2) -> (
     match msc2 with
     | MSC_contract             -> si.si_param.ti_contract.cc_v
     | MSC_source               -> si.si_param.ti_source.cc_v
     | MSC_sender               -> si.si_param.ti_sender.cc_v
     | MSC_param                -> si.si_param.ti_param.cc_v
     | MSC_amount               -> si.si_param.ti_amount.cc_v
     | MSC_time                 -> si.si_param.ti_time.cc_v
     | MSC_balance              -> si.si_balance.cc_v
     | MSC_bc_balance           -> si.si_bc_balance.cc_v
     | MSC_mich_stack loc       -> (get_from_stack si.si_mich ~loc).cc_v
     | MSC_dip_stack loc        -> (get_from_stack si.si_dip ~loc).cc_v
     | MSC_map_entry_stack loc  -> (get_from_stack si.si_map_entry ~loc).cc_v
     | MSC_map_exit_stack loc   -> (get_from_stack si.si_map_exit ~loc).cc_v
     | MSC_map_mapkey_stack loc -> (get_from_stack si.si_map_mapkey ~loc).cc_v
     | MSC_iter_stack loc       -> (get_from_stack si.si_iter ~loc).cc_v
     | _ as msc                 -> MV_symbol (t, msc)
   )
   | MV_ref_cont _    -> (
     match mci.mci_cutcat with
     | MCC_ln_map  -> (List.hd_exn si.si_mich).cc_v
     | MCC_ln_iter -> (List.hd_exn si.si_mich).cc_v
     | MCC_lb_map  -> (List.hd_exn si.si_map_exit).cc_v
     | MCC_lb_iter -> (List.hd_exn si.si_iter).cc_v
     | _           ->
       VcError "apply_inv_at_block : MV_ref_cont : wrong reference" |> raise
   )
   | mv               -> mv
   in
   mvcc_subst_mf inv ~mapf:(fun mvcc_ctx ->
       let (_, (ctx_v : mich_v cc)) =
          mvcc_map_innerfst ~mapf mvcc_ctx.ctx_v |> opt_mvcc ~ctx:sctx
       in
       gen_mich_v_ctx ~ctx:sctx ctx_v
   )
   |> opt_mf
(* function apply_inv_at_block end *)

let apply_inv_with_initial_storage :
    sctx:Tz.mich_sym_ctxt ->
    Tz.mich_cut_info ->
    Tz.sym_image ->
    Tz.mich_v Tz.cc ->
    Tz.mich_f ->
    Tz.mich_f =
   let open Tz in
   let open TzUtil in
   fun ~sctx mci si init_strg inv ->
   match mci.mci_cutcat with
   | MCC_trx_entry ->
     let (mapf : mich_v -> mich_v) = function
     | MV_ref (_, MSC_mich_stack 0) -> MV_pair (si.si_param.ti_param, init_strg)
     | mv -> mv
     in
     mvcc_subst_mf inv ~mapf:(fun mvcc_ctx ->
         let (_, (ctx_v : mich_v cc)) =
            mvcc_map_innerfst ~mapf mvcc_ctx.ctx_v |> opt_mvcc ~ctx:sctx
         in
         gen_mich_v_ctx ~ctx:sctx ctx_v
     )
     |> opt_mf
   | _             ->
     VcError "apply_inv_with_initial_storage : wrong mci" |> raise
(* function apply_inv_with_initial_storage end *)

(******************************************************************************)
(******************************************************************************)
(* Verification Condition                                                     *)
(******************************************************************************)
(******************************************************************************)

(* Strongest Postcondition ****************************************************)

let gen_sp : Tz.sym_state -> Tz.mich_f -> Tz.mich_f =
   let open Tz in
   (fun sstate precond -> MF_and (precond :: sstate.ss_constraints))
(* function gen_sp end *)

let gen_sp_from_ms : MState.t -> Tz.mich_f -> Tz.mich_f =
   let open Tz in
   let open MState in
   (fun mstate precond -> MF_and (precond :: get_constraint mstate))
(* function gen_sp_from_ms end *)

(* Verification Condition *****************************************************)

let gen_query_vc : Inv.inv_map -> Tz.sym_state -> Tz.mich_f =
   let open Tz in
   fun imap sstate ->
   let (start_inv : mich_f) =
      Inv.find_inv imap sstate.ss_start_mci
      |> Inv.fmla_of_cand_pre
      |> apply_inv_at_start ~sctx:sstate.ss_id sstate.ss_start_mci
           sstate.ss_start_si
   in
   let (sp : mich_f) = gen_sp sstate start_inv in
   let (query : mich_f) =
      property_of_query ~sctx:sstate.ss_id sstate.ss_block_mci
        sstate.ss_block_si
   in
   MF_imply (sp, query)
(* function gen_query_vc end *)

let gen_query_vc_from_ms : Inv.inv_map -> MState.t -> Tz.mich_f =
   let open Tz in
   let open MState in
   fun imap mstate ->
   let (start_state : sym_state) = get_first_ss mstate in
   let (block_state : sym_state) = get_last_ss mstate in
   let (start_inv : mich_f) =
      Inv.find_inv imap start_state.ss_start_mci
      |> Inv.fmla_of_cand_pre
      |> apply_inv_at_start ~sctx:start_state.ss_id start_state.ss_start_mci
           start_state.ss_start_si
   in
   let (sp : mich_f) = gen_sp_from_ms mstate start_inv in
   let (query : mich_f) =
      property_of_query ~sctx:block_state.ss_id block_state.ss_block_mci
        block_state.ss_block_si
   in
   MF_imply (sp, query)
(* function gen_query_vc_from_ms end *)

let gen_inductiveness_vc : Inv.inv_map -> Tz.sym_state -> Tz.mich_f =
   let open Tz in
   fun imap sstate ->
   let (start_inv : mich_f) =
      Inv.find_inv imap sstate.ss_start_mci
      |> Inv.fmla_of_cand_pre
      |> apply_inv_at_start ~sctx:sstate.ss_id sstate.ss_start_mci
           sstate.ss_start_si
   in
   let (block_inv : mich_f) =
      Inv.find_inv imap sstate.ss_block_mci
      |> Inv.fmla_of_cand_post
      |> apply_inv_at_block ~sctx:sstate.ss_id sstate.ss_block_mci
           sstate.ss_block_si
   in
   let (sp : mich_f) = gen_sp sstate start_inv in
   MF_imply (sp, block_inv)
(* function gen_inductiveness_vc end *)

let gen_preservation_vc : Inv.cand -> MState.t -> Tz.mich_f =
   let open Tz in
   let open TzUtil in
   let open MState in
   fun cand mstate ->
   let (start_state : sym_state) = get_first_ss mstate in
   let (block_state : sym_state) = get_last_ss mstate in
   if equal_r_mich_cut_info
        (get_reduced_mci start_state.ss_start_mci)
        (get_reduced_mci block_state.ss_block_mci)
   then (
     let (start_fmla : mich_f) =
        Inv.fmla_of_cand_pre cand
        |> apply_inv_at_start ~sctx:start_state.ss_id start_state.ss_start_mci
             start_state.ss_start_si
     in
     let (block_fmla : mich_f) =
        Inv.fmla_of_cand_post cand
        |> apply_inv_at_block ~sctx:block_state.ss_id block_state.ss_block_mci
             block_state.ss_block_si
     in
     let (sp : mich_f) = gen_sp_from_ms mstate start_fmla in
     MF_imply (sp, block_fmla)
   )
   else VcError "gen_preservation_vc : wrong merged state" |> raise
(* function gen_preservation_vc end *)

let gen_initial_inv_vc :
    Inv.inv_map -> Tz.mich_v Tz.cc -> Tz.sym_state -> Tz.mich_f =
   let open Tz in
   fun imap init_strg sstate ->
   if match sstate.ss_start_mci.mci_cutcat with
      | MCC_trx_entry -> true
      | _             -> false
   then (
     let (init_strg_fmla : mich_f) =
        Inv.find_inv imap sstate.ss_start_mci
        |> Inv.fmla_of_cand_pre
        |> apply_inv_with_initial_storage ~sctx:sstate.ss_id sstate.ss_start_mci
             sstate.ss_start_si init_strg
     in
     init_strg_fmla
   )
   else VcError "gen_initial_inv_vc : wrong state" |> raise
(* function gen_initial_inv_vc end *)

let gen_refute_vc : Tz.mich_v Tz.cc -> MState.t -> Tz.mich_f =
   let open Tz in
   let open MState in
   fun init_strg mstate ->
   let (start_state : sym_state) = get_first_ss mstate in
   let (block_state : sym_state) = get_last_ss mstate in
   let (init_strg_fmla : mich_f) =
      fmla_for_initial_storage ~sctx:start_state.ss_id start_state.ss_start_mci
        start_state.ss_start_si init_strg
   in
   let (sp : mich_f) = gen_sp_from_ms mstate init_strg_fmla in
   let (query : mich_f) =
      property_of_query ~sctx:block_state.ss_id block_state.ss_block_mci
        block_state.ss_block_si
   in
   (* MF_not (MF_imply (sp, query)) *)
   MF_and [ sp; MF_not query ]
(* function gen_refute_vc end *)

let gen_precond_vc : Inv.cand -> MState.t -> Tz.mich_f =
   let open Tz in
   let open MState in
   fun prec mstate ->
   let (start_state : sym_state) = get_first_ss mstate in
   let (block_state : sym_state) = get_last_ss mstate in
   let (prec_lst : mich_f) =
      Inv.fmla_of_cand_pre prec
      |> apply_inv_at_start ~sctx:start_state.ss_id start_state.ss_start_mci
           start_state.ss_start_si
   in
   let (sp : mich_f) = gen_sp_from_ms mstate prec_lst in
   let (query : mich_f) =
      property_of_query ~sctx:block_state.ss_id block_state.ss_block_mci
        block_state.ss_block_si
   in
   MF_imply (sp, query)
(* function gen_precond_vc end *)

(******************************************************************************)
(******************************************************************************)
(* Verification                                                               *)
(******************************************************************************)
(******************************************************************************)

let gen_ctx : unit -> Smt.Ctx.t = (fun () -> Smt.Ctx.create ())
(* function gen_ctx end *)

let gen_solver : Smt.Ctx.t -> Smt.Solver.t = (fun ctx -> Smt.Solver.create ctx)
(* function gen_solver end *)

let check_val :
    Smt.Ctx.t ->
    Smt.Solver.t ->
    Tz.mich_f ->
    Smt.Solver.validity * Smt.Model.t option =
   let open Smt in
   fun ctx solver mf ->
   let _ = Solver.reset solver in
   let (fmla : Formula.t) = Encoder.cv_mf ctx mf in
   Solver.check_val solver ctx fmla
(* function check_val end *)

let check_sat :
    Smt.Ctx.t ->
    Smt.Solver.t ->
    Tz.mich_f ->
    Smt.Solver.satisfiability * Smt.Model.t option =
   let open Smt in
   fun ctx solver mf ->
   let _ = Solver.reset solver in
   let (fmla : Formula.t) = Encoder.cv_mf ctx mf in
   Solver.check_sat solver ctx fmla
(* function check_sat end *)

let check_sat_lst :
    Smt.Ctx.t ->
    Smt.Solver.t ->
    Tz.mich_f list ->
    Smt.Solver.satisfiability * Smt.Model.t option =
   let open Smt in
   fun ctx solver mf_lst ->
   let _ = Solver.reset solver in
   let (fmla_lst : Formula.t list) = List.map ~f:(Encoder.cv_mf ctx) mf_lst in
   Solver.check_sat_lst solver ctx fmla_lst
(* function check_sat_lst end *)

(* DEBUGGING FUNCTION *)
let debug_check_sat :
    Smt.Ctx.t ->
    Smt.Solver.t ->
    Tz.mich_f ->
    (Tz.mich_f * Smt.Solver.satisfiability * Smt.Model.t option) list =
   let open Tz in
   let open Smt in
   fun ctx solver mf ->
   match mf with
   | MF_and (hd_f :: fl) ->
     List.fold fl
       ~init:(([ hd_f ], []), false)
       ~f:(fun ((flst, sat_lst), is_unsat) f ->
         if is_unsat
         then ((flst, sat_lst), is_unsat)
         else (
           let _ = Solver.reset solver in
           let (new_flst : Tz.mich_f list) = f :: flst in
           let (fmla : Formula.t) = Encoder.cv_mf ctx (MF_and new_flst) in
           let (sat, mdopt) : Smt.Solver.satisfiability * Smt.Model.t option =
              Solver.check_sat solver ctx fmla
           in
           (* ((new_flst, (f, sat, mdopt) :: sat_lst), Solver.is_unsat sat) *)
           ((new_flst, (f, sat, mdopt) :: sat_lst), Solver.is_sat sat |> not)
         ))
     |> fst
     |> snd
   | _ as f              ->
     let _ = Solver.reset solver in
     let (fmla : Formula.t) = Encoder.cv_mf ctx f in
     let (sat, mdopt) : Smt.Solver.satisfiability * Smt.Model.t option =
        Solver.check_sat solver ctx fmla
     in
     [ (f, sat, mdopt) ]
(* DEBUGGING FUNCTION *)

let is_cand_sat : Smt.Ctx.t -> Smt.Solver.t -> Inv.cand -> bool =
   let open Smt in
   fun ctx solver cand ->
   let ((sat : Solver.satisfiability), _) =
      Inv.fmla_of_cand_pre cand |> check_sat ctx solver
   in
   Solver.is_sat sat
(* function is_cand_sat end *)

let do_cand_sat_istrg :
    Smt.Ctx.t ->
    Smt.Solver.t ->
    Tz.mich_v Tz.cc ->
    Tz.sym_state ->
    Tz.r_mich_cut_info ->
    Inv.cand ->
    bool =
   let open Tz in
   let open Smt in
   fun ctx solver istrg istate rmci cand ->
   if not (equal_r_mich_cut_category rmci.rmci_cutcat RMCC_trx)
   then true
   else (
     let (vc : mich_f) =
        Inv.fmla_of_cand_post cand
        |> apply_inv_with_initial_storage ~sctx:istate.ss_id istate.ss_start_mci
             istate.ss_start_si istrg
     in
     let ((vld : Solver.validity), _) = check_val ctx solver vc in
     Solver.is_val vld
   )
