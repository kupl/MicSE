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
     let open Smt in
     fun ctx typ1 ->
     match typ1 with
     | MT_key                  -> ZKey.create_sort ctx
     | MT_unit                 -> ZUnit.create_sort ctx
     | MT_signature            -> ZSig.create_sort ctx
     | MT_option t1cc          -> ZOption.create_sort ctx ~content_typ:t1cc
     | MT_list t1cc            -> ZList.create_sort ctx ~content_typ:t1cc
     | MT_set t1cc             -> ZSet.create_sort ctx ~content_typ:t1cc
     | MT_operation            -> ZOperation.create_sort ctx
     | MT_contract t1cc        -> ZContract.create_sort ctx ~content_typ:t1cc
     | MT_pair (t1cc, t2cc)    -> ZPair.create_sort ctx ~content_typ:(t1cc, t2cc)
     | MT_or (t1cc, t2cc)      -> ZOr.create_sort ctx ~content_typ:(t1cc, t2cc)
     | MT_lambda (t1cc, t2cc)  ->
       ZLambda.create_sort ctx ~domain_typ:t1cc ~range_typ:t2cc
     | MT_map (t1cc, t2cc)     ->
       ZMap.create_sort ctx ~key_typ:t1cc ~data_typ:t2cc
     | MT_big_map (t1cc, t2cc) ->
       ZMap.create_sort ctx ~key_typ:t1cc ~data_typ:t2cc
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
    (fun _ _ -> raise Not_Implemented)
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
         ~if_:(Formula.create_is_keyhash_str ctx expr1)
         ~then_:
           (Formula.if_then_else ctx
              ~if_:(Formula.create_is_keyhash_str ctx expr2)
              ~then_:
                (cv_compare ctx
                   (MT_string, ZKeyHash.read_content_str expr1)
                   (MT_string, ZKeyHash.read_content_str expr2)
                )
              ~else_:dummy
           )
         ~else_:
           (Formula.if_then_else ctx
              ~if_:(Formula.create_is_keyhash_key ctx expr2)
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
         ~if_:(Formula.create_is_option_none ctx t1cc expr1)
         ~then_:
           (Formula.if_then_else ctx
              ~if_:(Formula.create_is_option_none ctx t2cc expr2)
              ~then_:zero ~else_:minus_one
           )
         ~else_:
           (Formula.if_then_else ctx
              ~if_:(Formula.create_is_option_none ctx t2cc expr2)
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
         ~if_:(Formula.create_is_signature_str ctx expr1)
         ~then_:
           (Formula.if_then_else ctx
              ~if_:(Formula.create_is_signature_str ctx expr2)
              ~then_:
                (cv_compare ctx
                   (MT_string, ZSig.read_str expr1)
                   (MT_string, ZSig.read_str expr2)
                )
              ~else_:dummy
           )
         ~else_:dummy
     | (MT_address, MT_address) -> raise Not_Implemented
     | (MT_or (t11cc, t12cc), MT_or (t21cc, t22cc))
       when Tz.equal_cc Tz.equal_mich_t t11cc t21cc
            && Tz.equal_cc Tz.equal_mich_t t12cc t22cc ->
       Formula.if_then_else ctx
         ~if_:(Formula.create_is_or_left ctx (t11cc, t12cc) expr1)
         ~then_:
           (Formula.if_then_else ctx
              ~if_:(Formula.create_is_or_left ctx (t21cc, t22cc) expr2)
              ~then_:
                (cv_compare ctx
                   (t11cc.cc_v, ZOr.read_content_left expr1)
                   (t21cc.cc_v, ZOr.read_content_left expr2)
                )
              ~else_:minus_one
           )
         ~else_:
           (Formula.if_then_else ctx
              ~if_:(Formula.create_is_or_right ctx (t21cc, t22cc) expr2)
              ~then_:
                (cv_compare ctx
                   (t12cc.cc_v, ZOr.read_content_right expr1)
                   (t22cc.cc_v, ZOr.read_content_right expr2)
                )
              ~else_:one
           )
     | _ -> VcError "Encoder : cv_compare : wrong comparison" |> raise
  (* function cv_compare end *)

  let cv_mf : Smt.Ctx.t -> Tz.mich_f -> Smt.Formula.t =
    (fun _ _ -> raise Not_Implemented)
  (* function cv_mf end *)
end
