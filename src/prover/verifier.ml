open ProverLib

exception Error of string

exception Not_Implemented_f of Vlang.t
exception Not_Implemented_e of Vlang.Expr.t

exception SMT_Encode_Error_f of (Vlang.Formula.t * string)
exception SMT_Encode_Error_e of (Vlang.Expr.t * string)


let rec smtsort_of_vlangtyp : Vlang.Ty.t -> Smt.ZSort.t
= let open Vlang.Ty in
  let sot = smtsort_of_vlangtyp in (* syntax sugar *)
  fun vt -> begin
  match vt with
  | T_key -> Smt.ZKey.sort
  | T_unit -> Smt.ZUnit.sort
  | T_signature -> Smt.ZSignature.sort
  | T_option t -> Smt.ZOption.create_sort ~content_sort:(sot t)
  | T_list t -> Smt.ZList.create_sort ~content_sort:(sot t)
  | T_set t -> Smt.ZList.create_sort ~content_sort:(sot t)
  | T_operation -> Smt.ZOperation.sort
  | T_contract _ -> Smt.ZContract.sort
  | T_pair (t1, t2) -> Smt.ZPair.create_sort ~fst_sort:(sot t1) ~snd_sort:(sot t2)
  | T_or (t1, t2) -> Smt.ZOr.create_sort ~left_sort:(sot t1) ~right_sort:(sot t2)
  | T_lambda (_, _) -> Smt.ZLambda.sort
  | T_map (t1, t2) -> Smt.ZMap.create_sort ~key_sort:(sot t1) ~value_sort:(sot t2)
  | T_big_map (t1, t2) -> Smt.ZMap.create_sort ~key_sort:(sot t1) ~value_sort:(sot t2)
  | T_chain_id -> Smt.ZStr.sort
  | T_int -> Smt.ZInt.sort
  | T_nat -> Smt.ZNat.sort
  | T_string -> Smt.ZStr.sort
  | T_bytes -> Smt.ZBytes.sort
  | T_mutez -> Smt.ZMutez.sort
  | T_bool -> Smt.ZBool.sort
  | T_key_hash -> Smt.ZKeyHash.sort
  | T_timestamp -> Smt.ZInt.sort
  | T_address -> Smt.ZAddress.sort
end (* function smttyp_of_vlangtyp end *)

let sort_of_typt : Pre.Lib.Adt.typ -> Smt.ZSort.t
= fun vt -> vt |> Vlang.TypeUtil.ty_of_mty |> smtsort_of_vlangtyp


let rec smtexpr_of_compare : Vlang.Expr.t -> Vlang.Expr.t -> Smt.ZExpr.t
= let open Vlang.Ty in
  let soe = smtexpr_of_vlangexpr in (* syntax sugar *)
  (*let soc = smtexpr_of_compare in (* syntax sugar *)*)
  let err e = Stdlib.raise (Not_Implemented_e e) in (* syntax sugar *)
  fun e1 e2 -> begin
    match Vlang.TypeUtil.ty_of_expr e1, Vlang.TypeUtil.ty_of_expr e2 with
    | T_int, T_int -> Smt.ZInt.create_cmp (e1 |> soe) (e2 |> soe)
    | T_nat, T_nat -> Smt.ZInt.create_cmp (e1 |> soe) (e2 |> soe)
    | T_string, T_string -> Smt.ZStr.create_cmp (e1 |> soe) (e2 |> soe)
    | T_bytes, T_bytes -> Smt.ZStr.create_cmp (e1 |> soe) (e2 |> soe)
    | T_mutez, T_mutez -> Smt.ZMutez.create_cmp (e1 |> soe) (e2 |> soe)
    | T_bool, T_bool -> err e1  (* True is larger than False, like OCaml *)
    | T_key_hash, T_key_hash -> Smt.ZKeyHash.create_cmp (e1 |> soe) (e2 |> soe)
    | T_timestamp, T_timestamp -> Smt.ZInt.create_cmp (e1 |> soe) (e2 |> soe)
    | T_address, T_address -> Smt.ZAddress.create_cmp (e1 |> soe) (e2 |> soe)
    | T_pair (_, _), T_pair (_, _) -> err e1
    | t1, t2 when t1 = t2 -> Stdlib.failwith ("Prover.Verifier.smtexpr_of_compare : expression like this cannot be compared")
    | _ -> Stdlib.failwith ("Prover.Verifier.smtexpr_of_compare : two expressions have different types [" ^ (e1 |> Vlang.TypeUtil.ty_of_expr |> Vlang.Ty.to_string) ^ "] & [" ^ (e2 |> Vlang.TypeUtil.ty_of_expr |> Vlang.Ty.to_string) ^ "]")

end (* smtexpr_of_compare end *)

(* Overall, lots of uninterpreted function needed, not dummy expression *)
and smtexpr_of_vlangexpr : Vlang.Expr.t -> Smt.ZExpr.t
= let open Vlang.Expr in
  let soe = smtexpr_of_vlangexpr in (* syntax sugar *)
  (* let err e = Stdlib.raise (Not_Implemented_e e) in syntax sugar *)
  let err (e: Vlang.Expr.t) = e |> Vlang.TypeUtil.ty_of_expr |> smtsort_of_vlangtyp |> Smt.ZExpr.create_dummy in
  fun ve -> begin
    try
      match ve with
      (*************************************************************************)
      (* Variable & Polymorphic                                                *)
      (*************************************************************************)
      | V_var (t,v) -> Smt.ZExpr.create_var (t |> smtsort_of_vlangtyp) ~name:v
      | V_car e -> Smt.ZPair.read_fst (e |> soe)
      | V_cdr e -> Smt.ZPair.read_snd (e |> soe)
      | V_unlift_option e -> Smt.ZOption.read (e |> soe)
      | V_unlift_left e -> Smt.ZOr.read_left (e |> soe)
      | V_unlift_right e -> Smt.ZOr.read_right (e |> soe)
      | V_hd_l e -> Smt.ZList.read_head (e |> soe)
      | V_hd_s _ -> err ve (* get the first element of the set *)
      | V_exec (_, _) -> err ve (* function application & execution *)
      | V_dup e -> soe e
      | V_itself e -> soe e

      (*************************************************************************)
      (* Integer                                                               *)
      (*************************************************************************)
      | V_lit_int zn -> zn |> Smt.ZInt.of_zarith
      | V_neg_ni e -> Smt.ZInt.create_neg (e |> soe)
      | V_neg_ii e -> Smt.ZInt.create_neg (e |> soe)
      | V_not_ni e -> Smt.ZInt.create_not (e |> soe)
      | V_not_ii e -> Smt.ZInt.create_not (e |> soe)
      | V_add_nii (e1, e2) -> Smt.ZInt.create_add [(e1 |> soe); (e2 |> soe);]
      | V_add_ini (e1, e2) -> Smt.ZInt.create_add [(e1 |> soe); (e2 |> soe);]
      | V_add_iii (e1, e2) -> Smt.ZInt.create_add [(e1 |> soe); (e2 |> soe);]
      | V_sub_nni (e1, e2) -> Smt.ZInt.create_sub [(e1 |> soe); (e2 |> soe);]
      | V_sub_nii (e1, e2) -> Smt.ZInt.create_sub [(e1 |> soe); (e2 |> soe);]
      | V_sub_ini (e1, e2) -> Smt.ZInt.create_sub [(e1 |> soe); (e2 |> soe);]
      | V_sub_iii (e1, e2) -> Smt.ZInt.create_sub [(e1 |> soe); (e2 |> soe);]
      | V_sub_tti (e1, e2) -> Smt.ZInt.create_sub [(e1 |> soe); (e2 |> soe);]
      | V_mul_nii (e1, e2) -> Smt.ZInt.create_mul [(e1 |> soe); (e2 |> soe);]
      | V_mul_ini (e1, e2) -> Smt.ZInt.create_mul [(e1 |> soe); (e2 |> soe);]
      | V_mul_iii (e1, e2) -> Smt.ZInt.create_mul [(e1 |> soe); (e2 |> soe);]
      | V_compare (e1, e2) -> smtexpr_of_compare e1 e2
      | V_int_of_nat e -> begin
          Smt.ZExpr.create_ite
            ~cond:(Smt.ZInt.create_ge (soe e) Smt.ZInt.zero_)
            ~t:(Smt.ZOption.create_some ~content:(soe e))
            ~f:(Smt.ZOption.create_none ~content_sort:(Smt.ZNat.sort))
        end

      (*************************************************************************)
      (* Natural Number                                                        *)
      (*************************************************************************)
      | V_lit_nat zn -> Smt.ZNat.of_zarith zn
      | V_abs_in e -> Smt.ZNat.create_abs (e |> soe)
      | V_add_nnn (e1, e2) -> Smt.ZNat.create_add [(e1 |> soe); (e2 |> soe);]
      | V_mul_nnn (e1, e2) -> Smt.ZNat.create_mul [(e1 |> soe); (e2 |> soe);]
      | V_shiftL_nnn (e1, e2) -> Smt.ZNat.create_shiftL (soe e1) (soe e2)
      | V_shiftR_nnn (e1, e2) -> Smt.ZNat.create_shiftR (soe e1) (soe e2)
      | V_and_nnn (e1, e2) -> Smt.ZNat.create_and (soe e1) (soe e2)
      | V_and_inn (e1, e2) -> Smt.ZNat.create_and (soe e1) (soe e2)
      | V_or_nnn (e1, e2) -> Smt.ZNat.create_or (soe e1) (soe e2)
      | V_xor_nnn (e1, e2) -> Smt.ZNat.create_xor (soe e1) (soe e2)
      | V_size_s _ -> err ve            (* not supported *)
      | V_size_m _ -> err ve            (* not supported *)
      | V_size_l _ -> err ve            (* not supported *)
      | V_size_str _ -> err ve          (* not supported *)
      | V_size_b _ -> err ve            (* not supported *)

      (*************************************************************************)
      (* String                                                                *)
      (*************************************************************************)
      | V_lit_string s -> Smt.ZStr.of_string s
      | V_concat_sss (e1, e2) -> Smt.ZStr.create_concat [(e1 |> soe); (e2 |> soe)]
      | V_concat_list_s _ -> err ve (* cannot find how to convert "z_expr (smt-string smt-list)" to "(z_expr (string)) list" *)

      (*************************************************************************)
      (* Bytes                                                                 *)
      (*************************************************************************)
      | V_lit_bytes s -> Smt.ZBytes.create_bytstr (Smt.ZStr.of_string s)
      | V_concat_bbb (e1, e2) -> Smt.ZBytes.create_concatenated ~fst_bytes:(soe e1) ~snd_bytes:(soe e2)
      | V_concat_list_b _ -> err ve (* same error reason as "V_concat_list_s" *)
      | V_pack _ -> (* Smt.ZBytes.create_pack (soe e) *) Smt.ZBytes.create_pack ()
      | V_blake2b e -> Smt.ZBytes.create_blake2b (soe e)
      | V_sha256 e -> Smt.ZBytes.create_sha256 (soe e)
      | V_sha512 e -> Smt.ZBytes.create_sha512 (soe e)

      (*************************************************************************)
      (* Mutez                                                                 *)
      (*************************************************************************)
      | V_lit_mutez zn -> Smt.ZMutez.of_zarith zn
      | V_amount -> err ve    (* native & uninterpreted symbol needed *)
      | V_balance -> err ve   (* native & uninterpreted symbol needed *)
      | V_add_mmm (e1, e2) -> Smt.ZMutez.create_add (e1 |> soe) (e2 |> soe)
      | V_sub_mmm (e1, e2) -> Smt.ZMutez.create_sub (e1 |> soe) (e2 |> soe)
      | V_mul_mnm (e1, e2) -> Smt.ZMutez.create_mul (e1 |> soe) (e2 |> soe |> Smt.ZInt.to_zmutez)
      | V_mul_nmm (e1, e2) -> Smt.ZMutez.create_mul (e1 |> soe |> Smt.ZInt.to_zmutez) (e2 |> soe)

      (*************************************************************************)
      (* Bool                                                                  *)
      (*************************************************************************)
      | V_lit_bool b -> Smt.ZBool.of_bool b
      | V_not_bb e -> Smt.ZBool.create_not (e |> soe) 
      | V_and_bbb (e1, e2) -> Smt.ZBool.create_and (e1 |> soe) (e2 |> soe)
      | V_or_bbb (e1, e2) -> Smt.ZBool.create_or (e1 |> soe) (e2 |> soe)
      | V_xor_bbb (e1, e2) -> Smt.ZBool.create_xor (e1 |> soe) (e2 |> soe)
      | V_eq_ib e -> Smt.ZInt.create_eq (e |> soe) (Smt.ZInt.zero_)
      | V_neq_ib e -> Smt.ZInt.create_neq (e |> soe) (Smt.ZInt.zero_)
      | V_lt_ib e -> Smt.ZInt.create_lt (e |> soe) (Smt.ZInt.zero_)
      | V_gt_ib e -> Smt.ZInt.create_gt (e |> soe) (Smt.ZInt.zero_)
      | V_leq_ib e -> Smt.ZInt.create_le (e |> soe) (Smt.ZInt.zero_)
      | V_geq_ib e -> Smt.ZInt.create_ge (e |> soe) (Smt.ZInt.zero_)
      | V_mem_xsb _ -> err ve (* not supported *)
      | V_mem_xmb (e1, e2) -> Smt.ZMap.read_exist ~key:(e1 |> soe) ~map:(e2 |> soe)
      | V_mem_xbmb (e1, e2) -> Smt.ZMap.read_exist ~key:(e1 |> soe) ~map:(e2 |> soe)
      | V_check_signature _ -> err ve (* not supported *)

      (*************************************************************************)
      (* Key Hash                                                              *)
      (*************************************************************************)
      | V_lit_key_hash s -> Smt.ZKeyHash.of_string s
      | V_hash_key k -> Smt.ZKeyHash.create_hashkey (soe k)

      (*************************************************************************)
      (* Timestamp                                                             *)
      (*************************************************************************)
      | V_lit_timestamp_str s -> begin
          (* About Ptime: see https://erratique.ch/software/ptime
              or https://github.com/dbuenzli/ptime
          *)
          Ptime.of_rfc3339 ~strict:false ~sub:true ~start:0 s 
          |> Result.get_ok  (* It might raise "Invalid_argument" *)
          |> (fun (pt, _, _) -> pt)
          |> Ptime.to_span
          |> Ptime.Span.to_int_s
          |> Option.get   (* It might raise "Invalid_argument" *)
          |> Smt.ZInt.of_int
        end
      | V_lit_timestamp_sec n -> begin
          Smt.ZInt.of_int (Z.to_int n)
        end
      | V_now -> err ve (* native & uninterpreted symbol needed *)
      | V_add_tit (e1, e2) -> Smt.ZInt.create_add [soe e1; soe e2]
      | V_add_itt (e1, e2) -> Smt.ZInt.create_add [soe e1; soe e2]
      | V_sub_tit (e1, e2) -> Smt.ZInt.create_sub [soe e1; soe e2]

      (*************************************************************************)
      (* Address                                                               *)
      (*************************************************************************)
      | V_lit_address kh -> Smt.ZAddress.create_addrkh (soe kh)
      | V_source -> Smt.ZAddress.of_string Smt.CONST._tmpname_source
      | V_sender -> Smt.ZAddress.of_string Smt.CONST._tmpname_sender
      | V_address_of_contract _ -> err ve (* not supported *)

      (*************************************************************************)
      (* Key                                                                   *)
      (*************************************************************************)
      | V_lit_key s -> Smt.ZKey.create_keystr (Smt.ZStr.of_string s)

      (*************************************************************************)
      (* Unit                                                                  *)
      (*************************************************************************)
      (* | V_lit_unit : t_unit t *) (* V_unit has the same feature. *)
      | V_unit -> Smt.ZUnit.create

      (*************************************************************************)
      (* Signature                                                             *)
      (*************************************************************************)
      | V_lit_signature_str _ -> err ve
      | V_lit_signature_signed (_, _) -> err ve (* uninterpreted function needed *)

      (*************************************************************************)
      (* Option                                                                *)
      (*************************************************************************)
      (* | V_lit_option : 'a t option -> 'a t_option t *) (* V_some and V_none has the same feature. *)
      | V_some e -> Smt.ZOption.create_some ~content:(e |> soe)
      | V_none t -> Smt.ZOption.create_none ~content_sort:(smtsort_of_vlangtyp t)
      | V_ediv_nnnn (e1, e2) -> begin
          let dividend, divisor = (e1 |> soe), (e2 |> soe) in
          let qr = Smt.ZPair.create ~fst:(Smt.ZInt.create_div dividend divisor) ~snd:(Smt.ZInt.create_mod dividend divisor) in
          let div_zero_result = Smt.ZOption.create_none ~content_sort:(qr |> Smt.ZExpr.read_sort) in
          Smt.ZExpr.create_ite ~cond:(Smt.ZInt.create_eq divisor (Smt.ZInt.zero_)) ~t:div_zero_result ~f:(Smt.ZOption.create_some ~content:(qr))
        end
      | V_ediv_niin (e1, e2) -> begin
          let dividend, divisor = (e1 |> soe), (e2 |> soe) in
          let qr = Smt.ZPair.create ~fst:(Smt.ZInt.create_div dividend divisor) ~snd:(Smt.ZInt.create_mod dividend divisor) in
          let div_zero_result = Smt.ZOption.create_none ~content_sort:(qr |> Smt.ZExpr.read_sort) in
          Smt.ZExpr.create_ite ~cond:(Smt.ZInt.create_eq divisor (Smt.ZInt.zero_)) ~t:div_zero_result ~f:(Smt.ZOption.create_some ~content:(qr))
        end
      | V_ediv_inin (e1, e2) -> begin
          let dividend, divisor = (e1 |> soe), (e2 |> soe) in
          let qr = Smt.ZPair.create ~fst:(Smt.ZInt.create_div dividend divisor) ~snd:(Smt.ZInt.create_mod dividend divisor) in
          let div_zero_result = Smt.ZOption.create_none ~content_sort:(qr |> Smt.ZExpr.read_sort) in
          Smt.ZExpr.create_ite ~cond:(Smt.ZInt.create_eq divisor (Smt.ZInt.zero_)) ~t:div_zero_result ~f:(Smt.ZOption.create_some ~content:(qr))
        end
      | V_ediv_iiin (e1, e2) -> begin
          let dividend, divisor = (e1 |> soe), (e2 |> soe) in
          let qr = Smt.ZPair.create ~fst:(Smt.ZInt.create_div dividend divisor) ~snd:(Smt.ZInt.create_mod dividend divisor) in
          let div_zero_result = Smt.ZOption.create_none ~content_sort:(qr |> Smt.ZExpr.read_sort) in
          Smt.ZExpr.create_ite ~cond:(Smt.ZInt.create_eq divisor (Smt.ZInt.zero_)) ~t:div_zero_result ~f:(Smt.ZOption.create_some ~content:(qr))
        end
      | V_ediv_mnmm (e1, e2) -> begin
          let dividend, divisor = (e1 |> soe), (e2 |> soe |> Smt.ZInt.to_zmutez) in
          let qr = Smt.ZPair.create ~fst:(Smt.ZMutez.create_div dividend divisor) ~snd:(Smt.ZMutez.create_mod dividend divisor) in
          let div_zero_result = Smt.ZOption.create_none ~content_sort:(qr |> Smt.ZExpr.read_sort) in
          Smt.ZExpr.create_ite ~cond:(Smt.ZMutez.create_eq divisor (Smt.ZMutez.zero_)) ~t:div_zero_result ~f:(Smt.ZOption.create_some ~content:(qr))
        end
      | V_ediv_mmnm (e1, e2) -> begin
          let dividend, divisor = (e1 |> soe), (e2 |> soe) in
          let qr = Smt.ZPair.create ~fst:(Smt.ZMutez.create_div dividend divisor |> Smt.ZMutez.to_zint) ~snd:(Smt.ZMutez.create_mod dividend divisor) in
          let div_zero_result = Smt.ZOption.create_none ~content_sort:(qr |> Smt.ZExpr.read_sort) in
          Smt.ZExpr.create_ite ~cond:(Smt.ZMutez.create_eq divisor (Smt.ZMutez.zero_)) ~t:div_zero_result ~f:(Smt.ZOption.create_some ~content:(qr))
        end
      | V_get_xmoy (e1, e2) -> Smt.ZMap.read_value ~key:(e1 |> soe) ~map:(e2 |> soe)
      | V_get_xbmo (e1, e2) -> Smt.ZMap.read_value ~key:(e1 |> soe) ~map:(e2 |> soe)
      | V_slice_nnso (e1, e2, e3) -> e3 |> soe |> Smt.ZStr.create_slice ~low:(e1 |> soe) ~high:(Smt.ZInt.create_add [(e1 |> soe); (e2 |> soe);])
      | V_slice_nnbo _ -> err ve
      | V_unpack _ -> err ve (* similar to pack *)
      | V_contract_of_address _ -> err ve (* encoding is complicated *)
      | V_isnat _ -> err ve (* not supported *)

      (*************************************************************************)
      (* List                                                                  *)
      (*************************************************************************)
      | V_lit_list (t, el) -> begin
          el |> Core.List.fold_right
                  ~f:(fun e l -> l |> Smt.ZList.update ~content:(e |> soe))
                  ~init:(Smt.ZList.create ~content_sort:(t |> smtsort_of_vlangtyp))
        end
      | V_nil t -> Smt.ZList.create ~content_sort:(t |> smtsort_of_vlangtyp)
      | V_cons (e1, e2) -> e2 |> soe |> Smt.ZList.update ~content:(e1 |> soe)
      | V_tl_l e -> e |> soe |> Smt.ZList.read_tail
      | V_append_l _ -> err ve

      (*************************************************************************)
      (* Set                                                                   *)
      (*************************************************************************)
      | V_lit_set (elt, e) -> begin
          let kt, vt = elt, Vlang.Ty.T_bool in
          e |> Core.Set.Poly.fold
            ~init:(Smt.ZMap.create ~key_sort:(kt |> smtsort_of_vlangtyp) ~value_sort:(vt |> smtsort_of_vlangtyp))
            ~f:(fun acc_set key -> Smt.ZMap.update ~key:(soe key) ~value:(Smt.ZOption.create_some ~content:(soe (V_lit_bool true))) ~map:acc_set)
        end
      | V_empty_set elt -> begin
          let kt, vt = elt, Vlang.Ty.T_bool in
          Smt.ZMap.create ~key_sort:(kt |> smtsort_of_vlangtyp) ~value_sort:(vt |> smtsort_of_vlangtyp)
        end
      | V_update_xbss (e1, e2, e3) -> begin
          let elem = begin
            Smt.ZExpr.create_ite
            ~cond:(soe e2)
            ~t:(Smt.ZOption.create_some ~content:(Smt.ZBool.true_))
            ~f:(Smt.ZOption.create_none ~content_sort:(Smt.ZBool.sort))
          end in
          Smt.ZMap.update ~key:(e1 |> soe) ~value:(elem) ~map:(e3 |> soe)
        end
      | V_tl_s _ -> err ve  (* not supported *)

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
      | V_pair (e1, e2) -> Smt.ZPair.create ~fst:(e1 |> soe) ~snd:(e2 |> soe)
      | V_hd_m _ -> err ve  (* previous implementation mixes list and array theories *) 
      | V_hd_bm _ -> err ve
      | V_hdtl_l e -> Smt.ZPair.create ~fst:(e |> soe |> Smt.ZList.read_head) ~snd:(e |> soe |> Smt.ZList.read_tail)
      | V_hdtl_s _ -> err ve
      | V_hdtl_m _ -> err ve

      (*************************************************************************)
      (* Or                                                                    *)
      (*************************************************************************)
      (* | V_lit_or *) (* It cannot construct any value, use V_left or V_right instead *)
      | V_left (t, e) -> Smt.ZOr.create_left ~left_content:(e |> soe) ~right_sort:(t |> Vlang.TypeUtil.get_innertyp2 |> Stdlib.snd |> smtsort_of_vlangtyp)
      | V_right (t, e) -> Smt.ZOr.create_right ~left_sort:(t |> Vlang.TypeUtil.get_innertyp2 |> Stdlib.fst |> smtsort_of_vlangtyp) ~right_content:(e |> soe)

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
      | V_lit_map (kt, vt, e) -> begin
          e |> Core.Map.Poly.fold
            ~init:(Smt.ZMap.create ~key_sort:(kt |> smtsort_of_vlangtyp) ~value_sort:(vt |> smtsort_of_vlangtyp))
            ~f:(fun ~key ~data acc_zm -> Smt.ZMap.update ~key:(key |> soe) ~value:(Smt.ZOption.create_some ~content:(data |> soe)) ~map:acc_zm)
        end
      | V_empty_map (kt, vt) -> Smt.ZMap.create ~key_sort:(kt |> smtsort_of_vlangtyp) ~value_sort:(vt |> smtsort_of_vlangtyp)
      | V_update_xomm (e1, e2, e3) -> Smt.ZMap.update ~key:(e1 |> soe) ~value:(e2 |> soe) ~map:(e3 |> soe)
      | V_tl_m _ -> err ve  (* not supported *)

      (*************************************************************************)
      (* Big Map                                                               *)
      (*************************************************************************)
      | V_lit_big_map (kt, vt, e) -> begin
          e |> Core.Map.Poly.fold
            ~init:(Smt.ZMap.create ~key_sort:(kt |> smtsort_of_vlangtyp) ~value_sort:(vt |> smtsort_of_vlangtyp))
            ~f:(fun ~key ~data acc_zm -> Smt.ZMap.update ~key:(key |> soe) ~value:(Smt.ZOption.create_some ~content:(data |> soe)) ~map:acc_zm)
        end
      | V_empty_big_map (kt, vt) -> Smt.ZMap.create ~key_sort:(kt |> smtsort_of_vlangtyp) ~value_sort:(vt |> smtsort_of_vlangtyp)
      | V_update_xobmbm (e1, e2, e3) -> Smt.ZMap.update ~key:(e1 |> soe) ~value:(e2 |> soe) ~map:(e3 |> soe)
      | V_tl_bm _ -> err ve  (* not supported *)
      
      (*************************************************************************)
      (* Chain Id                                                              *)
      (*************************************************************************)
      | V_lit_chain_id _ -> err ve
      | V_chain_id -> err ve
    with
    | Smt.ZError s -> SMT_Encode_Error_e (ve, s) |> raise
    | e -> e |> raise
end (* function smtexpr_of_vlangexpr end *)

let rec smtexpr_of_vlangformula : Vlang.t -> Smt.ZFormula.t
= let open Vlang.Formula in
  let sof = smtexpr_of_vlangformula in  (* syntax sugar *)
  let soe = smtexpr_of_vlangexpr in (* syntax sugar *)
  let err f = Stdlib.raise (Not_Implemented_f f) in (* syntax sugar *)
  fun vf -> begin
    try
      match vf with
      (* logical formula *)
      | VF_true -> Smt.ZFormula.true_
      | VF_false -> Smt.ZFormula.false_
      | VF_not f -> Smt.ZFormula.create_not (f |> sof)
      | VF_and fl -> Smt.ZFormula.create_and (fl |> Core.List.map ~f:sof)
      | VF_or fl -> Smt.ZFormula.create_or (fl |> Core.List.map ~f:sof)
      | VF_eq (e1,e2) -> Smt.ZFormula.create_eq (e1 |> soe) (e2 |> soe)
      | VF_imply (f1, f2) -> Smt.ZFormula.create_imply (f1 |> sof) (f2 |> sof)
      (* micse-cfg specific boolean *)
      | VF_mich_if e -> Smt.ZBool.create_eq (e |> soe) (Smt.ZBool.true_)
      | VF_mich_if_none e -> Smt.ZOption.is_none (e |> soe)
      | VF_mich_if_left e -> Smt.ZOr.is_left (e |> soe)
      | VF_mich_if_cons e -> Smt.ZList.is_cons (e |> soe)
      (* NOT USED. belows are not constructed from Prover.converter *)
      | VF_mich_loop e -> Smt.ZBool.create_eq (e |> soe) (Smt.ZBool.true_)
      | VF_mich_loop_left e -> Smt.ZOr.is_left (e |> soe)
      | VF_mich_map_l e -> Smt.ZOption.is_none (e |> soe)
      | VF_mich_map_m _ -> err vf (* check whether map's size is not 0 *)
      | VF_mich_iter_l e -> Smt.ZOption.is_none (e |> soe)
      | VF_mich_iter_s _ -> err vf (* check whether set's size is not 0 *)
      | VF_mich_iter_m _ -> err vf (* check whether map's size is not 0 *)
      | VF_mich_micse_check_value e -> Smt.ZBool.create_eq (e |> soe) (Smt.ZBool.true_)
      (* Custom Formula for verifiying *)
      | VF_add_mmm_no_overflow (e1, e2) -> Smt.ZMutez.check_add_no_overflow (e1 |> soe) (e2 |> soe)
      | VF_sub_mmm_no_underflow (e1, e2) -> Smt.ZMutez.check_sub_no_underflow (e1 |> soe) (e2 |> soe)
      | VF_mul_mnm_no_overflow (e1, e2) -> Smt.ZMutez.check_mul_no_overflow (e1 |> soe) (e2 |> soe |> Smt.ZInt.to_zmutez)
      | VF_mul_nmm_no_overflow (e1, e2) -> Smt.ZMutez.check_mul_no_overflow (e1 |> soe |> Smt.ZInt.to_zmutez) (e2 |> soe)
      (* Custom Domain Formula for Invariant Generation *)
      | VF_sigma_equal (_, _, _) -> Smt.ZBool.true_ (* TODO *)
    with
    | Smt.ZError s -> SMT_Encode_Error_f (vf, s) |> raise
    | e -> e |> raise
end (* function smtexpr_of_vlangformula end *)

let verify : Vlang.Formula.t -> Smt.ZSolver.validity * Smt.ZModel.t option
= let fts = Vlang.Formula.to_string in  (* syntax sugar *)
  let ets = Vlang.Expr.to_string in     (* syntax sugar *)
  fun f -> begin
  try Smt.ZSolver.check_validity [(f |> smtexpr_of_vlangformula)] with
  | Not_Implemented_f f -> Error ((f |> fts) ^ " is not implemented") |> raise
  | Not_Implemented_e e -> Error ((e |> ets) ^ " is not implemented") |> raise
  | SMT_Encode_Error_f (f, z3s) -> Error ((f |> fts) ^ " occurs encoding error \"" ^ z3s ^ "\"") |> raise
  | SMT_Encode_Error_e (e, z3s) -> Error ((e |> ets) ^ " occurs encoding error \"" ^ z3s ^ "\"") |> raise
  | e -> e |> raise
end