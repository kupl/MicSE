(* TzCvt - Convert for Tz module *)


(*****************************************************************************)
(*****************************************************************************)
(* Michelson to Tz                                                           *)
(*****************************************************************************)
(*****************************************************************************)

module M2T = struct
  open PreLib.Mich

  exception Error of string

  let cv_pos : pos -> Tz.ccp_pos = fun {col=c; lin=l} -> {col=c; lin=l}
  let cv_loc : loc -> Tz.ccp_loc = (function | Unknown -> CCLOC_Unknown | Pos (p1,p2) -> CCLOC_Pos (cv_pos p1, cv_pos p2))
  let cv_annot : annot -> Tz.ccp_annot = (function | A_typ s -> CCA_typ s | A_var s -> CCA_var s | A_fld s -> CCA_fld s)
  let cv_t : 'a t -> 'a Tz.cc = fun {pos; ann; d} -> {cc_loc=(cv_loc pos); cc_anl=(List.map cv_annot ann); cc_v=d}
  let rec cv_typ : typ -> Tz.mich_t = 
    (function
    | T_key        -> MT_key
    | T_unit       -> MT_unit
    | T_signature  -> MT_signature
    | T_option t   -> MT_option (cv_typt t)
    | T_list t     -> MT_list (cv_typt t)
    | T_set t      -> MT_set (cv_typt t)
    | T_operation  -> MT_operation
    | T_contract t -> MT_contract (cv_typt t)
    | T_pair      (t1, t2) -> MT_pair (cv_typt t1, cv_typt t2)
    | T_or        (t1, t2) -> MT_or (cv_typt t1, cv_typt t2)
    | T_lambda    (t1, t2) -> MT_lambda (cv_typt t1, cv_typt t2)
    | T_map       (t1, t2) -> MT_map (cv_typt t1, cv_typt t2)
    | T_big_map   (t1, t2) -> MT_big_map (cv_typt t1, cv_typt t2)
    | T_chain_id  -> MT_chain_id
    | T_int       -> MT_int
    | T_nat       -> MT_nat
    | T_string    -> MT_string
    | T_bytes     -> MT_bytes
    | T_mutez     -> MT_mutez
    | T_bool      -> MT_bool
    | T_key_hash  -> MT_key_hash
    | T_timestamp -> MT_timestamp
    | T_address   -> MT_address
    )
  and cv_typt : typ t -> Tz.mich_t Tz.cc = fun x -> cv_t {x with d=(cv_typ x.d)}
  let rec cv_inst : inst -> Tz.mich_i =
    (function
    | I_seq (i1,i2)   -> MI_seq (cv_instt i1, cv_instt i2)
    | I_drop          -> MI_drop (Z.one)
    | I_drop_n zn     -> MI_drop zn
    | I_dup           -> MI_dup (Z.one)
    | I_swap          -> MI_swap
    | I_dig zn        -> MI_dig zn
    | I_dug zn        -> MI_dug zn
    | I_push (t, d)   -> MI_push (cv_typt t, cv_datat t d)
    | I_some          -> MI_some
    | I_none t        -> MI_none (cv_typt t)
    | I_unit          -> MI_unit
    | I_if_none (i1,i2) -> MI_if_none (cv_instt i1, cv_instt i2)
    | I_pair          -> MI_pair
    | I_car           -> MI_car
    | I_cdr           -> MI_cdr
    | I_left t        -> MI_left (cv_typt t)
    | I_right t       -> MI_right (cv_typt t)
    | I_if_left (i1,i2) -> MI_if_left (cv_instt i1, cv_instt i2)
    | I_nil t         -> MI_nil (cv_typt t)
    | I_cons          -> MI_cons
    | I_if_cons (i1,i2) -> MI_if_cons (cv_instt i1, cv_instt i2)
    | I_size          -> MI_size
    | I_empty_set t   -> MI_empty_set (cv_typt t)
    | I_empty_map (t1,t2) -> MI_empty_map (cv_typt t1, cv_typt t2)
    | I_empty_big_map (t1,t2) -> MI_empty_big_map (cv_typt t1, cv_typt t2)
    | I_map i         -> MI_map (cv_instt i)
    | I_iter i        -> MI_iter (cv_instt i)
    | I_mem           -> MI_mem
    | I_get           -> MI_get
    | I_update        -> MI_update
    | I_if (i1,i2)    -> MI_if (cv_instt i1, cv_instt i2)
    | I_loop i        -> MI_loop (cv_instt i)
    | I_loop_left i   -> MI_loop_left (cv_instt i)
    | I_lambda (t1,t2,i) -> MI_lambda (cv_typt t1, cv_typt t2, cv_instt i)
    | I_exec          -> MI_exec
    | I_dip i         -> MI_dip_n (Z.one, cv_instt i)
    | I_dip_n (zn,i)  -> MI_dip_n (zn, cv_instt i)
    | I_failwith      -> MI_failwith
    | I_cast t        -> MI_cast (cv_typt t)
    | I_rename        -> MI_rename
    | I_concat        -> MI_concat
    | I_slice         -> MI_slice
    | I_pack          -> MI_pack
    | I_unpack t      -> MI_unpack (cv_typt t)
    | I_add           -> MI_add
    | I_sub           -> MI_sub
    | I_mul           -> MI_mul
    | I_ediv          -> MI_ediv
    | I_abs           -> MI_abs
    | I_isnat         -> MI_isnat
    | I_int           -> MI_int
    | I_neg           -> MI_neg
    | I_lsl           -> MI_lsl
    | I_lsr           -> MI_lsr
    | I_or            -> MI_or
    | I_and           -> MI_and
    | I_xor           -> MI_xor
    | I_not           -> MI_not
    | I_compare       -> MI_compare
    | I_eq            -> MI_eq
    | I_neq           -> MI_neq
    | I_lt            -> MI_lt
    | I_gt            -> MI_gt
    | I_le            -> MI_le
    | I_ge            -> MI_ge
    | I_self          -> MI_self
    | I_contract t    -> MI_contract (cv_typt t)
    | I_transfer_tokens -> MI_transfer_tokens
    | I_set_delegate  -> MI_set_delegate
    | I_create_account -> MI_create_account
    | I_create_contract p -> (let (tp,ts,i) = cv_program p in MI_create_contract (tp,ts,i))
    | I_implicit_account -> MI_implicit_account
    | I_now           -> MI_now
    | I_amount        -> MI_amount
    | I_balance       -> MI_balance
    | I_check_signature -> MI_check_signature
    | I_blake2b       -> MI_blake2b
    | I_sha256        -> MI_sha256
    | I_sha512        -> MI_sha512
    | I_hash_key      -> MI_hash_key
    | I_steps_to_quota -> MI_steps_to_quota
    | I_source        -> MI_source
    | I_sender        -> MI_sender
    | I_address       -> MI_address
    | I_chain_id      -> MI_chain_id
    | I_unpair        -> MI_unpair
    (* Non-Standard Instruction : Special Comment : MicSE user defined safety property *)
    | I_micse_check i -> MI_micse_check (cv_instt i)
    (* Error case - Macros & Undefined *)
    | _ -> Error "cv_inst" |> raise
    )
  and cv_instt : inst t -> Tz.mich_i Tz.cc = fun x -> cv_t {x with d=(cv_inst x.d)}
  and cv_data : typ t -> data -> Tz.mich_v =
    let open Tz in
    fun tt dd ->
    (match tt.PreLib.Mich.d, dd with
    | T_key,              D_string s -> MV_lit_key s
    | T_unit,             D_unit -> MV_unit
    | T_signature,        D_string s -> MV_lit_signature_str s
    | T_option t,         D_none -> MV_none (cv_typt t)
    | T_option t,         D_some d -> MV_some (cv_datat t d)
    | T_list t,           D_list d -> MV_lit_list (cv_typt t, List.map (cv_datat t) d)
    | T_set t,            D_list d -> MV_lit_set (cv_typt t, PSet.of_list (List.map (cv_datat t) d))
    | T_pair (t1,t2),     D_pair (d1,d2) -> MV_pair (cv_datat t1 d1, cv_datat t2 d2)
    | T_or (t1,_),        D_left d -> MV_left (cv_typt tt, cv_datat t1 d)
    | T_or (_,t2),        D_right d -> MV_right (cv_typt tt, cv_datat t2 d)
    | T_lambda (t1,t2),   D_lambda i -> MV_lit_lambda (cv_typt t1, cv_typt t2, cv_instt i)
    | T_map (t1,t2),      D_list d ->
      ( let m : (mich_v cc, mich_v cc) PMap.t = 
          List.fold_left 
          (fun acc ddd -> 
            match ddd.d with 
            | D_elt (d1,d2) -> 
              (PMap.add acc ~key:(cv_datat t1 d1) ~data:(cv_datat t2 d2)) 
              |> (function | `Ok m -> m | `Duplicate -> Error "cv_data : (T_map, D_list) : duplicated" |> raise)
            | _ -> Error "cv_data : (T_map, D_list) : not-Elt" |> raise)
          PMap.empty
          d
        in 
        MV_lit_map (cv_typt t1, cv_typt t2, m)
      )
    | T_big_map (t1,t2),  D_list d ->
      ( let m : (mich_v cc, mich_v cc) PMap.t = 
          List.fold_left 
          (fun acc ddd -> 
            match ddd.d with 
            | D_elt (d1,d2) -> 
              (PMap.add acc ~key:(cv_datat t1 d1) ~data:(cv_datat t2 d2)) 
              |> (function | `Ok m -> m | `Duplicate -> Error "cv_data : (T_big_map, D_list) : duplicated" |> raise)
            | _ -> Error "cv_data : (T_big_map, D_list) : not-Elt" |> raise)
          PMap.empty
          d
        in 
        MV_lit_big_map (cv_typt t1, cv_typt t2, m)
      )
    | T_chain_id,         D_string s -> MV_lit_chain_id s
    | T_int,              D_int zn -> MV_lit_int zn
    | T_nat,              D_int zn -> MV_lit_nat zn
    | T_string,           D_string s -> MV_lit_string s
    | T_bytes,            D_bytes s -> MV_lit_bytes s
    | T_mutez,            D_int zn -> MV_lit_mutez zn
    | T_bool,             D_bool b -> MV_lit_bool b
    | T_key_hash,         D_string s -> MV_lit_key_hash s
    | T_timestamp,        D_string s -> MV_lit_timestamp_str s
    | T_timestamp,        D_int zn -> MV_lit_timestamp_sec zn
    | T_address,          D_string s -> MV_lit_address (MV_lit_key_hash s |> gen_dummy_cc)
    | _ -> Error "cv_data : match failed" |> raise
    (* unused *)
    (* | T_operation,        D_ -> *)
    (* | T_contract t,       D_ -> *)
    )
  and cv_datat : typ t -> data t -> Tz.mich_v Tz.cc = fun t x -> cv_t {x with d=(cv_data t x.d)}
  and cv_program : program -> (Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc) =
    fun {param; storage; code} -> (cv_typt param, cv_typt storage, cv_instt code)
end (* module M2T end *)


(*****************************************************************************)
(*****************************************************************************)
(* Tz to SMT                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

module T2S = struct
  
  open Tz
  open ProverLib.Smt

  exception Not_Implemented_f of Tz.mich_f
  exception Not_Implemented_e of (Tz.mich_v Tz.cc)
  exception SMT_Encode_Error_f of (mich_f * string)
  exception SMT_Encode_Error_e of (mich_v cc * string)

  let rec cv_mt : mich_t -> ZSort.t = 
    (let sot = cv_mtcc in (* syntax sugar *)
    function
    | MT_key -> ZKey.sort ()
    | MT_unit -> ZUnit.sort ()
    | MT_signature -> ZSignature.sort ()
    | MT_option t -> ZOption.create_sort ~content_sort:(sot t)
    | MT_list t -> ZList.create_sort ~content_sort:(sot t)
    | MT_set t -> ZList.create_sort ~content_sort:(sot t)
    | MT_operation -> ZOperation.sort ()
    | MT_contract _ -> ZContract.sort ()
    | MT_pair (t1, t2) -> ZPair.create_sort ~fst_sort:(sot t1) ~snd_sort:(sot t2)
    | MT_or (t1, t2) -> ZOr.create_sort ~left_sort:(sot t1) ~right_sort:(sot t2)
    | MT_lambda (_, _) -> ZLambda.sort ()
    | MT_map (t1, t2) -> ZMap.create_sort ~key_sort:(sot t1) ~value_sort:(sot t2)
    | MT_big_map (t1, t2) -> ZMap.create_sort ~key_sort:(sot t1) ~value_sort:(sot t2)
    | MT_chain_id -> ZStr.sort ()
    | MT_int -> ZInt.sort ()
    | MT_nat -> ZNat.sort ()
    | MT_string -> ZStr.sort ()
    | MT_bytes -> ZBytes.sort ()
    | MT_mutez -> ZMutez.sort ()
    | MT_bool -> ZBool.sort ()
    | MT_key_hash -> ZKeyHash.sort ()
    | MT_timestamp -> ZInt.sort ()
    | MT_address -> ZAddress.sort ()
    ) (* function cv_mt end *)
  and cv_mtcc : mich_t cc -> ZSort.t = (fun x -> cv_mt x.cc_v)
  let rec cv_compare : mich_v cc -> mich_v cc -> ZExpr.t =
    (fun e1 e2 ->
    match (typ_of_val e1).cc_v, (typ_of_val e2).cc_v with
    | MT_int, MT_int -> ZInt.create_cmp (cv_mvcc e1) (cv_mvcc e2)
    | MT_nat, MT_nat -> ZInt.create_cmp (cv_mvcc e1) (cv_mvcc e2)
    | MT_string, MT_string -> ZStr.create_cmp (cv_mvcc e1) (cv_mvcc e2)
    | MT_bytes, MT_bytes -> ZStr.create_cmp (cv_mvcc e1) (cv_mvcc e2)
    | MT_mutez, MT_mutez -> ZMutez.create_cmp (cv_mvcc e1) (cv_mvcc e2)
    | MT_bool, MT_bool -> Stdlib.raise (Not_Implemented_e e1)
    | MT_key_hash, MT_key_hash -> ZKeyHash.create_cmp (cv_mvcc e1) (cv_mvcc e2)
    | MT_timestamp, MT_timestamp -> ZInt.create_cmp (cv_mvcc e1) (cv_mvcc e2)
    | MT_address, MT_address -> ZAddress.create_cmp (cv_mvcc e1) (cv_mvcc e2)
    | MT_pair (_, _), MT_pair (_, _) -> 
      let fstcmp : ZInt.t = cv_compare (MV_car e1 |> gen_dummy_cc) (MV_car e2 |> gen_dummy_cc) in
      let fst_is_zero : ZExpr.t = ZInt.create_eq fstcmp (ZInt.zero_ ()) in
      let sndcmp : ZInt.t = cv_compare (MV_cdr e1 |> gen_dummy_cc) (MV_cdr e2 |> gen_dummy_cc) in
      ZExpr.create_ite ~cond:(fst_is_zero) ~t:(sndcmp) ~f:(fstcmp)
    | t1, t2 when t1 = t2 -> Stdlib.failwith ("TzCvt.cv_compare : expression like this cannot be compared")
    | _ -> Stdlib.failwith ("TzCvt.cv_compare : two expressions have different types")
    ) (* function cv_compare end *)
  and cv_mv : mich_v -> ZExpr.t =
    (let err (e: mich_v) = (gen_dummy_cc e) |> typ_of_val |> cv_mtcc |> ZExpr.create_dummy in
    fun eee -> match eee with
    (*************************************************************************)
    (* Symbol & Polymorphic                                                  *)
    (*************************************************************************)
    | MV_symbol (t,v) -> ZExpr.create_var (cv_mtcc t) ~name:v
    | MV_car e -> ZPair.read_fst (cv_mvcc e)
    | MV_cdr e -> ZPair.read_snd (cv_mvcc e)
    | MV_unlift_option e -> ZOption.read (cv_mvcc e)
    | MV_unlift_left e -> ZOr.read_left (cv_mvcc e)
    | MV_unlift_right e -> ZOr.read_right (cv_mvcc e)
    | MV_hd_l e -> ZList.read_head (cv_mvcc e)
  
    (*************************************************************************)
    (* Integer                                                               *)
    (*************************************************************************)
    | MV_lit_int zn -> ZInt.of_zarith zn
    | MV_neg_ni e -> ZInt.create_neg (cv_mvcc e)
    | MV_neg_ii e -> ZInt.create_neg (cv_mvcc e)
    | MV_not_ni e -> ZInt.create_not (cv_mvcc e)
    | MV_not_ii e -> ZInt.create_not (cv_mvcc e)
    | MV_add_nii (e1, e2) -> ZInt.create_add [cv_mvcc e1; cv_mvcc e2;]
    | MV_add_ini (e1, e2) -> ZInt.create_add [cv_mvcc e1; cv_mvcc e2;]
    | MV_add_iii (e1, e2) -> ZInt.create_add [cv_mvcc e1; cv_mvcc e2;]
    | MV_sub_nni (e1, e2) -> ZInt.create_sub [cv_mvcc e1; cv_mvcc e2;]
    | MV_sub_nii (e1, e2) -> ZInt.create_sub [cv_mvcc e1; cv_mvcc e2;]
    | MV_sub_ini (e1, e2) -> ZInt.create_sub [cv_mvcc e1; cv_mvcc e2;]
    | MV_sub_iii (e1, e2) -> ZInt.create_sub [cv_mvcc e1; cv_mvcc e2;]
    | MV_sub_tti (e1, e2) -> ZInt.create_sub [cv_mvcc e1; cv_mvcc e2;]
    | MV_mul_nii (e1, e2) -> ZInt.create_mul [cv_mvcc e1; cv_mvcc e2;]
    | MV_mul_ini (e1, e2) -> ZInt.create_mul [cv_mvcc e1; cv_mvcc e2;]
    | MV_mul_iii (e1, e2) -> ZInt.create_mul [cv_mvcc e1; cv_mvcc e2;]
    | MV_compare (e1, e2) -> cv_compare e1 e2
    | MV_int_of_nat e -> cv_mvcc e
  
    (*************************************************************************)
    (* Natural Number                                                        *)
    (*************************************************************************)
    | MV_lit_nat zn -> ZNat.of_zarith zn
    | MV_abs_in e -> ZNat.create_abs (cv_mvcc e)
    | MV_add_nnn (e1, e2) -> ZNat.create_add [cv_mvcc e1; cv_mvcc e2;]
    | MV_mul_nnn (e1, e2) -> ZNat.create_mul [cv_mvcc e1; cv_mvcc e2;]
    | MV_shiftL_nnn (e1, e2) -> ZNat.create_shiftL (cv_mvcc e1) (cv_mvcc e2)
    | MV_shiftR_nnn (e1, e2) -> ZNat.create_shiftR (cv_mvcc e1) (cv_mvcc e2)
    | MV_and_nnn (e1, e2) -> ZNat.create_and (cv_mvcc e1) (cv_mvcc e2)
    | MV_and_inn (e1, e2) -> ZNat.create_and (cv_mvcc e1) (cv_mvcc e2)
    | MV_or_nnn (e1, e2) -> ZNat.create_or (cv_mvcc e1) (cv_mvcc e2)
    | MV_xor_nnn (e1, e2) -> ZNat.create_xor (cv_mvcc e1) (cv_mvcc e2)
    | MV_size_s _ -> err eee
    | MV_size_m _ -> err eee
    | MV_size_l _ -> err eee
    | MV_size_str _ -> err eee
    | MV_size_b _ -> err eee
  
    (*************************************************************************)
    (* String                                                                *)
    (*************************************************************************)
    | MV_lit_string s -> ZStr.of_string s
    | MV_concat_sss (e1, e2) -> ZStr.create_concat [cv_mvcc e1; cv_mvcc e2;]
    | MV_concat_list_s _ -> err eee
  
    (*************************************************************************)
    (* Bytes                                                                 *)
    (*************************************************************************)
    | MV_lit_bytes s -> ZBytes.create_bytstr (ZStr.of_string s)
    | MV_concat_bbb (e1, e2) -> ZBytes.create_concatenated ~fst_bytes:(cv_mvcc e1) ~snd_bytes:(cv_mvcc e2)
    | MV_concat_list_b _ -> err eee
    | MV_pack _ -> ZBytes.create_pack ()
    | MV_blake2b e -> ZBytes.create_blake2b (cv_mvcc e)
    | MV_sha256  e -> ZBytes.create_sha256 (cv_mvcc e)
    | MV_sha512  e -> ZBytes.create_sha512 (cv_mvcc e)
  
    (*************************************************************************)
    (* Mutez                                                                 *)
    (*************************************************************************)
    | MV_lit_mutez zn -> ZMutez.of_zarith zn
    | MV_add_mmm (e1, e2) -> ZMutez.create_mul (cv_mvcc e1) (cv_mvcc e2)
    | MV_sub_mmm (e1, e2) -> ZMutez.create_mul (cv_mvcc e1) (cv_mvcc e2)
    | MV_mul_mnm (e1, e2) -> ZMutez.create_mul (cv_mvcc e1) (cv_mvcc e2 |> ZNat.to_zmutez)
    | MV_mul_nmm (e1, e2) -> ZMutez.create_mul (cv_mvcc e1 |> ZNat.to_zmutez) (cv_mvcc e2)
  
    (*************************************************************************)
    (* Bool                                                                  *)
    (*************************************************************************)
    | MV_lit_bool b -> ZBool.of_bool b
    | MV_not_bb e -> ZBool.create_not (cv_mvcc e)
    | MV_and_bbb (e1, e2) -> ZBool.create_and (cv_mvcc e1) (cv_mvcc e2)
    | MV_or_bbb  (e1, e2) -> ZBool.create_or  (cv_mvcc e1) (cv_mvcc e2)
    | MV_xor_bbb (e1, e2) -> ZBool.create_xor (cv_mvcc e1) (cv_mvcc e2)
    | MV_eq_ib   (e1, e2) -> ZInt.create_eq   (cv_mvcc e1) (cv_mvcc e2)
    | MV_neq_ib  (e1, e2) -> ZInt.create_neq  (cv_mvcc e1) (cv_mvcc e2)
    | MV_lt_ib   (e1, e2) -> ZInt.create_lt   (cv_mvcc e1) (cv_mvcc e2)
    | MV_gt_ib   (e1, e2) -> ZInt.create_gt   (cv_mvcc e1) (cv_mvcc e2)
    | MV_leq_ib  (e1, e2) -> ZInt.create_le   (cv_mvcc e1) (cv_mvcc e2)
    | MV_geq_ib  (e1, e2) -> ZInt.create_ge   (cv_mvcc e1) (cv_mvcc e2)
    | MV_mem_xsb _ -> err eee
    | MV_mem_xmb (e1, e2) -> ZMap.read_exist ~key:(cv_mvcc e1) ~map:(cv_mvcc e2)
    | MV_mem_xbmb (e1, e2) -> ZMap.read_exist ~key:(cv_mvcc e1) ~map:(cv_mvcc e2)
    | MV_check_signature _ -> err eee
  
    (*************************************************************************)
    (* Key Hash                                                              *)
    (*************************************************************************)
    | MV_lit_key_hash s -> ZKeyHash.of_string s
    | MV_hash_key k -> ZKeyHash.create_hashkey (cv_mvcc k)
  
    (*************************************************************************)
    (* Timestamp                                                             *)
    (*************************************************************************)
    | MV_lit_timestamp_str s -> begin
        (* About Ptime: see https://erratique.ch/software/ptime
            or https://github.com/dbuenzli/ptime
        *)
        Ptime.of_rfc3339 ~strict:false ~sub:true ~start:0 s 
        |> Result.get_ok  (* It might raise "Invalid_argument" *)
        |> (fun (pt, _, _) -> pt)
        |> Ptime.to_span
        |> Ptime.Span.to_int_s
        |> Option.get   (* It might raise "Invalid_argument" *)
        |> ZInt.of_int
      end
    | MV_lit_timestamp_sec zn -> ZInt.of_int (Z.to_int zn)
    | MV_add_tit (e1, e2) -> ZInt.create_add [cv_mvcc e1; cv_mvcc e2;]
    | MV_add_itt (e1, e2) -> ZInt.create_add [cv_mvcc e1; cv_mvcc e2;]
    | MV_sub_tit (e1, e2) -> ZInt.create_sub [cv_mvcc e1; cv_mvcc e2;]
  
    (*************************************************************************)
    (* Address                                                               *)
    (*************************************************************************)
    | MV_lit_address kh -> ZAddress.create_addrkh (cv_mvcc kh)
    | MV_address_of_contract _ -> err eee
  
    (*************************************************************************)
    (* Key                                                                   *)
    (*************************************************************************)
    | MV_lit_key s -> ZKey.create_keystr (ZStr.of_string s)
  
    (*************************************************************************)
    (* Unit                                                                  *)
    (*************************************************************************)
    | MV_unit -> ZUnit.create ()
  
    (*************************************************************************)
    (* Signature                                                             *)
    (*************************************************************************)
    | MV_lit_signature_str _ -> err eee
    | MV_lit_signature_signed (_, _) -> err eee
  
    (*************************************************************************)
    (* Option                                                                *)
    (*************************************************************************)
    | MV_some e -> ZOption.create_some ~content:(cv_mvcc e)
    | MV_none t -> ZOption.create_none ~content_sort:(cv_mtcc t)
    | MV_ediv_nnnn (e1, e2) 
    | MV_ediv_niin (e1, e2)
    | MV_ediv_inin (e1, e2)
    | MV_ediv_iiin (e1, e2) -> begin
        let dividend, divisor = (cv_mvcc e1), (cv_mvcc e2) in
        let qr = ZPair.create ~fst:(ZInt.create_div dividend divisor) ~snd:(ZInt.create_mod dividend divisor) in
        let div_zero_result = ZOption.create_none ~content_sort:(qr |> ZExpr.read_sort) in
        ZExpr.create_ite ~cond:(ZInt.create_eq divisor (ZInt.zero_ ())) ~t:div_zero_result ~f:(ZOption.create_some ~content:(qr))
      end
    | MV_ediv_mnmm (e1, e2) -> begin
        let dividend, divisor = (cv_mvcc e1), (cv_mvcc e2 |> ZInt.to_zmutez) in
        let qr = ZPair.create ~fst:(ZMutez.create_div dividend divisor) ~snd:(ZMutez.create_mod dividend divisor) in
        let div_zero_result = ZOption.create_none ~content_sort:(qr |> ZExpr.read_sort) in
        ZExpr.create_ite ~cond:(ZMutez.create_eq divisor (ZMutez.zero_ ())) ~t:div_zero_result ~f:(ZOption.create_some ~content:(qr))
      end
    | MV_ediv_mmnm (e1, e2) -> begin
        let dividend, divisor = (cv_mvcc e1), (cv_mvcc e2) in
        let qr = ZPair.create ~fst:(ZMutez.create_div dividend divisor |> ZMutez.to_zint) ~snd:(ZMutez.create_mod dividend divisor) in
        let div_zero_result = ZOption.create_none ~content_sort:(qr |> ZExpr.read_sort) in
        ZExpr.create_ite ~cond:(ZMutez.create_eq divisor (ZMutez.zero_ ())) ~t:div_zero_result ~f:(ZOption.create_some ~content:(qr))
      end
    | MV_get_xmoy (e1, e2) -> ZMap.read_value ~key:(cv_mvcc e1) ~map:(cv_mvcc e2)
    | MV_get_xbmo (e1, e2) -> ZMap.read_value ~key:(cv_mvcc e1) ~map:(cv_mvcc e2)
    | MV_slice_nnso (e1, e2, e3) -> (cv_mvcc e3) |> ZStr.create_slice ~low:(cv_mvcc e1) ~high:(ZInt.create_add [cv_mvcc e1; cv_mvcc e2;])
    | MV_slice_nnbo _ -> err eee
    | MV_unpack _ -> err eee
    | MV_contract_of_address _ -> err eee
    | MV_isnat _ -> err eee
  
    (*************************************************************************)
    (* List                                                                  *)
    (*************************************************************************)
    | MV_lit_list (t, el) -> begin
        el |> Core.List.fold_right
                ~f:(fun e l -> l |> ZList.update ~content:(cv_mvcc e))
                ~init:(ZList.create ~content_sort:(cv_mtcc t))
      end
    | MV_nil t -> ZList.create ~content_sort:(cv_mtcc t)
    | MV_cons (e1, e2) -> ZList.update ~content:(cv_mvcc e1) (cv_mvcc e2)
    | MV_tl_l e -> ZList.read_tail (cv_mvcc e)
  
    (*************************************************************************)
    (* Set                                                                   *)
    (*************************************************************************)
    | MV_lit_set (elt, e) -> begin
      let kt, vt = elt, MT_bool in
      e |> PSet.fold
            ~init:(ZMap.create ~key_sort:(cv_mtcc kt) ~value_sort:(cv_mt vt))
            ~f:(fun acc_set key -> ZMap.update ~key:(cv_mvcc key) ~value:(ZOption.create_some ~content:(cv_mv (MV_lit_bool true))) ~map:acc_set)
      end
    | MV_empty_set elt -> begin
        let kt, vt = elt, MT_bool in
        ZMap.create ~key_sort:(cv_mtcc kt) ~value_sort:(cv_mt vt)
      end
    | MV_update_xbss (e1, e2, e3) -> begin
        let elem = begin
          ZExpr.create_ite
          ~cond:(cv_mvcc e2)
          ~t:(ZOption.create_some ~content:(ZBool.true_ ()))
          ~f:(ZOption.create_none ~content_sort:(ZBool.sort ()))
        end in
        ZMap.update ~key:(cv_mvcc e1) ~value:(elem) ~map:(cv_mvcc e3)
      end
  
    (*************************************************************************)
    (* Operation                                                             *)
    (*************************************************************************)
    | MV_create_contract _ -> err eee
    | MV_transfer_tokens _ -> err eee
    | MV_set_delegate _ -> err eee
  
    (*************************************************************************)
    (* Contract                                                              *)
    (*************************************************************************)
    | MV_lit_contract _ -> err eee
    | MV_self _ -> err eee
    | MV_implicit_account _ -> err eee
  
    (*************************************************************************)
    (* Pair                                                                  *)
    (*************************************************************************)
    | MV_pair (e1, e2) -> ZPair.create ~fst:(cv_mvcc e1) ~snd:(cv_mvcc e2)
  
    (*************************************************************************)
    (* Or                                                                    *)
    (*************************************************************************)
    | MV_left (t, e) -> ZOr.create_left ~left_content:(cv_mvcc e) ~right_sort:(t |> get_innertyp2 |> Stdlib.snd |> cv_mtcc)
    | MV_right (t, e) -> ZOr.create_right ~left_sort:(t |> get_innertyp2 |> Stdlib.fst |> cv_mtcc) ~right_content:(cv_mvcc e)
  
    (*************************************************************************)
    (* Lambda                                                                *)
    (*************************************************************************)
    | MV_lit_lambda _ -> err eee
    | MV_lambda_unknown _ -> err eee
    | MV_lambda_closure _ -> err eee
  
    (*************************************************************************)
    (* Map                                                                   *)
    (*************************************************************************)
    | MV_lit_map (kt, vt, e) -> begin
        e |> Core.Map.Poly.fold
          ~init:(ZMap.create ~key_sort:(cv_mtcc kt) ~value_sort:(cv_mtcc vt))
          ~f:(fun ~key ~data acc_zm -> ZMap.update ~key:(cv_mvcc key) ~value:(ZOption.create_some ~content:(cv_mvcc data)) ~map:acc_zm)
      end
    | MV_empty_map (kt, vt) -> ZMap.create ~key_sort:(cv_mtcc kt) ~value_sort:(cv_mtcc vt)
    | MV_update_xomm (e1, e2, e3) -> ZMap.update ~key:(cv_mvcc e1) ~value:(cv_mvcc e2) ~map:(cv_mvcc e3)
  
    (*************************************************************************)
    (* Big Map                                                               *)
    (*************************************************************************)
    | MV_lit_big_map (kt, vt, e) -> begin
        e |> Core.Map.Poly.fold
          ~init:(ZMap.create ~key_sort:(cv_mtcc kt) ~value_sort:(cv_mtcc vt))
          ~f:(fun ~key ~data acc_zm -> ZMap.update ~key:(cv_mvcc key) ~value:(ZOption.create_some ~content:(cv_mvcc data)) ~map:acc_zm)
      end
    | MV_empty_big_map (kt, vt) -> ZMap.create ~key_sort:(cv_mtcc kt) ~value_sort:(cv_mtcc vt)
    | MV_update_xobmbm (e1, e2, e3) -> ZMap.update ~key:(cv_mvcc e1) ~value:(cv_mvcc e2) ~map:(cv_mvcc e3)
  
    (*************************************************************************)
    (* Chain Id                                                              *)
    (*************************************************************************)
    | MV_lit_chain_id _ -> err eee
    ) (* function cv_mv end *)

  and cv_mvcc : mich_v cc -> ZExpr.t = (fun x -> cv_mv x.cc_v)
  let rec cv_mf : mich_f -> ZFormula.t =
    (fun vf -> try
      (match vf with
      (* Logical Formula *)
      | MF_true -> ZFormula.true_ ()
      | MF_false -> ZFormula.false_ ()
      | MF_not f -> ZFormula.create_not (cv_mf f)
      | MF_and fl -> ZFormula.create_and (Core.List.map ~f:cv_mf fl)
      | MF_or fl -> ZFormula.create_or (Core.List.map ~f:cv_mf fl)
      | MF_eq (e1, e2) -> ZFormula.create_eq (cv_mvcc e1) (cv_mvcc e2)
      | MF_imply (f1, f2) -> ZFormula.create_imply (cv_mf f1) (cv_mf f2)
      (* MicSE-Cfg Pattern Matching *)
      | MF_is_true e -> ZBool.create_eq (cv_mvcc e) (ZBool.true_ ())
      | MF_is_none e -> ZOption.is_none (cv_mvcc e)
      | MF_is_left e -> ZOr.is_left (cv_mvcc e)
      | MF_is_cons e -> begin
          match (typ_of_val e).cc_v with
          | MT_list _ -> ZList.is_cons (cv_mvcc e)
          | _ -> SMT_Encode_Error_f (vf, "Wrong IS_CONS checking") |> raise
        end
      (* Custom Formula for verifiying *)
      | MF_add_mmm_no_overflow (e1, e2) -> begin
          let (soe1, soe2) = (cv_mvcc e1, cv_mvcc e2) in
          ZMutez.create_ge (ZMutez.create_add soe1 soe2) soe1
        end
      | MF_sub_mmm_no_underflow (e1, e2) -> begin
          ZMutez.create_ge (cv_mvcc e1) (cv_mvcc e2)
        end
      | MF_mul_mnm_no_overflow (e1, e2) -> begin
          let soe1, soe2 = (cv_mvcc e1), (cv_mvcc e2 |> ZInt.to_zmutez) in
          let e1_mul_e2 = ZMutez.create_mul soe1 soe2 in  (* e1 * e2 *)
          let e1_mul_e2_div_e1 = ZMutez.create_div e1_mul_e2 soe2 in (* (e1 * e2) / e1 *)
          let e1_is_zero = ZMutez.create_eq soe1 (ZMutez.zero_ ()) in (* e1 = 0 *)
          let e2_is_zero = ZNat.create_eq (cv_mvcc e2) (ZNat.zero_ ()) in  (* e2 = 0 *)
          let e1_is_not_zero = ZFormula.create_and [ (* e1 != 0 /\ ((e1 * e2) / e1) = e2 *)
            (ZFormula.create_not e1_is_zero);
            (ZMutez.create_eq e1_mul_e2_div_e1 soe1)
          ] in
          ZFormula.create_or [e1_is_zero; e2_is_zero; e1_is_not_zero] (* (e1 = 0) \/ (e1 != 0 /\ ((e1 * e2) / e1) = e2) *)
        end
      | MF_mul_nmm_no_overflow (e1, e2) -> begin
          let soe1, soe2 = (cv_mvcc e1 |> ZInt.to_zmutez), (cv_mvcc e2) in
          let e1_mul_e2 = ZMutez.create_mul soe1 soe2 in  (* e1 * e2 *)
          let e1_mul_e2_div_e1 = ZMutez.create_div e1_mul_e2 soe2 in (* (e1 * e2) / e1 *)
          let e1_is_zero = ZNat.create_eq (cv_mvcc e1) (ZNat.zero_ ()) in  (* e1 = 0 *)
          let e2_is_zero = ZMutez.create_eq (cv_mvcc e2) (ZMutez.zero_ ()) in (* e2 = 0 *)
          let e1_is_not_zero = ZFormula.create_and [ (* e1 != 0 /\ ((e1 * e2) / e1) = e2 *)
            (ZFormula.create_not e1_is_zero);
            (ZMutez.create_eq e1_mul_e2_div_e1 soe1)
          ] in
          ZFormula.create_or [e1_is_zero; e2_is_zero; e1_is_not_zero] (* (e1 = 0) \/ (e1 != 0 /\ ((e1 * e2) / e1) = e2) *)
        end
      | MF_shiftL_nnn_rhs_in_256 (_, e2) -> ZMutez.create_le (cv_mvcc e2) (ZMutez.of_int 256)
      | MF_shiftR_nnn_rhs_in_256 (_, e2) -> ZMutez.create_le (cv_mvcc e2) (ZMutez.of_int 256)
      (* Custom Domain Formula for Invariant Generation *)
      | MF_sigma_equal _ -> ZBool.true_ () (* TODO *)
      )
    with
    | ZError s -> SMT_Encode_Error_f (vf, s) |> raise
    ) (* function cv_mf end *)
end (* module T2S end *)
