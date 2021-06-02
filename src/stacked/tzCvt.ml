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
    | I_noop          -> MI_drop (Z.of_int 0)
    | I_micse_check i -> MI_micse_check (cv_instt i)
    (* Error case - Macros & Undefined *)
    (* | _ as inst -> Error ("cv_inst : " ^ (PreLib.Mich.string_of_instt_ol (PreLib.Mich.gen_t inst))) |> raise *)
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
    | T_contract t,       D_string s when t.PreLib.Mich.d = PreLib.Mich.T_unit -> MV_implicit_account (MV_lit_key_hash s |> gen_dummy_cc)
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
(* Tz to Abbreviation                                                        *)
(*****************************************************************************)
(*****************************************************************************)

module T2A = struct
  
  open Tz
  open Jc

  let rec cv_mt : mich_t -> string list =
    (let abt = cv_mtcc in (* syntax sugar *)
    fun ttt -> match ttt with
    | MT_address          -> abr_t_address::[]
    | MT_bool             -> abr_t_bool::[]
    | MT_big_map (t1, t2) -> abr_t_big_map::((abt t1)@(abt t2))
    | MT_bytes            -> abr_t_bytes::[]
    | MT_chain_id         -> abr_t_chain_id::[]
    | MT_contract t1      -> abr_t_contract::(abt t1)
    | MT_int              -> abr_t_int::[]
    | MT_key_hash         -> abr_t_key_hash::[]
    | MT_key              -> abr_t_key::[]
    | MT_lambda (t1, t2)  -> abr_t_lambda::((abt t1)@(abt t2))
    | MT_list t1          -> abr_t_list::(abt t1)
    | MT_map (t1, t2)     -> abr_t_map::((abt t1)@(abt t2))
    | MT_mutez            -> abr_t_mutez::[]
    | MT_nat              -> abr_t_nat::[]
    | MT_operation        -> abr_t_operation::[]
    | MT_or (t1, t2)      -> abr_t_or::((abt t1)@(abt t2))
    | MT_option t1        -> abr_t_option::(abt t1)
    | MT_pair (t1, t2)    -> abr_t_pair::((abt t1)@(abt t2))
    | MT_signature        -> abr_t_signature::[]
    | MT_set t1           -> abr_t_set::(abt t1)
    | MT_string           -> abr_t_string::[]
    | MT_timestamp        -> abr_t_timestamp::[]
    | MT_unit             -> abr_t_unit::[]
    )
  
  and cv_mtcc : mich_t cc -> string list = (fun x -> cv_mt x.cc_v)

  let rec cv_mv : mich_v -> string list =
    (let abv = cv_mvcc in (* syntax sugar *)
    let st = (fun t -> t |> cv_mtcc |> Core.String.concat) in (* syntax sugar *)
    let app = (fun l1 s -> l1@(s::[])) in (* syntax sugar *)
    fun eee -> match eee with
    (*************************************************************************)
    (* Symbol & Polymorphic                                                  *)
    (*************************************************************************)
    | MV_symbol (t1, s2)   -> app [s2] (abr_v_symbol ^ (st t1) ^ abr_symbol)
    | MV_car v1           -> app (abv v1) abr_v_car
    | MV_cdr v1           -> app (abv v1) abr_v_cdr
    | MV_unlift_option v1 -> app (abv v1) abr_v_unlift_option
    | MV_unlift_left v1   -> app (abv v1) abr_v_unlift_left
    | MV_unlift_right v1  -> app (abv v1) abr_v_unlift_right
    | MV_hd_l v1          -> app (abv v1) abr_v_hd
    (*************************************************************************)
    (* Integer                                                               *)
    (*************************************************************************)
    | MV_lit_int d1       -> app [(d1 |> Z.to_string)] (abr_v_lit ^ abr_t_int)
    | MV_neg_ni v1        -> app (abv v1) (abr_v_neg ^ abr_t_nat ^ abr_t_int)
    | MV_neg_ii v1        -> app (abv v1) (abr_v_neg ^ abr_t_int ^ abr_t_int)
    | MV_not_ni v1        -> app (abv v1) (abr_v_not ^ abr_t_nat ^ abr_t_int)
    | MV_not_ii v1        -> app (abv v1) (abr_v_not ^ abr_t_int ^ abr_t_int)
    | MV_add_nii (v1, v2) -> app ((abv v1)@(abv v2)) (abr_v_add ^ abr_t_nat ^ abr_t_int ^ abr_t_int)
    | MV_add_ini (v1, v2) -> app ((abv v1)@(abv v2)) (abr_v_add ^ abr_t_int ^ abr_t_nat ^ abr_t_int)
    | MV_add_iii (v1, v2) -> app ((abv v1)@(abv v2)) (abr_v_add ^ abr_t_int ^ abr_t_int ^ abr_t_int)
    | MV_sub_nni (v1, v2) -> app ((abv v1)@(abv v2)) (abr_v_sub ^ abr_t_nat ^ abr_t_nat ^ abr_t_int)
    | MV_sub_nii (v1, v2) -> app ((abv v1)@(abv v2)) (abr_v_sub ^ abr_t_nat ^ abr_t_int ^ abr_t_int)
    | MV_sub_ini (v1, v2) -> app ((abv v1)@(abv v2)) (abr_v_sub ^ abr_t_int ^ abr_t_nat ^ abr_t_int)
    | MV_sub_iii (v1, v2) -> app ((abv v1)@(abv v2)) (abr_v_sub ^ abr_t_int ^ abr_t_int ^ abr_t_int)
    | MV_sub_tti (v1, v2) -> app ((abv v1)@(abv v2)) (abr_v_sub ^ abr_t_timestamp ^ abr_t_timestamp ^ abr_t_int)
    | MV_mul_nii (v1, v2) -> app ((abv v1)@(abv v2)) (abr_v_mul ^ abr_t_nat ^ abr_t_int ^ abr_t_int)
    | MV_mul_ini (v1, v2) -> app ((abv v1)@(abv v2)) (abr_v_mul ^ abr_t_int ^ abr_t_nat ^ abr_t_int)
    | MV_mul_iii (v1, v2) -> app ((abv v1)@(abv v2)) (abr_v_mul ^ abr_t_int ^ abr_t_int ^ abr_t_int)
    | MV_compare (v1, v2) -> app ((abv v1)@(abv v2)) abr_v_compare
    | MV_int_of_nat v1    -> app (abv v1) abr_v_int_of_nat
    (*************************************************************************)
    (* Natural Number                                                        *)
    (*************************************************************************)
    | MV_lit_nat d1           -> app [(d1 |> Z.to_string)] (abr_v_lit ^ abr_t_nat)
    | MV_abs_in v1            -> app (abv v1) (abr_v_abs ^ abr_t_int ^ abr_t_nat)
    | MV_add_nnn (v1, v2)     -> app ((abv v1)@(abv v2)) (abr_v_add ^ abr_t_nat ^ abr_t_nat ^ abr_t_nat)
    | MV_mul_nnn (v1, v2)     -> app ((abv v1)@(abv v2)) (abr_v_mul ^ abr_t_nat ^ abr_t_nat ^ abr_t_nat)
    | MV_shiftL_nnn (v1, v2)  -> app ((abv v1)@(abv v2)) (abr_v_shiftL ^ abr_t_nat ^ abr_t_nat ^ abr_t_nat)
    | MV_shiftR_nnn (v1, v2)  -> app ((abv v1)@(abv v2)) (abr_v_shiftR ^ abr_t_nat ^ abr_t_nat ^ abr_t_nat)
    | MV_and_nnn (v1, v2)     -> app ((abv v1)@(abv v2)) (abr_v_and ^ abr_t_nat ^ abr_t_nat ^ abr_t_nat)
    | MV_and_inn (v1, v2)     -> app ((abv v1)@(abv v2)) (abr_v_and ^ abr_t_int ^ abr_t_nat ^ abr_t_nat)
    | MV_or_nnn (v1, v2)      -> app ((abv v1)@(abv v2)) (abr_v_or ^ abr_t_nat ^ abr_t_nat ^ abr_t_nat)
    | MV_xor_nnn (v1, v2)     -> app ((abv v1)@(abv v2)) (abr_v_xor ^ abr_t_nat ^ abr_t_nat ^ abr_t_nat)
    | MV_size_s v1            -> app (abv v1) (abr_v_size ^ abr_t_set)
    | MV_size_m v1            -> app (abv v1) (abr_v_size ^ abr_t_map)
    | MV_size_l v1            -> app (abv v1) (abr_v_size ^ abr_t_list)
    | MV_size_str v1          -> app (abv v1) (abr_v_size ^ abr_t_string)
    | MV_size_b v1            -> app (abv v1) (abr_v_size ^ abr_t_bytes)
    (*************************************************************************)
    (* String                                                                *)
    (*************************************************************************)
    | MV_lit_string s1        -> app [s1] (abr_v_lit ^ abr_t_string)
    | MV_concat_sss (v1, v2)  -> app ((abv v1)@(abv v2)) (abr_v_concat ^ abr_t_string ^ abr_t_string ^ abr_t_string)
    | MV_concat_list_s v1     -> app (abv v1) (abr_v_concat ^ abr_t_list ^ abr_t_string)
    (*************************************************************************)
    (* Bytes                                                                 *)
    (*************************************************************************)
    | MV_lit_bytes s1         -> app [s1] (abr_v_lit ^ abr_t_bytes)
    | MV_concat_bbb (v1, v2)  -> app ((abv v1)@(abv v2)) (abr_v_concat ^ abr_t_bytes ^ abr_t_bytes ^ abr_t_bytes)
    | MV_concat_list_b v1     -> app (abv v1) (abr_v_concat ^ abr_t_list ^ abr_t_bytes)
    | MV_pack v1              -> app (abv v1) abr_v_pack
    | MV_blake2b v1           -> app (abv v1) abr_v_blake2b
    | MV_sha256 v1            -> app (abv v1) abr_v_sha256
    | MV_sha512 v1            -> app (abv v1) abr_v_sha512
    (*************************************************************************)
    (* Mutez                                                                 *)
    (*************************************************************************)
    | MV_lit_mutez d1     -> app [(d1 |> Z.to_string)] (abr_v_lit ^ abr_t_mutez)
    | MV_add_mmm (v1, v2) -> app ((abv v1)@(abv v2)) (abr_v_add ^ abr_t_mutez ^ abr_t_mutez ^ abr_t_mutez)
    | MV_sub_mmm (v1, v2) -> app ((abv v1)@(abv v2)) (abr_v_sub ^ abr_t_mutez ^ abr_t_mutez ^ abr_t_mutez)
    | MV_mul_mnm (v1, v2) -> app ((abv v1)@(abv v2)) (abr_v_mul ^ abr_t_mutez ^ abr_t_nat ^ abr_t_mutez)
    | MV_mul_nmm (v1, v2) -> app ((abv v1)@(abv v2)) (abr_v_mul ^ abr_t_nat ^ abr_t_mutez ^ abr_t_mutez)
    (*************************************************************************)
    (* Bool                                                                  *)
    (*************************************************************************)
    | MV_lit_bool b1                  -> app [(b1 |> string_of_bool)] (abr_v_lit ^ abr_t_bool)
    | MV_not_bb v1                    -> app (abv v1) (abr_v_not ^ abr_t_bool ^ abr_t_bool)
    | MV_and_bbb (v1, v2)             -> app ((abv v1)@(abv v2)) (abr_v_and ^ abr_t_bool ^ abr_t_bool ^ abr_t_bool)
    | MV_or_bbb (v1, v2)              -> app ((abv v1)@(abv v2)) (abr_v_or ^ abr_t_bool ^ abr_t_bool ^ abr_t_bool)
    | MV_xor_bbb (v1, v2)             -> app ((abv v1)@(abv v2)) (abr_v_xor ^ abr_t_bool ^ abr_t_bool ^ abr_t_bool)
    | MV_eq_ib (v1, v2)               -> app ((abv v1)@(abv v2)) (abr_v_eq ^ abr_t_int ^ abr_t_bool)
    | MV_neq_ib (v1, v2)              -> app ((abv v1)@(abv v2)) (abr_v_neq ^ abr_t_int ^ abr_t_bool)
    | MV_lt_ib (v1, v2)               -> app ((abv v1)@(abv v2)) (abr_v_lt ^ abr_t_int ^ abr_t_bool)
    | MV_gt_ib (v1, v2)               -> app ((abv v1)@(abv v2)) (abr_v_gt ^ abr_t_int ^ abr_t_bool)
    | MV_leq_ib (v1, v2)              -> app ((abv v1)@(abv v2)) (abr_v_leq ^ abr_t_int ^ abr_t_bool)
    | MV_geq_ib (v1, v2)              -> app ((abv v1)@(abv v2)) (abr_v_geq ^ abr_t_int ^ abr_t_bool)
    | MV_mem_xsb (v1, v2)             -> app ((abv v1)@(abv v2)) (abr_v_mem ^ abr_t_set ^ abr_t_bool)
    | MV_mem_xmb (v1, v2)             -> app ((abv v1)@(abv v2)) (abr_v_mem ^ abr_t_map ^ abr_t_bool)
    | MV_mem_xbmb (v1, v2)            -> app ((abv v1)@(abv v2)) (abr_v_mem ^ abr_t_big_map ^ abr_t_bool)
    | MV_check_signature (v1, v2, v3) -> app ((abv v1)@(abv v2)@(abv v3)) abr_v_check_signature
    (*************************************************************************)
    (* Key Hash                                                              *)
    (*************************************************************************)
    | MV_lit_key_hash s1  -> app [s1] (abr_v_lit ^ abr_t_key_hash)
    | MV_hash_key v1      -> app (abv v1) abr_v_hash_key
    (*************************************************************************)
    (* Timestamp                                                             *)
    (*************************************************************************)
    | MV_lit_timestamp_str s1 -> app [s1] (abr_v_lit ^ abr_t_string ^ abr_t_timestamp)
    | MV_lit_timestamp_sec d1 -> app [(d1 |> Z.to_string)] (abr_v_lit ^ abr_t_int ^ abr_t_timestamp)
    | MV_add_tit (v1, v2)     -> app ((abv v1)@(abv v2)) (abr_v_add ^ abr_t_timestamp ^ abr_t_int ^ abr_t_timestamp)
    | MV_add_itt (v1, v2)     -> app ((abv v1)@(abv v2)) (abr_v_add ^ abr_t_int ^ abr_t_timestamp ^ abr_t_timestamp)
    | MV_sub_tit (v1, v2)     -> app ((abv v1)@(abv v2)) (abr_v_sub ^ abr_t_timestamp ^ abr_t_int ^ abr_t_timestamp)
    (*************************************************************************)
    (* Address                                                               *)
    (*************************************************************************)
    | MV_lit_address v1         -> app (abv v1) (abr_v_lit ^ abr_t_address)
    | MV_address_of_contract v1 -> app (abv v1) abr_v_address_of_contract
    (*************************************************************************)
    (* Key                                                                   *)
    (*************************************************************************)
    | MV_lit_key s1 -> app [s1] (abr_v_lit ^ abr_t_key)
    (*************************************************************************)
    (* Unit                                                                  *)
    (*************************************************************************)
    | MV_unit -> app [] abr_v_unit
    (*************************************************************************)
    (* Signature                                                             *)
    (*************************************************************************)
    | MV_lit_signature_str s1           -> app [s1] (abr_v_lit ^ abr_t_signature)
    | MV_lit_signature_signed (v1, v2)  -> app ((abv v1)@(abv v2)) (abr_v_lit ^ abr_t_signature)
    (*************************************************************************)
    (* Option                                                                *)
    (*************************************************************************)
    | MV_some v1                      -> app (abv v1) abr_v_some
    | MV_none t1                      -> app [] (abr_v_none ^ (st t1))
    | MV_ediv_nnnn (v1, v2)           -> app ((abv v1)@(abv v2)) (abr_v_ediv ^ abr_t_nat ^ abr_t_nat ^ abr_t_nat ^ abr_t_nat)
    | MV_ediv_niin (v1, v2)           -> app ((abv v1)@(abv v2)) (abr_v_ediv ^ abr_t_nat ^ abr_t_int ^ abr_t_int ^ abr_t_nat)
    | MV_ediv_inin (v1, v2)           -> app ((abv v1)@(abv v2)) (abr_v_ediv ^ abr_t_int ^ abr_t_nat ^ abr_t_int ^ abr_t_nat)
    | MV_ediv_iiin (v1, v2)           -> app ((abv v1)@(abv v2)) (abr_v_ediv ^ abr_t_int ^ abr_t_int ^ abr_t_int ^ abr_t_nat)
    | MV_ediv_mnmm (v1, v2)           -> app ((abv v1)@(abv v2)) (abr_v_ediv ^ abr_t_mutez ^ abr_t_nat ^ abr_t_mutez ^ abr_t_mutez)
    | MV_ediv_mmnm (v1, v2)           -> app ((abv v1)@(abv v2)) (abr_v_ediv ^ abr_t_mutez ^ abr_t_mutez ^ abr_t_nat ^ abr_t_mutez)
    | MV_get_xmoy  (v1, v2)           -> app ((abv v1)@(abv v2)) (abr_v_get ^ abr_t_map)
    | MV_get_xbmo  (v1, v2)           -> app ((abv v1)@(abv v2)) (abr_v_get ^ abr_t_big_map)
    | MV_slice_nnso (v1, v2, v3)      -> app ((abv v1)@(abv v2)@(abv v3)) (abr_v_slice ^ abr_t_nat ^ abr_t_nat ^ abr_t_string)
    | MV_slice_nnbo (v1, v2, v3)      -> app ((abv v1)@(abv v2)@(abv v3)) (abr_v_slice ^ abr_t_nat ^ abr_t_nat ^ abr_t_bytes)
    | MV_unpack (t1, v2)              -> app (abv v2) (abr_v_unpack ^ abr_t_bytes ^ (st t1))
    | MV_contract_of_address (t1, v2) -> app (abv v2) (abr_v_contract_of_address ^ (st t1) ^ abr_t_address)
    | MV_isnat v1                     -> app (abv v1) (abr_v_isnat)
    (*************************************************************************)
    (* List                                                                  *)
    (*************************************************************************)
    | MV_lit_list (t1, vl2) -> app (vl2 |> Core.List.map ~f:abv |> Core.List.join) (abr_v_lit ^ (st t1) ^ abr_t_list)
    | MV_nil t1             -> app [] (abr_v_nil ^ (st t1))
    | MV_cons (v1, v2)      -> app ((abv v1)@(abv v2)) abr_v_cons
    | MV_tl_l v1            -> app (abv v1) abr_v_tl
    (*************************************************************************)
    (* Set                                                                   *)
    (*************************************************************************)
    | MV_lit_set (t1, vs2)        -> app (vs2 |> Core.Set.to_list |> Core.List.map ~f:abv |> Core.List.join) (abr_v_lit ^ (st t1) ^ abr_t_set)
    | MV_empty_set t1             -> app [] (abr_v_empty ^ (st t1) ^ abr_t_set)
    | MV_update_xbss (v1, v2, v3) -> app ((abv v1)@(abv v2)@(abv v3)) (abr_v_update ^ abr_t_bool ^ abr_t_set)
    (*************************************************************************)
    (* Operation                                                             *)
    (*************************************************************************)
    | MV_create_contract (t1, t2, v3, v4, v5, v6, v7) -> app ((abv v3)@(abv v4)@(abv v5)@(abv v6)@(abv v7)) (abr_v_create_contract ^ (st t1) ^ (st t2))
    | MV_transfer_tokens (v1, v2, v3)                 -> app ((abv v1)@(abv v2)@(abv v3)) abr_v_transfer_tokens
    | MV_set_delegate v1                              -> app (abv v1) abr_v_set_delegate
    (*************************************************************************)
    (* Contract                                                              *)
    (*************************************************************************)
    | MV_lit_contract (t1, v2)  -> app (abv v2) (abr_v_lit ^ (st t1) ^ abr_t_contract)
    | MV_self t1                -> app [] (abr_v_self ^ (st t1))
    | MV_implicit_account v1    -> app (abv v1) abr_v_implicit_account
    (*************************************************************************)
    (* Pair                                                                  *)
    (*************************************************************************)
    | MV_pair (v1, v2) -> app ((abv v1)@(abv v2)) abr_v_pair
    (*************************************************************************)
    (* Or                                                                    *)
    (*************************************************************************)
    | MV_left (t1, v2)  -> app (abv v2) (abr_v_left ^ (st t1))
    | MV_right (t1, v2) -> app (abv v2) (abr_v_right ^ (st t1))
    (*************************************************************************)
    (* Lambda                                                                *)
    (*************************************************************************)
    | MV_lit_lambda (t1, t2, _) -> app [] (abr_v_lit ^ (st t1) ^ (st t2) ^ abr_t_lambda)
    | MV_lambda_unknown (t1, t2) -> app [] (abr_v_lambda_unknown ^ (st t1) ^ (st t2))
    | MV_lambda_closure (v1, v2) -> app ((abv v1)@(abv v2)) (abr_v_lambda_closure)
    (*************************************************************************)
    (* Map                                                                   *)
    (*************************************************************************)
    | MV_lit_map (t1, t2, vm3) -> app (vm3 |> Core.Map.to_alist |> Core.List.map ~f:(fun (_, v) -> abv v) |> Core.List.join) (abr_v_lit ^ (st t1) ^ (st t2) ^ abr_t_map)
    | MV_empty_map (t1, t2) -> app [] (abr_v_empty ^ (st t1) ^ (st t2) ^ abr_t_map)
    | MV_update_xomm  (v1, v2, v3) -> app ((abv v1)@(abv v2)@(abv v3)) (abr_v_update ^ abr_t_map)
    (*************************************************************************)
    (* Big Map                                                               *)
    (*************************************************************************)
    | MV_lit_big_map (t1, t2, vm3) -> app (vm3 |> Core.Map.to_alist |> Core.List.map ~f:(fun (_, v) -> abv v) |> Core.List.join) (abr_v_lit ^ (st t1) ^ (st t2) ^ abr_t_big_map)
    | MV_empty_big_map (t1, t2) -> app [] (abr_v_empty ^ (st t1) ^ (st t2) ^ abr_t_big_map)
    | MV_update_xobmbm  (v1, v2, v3) -> app ((abv v1)@(abv v2)@(abv v3)) (abr_v_update ^ abr_t_big_map)
    (*************************************************************************)
    (* Chain Id                                                              *)
    (*************************************************************************)
    | MV_lit_chain_id s1 -> app [s1] (abr_v_lit ^ abr_t_chain_id)
    (*************************************************************************)
    (* Custom Domain Value for Invariant Synthesis                           *)
    (*************************************************************************)
    | MV_sigma_lm (v1, v2) -> app ((abv v1)@(abv v2)) (abr_v_sigma ^ abr_t_list ^ abr_t_mutez)
    )
  
  and cv_mvcc : mich_v cc -> string list = (fun x -> cv_mv x.cc_v)


end


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
  exception SMT_Encode_Error_f of (mich_f * string * int)
  exception SMT_Encode_Error_e of (mich_v cc * string * int)

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
    | MT_bool, MT_bool -> ZBool.create_cmp (cv_mvcc e1) (cv_mvcc e2)
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
    | MV_add_mmm (e1, e2) -> ZMutez.create_add (cv_mvcc e1) (cv_mvcc e2)
    | MV_sub_mmm (e1, e2) -> ZMutez.create_sub (cv_mvcc e1) (cv_mvcc e2)
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

    (*************************************************************************)
    (* Custom Domain Value for Invariant Synthesis                           *)
    (*************************************************************************)
    | MV_sigma_lm (v1, v2) -> begin
      let map_elem_acc : mich_v cc -> mich_v cc = ( (* syntax sugar *)
        fun vvv -> 
          map_v_v2v_outer v2 ~v2v:(fun x -> (
            match x.cc_v with
            | MV_symbol (_, _) -> Some vvv
            | _ -> None))) in
      let v1' : mich_v cc = v1 |> Tz.optimize_v in
      if (v2 |> Tz.typ_of_val).cc_v = MT_mutez then (
        match v1'.cc_v with
        | MV_lit_list (_, vl12) -> (
          Core.List.fold
            vl12
            ~init:(ZMutez.zero_ ())
            ~f:(fun acc vvv -> (
              let vvv' : mich_v cc = map_elem_acc vvv in
              ZMutez.create_add acc (cv_mvcc vvv'))))
        | MV_nil _ -> ZMutez.zero_ ()
        | MV_cons (v11, vl12) -> (
          let v11' : mich_v cc = map_elem_acc v11 in
          let vl12' : mich_v cc = MV_sigma_lm (vl12, v2) |> gen_dummy_cc in
          ZMutez.create_add (cv_mvcc vl12') (cv_mvcc v11'))
        | _ -> (
          let v1_abbrs : string list = T2A.cv_mvcc v1' in
          let v2_abbrs : string list = T2A.cv_mvcc (v2 |> Tz.optimize_v) in
          ZExpr.create_var
            (ZMutez.sort ())
            ~name:(
              { Jc.Fsvn.typ=`Remain;
                Jc.Fsvn.c_vn=(Core.List.hd v1_abbrs |> (function Some ss -> ss | None -> Error "cv_mv : MV_sigma_lm : _ : c_vn" |> Stdlib.raise));
                Jc.Fsvn.c_acc_l=(Core.List.tl v1_abbrs |> (function Some ll -> ll | None -> Error "cv_mv : MV_sigma_lm : _ : c_acc_l" |> Stdlib.raise));
                Jc.Fsvn.e_acc_l=(Core.List.tl v2_abbrs |> (function Some ll -> ll | None -> Error "cv_mv : MV_sigma_lm : _ : e_acc_l" |> Stdlib.raise)); }
              |> Jc.Fsvn.to_string
            )))
      else err eee
      end
    ) (* function cv_mv end *)

  and cv_mvcc : mich_v cc -> ZExpr.t = (fun x -> try cv_mv x.cc_v with | ZError s -> SMT_Encode_Error_e (x, s, Stdlib.__LINE__) |> raise)
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
          | _ -> SMT_Encode_Error_f (vf, "Wrong IS_CONS checking", Stdlib.__LINE__) |> raise
        end
      (* Custom Formula for verifiying *)
      | MF_add_mmm_no_overflow (e1, e2) -> begin
          ZMutez.check_add_no_overflow (cv_mvcc e1) (cv_mvcc e2)
          (* let (soe1, soe2) = (cv_mvcc e1, cv_mvcc e2) in
          ZMutez.create_ge (ZMutez.create_add soe1 soe2) soe1 *)
        end
      | MF_sub_mmm_no_underflow (e1, e2) -> begin
          ZMutez.check_sub_no_underflow (cv_mvcc e1) (cv_mvcc e2)
          (* ZMutez.create_ge (cv_mvcc e1) (cv_mvcc e2) *)
        end
      | MF_mul_mnm_no_overflow (e1, e2) -> begin
          ZMutez.check_mul_no_overflow (cv_mvcc e1) (cv_mvcc e2)
          (* let soe1, soe2 = (cv_mvcc e1), (cv_mvcc e2 |> ZInt.to_zmutez) in
          let e1_mul_e2 = ZMutez.create_mul soe1 soe2 in  (* e1 * e2 *)
          let e1_mul_e2_div_e1 = ZMutez.create_div e1_mul_e2 soe2 in (* (e1 * e2) / e1 *)
          let e1_is_zero = ZMutez.create_eq soe1 (ZMutez.zero_ ()) in (* e1 = 0 *)
          let e2_is_zero = ZNat.create_eq (cv_mvcc e2) (ZNat.zero_ ()) in  (* e2 = 0 *)
          let e1_is_not_zero = ZFormula.create_and [ (* e1 != 0 /\ ((e1 * e2) / e1) = e2 *)
            (ZFormula.create_not e1_is_zero);
            (ZMutez.create_eq e1_mul_e2_div_e1 soe1)
          ] in
          ZFormula.create_or [e1_is_zero; e2_is_zero; e1_is_not_zero] (* (e1 = 0) \/ (e1 != 0 /\ ((e1 * e2) / e1) = e2) *) *)
        end
      | MF_mul_nmm_no_overflow (e1, e2) -> begin
          ZMutez.check_mul_no_overflow (cv_mvcc e1) (cv_mvcc e2)
          (* let soe1, soe2 = (cv_mvcc e1 |> ZInt.to_zmutez), (cv_mvcc e2) in
          let e1_mul_e2 = ZMutez.create_mul soe1 soe2 in  (* e1 * e2 *)
          let e1_mul_e2_div_e1 = ZMutez.create_div e1_mul_e2 soe2 in (* (e1 * e2) / e1 *)
          let e1_is_zero = ZNat.create_eq (cv_mvcc e1) (ZNat.zero_ ()) in  (* e1 = 0 *)
          let e2_is_zero = ZMutez.create_eq (cv_mvcc e2) (ZMutez.zero_ ()) in (* e2 = 0 *)
          let e1_is_not_zero = ZFormula.create_and [ (* e1 != 0 /\ ((e1 * e2) / e1) = e2 *)
            (ZFormula.create_not e1_is_zero);
            (ZMutez.create_eq e1_mul_e2_div_e1 soe1)
          ] in
          ZFormula.create_or [e1_is_zero; e2_is_zero; e1_is_not_zero] (* (e1 = 0) \/ (e1 != 0 /\ ((e1 * e2) / e1) = e2) *) *)
        end
      | MF_shiftL_nnn_rhs_in_256 (_, e2) -> ZNat.create_le (cv_mvcc e2) (ZNat.of_int 256)
      | MF_shiftR_nnn_rhs_in_256 (_, e2) -> ZNat.create_le (cv_mvcc e2) (ZNat.of_int 256)
      )
    with
    | ZError s -> SMT_Encode_Error_f (vf, s, Stdlib.__LINE__) |> raise
    ) (* function cv_mf end *)
end (* module T2S end *)



(*****************************************************************************)
(*****************************************************************************)
(* Tz to Json                                                                *)
(*****************************************************************************)
(*****************************************************************************)

module T2J = struct
  type js = Yojson.Safe.t
  open Jc
  let cv_pos : Tz.ccp_pos -> js
  = fun {col; lin} -> `Tuple [`Int lin; `Int col] (* function cv_pos end *)
  let cv_loc : Tz.ccp_loc -> js
  = (function
    | CCLOC_Unknown -> `Variant (cc_l_unk, None)
    | CCLOC_Pos (p1, p2) -> `Variant (cc_l_pos, Some (`Tuple [cv_pos p1; cv_pos p2]))
  ) (* function cv_loc end *)
  let cv_annot : Tz.ccp_annot -> js
  = (function
    | CCA_typ s -> `Variant (cc_a_typ, Some (`String s))
    | CCA_var s -> `Variant (cc_a_var, Some (`String s))
    | CCA_fld s -> `Variant (cc_a_fld, Some (`String s))
  ) (* function cv_annot end *)
  let cv_cc : ('a -> js) -> 'a Tz.cc -> js
  = (fun f x ->
    `Assoc [cc_loc, cv_loc x.cc_loc; cc_anl, `List (List.map cv_annot x.cc_anl); cc_val, f x.cc_v;]
  ) (* function cv_cc end *)
  let rec cv_mt : Tz.mich_t -> js
  = let s t = Some (cv_mtcc t) in
    let s2 t1 t2 = Some (`Tuple [cv_mtcc t1; cv_mtcc t2;]) in
    (function
    | MT_key              -> `Variant (t_key, None)
    | MT_unit             -> `Variant (t_unit, None)
    | MT_signature        -> `Variant (t_signature, None)
    | MT_option t         -> `Variant (t_option, s t)
    | MT_list t           -> `Variant (t_list, s t)
    | MT_set t            -> `Variant (t_set, s t)
    | MT_operation        -> `Variant (t_operation, None)
    | MT_contract t       -> `Variant (t_contract, s t)
    | MT_pair (t1, t2)    -> `Variant (t_pair, s2 t1 t2)
    | MT_or (t1, t2)      -> `Variant (t_or, s2 t1 t2)
    | MT_lambda (t1, t2)  -> `Variant (t_lambda, s2 t1 t2)
    | MT_map (t1, t2)     -> `Variant (t_map, s2 t1 t2)
    | MT_big_map (t1, t2) -> `Variant (t_big_map, s2 t1 t2)
    | MT_chain_id         -> `Variant (t_chain_id, None)
    | MT_int              -> `Variant (t_int, None)
    | MT_nat              -> `Variant (t_nat, None)
    | MT_string           -> `Variant (t_string, None)
    | MT_bytes            -> `Variant (t_bytes, None)
    | MT_mutez            -> `Variant (t_mutez, None)
    | MT_bool             -> `Variant (t_bool, None)
    | MT_key_hash         -> `Variant (t_key_hash, None)
    | MT_timestamp        -> `Variant (t_timestamp, None)
    | MT_address          -> `Variant (t_address, None)
    ) (* function cv_mt end *)
  and cv_mv : Tz.mich_v -> js
  = let s e = Some (cv_mvcc e) in
    let s2 e1 e2 = Some (`Tuple [cv_mvcc e1; cv_mvcc e2;]) in
    let s3 e1 e2 e3 = Some (`Tuple [cv_mvcc e1; cv_mvcc e2; cv_mvcc e3;]) in
    (function
    (*************************************************************************)
    (* Symbol & Polymorphic                                                  *)
    (*************************************************************************)
    (* | MV_symbol (t,v)     -> `Variant (v_symbol,        Some (`Tuple [cv_mtcc t; `String v])) *)
    | MV_symbol (t,v)     -> `Variant (v_symbol,        Some (`Tuple [cv_mt t.cc_v; `String v]))
    | MV_car e            -> `Variant (v_car,           s e)
    | MV_cdr e            -> `Variant (v_cdr,           s e)
    | MV_unlift_option e  -> `Variant (v_unlift_option, s e)
    | MV_unlift_left e    -> `Variant (v_unlift_left,   s e)
    | MV_unlift_right e   -> `Variant (v_unlift_right,  s e)
    | MV_hd_l e           -> `Variant (v_hd_l,          s e)
  
    (*************************************************************************)
    (* Integer                                                               *)
    (*************************************************************************)
    | MV_lit_int zn       -> `Variant (v_lit_int,     Some (`Intlit (Z.to_string zn)))
    | MV_neg_ni e         -> `Variant (v_neg_ni,      s e)
    | MV_neg_ii e         -> `Variant (v_neg_ii,      s e)
    | MV_not_ni e         -> `Variant (v_not_ni,      s e)
    | MV_not_ii e         -> `Variant (v_not_ii,      s e)
    | MV_add_nii (e1, e2) -> `Variant (v_add_nii,     s2 e1 e2)
    | MV_add_ini (e1, e2) -> `Variant (v_add_ini,     s2 e1 e2)
    | MV_add_iii (e1, e2) -> `Variant (v_add_iii,     s2 e1 e2)
    | MV_sub_nni (e1, e2) -> `Variant (v_sub_nni,     s2 e1 e2)
    | MV_sub_nii (e1, e2) -> `Variant (v_sub_nii,     s2 e1 e2)
    | MV_sub_ini (e1, e2) -> `Variant (v_sub_ini,     s2 e1 e2)
    | MV_sub_iii (e1, e2) -> `Variant (v_sub_iii,     s2 e1 e2)
    | MV_sub_tti (e1, e2) -> `Variant (v_sub_tti,     s2 e1 e2)
    | MV_mul_nii (e1, e2) -> `Variant (v_mul_nii,     s2 e1 e2)
    | MV_mul_ini (e1, e2) -> `Variant (v_mul_ini,     s2 e1 e2)
    | MV_mul_iii (e1, e2) -> `Variant (v_mul_iii,     s2 e1 e2)
    | MV_compare (e1, e2) -> `Variant (v_compare,     s2 e1 e2)
    | MV_int_of_nat e     -> `Variant (v_int_of_nat,  s e)
  
    (*************************************************************************)
    (* Natural Number                                                        *)
    (*************************************************************************)
    | MV_lit_nat zn           -> `Variant (v_lit_nat,     Some (`Intlit (Z.to_string zn)))
    | MV_abs_in e             -> `Variant (v_abs_in,      s e)
    | MV_add_nnn (e1, e2)     -> `Variant (v_add_nnn,     s2 e1 e2)
    | MV_mul_nnn (e1, e2)     -> `Variant (v_mul_nnn,     s2 e1 e2)
    | MV_shiftL_nnn (e1, e2)  -> `Variant (v_shiftL_nnn,  s2 e1 e2)
    | MV_shiftR_nnn (e1, e2)  -> `Variant (v_shiftR_nnn,  s2 e1 e2)
    | MV_and_nnn (e1, e2)     -> `Variant (v_and_nnn,     s2 e1 e2)
    | MV_and_inn (e1, e2)     -> `Variant (v_and_inn,     s2 e1 e2)
    | MV_or_nnn (e1, e2)      -> `Variant (v_or_nnn,      s2 e1 e2)
    | MV_xor_nnn (e1, e2)     -> `Variant (v_xor_nnn,     s2 e1 e2)
    | MV_size_s e             -> `Variant (v_size_s,      s e)
    | MV_size_m e             -> `Variant (v_size_m,      s e)
    | MV_size_l e             -> `Variant (v_size_l,      s e)
    | MV_size_str e           -> `Variant (v_size_str,    s e)
    | MV_size_b e             -> `Variant (v_size_b,      s e)
  
    (*************************************************************************)
    (* String                                                                *)
    (*************************************************************************)
    | MV_lit_string s         -> `Variant (v_lit_string,    Some (`String s))
    | MV_concat_sss (e1, e2)  -> `Variant (v_concat_sss,    s2 e1 e2)
    | MV_concat_list_s e      -> `Variant (v_concat_list_s, s e)
  
    (*************************************************************************)
    (* Bytes                                                                 *)
    (*************************************************************************)
    | MV_lit_bytes s          -> `Variant (v_lit_bytes,     Some (`String s))
    | MV_concat_bbb (e1, e2)  -> `Variant (v_concat_bbb,    s2 e1 e2)
    | MV_concat_list_b e      -> `Variant (v_concat_list_b, s e)
    | MV_pack e               -> `Variant (v_pack,          s e)
    | MV_blake2b e            -> `Variant (v_blake2b,       s e)
    | MV_sha256  e            -> `Variant (v_sha256,        s e)
    | MV_sha512  e            -> `Variant (v_sha512,        s e)
  
    (*************************************************************************)
    (* Mutez                                                                 *)
    (*************************************************************************)
    | MV_lit_mutez zn     -> `Variant (v_lit_mutez, Some (`Intlit (Z.to_string zn)))
    | MV_add_mmm (e1, e2) -> `Variant (v_add_mmm,   s2 e1 e2)
    | MV_sub_mmm (e1, e2) -> `Variant (v_sub_mmm,   s2 e1 e2)
    | MV_mul_mnm (e1, e2) -> `Variant (v_mul_mnm,   s2 e1 e2)
    | MV_mul_nmm (e1, e2) -> `Variant (v_mul_nmm,   s2 e1 e2)
  
    (*************************************************************************)
    (* Bool                                                                  *)
    (*************************************************************************)
    | MV_lit_bool b                 -> `Variant (v_lit_bool,        Some (`Bool b))
    | MV_not_bb e                   -> `Variant (v_not_bb,          s e)
    | MV_and_bbb (e1, e2)           -> `Variant (v_and_bbb,         s2 e1 e2)
    | MV_or_bbb  (e1, e2)           -> `Variant (v_or_bbb,          s2 e1 e2)
    | MV_xor_bbb (e1, e2)           -> `Variant (v_xor_bbb,         s2 e1 e2)
    | MV_eq_ib   (e1, e2)           -> `Variant (v_eq_ib,           s2 e1 e2)
    | MV_neq_ib  (e1, e2)           -> `Variant (v_neq_ib,          s2 e1 e2)
    | MV_lt_ib   (e1, e2)           -> `Variant (v_lt_ib,           s2 e1 e2)
    | MV_gt_ib   (e1, e2)           -> `Variant (v_gt_ib,           s2 e1 e2)
    | MV_leq_ib  (e1, e2)           -> `Variant (v_leq_ib,          s2 e1 e2)
    | MV_geq_ib  (e1, e2)           -> `Variant (v_geq_ib,          s2 e1 e2)
    | MV_mem_xsb (e1, e2)           -> `Variant (v_mem_xsb,         s2 e1 e2)
    | MV_mem_xmb (e1, e2)           -> `Variant (v_mem_xmb,         s2 e1 e2)
    | MV_mem_xbmb (e1, e2)          -> `Variant (v_mem_xbmb,        s2 e1 e2)
    | MV_check_signature (e1,e2,e3) -> `Variant (v_check_signature, s3 e1 e2 e3)
  
    (*************************************************************************)
    (* Key Hash                                                              *)
    (*************************************************************************)
    | MV_lit_key_hash s -> `Variant (v_lit_key_hash, Some (`String s))
    | MV_hash_key e     -> `Variant (v_hash_key    , s e)
  
    (*************************************************************************)
    (* Timestamp                                                             *)
    (*************************************************************************)
    | MV_lit_timestamp_str s  -> `Variant (v_lit_timestamp_str, Some (`String s))
    | MV_lit_timestamp_sec zn -> `Variant (v_lit_timestamp_sec, Some (`Intlit (Z.to_string zn)))
    | MV_add_tit (e1, e2)     -> `Variant (v_add_tit,           s2 e1 e2)
    | MV_add_itt (e1, e2)     -> `Variant (v_add_itt,           s2 e1 e2)
    | MV_sub_tit (e1, e2)     -> `Variant (v_sub_tit,           s2 e1 e2)
  
    (*************************************************************************)
    (* Address                                                               *)
    (*************************************************************************)
    | MV_lit_address e          -> `Variant (v_lit_address,         s e)
    | MV_address_of_contract e  -> `Variant (v_address_of_contract, s e)
  
    (*************************************************************************)
    (* Key                                                                   *)
    (*************************************************************************)
    | MV_lit_key s -> `Variant (v_lit_key, Some (`String s))
  
    (*************************************************************************)
    (* Unit                                                                  *)
    (*************************************************************************)
    | MV_unit -> `Variant (v_unit, None)
  
    (*************************************************************************)
    (* Signature                                                             *)
    (*************************************************************************)
    | MV_lit_signature_str s            -> `Variant (v_lit_signature_str,     Some (`String s))
    | MV_lit_signature_signed (e1, e2)  -> `Variant (v_lit_signature_signed,  s2 e1 e2)
  
    (*************************************************************************)
    (* Option                                                                *)
    (*************************************************************************)
    | MV_some e                     -> `Variant (v_some,                s e)
    | MV_none t                     -> `Variant (v_none,                Some (cv_mtcc t))
    | MV_ediv_nnnn (e1, e2)         -> `Variant (v_ediv_nnnn,           s2 e1 e2)
    | MV_ediv_niin (e1, e2)         -> `Variant (v_ediv_niin,           s2 e1 e2)
    | MV_ediv_inin (e1, e2)         -> `Variant (v_ediv_inin,           s2 e1 e2)
    | MV_ediv_iiin (e1, e2)         -> `Variant (v_ediv_iiin,           s2 e1 e2)
    | MV_ediv_mnmm (e1, e2)         -> `Variant (v_ediv_mnmm,           s2 e1 e2)
    | MV_ediv_mmnm (e1, e2)         -> `Variant (v_ediv_mmnm,           s2 e1 e2)
    | MV_get_xmoy (e1, e2)          -> `Variant (v_get_xmoy,            s2 e1 e2)
    | MV_get_xbmo (e1, e2)          -> `Variant (v_get_xbmo,            s2 e1 e2)
    | MV_slice_nnso (e1, e2, e3)    -> `Variant (v_slice_nnso,          s3 e1 e2 e3)
    | MV_slice_nnbo (e1, e2, e3)    -> `Variant (v_slice_nnbo,          s3 e1 e2 e3)
    | MV_unpack (t, e)              -> `Variant (v_unpack,              Some (`Tuple [cv_mtcc t; cv_mvcc e;]))
    | MV_contract_of_address (t,e)  -> `Variant (v_contract_of_address, Some (`Tuple [cv_mtcc t; cv_mvcc e;]))
    | MV_isnat e                    -> `Variant (v_isnat,               s e)
  
    (*************************************************************************)
    (* List                                                                  *)
    (*************************************************************************)
    | MV_lit_list (t, el) -> `Variant (v_lit_list,  Some (`Tuple [cv_mtcc t; `List (List.map cv_mvcc el);]))
    | MV_nil t            -> `Variant (v_nil,       Some (cv_mtcc t))
    | MV_cons (e1, e2)    -> `Variant (v_cons,      s2 e1 e2)
    | MV_tl_l e           -> `Variant (v_tl_l,      s e)
  
    (*************************************************************************)
    (* Set                                                                   *)
    (*************************************************************************)
    | MV_lit_set (elt, e)         -> `Variant (v_lit_set,     Some (`Tuple [cv_mtcc elt; `List (Tz.PSet.map e ~f:(cv_mvcc) |> Tz.PSet.to_list)]))
    | MV_empty_set elt            -> `Variant (v_empty_set,   Some (cv_mtcc elt))
    | MV_update_xbss (e1, e2, e3) -> `Variant (v_update_xbss, s3 e1 e2 e3)
  
    (*************************************************************************)
    (* Operation                                                             *)
    (*************************************************************************)
    | MV_create_contract (pt, st, e1, e2, e3, e4, e5) -> `Variant (v_create_contract, Some (`Tuple [cv_mtcc pt; cv_mtcc st; cv_mvcc e1; cv_mvcc e2; cv_mvcc e3; cv_mvcc e4; cv_mvcc e5]))
    | MV_transfer_tokens (e1, e2, e3)                 -> `Variant (v_transfer_tokens, s3 e1 e2 e3)
    | MV_set_delegate e                               -> `Variant (v_set_delegate,    s e)
  
    (*************************************************************************)
    (* Contract                                                              *)
    (*************************************************************************)
    | MV_lit_contract (t, e)  -> `Variant (v_lit_contract,      Some (`Tuple [cv_mtcc t; cv_mvcc e]))
    | MV_self t               -> `Variant (v_self,              Some (cv_mtcc t))
    | MV_implicit_account e   -> `Variant (v_implicit_account,  s e)
  
    (*************************************************************************)
    (* Pair                                                                  *)
    (*************************************************************************)
    | MV_pair (e1, e2) -> `Variant (v_pair, s2 e1 e2)
  
    (*************************************************************************)
    (* Or                                                                    *)
    (*************************************************************************)
    | MV_left (t, e)  -> `Variant (v_left, Some (`Tuple [cv_mtcc t; cv_mvcc e]))
    | MV_right (t, e) -> `Variant (v_right, Some (`Tuple [cv_mtcc t; cv_mvcc e]))
  
    (*************************************************************************)
    (* Lambda                                                                *)
    (*************************************************************************)
    | MV_lit_lambda (pt, rt, i)  -> `Variant (v_lit_lambda,     Some (`Tuple [cv_mtcc pt; cv_mtcc rt; cv_micc i;]))
    | MV_lambda_unknown (pt, rt) -> `Variant (v_lambda_unknown, Some (`Tuple [cv_mtcc pt; cv_mtcc rt;]))
    | MV_lambda_closure (e1, e2) -> `Variant (v_lambda_closure, s2 e1 e2)
  
    (*************************************************************************)
    (* Map                                                                   *)
    (*************************************************************************)
    | MV_lit_map (kt, vt, e)      -> let m2pl m : js list = Tz.PMap.fold m ~init:[] ~f:(fun ~key ~data accl -> `Tuple [cv_mvcc key; cv_mvcc data] :: accl) in
                                     `Variant (v_lit_map,     Some (`Tuple [cv_mtcc kt; cv_mtcc vt; `List (m2pl e);]))
    | MV_empty_map (kt, vt)       -> `Variant (v_empty_map,   Some (`Tuple [cv_mtcc kt; cv_mtcc vt;]))
    | MV_update_xomm (e1, e2, e3) -> `Variant (v_update_xomm, s3 e1 e2 e3)
  
    (*************************************************************************)
    (* Big Map                                                               *)
    (*************************************************************************)
    | MV_lit_big_map (kt, vt, e)    -> let m2pl m : js list = Tz.PMap.fold m ~init:[] ~f:(fun ~key ~data accl -> `Tuple [cv_mvcc key; cv_mvcc data] :: accl) in
                                       `Variant (v_lit_big_map,   Some (`Tuple [cv_mtcc kt; cv_mtcc vt; `List (m2pl e);]))
    | MV_empty_big_map (kt, vt)     -> `Variant (v_empty_big_map, Some (`Tuple [cv_mtcc kt; cv_mtcc vt;]))
    | MV_update_xobmbm (e1, e2, e3) -> `Variant (v_update_xobmbm, s3 e1 e2 e3)
  
    (*************************************************************************)
    (* Chain Id                                                              *)
    (*************************************************************************)
    | MV_lit_chain_id s -> `Variant (v_lit_chain_id, Some (`String s))

    (*************************************************************************)
    (* Custom Domain Value for Invariant Synthesis                           *)
    (*************************************************************************)
    | MV_sigma_lm (e1, e2) -> `Variant (v_sigma_lm, s2 e1 e2) (* 'a list * mutez -> mutez *)
    ) (* function cv_mv end *)
  and cv_mi : Tz.mich_i -> js
  = let ss1 i = Some (cv_micc i) in
    let ss2 i1 i2 = Some (`Tuple [cv_micc i1; cv_micc i2;]) in
    let zz1 zn = Some (`Intlit (Z.to_string zn)) in
    let tt1 t = Some (cv_mtcc t) in
    let tt2 t1 t2 = Some (`Tuple [cv_mtcc t1; cv_mtcc t2;]) in
    let t2s1 t1 t2 i = Some (`Tuple [cv_mtcc t1; cv_mtcc t2; cv_micc i;]) in
    ( function
    | MI_seq (i1,i2)                -> `Variant (i_seq,               ss2 i1 i2)
    | MI_drop zn                    -> `Variant (i_drop,              zz1 zn)
    | MI_dup zn                     -> `Variant (i_dup,               zz1 zn)
    | MI_swap                       -> `Variant (i_swap,              None)
    | MI_dig zn                     -> `Variant (i_dig,               zz1 zn)
    | MI_dug zn                     -> `Variant (i_dug,               zz1 zn)
    | MI_push (t,v)                 -> `Variant (i_push,              Some (`Tuple [cv_mtcc t; cv_mvcc v;]))
    | MI_some                       -> `Variant (i_some,              None)
    | MI_none t                     -> `Variant (i_none,              tt1 t)
    | MI_unit                       -> `Variant (i_unit,              None)
    | MI_if_none (i1,i2)            -> `Variant (i_if_none,           ss2 i1 i2)
    | MI_pair                       -> `Variant (i_pair,              None)
    | MI_car                        -> `Variant (i_car,               None)
    | MI_cdr                        -> `Variant (i_cdr,               None)
    | MI_left t                     -> `Variant (i_left,              tt1 t)
    | MI_right t                    -> `Variant (i_right,             tt1 t)
    | MI_if_left (i1,i2)            -> `Variant (i_if_left,           ss2 i1 i2)
    | MI_nil t                      -> `Variant (i_nil,               tt1 t)
    | MI_cons                       -> `Variant (i_cons,              None)
    | MI_if_cons (i1,i2)            -> `Variant (i_if_cons,           ss2 i1 i2)
    | MI_size                       -> `Variant (i_size,              None)
    | MI_empty_set t                -> `Variant (i_empty_set,         tt1 t)
    | MI_empty_map (t1,t2)          -> `Variant (i_empty_map,         tt2 t1 t2)
    | MI_empty_big_map (t1,t2)      -> `Variant (i_empty_big_map,     tt2 t1 t2)
    | MI_map (i)                    -> `Variant (i_map,               ss1 i)
    | MI_iter (i)                   -> `Variant (i_iter,              ss1 i)
    | MI_mem                        -> `Variant (i_mem,               None)
    | MI_get                        -> `Variant (i_get,               None)
    | MI_update                     -> `Variant (i_update,            None)
    | MI_if (i1,i2)                 -> `Variant (i_if,                ss2 i1 i2)
    | MI_loop (i)                   -> `Variant (i_loop,              ss1 i)
    | MI_loop_left (i)              -> `Variant (i_loop_left,         ss1 i)
    | MI_lambda (t1,t2,i)           -> `Variant (i_lambda,            t2s1 t1 t2 i)
    | MI_exec                       -> `Variant (i_exec,              None)
    | MI_dip_n (zn, i)              -> `Variant (i_dip_n,             Some (`Tuple [`Intlit (Z.to_string zn); cv_micc i;]))
    | MI_failwith                   -> `Variant (i_failwith,          None)
    | MI_cast t                     -> `Variant (i_cast,              tt1 t)
    | MI_rename                     -> `Variant (i_rename,            None)
    | MI_concat                     -> `Variant (i_concat,            None)
    | MI_slice                      -> `Variant (i_slice,             None)
    | MI_pack                       -> `Variant (i_pack,              None)
    | MI_unpack t                   -> `Variant (i_unpack,            tt1 t)
    | MI_add                        -> `Variant (i_add,               None)
    | MI_sub                        -> `Variant (i_sub,               None)
    | MI_mul                        -> `Variant (i_mul,               None)
    | MI_ediv                       -> `Variant (i_ediv,              None)
    | MI_abs                        -> `Variant (i_abs,               None)
    | MI_isnat                      -> `Variant (i_isnat,             None)
    | MI_int                        -> `Variant (i_int,               None)
    | MI_neg                        -> `Variant (i_neg,               None)
    | MI_lsl                        -> `Variant (i_lsl,               None)
    | MI_lsr                        -> `Variant (i_lsr,               None)
    | MI_or                         -> `Variant (i_or,                None)
    | MI_and                        -> `Variant (i_and,               None)
    | MI_xor                        -> `Variant (i_xor,               None)
    | MI_not                        -> `Variant (i_not,               None)
    | MI_compare                    -> `Variant (i_compare,           None)
    | MI_eq                         -> `Variant (i_eq,                None)
    | MI_neq                        -> `Variant (i_neq,               None)
    | MI_lt                         -> `Variant (i_lt,                None)
    | MI_gt                         -> `Variant (i_gt,                None)
    | MI_le                         -> `Variant (i_le,                None)
    | MI_ge                         -> `Variant (i_ge,                None)
    | MI_self                       -> `Variant (i_self,              None)
    | MI_contract t                 -> `Variant (i_contract,          tt1 t)
    | MI_transfer_tokens            -> `Variant (i_transfer_tokens,   None)
    | MI_set_delegate               -> `Variant (i_set_delegate,      None)
    | MI_create_account             -> `Variant (i_create_account,    None)
    | MI_create_contract (t1,t2,i)  -> `Variant (i_create_contract,   t2s1 t1 t2 i)
    | MI_implicit_account           -> `Variant (i_implicit_account,  None)
    | MI_now                        -> `Variant (i_now,               None)
    | MI_amount                     -> `Variant (i_amount,            None)
    | MI_balance                    -> `Variant (i_balance,           None)
    | MI_check_signature            -> `Variant (i_check_signature,   None)
    | MI_blake2b                    -> `Variant (i_blake2b,           None)
    | MI_sha256                     -> `Variant (i_sha256,            None)
    | MI_sha512                     -> `Variant (i_sha512,            None)
    | MI_hash_key                   -> `Variant (i_hash_key,          None)
    | MI_steps_to_quota             -> `Variant (i_steps_to_quota,    None)
    | MI_source                     -> `Variant (i_source,            None)
    | MI_sender                     -> `Variant (i_sender,            None)
    | MI_address                    -> `Variant (i_address,           None)
    | MI_chain_id                   -> `Variant (i_chain_id,          None)
    | MI_unpair                     -> `Variant (i_unpair,            None)
    | MI_micse_check (i)            -> `Variant (i_micse_check,       ss1 i)

    ) (* function cv_mi end *)
  and cv_mtcc : Tz.mich_t Tz.cc -> js
  = fun x -> (cv_cc cv_mt) x (* function cv_mtcc end *)
  and cv_mvcc : Tz.mich_v Tz.cc -> js
  = fun x -> (cv_cc cv_mv) x (* function cv_mvcc end *)
  and cv_micc : Tz.mich_i Tz.cc -> js
  = fun x -> (cv_cc cv_mi) x (* function cv_micc end *)
  let rec cv_mf : Tz.mich_f -> js
  = let sf f = Some (cv_mf f) in
    let sf2 f1 f2 = Some (`Tuple [cv_mf f1; cv_mf f2;]) in
    let sfl fl = Some (`List (List.map cv_mf fl)) in
    let sv v = Some (cv_mvcc v) in
    let sv2 v1 v2 = Some (`Tuple [cv_mvcc v1; cv_mvcc v2;]) in
    (function
    | MF_true                             -> `Variant (f_true,                  None)
    | MF_false                            -> `Variant (f_false,                 None)
    | MF_not                    f         -> `Variant (f_not,                   sf f)
    | MF_and                    fl        -> `Variant (f_and,                   sfl fl)
    | MF_or                     fl        -> `Variant (f_or,                    sfl fl)
    | MF_eq                     (v1,v2)   -> `Variant (f_eq,                    sv2 v1 v2)
    | MF_imply                  (f1,f2)   -> `Variant (f_imply,                 sf2 f1 f2)
    | MF_is_true                v         -> `Variant (f_is_true,               sv v)
    | MF_is_none                v         -> `Variant (f_is_none,               sv v)
    | MF_is_left                v         -> `Variant (f_is_left,               sv v)
    | MF_is_cons                v         -> `Variant (f_is_cons,               sv v)
    | MF_add_mmm_no_overflow    (v1,v2)   -> `Variant (f_add_mmm_no_overflow,   sv2 v1 v2)
    | MF_sub_mmm_no_underflow   (v1,v2)   -> `Variant (f_sub_mmm_no_underflow,  sv2 v1 v2)
    | MF_mul_mnm_no_overflow    (v1,v2)   -> `Variant (f_mul_mnm_no_overflow,   sv2 v1 v2)
    | MF_mul_nmm_no_overflow    (v1,v2)   -> `Variant (f_mul_nmm_no_overflow,   sv2 v1 v2)
    | MF_shiftL_nnn_rhs_in_256  (v1,v2)   -> `Variant (f_shiftL_nnn_rhs_in_256, sv2 v1 v2)
    | MF_shiftR_nnn_rhs_in_256  (v1,v2)   -> `Variant (f_shiftR_nnn_rhs_in_256, sv2 v1 v2)
    ) (* function cv_mf end *)

  
  
  let cv_bc : Tz.blockchain -> js
  = let m2pl m : js list = Tz.PMap.fold m ~init:[] ~f:(fun ~key ~data accl -> `Tuple [cv_mvcc key; cv_mvcc data] :: accl) in
    let m2pli m : js list = Tz.PMap.fold m ~init:[] ~f:(fun ~key ~data accl -> `Tuple [cv_mvcc key; cv_micc data] :: accl) in
    fun bc -> begin
    `Assoc [
      jc_bc_storage,  `List (m2pl bc.bc_storage);
      jc_bc_code,     `List (m2pli bc.bc_code);
      jc_bc_balance,  cv_mvcc bc.bc_balance;
      jc_bc_delegate, cv_mvcc bc.bc_delegate;
      jc_bc_chain_id, cv_mvcc bc.bc_chain_id;
      jc_bc_last_blocktime, cv_mvcc bc.bc_last_blocktime;
    ]
  end (* function cv_bc end *)
  let cv_exop : Tz.explicit_operation -> js
  = (function
    | EXOP_transfer_token (e1, e2, e3, e4) -> `Variant (exop_transfer_token, Some (`Tuple [cv_mvcc e1; cv_mvcc e2; cv_mvcc e3; cv_mvcc e4;]))
    ) (* function cv_exop end *)
  let cv_oper_transfertoken : Tz.oper_transfertoken -> js
  = fun ot -> begin
    `Assoc [
      jc_optt_addr,   cv_mvcc ot.optt_addr;
      jc_optt_source, cv_mvcc ot.optt_source;
      jc_optt_sender, cv_mvcc ot.optt_sender;
      jc_optt_amount, cv_mvcc ot.optt_amount;
      jc_optt_param,  cv_mvcc ot.optt_param ;
      jc_optt_now,    cv_mvcc ot.optt_now;
    ]
  end (* function cv_oper_transfertoken end *)
  let cv_mich_cut_category : Tz.mich_cut_category -> js
  = (function
    | MCC_trx_entry     -> `Variant (mcc_trx_entry,   None)
    | MCC_trx_exit      -> `Variant (mcc_trx_exit,    None)
    | MCC_ln_loop       -> `Variant (mcc_ln_loop,     None)
    | MCC_ln_loopleft   -> `Variant (mcc_ln_loopleft, None)
    | MCC_ln_map        -> `Variant (mcc_ln_map,      None)
    | MCC_ln_iter       -> `Variant (mcc_ln_iter,     None)
    | MCC_lb_loop       -> `Variant (mcc_lb_loop,     None)
    | MCC_lb_loopleft   -> `Variant (mcc_lb_loopleft, None)
    | MCC_lb_map        -> `Variant (mcc_lb_map,      None)
    | MCC_lb_iter       -> `Variant (mcc_lb_iter,     None)
    | MCC_query         -> `Variant (mcc_query,       None)
    ) (* function cv_mich_cut_category end *)
  let cv_mich_cut_info : Tz.mich_cut_info -> js
  = fun m -> begin
    `Assoc [
      jc_mci_loc,     cv_loc m.mci_loc;
      jc_mci_cutcat,  cv_mich_cut_category m.mci_cutcat;
    ]
  end (* function cv_mich_cut_info end *)
  let cv_ss : Tz.sym_state -> js
  = fun ss -> begin
    `Assoc [
      jc_ss_fixchain,       cv_bc ss.ss_fixchain;    
      jc_ss_exop,           cv_exop ss.ss_exop;
      jc_ss_dynchain,       cv_bc ss.ss_dynchain;
      jc_ss_exec_addrs,     cv_mvcc ss.ss_exec_addrs;
      jc_ss_oper_queue,     cv_mvcc ss.ss_oper_queue;
      jc_ss_optt,           cv_oper_transfertoken ss.ss_optt;
      jc_ss_entry_mci,      cv_mich_cut_info ss.ss_entry_mci;
      jc_ss_entry_symstack, `List (List.map cv_mvcc ss.ss_entry_symstack);
      jc_ss_block_mci,      cv_mich_cut_info ss.ss_block_mci;
      jc_ss_symstack,       `List (List.map cv_mvcc ss.ss_symstack);
      jc_ss_constraints,    `List (List.map cv_mf ss.ss_constraints);
    ]
  end (* function cv_ss end *)


(*************************************************************************)
(* P1 : Debugging info for Prover                                        *)
(*************************************************************************)

  let cv_p1_ss_strop : Tz.sym_state -> js
  = fun ss -> begin
    let strg : Tz.mich_v Tz.cc =
      (Tz.PMap.find ss.ss_dynchain.bc_storage ss.ss_optt.optt_addr)
      |> (function | Some s -> s | None -> Stdlib.failwith "TzCvt.T2J.cv_p1_ss_bcop") 
    in
    `Assoc [
      jc_bc_storage,        cv_mvcc strg;
      jc_bc_balance,        cv_mvcc ss.ss_dynchain.bc_balance;
      jc_ss_optt,           cv_oper_transfertoken ss.ss_optt;
    ]
  end (* function cv_p1_ss_strop end *)

  let cv_p1_ss_path : Tz.sym_state -> js
  = fun ss -> begin
    `Assoc [
      jc_ss_entry_mci,      cv_mich_cut_info ss.ss_entry_mci;
      jc_ss_entry_symstack, `List (List.map cv_mvcc ss.ss_entry_symstack);
      jc_ss_block_mci,      cv_mich_cut_info ss.ss_block_mci;
      jc_ss_symstack,       `List (List.map cv_mvcc ss.ss_symstack);
      jc_ss_constraints,    `List (List.map cv_mf ss.ss_constraints);
    ]
  end (* function cv_p1_ss_path end *)
end (* module T2J end *)


(*****************************************************************************)
(*****************************************************************************)
(* Tz to Json (No CC)                                                        *)
(*****************************************************************************)
(*****************************************************************************)

module T2Jnocc = struct
  type js = Yojson.Safe.t
  open Jc
  let cv_pos : Tz.ccp_pos -> js
  = fun {col; lin} -> `Tuple [`Int lin; `Int col] (* function cv_pos end *)
  let cv_loc : Tz.ccp_loc -> js
  = (function
    | CCLOC_Unknown -> `Variant (cc_l_unk, None)
    | CCLOC_Pos (p1, p2) -> `Variant (cc_l_pos, Some (`Tuple [cv_pos p1; cv_pos p2]))
  ) (* function cv_loc end *)
  let cv_annot : Tz.ccp_annot -> js
  = (function
    | CCA_typ s -> `Variant (cc_a_typ, Some (`String s))
    | CCA_var s -> `Variant (cc_a_var, Some (`String s))
    | CCA_fld s -> `Variant (cc_a_fld, Some (`String s))
  ) (* function cv_annot end *)
  let cv_cc : ('a -> js) -> 'a Tz.cc -> js
  = (fun f x ->
    `Assoc [cc_loc, cv_loc x.cc_loc; cc_anl, `List (List.map cv_annot x.cc_anl); cc_val, f x.cc_v;]
  ) (* function cv_cc end *)
  let rec cv_mt : Tz.mich_t -> js
  = let s t = Some (cv_mtcc t) in
    let s2 t1 t2 = Some (`Tuple [cv_mtcc t1; cv_mtcc t2;]) in
    (function
    | MT_key              -> `Variant (t_key, None)
    | MT_unit             -> `Variant (t_unit, None)
    | MT_signature        -> `Variant (t_signature, None)
    | MT_option t         -> `Variant (t_option, s t)
    | MT_list t           -> `Variant (t_list, s t)
    | MT_set t            -> `Variant (t_set, s t)
    | MT_operation        -> `Variant (t_operation, None)
    | MT_contract t       -> `Variant (t_contract, s t)
    | MT_pair (t1, t2)    -> `Variant (t_pair, s2 t1 t2)
    | MT_or (t1, t2)      -> `Variant (t_or, s2 t1 t2)
    | MT_lambda (t1, t2)  -> `Variant (t_lambda, s2 t1 t2)
    | MT_map (t1, t2)     -> `Variant (t_map, s2 t1 t2)
    | MT_big_map (t1, t2) -> `Variant (t_big_map, s2 t1 t2)
    | MT_chain_id         -> `Variant (t_chain_id, None)
    | MT_int              -> `Variant (t_int, None)
    | MT_nat              -> `Variant (t_nat, None)
    | MT_string           -> `Variant (t_string, None)
    | MT_bytes            -> `Variant (t_bytes, None)
    | MT_mutez            -> `Variant (t_mutez, None)
    | MT_bool             -> `Variant (t_bool, None)
    | MT_key_hash         -> `Variant (t_key_hash, None)
    | MT_timestamp        -> `Variant (t_timestamp, None)
    | MT_address          -> `Variant (t_address, None)
    ) (* function cv_mt end *)
  and cv_mv : Tz.mich_v -> js
  = let s e = Some (cv_mvcc e) in
    let s2 e1 e2 = Some (`Tuple [cv_mvcc e1; cv_mvcc e2;]) in
    let s3 e1 e2 e3 = Some (`Tuple [cv_mvcc e1; cv_mvcc e2; cv_mvcc e3;]) in
    (function
    (*************************************************************************)
    (* Symbol & Polymorphic                                                  *)
    (*************************************************************************)
    (* | MV_symbol (t,v)     -> `Variant (v_symbol,        Some (`Tuple [cv_mtcc t; `String v])) *)
    | MV_symbol (t,v)     -> `Variant (v_symbol,        Some (`Tuple [cv_mt t.cc_v; `String v]))
    | MV_car e            -> `Variant (v_car,           s e)
    | MV_cdr e            -> `Variant (v_cdr,           s e)
    | MV_unlift_option e  -> `Variant (v_unlift_option, s e)
    | MV_unlift_left e    -> `Variant (v_unlift_left,   s e)
    | MV_unlift_right e   -> `Variant (v_unlift_right,  s e)
    | MV_hd_l e           -> `Variant (v_hd_l,          s e)
  
    (*************************************************************************)
    (* Integer                                                               *)
    (*************************************************************************)
    | MV_lit_int zn       -> `Variant (v_lit_int,     Some (`Intlit (Z.to_string zn)))
    | MV_neg_ni e         -> `Variant (v_neg_ni,      s e)
    | MV_neg_ii e         -> `Variant (v_neg_ii,      s e)
    | MV_not_ni e         -> `Variant (v_not_ni,      s e)
    | MV_not_ii e         -> `Variant (v_not_ii,      s e)
    | MV_add_nii (e1, e2) -> `Variant (v_add_nii,     s2 e1 e2)
    | MV_add_ini (e1, e2) -> `Variant (v_add_ini,     s2 e1 e2)
    | MV_add_iii (e1, e2) -> `Variant (v_add_iii,     s2 e1 e2)
    | MV_sub_nni (e1, e2) -> `Variant (v_sub_nni,     s2 e1 e2)
    | MV_sub_nii (e1, e2) -> `Variant (v_sub_nii,     s2 e1 e2)
    | MV_sub_ini (e1, e2) -> `Variant (v_sub_ini,     s2 e1 e2)
    | MV_sub_iii (e1, e2) -> `Variant (v_sub_iii,     s2 e1 e2)
    | MV_sub_tti (e1, e2) -> `Variant (v_sub_tti,     s2 e1 e2)
    | MV_mul_nii (e1, e2) -> `Variant (v_mul_nii,     s2 e1 e2)
    | MV_mul_ini (e1, e2) -> `Variant (v_mul_ini,     s2 e1 e2)
    | MV_mul_iii (e1, e2) -> `Variant (v_mul_iii,     s2 e1 e2)
    | MV_compare (e1, e2) -> `Variant (v_compare,     s2 e1 e2)
    | MV_int_of_nat e     -> `Variant (v_int_of_nat,  s e)
  
    (*************************************************************************)
    (* Natural Number                                                        *)
    (*************************************************************************)
    | MV_lit_nat zn           -> `Variant (v_lit_nat,     Some (`Intlit (Z.to_string zn)))
    | MV_abs_in e             -> `Variant (v_abs_in,      s e)
    | MV_add_nnn (e1, e2)     -> `Variant (v_add_nnn,     s2 e1 e2)
    | MV_mul_nnn (e1, e2)     -> `Variant (v_mul_nnn,     s2 e1 e2)
    | MV_shiftL_nnn (e1, e2)  -> `Variant (v_shiftL_nnn,  s2 e1 e2)
    | MV_shiftR_nnn (e1, e2)  -> `Variant (v_shiftR_nnn,  s2 e1 e2)
    | MV_and_nnn (e1, e2)     -> `Variant (v_and_nnn,     s2 e1 e2)
    | MV_and_inn (e1, e2)     -> `Variant (v_and_inn,     s2 e1 e2)
    | MV_or_nnn (e1, e2)      -> `Variant (v_or_nnn,      s2 e1 e2)
    | MV_xor_nnn (e1, e2)     -> `Variant (v_xor_nnn,     s2 e1 e2)
    | MV_size_s e             -> `Variant (v_size_s,      s e)
    | MV_size_m e             -> `Variant (v_size_m,      s e)
    | MV_size_l e             -> `Variant (v_size_l,      s e)
    | MV_size_str e           -> `Variant (v_size_str,    s e)
    | MV_size_b e             -> `Variant (v_size_b,      s e)
  
    (*************************************************************************)
    (* String                                                                *)
    (*************************************************************************)
    | MV_lit_string s         -> `Variant (v_lit_string,    Some (`String s))
    | MV_concat_sss (e1, e2)  -> `Variant (v_concat_sss,    s2 e1 e2)
    | MV_concat_list_s e      -> `Variant (v_concat_list_s, s e)
  
    (*************************************************************************)
    (* Bytes                                                                 *)
    (*************************************************************************)
    | MV_lit_bytes s          -> `Variant (v_lit_bytes,     Some (`String s))
    | MV_concat_bbb (e1, e2)  -> `Variant (v_concat_bbb,    s2 e1 e2)
    | MV_concat_list_b e      -> `Variant (v_concat_list_b, s e)
    | MV_pack e               -> `Variant (v_pack,          s e)
    | MV_blake2b e            -> `Variant (v_blake2b,       s e)
    | MV_sha256  e            -> `Variant (v_sha256,        s e)
    | MV_sha512  e            -> `Variant (v_sha512,        s e)
  
    (*************************************************************************)
    (* Mutez                                                                 *)
    (*************************************************************************)
    | MV_lit_mutez zn     -> `Variant (v_lit_mutez, Some (`Intlit (Z.to_string zn)))
    | MV_add_mmm (e1, e2) -> `Variant (v_add_mmm,   s2 e1 e2)
    | MV_sub_mmm (e1, e2) -> `Variant (v_sub_mmm,   s2 e1 e2)
    | MV_mul_mnm (e1, e2) -> `Variant (v_mul_mnm,   s2 e1 e2)
    | MV_mul_nmm (e1, e2) -> `Variant (v_mul_nmm,   s2 e1 e2)
  
    (*************************************************************************)
    (* Bool                                                                  *)
    (*************************************************************************)
    | MV_lit_bool b                 -> `Variant (v_lit_bool,        Some (`Bool b))
    | MV_not_bb e                   -> `Variant (v_not_bb,          s e)
    | MV_and_bbb (e1, e2)           -> `Variant (v_and_bbb,         s2 e1 e2)
    | MV_or_bbb  (e1, e2)           -> `Variant (v_or_bbb,          s2 e1 e2)
    | MV_xor_bbb (e1, e2)           -> `Variant (v_xor_bbb,         s2 e1 e2)
    | MV_eq_ib   (e1, e2)           -> `Variant (v_eq_ib,           s2 e1 e2)
    | MV_neq_ib  (e1, e2)           -> `Variant (v_neq_ib,          s2 e1 e2)
    | MV_lt_ib   (e1, e2)           -> `Variant (v_lt_ib,           s2 e1 e2)
    | MV_gt_ib   (e1, e2)           -> `Variant (v_gt_ib,           s2 e1 e2)
    | MV_leq_ib  (e1, e2)           -> `Variant (v_leq_ib,          s2 e1 e2)
    | MV_geq_ib  (e1, e2)           -> `Variant (v_geq_ib,          s2 e1 e2)
    | MV_mem_xsb (e1, e2)           -> `Variant (v_mem_xsb,         s2 e1 e2)
    | MV_mem_xmb (e1, e2)           -> `Variant (v_mem_xmb,         s2 e1 e2)
    | MV_mem_xbmb (e1, e2)          -> `Variant (v_mem_xbmb,        s2 e1 e2)
    | MV_check_signature (e1,e2,e3) -> `Variant (v_check_signature, s3 e1 e2 e3)
  
    (*************************************************************************)
    (* Key Hash                                                              *)
    (*************************************************************************)
    | MV_lit_key_hash s -> `Variant (v_lit_key_hash, Some (`String s))
    | MV_hash_key e     -> `Variant (v_hash_key    , s e)
  
    (*************************************************************************)
    (* Timestamp                                                             *)
    (*************************************************************************)
    | MV_lit_timestamp_str s  -> `Variant (v_lit_timestamp_str, Some (`String s))
    | MV_lit_timestamp_sec zn -> `Variant (v_lit_timestamp_sec, Some (`Intlit (Z.to_string zn)))
    | MV_add_tit (e1, e2)     -> `Variant (v_add_tit,           s2 e1 e2)
    | MV_add_itt (e1, e2)     -> `Variant (v_add_itt,           s2 e1 e2)
    | MV_sub_tit (e1, e2)     -> `Variant (v_sub_tit,           s2 e1 e2)
  
    (*************************************************************************)
    (* Address                                                               *)
    (*************************************************************************)
    | MV_lit_address e          -> `Variant (v_lit_address,         s e)
    | MV_address_of_contract e  -> `Variant (v_address_of_contract, s e)
  
    (*************************************************************************)
    (* Key                                                                   *)
    (*************************************************************************)
    | MV_lit_key s -> `Variant (v_lit_key, Some (`String s))
  
    (*************************************************************************)
    (* Unit                                                                  *)
    (*************************************************************************)
    | MV_unit -> `Variant (v_unit, None)
  
    (*************************************************************************)
    (* Signature                                                             *)
    (*************************************************************************)
    | MV_lit_signature_str s            -> `Variant (v_lit_signature_str,     Some (`String s))
    | MV_lit_signature_signed (e1, e2)  -> `Variant (v_lit_signature_signed,  s2 e1 e2)
  
    (*************************************************************************)
    (* Option                                                                *)
    (*************************************************************************)
    | MV_some e                     -> `Variant (v_some,                s e)
    | MV_none t                     -> `Variant (v_none,                Some (cv_mtcc t))
    | MV_ediv_nnnn (e1, e2)         -> `Variant (v_ediv_nnnn,           s2 e1 e2)
    | MV_ediv_niin (e1, e2)         -> `Variant (v_ediv_niin,           s2 e1 e2)
    | MV_ediv_inin (e1, e2)         -> `Variant (v_ediv_inin,           s2 e1 e2)
    | MV_ediv_iiin (e1, e2)         -> `Variant (v_ediv_iiin,           s2 e1 e2)
    | MV_ediv_mnmm (e1, e2)         -> `Variant (v_ediv_mnmm,           s2 e1 e2)
    | MV_ediv_mmnm (e1, e2)         -> `Variant (v_ediv_mmnm,           s2 e1 e2)
    | MV_get_xmoy (e1, e2)          -> `Variant (v_get_xmoy,            s2 e1 e2)
    | MV_get_xbmo (e1, e2)          -> `Variant (v_get_xbmo,            s2 e1 e2)
    | MV_slice_nnso (e1, e2, e3)    -> `Variant (v_slice_nnso,          s3 e1 e2 e3)
    | MV_slice_nnbo (e1, e2, e3)    -> `Variant (v_slice_nnbo,          s3 e1 e2 e3)
    | MV_unpack (t, e)              -> `Variant (v_unpack,              Some (`Tuple [cv_mtcc t; cv_mvcc e;]))
    | MV_contract_of_address (t,e)  -> `Variant (v_contract_of_address, Some (`Tuple [cv_mtcc t; cv_mvcc e;]))
    | MV_isnat e                    -> `Variant (v_isnat,               s e)
  
    (*************************************************************************)
    (* List                                                                  *)
    (*************************************************************************)
    | MV_lit_list (t, el) -> `Variant (v_lit_list,  Some (`Tuple [cv_mtcc t; `List (List.map cv_mvcc el);]))
    | MV_nil t            -> `Variant (v_nil,       Some (cv_mtcc t))
    | MV_cons (e1, e2)    -> `Variant (v_cons,      s2 e1 e2)
    | MV_tl_l e           -> `Variant (v_tl_l,      s e)
  
    (*************************************************************************)
    (* Set                                                                   *)
    (*************************************************************************)
    | MV_lit_set (elt, e)         -> `Variant (v_lit_set,     Some (`Tuple [cv_mtcc elt; `List (Tz.PSet.map e ~f:(cv_mvcc) |> Tz.PSet.to_list)]))
    | MV_empty_set elt            -> `Variant (v_empty_set,   Some (cv_mtcc elt))
    | MV_update_xbss (e1, e2, e3) -> `Variant (v_update_xbss, s3 e1 e2 e3)
  
    (*************************************************************************)
    (* Operation                                                             *)
    (*************************************************************************)
    | MV_create_contract (pt, st, e1, e2, e3, e4, e5) -> `Variant (v_create_contract, Some (`Tuple [cv_mtcc pt; cv_mtcc st; cv_mvcc e1; cv_mvcc e2; cv_mvcc e3; cv_mvcc e4; cv_mvcc e5;]))
    | MV_transfer_tokens (e1, e2, e3)                 -> `Variant (v_transfer_tokens, s3 e1 e2 e3)
    | MV_set_delegate e                               -> `Variant (v_set_delegate,    s e)
  
    (*************************************************************************)
    (* Contract                                                              *)
    (*************************************************************************)
    | MV_lit_contract (t, e)  -> `Variant (v_lit_contract,      Some (`Tuple [cv_mtcc t; cv_mvcc e]))
    | MV_self t               -> `Variant (v_self,              Some (cv_mtcc t))
    | MV_implicit_account e   -> `Variant (v_implicit_account,  s e)
  
    (*************************************************************************)
    (* Pair                                                                  *)
    (*************************************************************************)
    | MV_pair (e1, e2) -> `Variant (v_pair, s2 e1 e2)
  
    (*************************************************************************)
    (* Or                                                                    *)
    (*************************************************************************)
    | MV_left (t, e)  -> `Variant (v_left, Some (`Tuple [cv_mtcc t; cv_mvcc e]))
    | MV_right (t, e) -> `Variant (v_right, Some (`Tuple [cv_mtcc t; cv_mvcc e]))
  
    (*************************************************************************)
    (* Lambda                                                                *)
    (*************************************************************************)
    | MV_lit_lambda (pt, rt, i)  -> `Variant (v_lit_lambda,     Some (`Tuple [cv_mtcc pt; cv_mtcc rt; cv_micc i;]))
    | MV_lambda_unknown (pt, rt) -> `Variant (v_lambda_unknown, Some (`Tuple [cv_mtcc pt; cv_mtcc rt;]))
    | MV_lambda_closure (e1, e2) -> `Variant (v_lambda_closure, s2 e1 e2)
  
    (*************************************************************************)
    (* Map                                                                   *)
    (*************************************************************************)
    | MV_lit_map (kt, vt, e)      -> let m2pl m : js list = Tz.PMap.fold m ~init:[] ~f:(fun ~key ~data accl -> `Tuple [cv_mvcc key; cv_mvcc data] :: accl) in
                                     `Variant (v_lit_map,     Some (`Tuple [cv_mtcc kt; cv_mtcc vt; `List (m2pl e);]))
    | MV_empty_map (kt, vt)       -> `Variant (v_empty_map,   Some (`Tuple [cv_mtcc kt; cv_mtcc vt;]))
    | MV_update_xomm (e1, e2, e3) -> `Variant (v_update_xomm, s3 e1 e2 e3)
  
    (*************************************************************************)
    (* Big Map                                                               *)
    (*************************************************************************)
    | MV_lit_big_map (kt, vt, e)    -> let m2pl m : js list = Tz.PMap.fold m ~init:[] ~f:(fun ~key ~data accl -> `Tuple [cv_mvcc key; cv_mvcc data] :: accl) in
                                       `Variant (v_lit_big_map,   Some (`Tuple [cv_mtcc kt; cv_mtcc vt; `List (m2pl e);]))
    | MV_empty_big_map (kt, vt)     -> `Variant (v_empty_big_map, Some (`Tuple [cv_mtcc kt; cv_mtcc vt;]))
    | MV_update_xobmbm (e1, e2, e3) -> `Variant (v_update_xobmbm, s3 e1 e2 e3)
  
    (*************************************************************************)
    (* Chain Id                                                              *)
    (*************************************************************************)
    | MV_lit_chain_id s -> `Variant (v_lit_chain_id, Some (`String s))

    (*************************************************************************)
    (* Custom Domain Value for Invariant Synthesis                           *)
    (*************************************************************************)
    | MV_sigma_lm (e1, e2) -> `Variant (v_sigma_lm, s2 e1 e2) (* 'a list -> 'a *)
    ) (* function cv_mv end *)
  and cv_mi : Tz.mich_i -> js
  = let ss1 i = Some (cv_micc i) in
    let ss2 i1 i2 = Some (`Tuple [cv_micc i1; cv_micc i2;]) in
    let zz1 zn = Some (`Intlit (Z.to_string zn)) in
    let tt1 t = Some (cv_mtcc t) in
    let tt2 t1 t2 = Some (`Tuple [cv_mtcc t1; cv_mtcc t2;]) in
    let t2s1 t1 t2 i = Some (`Tuple [cv_mtcc t1; cv_mtcc t2; cv_micc i;]) in
    ( function
    | MI_seq (i1,i2)                -> `Variant (i_seq,               ss2 i1 i2)
    | MI_drop zn                    -> `Variant (i_drop,              zz1 zn)
    | MI_dup zn                     -> `Variant (i_dup,               zz1 zn)
    | MI_swap                       -> `Variant (i_swap,              None)
    | MI_dig zn                     -> `Variant (i_dig,               zz1 zn)
    | MI_dug zn                     -> `Variant (i_dug,               zz1 zn)
    | MI_push (t,v)                 -> `Variant (i_push,              Some (`Tuple [cv_mtcc t; cv_mvcc v;]))
    | MI_some                       -> `Variant (i_some,              None)
    | MI_none t                     -> `Variant (i_none,              tt1 t)
    | MI_unit                       -> `Variant (i_unit,              None)
    | MI_if_none (i1,i2)            -> `Variant (i_if_none,           ss2 i1 i2)
    | MI_pair                       -> `Variant (i_pair,              None)
    | MI_car                        -> `Variant (i_car,               None)
    | MI_cdr                        -> `Variant (i_cdr,               None)
    | MI_left t                     -> `Variant (i_left,              tt1 t)
    | MI_right t                    -> `Variant (i_right,             tt1 t)
    | MI_if_left (i1,i2)            -> `Variant (i_if_left,           ss2 i1 i2)
    | MI_nil t                      -> `Variant (i_nil,               tt1 t)
    | MI_cons                       -> `Variant (i_cons,              None)
    | MI_if_cons (i1,i2)            -> `Variant (i_if_cons,           ss2 i1 i2)
    | MI_size                       -> `Variant (i_size,              None)
    | MI_empty_set t                -> `Variant (i_empty_set,         tt1 t)
    | MI_empty_map (t1,t2)          -> `Variant (i_empty_map,         tt2 t1 t2)
    | MI_empty_big_map (t1,t2)      -> `Variant (i_empty_big_map,     tt2 t1 t2)
    | MI_map (i)                    -> `Variant (i_map,               ss1 i)
    | MI_iter (i)                   -> `Variant (i_iter,              ss1 i)
    | MI_mem                        -> `Variant (i_mem,               None)
    | MI_get                        -> `Variant (i_get,               None)
    | MI_update                     -> `Variant (i_update,            None)
    | MI_if (i1,i2)                 -> `Variant (i_if,                ss2 i1 i2)
    | MI_loop (i)                   -> `Variant (i_loop,              ss1 i)
    | MI_loop_left (i)              -> `Variant (i_loop_left,         ss1 i)
    | MI_lambda (t1,t2,i)           -> `Variant (i_lambda,            t2s1 t1 t2 i)
    | MI_exec                       -> `Variant (i_exec,              None)
    | MI_dip_n (zn, i)              -> `Variant (i_dip_n,             Some (`Tuple [`Intlit (Z.to_string zn); cv_micc i;]))
    | MI_failwith                   -> `Variant (i_failwith,          None)
    | MI_cast t                     -> `Variant (i_cast,              tt1 t)
    | MI_rename                     -> `Variant (i_rename,            None)
    | MI_concat                     -> `Variant (i_concat,            None)
    | MI_slice                      -> `Variant (i_slice,             None)
    | MI_pack                       -> `Variant (i_pack,              None)
    | MI_unpack t                   -> `Variant (i_unpack,            tt1 t)
    | MI_add                        -> `Variant (i_add,               None)
    | MI_sub                        -> `Variant (i_sub,               None)
    | MI_mul                        -> `Variant (i_mul,               None)
    | MI_ediv                       -> `Variant (i_ediv,              None)
    | MI_abs                        -> `Variant (i_abs,               None)
    | MI_isnat                      -> `Variant (i_isnat,             None)
    | MI_int                        -> `Variant (i_int,               None)
    | MI_neg                        -> `Variant (i_neg,               None)
    | MI_lsl                        -> `Variant (i_lsl,               None)
    | MI_lsr                        -> `Variant (i_lsr,               None)
    | MI_or                         -> `Variant (i_or,                None)
    | MI_and                        -> `Variant (i_and,               None)
    | MI_xor                        -> `Variant (i_xor,               None)
    | MI_not                        -> `Variant (i_not,               None)
    | MI_compare                    -> `Variant (i_compare,           None)
    | MI_eq                         -> `Variant (i_eq,                None)
    | MI_neq                        -> `Variant (i_neq,               None)
    | MI_lt                         -> `Variant (i_lt,                None)
    | MI_gt                         -> `Variant (i_gt,                None)
    | MI_le                         -> `Variant (i_le,                None)
    | MI_ge                         -> `Variant (i_ge,                None)
    | MI_self                       -> `Variant (i_self,              None)
    | MI_contract t                 -> `Variant (i_contract,          tt1 t)
    | MI_transfer_tokens            -> `Variant (i_transfer_tokens,   None)
    | MI_set_delegate               -> `Variant (i_set_delegate,      None)
    | MI_create_account             -> `Variant (i_create_account,    None)
    | MI_create_contract (t1,t2,i)  -> `Variant (i_create_contract,   t2s1 t1 t2 i)
    | MI_implicit_account           -> `Variant (i_implicit_account,  None)
    | MI_now                        -> `Variant (i_now,               None)
    | MI_amount                     -> `Variant (i_amount,            None)
    | MI_balance                    -> `Variant (i_balance,           None)
    | MI_check_signature            -> `Variant (i_check_signature,   None)
    | MI_blake2b                    -> `Variant (i_blake2b,           None)
    | MI_sha256                     -> `Variant (i_sha256,            None)
    | MI_sha512                     -> `Variant (i_sha512,            None)
    | MI_hash_key                   -> `Variant (i_hash_key,          None)
    | MI_steps_to_quota             -> `Variant (i_steps_to_quota,    None)
    | MI_source                     -> `Variant (i_source,            None)
    | MI_sender                     -> `Variant (i_sender,            None)
    | MI_address                    -> `Variant (i_address,           None)
    | MI_chain_id                   -> `Variant (i_chain_id,          None)
    | MI_unpair                     -> `Variant (i_unpair,            None)
    | MI_micse_check (i)            -> `Variant (i_micse_check,       ss1 i)

    ) (* function cv_mi end *)
  and cv_mtcc : Tz.mich_t Tz.cc -> js
  = fun x -> cv_mt x.cc_v (* function cv_mtcc end *)
  and cv_mvcc : Tz.mich_v Tz.cc -> js
  = fun x -> cv_mv x.cc_v (* function cv_mvcc end *)
  and cv_micc : Tz.mich_i Tz.cc -> js
  = fun x -> cv_mi x.cc_v (* function cv_micc end *)
  let rec cv_mf : Tz.mich_f -> js
  = let sf f = Some (cv_mf f) in
    let sf2 f1 f2 = Some (`Tuple [cv_mf f1; cv_mf f2;]) in
    let sfl fl = Some (`List (List.map cv_mf fl)) in
    let sv v = Some (cv_mvcc v) in
    let sv2 v1 v2 = Some (`Tuple [cv_mvcc v1; cv_mvcc v2;]) in
    (function
    | MF_true                             -> `Variant (f_true,                  None)
    | MF_false                            -> `Variant (f_false,                 None)
    | MF_not                    f         -> `Variant (f_not,                   sf f)
    | MF_and                    fl        -> `Variant (f_and,                   sfl fl)
    | MF_or                     fl        -> `Variant (f_or,                    sfl fl)
    | MF_eq                     (v1,v2)   -> `Variant (f_eq,                    sv2 v1 v2)
    | MF_imply                  (f1,f2)   -> `Variant (f_imply,                 sf2 f1 f2)
    | MF_is_true                v         -> `Variant (f_is_true,               sv v)
    | MF_is_none                v         -> `Variant (f_is_none,               sv v)
    | MF_is_left                v         -> `Variant (f_is_left,               sv v)
    | MF_is_cons                v         -> `Variant (f_is_cons,               sv v)
    | MF_add_mmm_no_overflow    (v1,v2)   -> `Variant (f_add_mmm_no_overflow,   sv2 v1 v2)
    | MF_sub_mmm_no_underflow   (v1,v2)   -> `Variant (f_sub_mmm_no_underflow,  sv2 v1 v2)
    | MF_mul_mnm_no_overflow    (v1,v2)   -> `Variant (f_mul_mnm_no_overflow,   sv2 v1 v2)
    | MF_mul_nmm_no_overflow    (v1,v2)   -> `Variant (f_mul_nmm_no_overflow,   sv2 v1 v2)
    | MF_shiftL_nnn_rhs_in_256  (v1,v2)   -> `Variant (f_shiftL_nnn_rhs_in_256, sv2 v1 v2)
    | MF_shiftR_nnn_rhs_in_256  (v1,v2)   -> `Variant (f_shiftR_nnn_rhs_in_256, sv2 v1 v2)
    ) (* function cv_mf end *)



    let cv_bc : Tz.blockchain -> js
    = let m2pl m : js list = Tz.PMap.fold m ~init:[] ~f:(fun ~key ~data accl -> `Tuple [cv_mvcc key; cv_mvcc data] :: accl) in
      let m2pli m : js list = Tz.PMap.fold m ~init:[] ~f:(fun ~key ~data accl -> `Tuple [cv_mvcc key; cv_micc data] :: accl) in
      fun bc -> begin
      `Assoc [
        jc_bc_storage,  `List (m2pl bc.bc_storage);
        jc_bc_code,     `List (m2pli bc.bc_code);
        jc_bc_balance,  cv_mvcc bc.bc_balance;
        jc_bc_delegate, cv_mvcc bc.bc_delegate;
        jc_bc_chain_id, cv_mvcc bc.bc_chain_id;
        jc_bc_last_blocktime, cv_mvcc bc.bc_last_blocktime;
      ]
    end (* function cv_bc end *)
    let cv_exop : Tz.explicit_operation -> js
    = (function
      | EXOP_transfer_token (e1, e2, e3, e4) -> `Variant (exop_transfer_token, Some (`Tuple [cv_mvcc e1; cv_mvcc e2; cv_mvcc e3; cv_mvcc e4;]))
      ) (* function cv_exop end *)
    let cv_oper_transfertoken : Tz.oper_transfertoken -> js
    = fun ot -> begin
      `Assoc [
        jc_optt_addr,   cv_mvcc ot.optt_addr;
        jc_optt_source, cv_mvcc ot.optt_source;
        jc_optt_sender, cv_mvcc ot.optt_sender;
        jc_optt_amount, cv_mvcc ot.optt_amount;
        jc_optt_param,  cv_mvcc ot.optt_param ;
        jc_optt_now,    cv_mvcc ot.optt_now;
      ]
    end (* function cv_oper_transfertoken end *)
    let cv_mich_cut_category : Tz.mich_cut_category -> js
    = (function
      | MCC_trx_entry     -> `Variant (mcc_trx_entry,   None)
      | MCC_trx_exit      -> `Variant (mcc_trx_exit,    None)
      | MCC_ln_loop       -> `Variant (mcc_ln_loop,     None)
      | MCC_ln_loopleft   -> `Variant (mcc_ln_loopleft, None)
      | MCC_ln_map        -> `Variant (mcc_ln_map,      None)
      | MCC_ln_iter       -> `Variant (mcc_ln_iter,     None)
      | MCC_lb_loop       -> `Variant (mcc_lb_loop,     None)
      | MCC_lb_loopleft   -> `Variant (mcc_lb_loopleft, None)
      | MCC_lb_map        -> `Variant (mcc_lb_map,      None)
      | MCC_lb_iter       -> `Variant (mcc_lb_iter,     None)
      | MCC_query         -> `Variant (mcc_query,       None)
      ) (* function cv_mich_cut_category end *)
    let cv_mich_cut_info : Tz.mich_cut_info -> js
    = fun m -> begin
      `Assoc [
        jc_mci_loc,     cv_loc m.mci_loc;
        jc_mci_cutcat,  cv_mich_cut_category m.mci_cutcat;
      ]
    end (* function cv_mich_cut_info end *)
    let cv_ss : Tz.sym_state -> js
    = fun ss -> begin
      `Assoc [
        jc_ss_fixchain,       cv_bc ss.ss_fixchain;    
        jc_ss_exop,           cv_exop ss.ss_exop;
        jc_ss_dynchain,       cv_bc ss.ss_dynchain;
        jc_ss_exec_addrs,     cv_mvcc ss.ss_exec_addrs;
        jc_ss_oper_queue,     cv_mvcc ss.ss_oper_queue;
        jc_ss_optt,           cv_oper_transfertoken ss.ss_optt;
        jc_ss_entry_mci,      cv_mich_cut_info ss.ss_entry_mci;
        jc_ss_entry_symstack, `List (List.map cv_mvcc ss.ss_entry_symstack);
        jc_ss_block_mci,      cv_mich_cut_info ss.ss_block_mci;
        jc_ss_symstack,       `List (List.map cv_mvcc ss.ss_symstack);
        jc_ss_constraints,    `List (List.map cv_mf ss.ss_constraints);
      ]
    end (* function cv_ss end *)

  (*************************************************************************)
  (* P1 : Debugging info for Prover                                        *)
  (*************************************************************************)

  let cv_p1_ss_strop : Tz.sym_state -> js
  = fun ss -> begin
    let strg : Tz.mich_v Tz.cc =
      (Tz.PMap.find ss.ss_dynchain.bc_storage ss.ss_optt.optt_addr)
      |> (function | Some s -> s | None -> Stdlib.failwith "TzCvt.T2J.cv_p1_ss_bcop") 
    in
    `Assoc [
      jc_bc_storage,        cv_mvcc strg;
      jc_bc_balance,        cv_mvcc ss.ss_dynchain.bc_balance;
      jc_ss_optt,           cv_oper_transfertoken ss.ss_optt;
    ]
  end (* function cv_p1_ss_strop end *)

  let cv_p1_ss_path : Tz.sym_state -> js
  = fun ss -> begin
    `Assoc [
      jc_ss_entry_mci,      cv_mich_cut_info ss.ss_entry_mci;
      jc_ss_entry_symstack, `List (List.map cv_mvcc ss.ss_entry_symstack);
      jc_ss_block_mci,      cv_mich_cut_info ss.ss_block_mci;
      jc_ss_symstack,       `List (List.map cv_mvcc ss.ss_symstack);
      jc_ss_constraints,    `List (List.map cv_mf ss.ss_constraints);
    ]
  end (* function cv_p1_ss_path end *)
end (* moduel T2Jnocc end *)


(*****************************************************************************)
(*****************************************************************************)
(* Tz to Comparable Numbers (No CC)                                          *)
(*****************************************************************************)
(*****************************************************************************)

module T2Nnocc = struct
  type numbers = int list

  let rec cv_mt : Tz.mich_t -> numbers
  = let module CList = Core.List in
    let c tl = tl |> CList.map ~f:cv_mtcc |> CList.join in
    (function
    | MT_key              -> [ 0]
    | MT_unit             -> [ 1]
    | MT_signature        -> [ 2]
    | MT_option t         -> [ 3]@(c [t;])
    | MT_list t           -> [ 4]@(c [t;])
    | MT_set t            -> [ 5]@(c [t;])
    | MT_operation        -> [ 6]
    | MT_contract t       -> [ 7]@(c [t;])
    | MT_pair (t1, t2)    -> [ 8]@(c [t1; t2;])
    | MT_or (t1, t2)      -> [ 9]@(c [t1; t2;])
    | MT_lambda (t1, t2)  -> [10]@(c [t1; t2;])
    | MT_map (t1, t2)     -> [11]@(c [t1; t2;])
    | MT_big_map (t1, t2) -> [12]@(c [t1; t2;])
    | MT_chain_id         -> [13]
    | MT_int              -> [14]
    | MT_nat              -> [15]
    | MT_string           -> [16]
    | MT_bytes            -> [17]
    | MT_mutez            -> [18]
    | MT_bool             -> [19]
    | MT_key_hash         -> [20]
    | MT_timestamp        -> [21]
    | MT_address          -> [22]) (* function cv_mt end *)

  and cv_mtcc : Tz.mich_t Tz.cc -> numbers
  = fun x -> cv_mt x.cc_v (* function cv_mtcc end *)
end


(*****************************************************************************)
(*****************************************************************************)
(* Tz to Core.Sexp.t                                                         *)
(*****************************************************************************)
(*****************************************************************************)

module T2CS = struct
  type csexp = Core.Sexp.t

  let gen_template : string -> csexp -> csexp
  = let module CSexp = Core.Sexp in
    let a e = CSexp.Atom e in
    let l el = CSexp.List el in
    (* function gen_template start *)
    fun temp x -> begin
    l [(a temp); x;]
  end (* function gen_template end *)

  let cv_pos : Tz.ccp_pos -> csexp
  = let open Jc in
    let module CSexp = Core.Sexp in
    let a e = CSexp.Atom e in
    let l el = CSexp.List el in
    (* function cv_pos start *)
    fun {col; lin} -> begin
    gen_template cs_pos (l [(a (string_of_int lin)); (a (string_of_int col));])
  end (* function cv_pos end *)
  let cv_loc : Tz.ccp_loc -> csexp
  = let open Jc in
    let module CSexp = Core.Sexp in
    let a e = CSexp.Atom e in
    let l el = CSexp.List el in
    (* function cv_loc start *)
    fun loc -> begin
    (match loc with
    | CCLOC_Unknown -> l [(a cc_l_unk);]
    | CCLOC_Pos (p1, p2) -> l [(a cc_l_pos); (cv_pos p1); (cv_pos p2);])
    |> gen_template cs_loc
  end (* function cv_loc end *)
  let cv_annot : Tz.ccp_annot -> csexp
  = let open Jc in
    let module CSexp = Core.Sexp in
    let a e = CSexp.Atom e in
    let l el = CSexp.List el in
    (* function cv_annot start *)
    fun annot -> begin
    (match annot with
    | CCA_typ s -> l [(a cc_a_typ); (a s);]
    | CCA_var s -> l [(a cc_a_var); (a s);]
    | CCA_fld s -> l [(a cc_a_fld); (a s);])
    |> (fun body -> l [(a cs_annot); body;])
  end (* function cv_annot end *)
  let cv_cc : ('a -> csexp) -> 'a Tz.cc -> csexp
  = let open Jc in
    let module CSexp = Core.Sexp in
    let l el = CSexp.List el in
    (* function cv_cc start *)
    fun f x -> begin
    gen_template cs_cc (l [(cv_loc x.cc_loc); l (Core.List.map ~f:cv_annot x.cc_anl); (f x.cc_v);])
  end (* function cv_cc end *)

  let rec cv_mt : Tz.mich_t -> csexp
  = let open Jc in
    let module CSexp = Core.Sexp in
    let a e = CSexp.Atom e in
    let l el = CSexp.List el in
    let s0 b = l [(a b);] in
    let s1 b t1 = l [(a b); (l [(cv_mtcc t1)])] in
    let s2 b t1 t2 = l [(a b); (l [(cv_mtcc t1); (cv_mtcc t2)])] in
    (* function cv_mt start *)
    fun x -> begin
    (match x with
    | MT_key              -> s0 t_key
    | MT_unit             -> s0 t_unit
    | MT_signature        -> s0 t_signature
    | MT_option t1        -> s1 t_option t1
    | MT_list t1          -> s1 t_list t1
    | MT_set t1           -> s1 t_set t1
    | MT_operation        -> s0 t_operation
    | MT_contract t1      -> s1 t_contract t1
    | MT_pair (t1, t2)    -> s2 t_pair t1 t2
    | MT_or (t1, t2)      -> s2 t_or t1 t2
    | MT_lambda (t1, t2)  -> s2 t_lambda t1 t2
    | MT_map (t1, t2)     -> s2 t_map t1 t2
    | MT_big_map (t1, t2) -> s2 t_big_map t1 t2
    | MT_chain_id         -> s0 t_chain_id
    | MT_int              -> s0 t_int
    | MT_nat              -> s0 t_nat
    | MT_string           -> s0 t_string
    | MT_bytes            -> s0 t_bytes
    | MT_mutez            -> s0 t_mutez
    | MT_bool             -> s0 t_bool
    | MT_key_hash         -> s0 t_key_hash
    | MT_timestamp        -> s0 t_timestamp
    | MT_address          -> s0 t_address)
    |> gen_template cs_mich_t
  end (* function cv_mt end *)
  and cv_mv : Tz.mich_v -> csexp
  = let open Jc in
    let module CSexp = Core.Sexp in
    let a e = CSexp.Atom e in
    let l el = CSexp.List el in
    let s0 b = l [(a b);] in
    let s1 b e1 = l [(a b); (l [(cv_mvcc e1);]);] in
    let s0t1 b t1 = l [(a b); (l [(cv_mtcc t1);]);] in
    let s0v1 b v1 = l [(a b); (l [(a v1);]);] in
    let s2 b e1 e2 = l [(a b); (l [(cv_mvcc e1); (cv_mvcc e2);]);] in
    let s0t2 b t1 t2 = l [(a b); (l [(cv_mtcc t1); (cv_mtcc t2);]);] in
    let t1s1 b t1 e2 = l [(a b); (l [(cv_mtcc t1); (cv_mvcc e2);]);] in
    let t1v1 b t1 v2 = l [(a b); (l [(cv_mtcc t1); (a v2);]);] in
    let s3 b e1 e2 e3 = l [(a b); (l [(cv_mvcc e1); (cv_mvcc e2); (cv_mvcc e3);]);] in
    let t2i1 b t1 t2 i3 = l [(a b); (l [(cv_mtcc t1); (cv_mtcc t2); (cv_micc i3)]);] in
    let t2s5 b t1 t2 e3 e4 e5 e6 e7 = l [(a b); (l [(cv_mtcc t1); (cv_mtcc t2); (cv_mvcc e3); (cv_mvcc e4); (cv_mvcc e5); (cv_mvcc e6); (cv_mvcc e7);]);] in
    let t1sl b t1 sl = l [(a b); (l [(cv_mtcc t1); (l sl);]);] in
    let t2sl b t1 t2 sl = l [(a b); (l [(cv_mtcc t1); (cv_mtcc t2); (l sl);]);] in
    (* function cv_mv start *)
    fun x -> begin
    (match x with
    (*************************************************************************)
    (* Symbol & Polymorphic                                                  *)
    (*************************************************************************)
    | MV_symbol (t,v)     -> t1v1 v_symbol t v
    | MV_car e            -> s1   v_car e
    | MV_cdr e            -> s1   v_cdr e
    | MV_unlift_option e  -> s1   v_unlift_option e
    | MV_unlift_left e    -> s1   v_unlift_left e
    | MV_unlift_right e   -> s1   v_unlift_right e
    | MV_hd_l e           -> s1   v_hd_l e
  
    (*************************************************************************)
    (* Integer                                                               *)
    (*************************************************************************)
    | MV_lit_int zn       -> s0v1 v_lit_int (Z.to_string zn)
    | MV_neg_ni e         -> s1   v_neg_ni e
    | MV_neg_ii e         -> s1   v_neg_ii e
    | MV_not_ni e         -> s1   v_not_ni e
    | MV_not_ii e         -> s1   v_not_ii e
    | MV_add_nii (e1, e2) -> s2   v_add_nii e1 e2
    | MV_add_ini (e1, e2) -> s2   v_add_ini e1 e2
    | MV_add_iii (e1, e2) -> s2   v_add_iii e1 e2
    | MV_sub_nni (e1, e2) -> s2   v_sub_nni e1 e2
    | MV_sub_nii (e1, e2) -> s2   v_sub_nii e1 e2
    | MV_sub_ini (e1, e2) -> s2   v_sub_ini e1 e2
    | MV_sub_iii (e1, e2) -> s2   v_sub_iii e1 e2
    | MV_sub_tti (e1, e2) -> s2   v_sub_tti e1 e2
    | MV_mul_nii (e1, e2) -> s2   v_mul_nii e1 e2
    | MV_mul_ini (e1, e2) -> s2   v_mul_ini e1 e2
    | MV_mul_iii (e1, e2) -> s2   v_mul_iii e1 e2
    | MV_compare (e1, e2) -> s2   v_compare e1 e2
    | MV_int_of_nat e     -> s1   v_int_of_nat e
  
    (*************************************************************************)
    (* Natural Number                                                        *)
    (*************************************************************************)
    | MV_lit_nat zn           -> s0v1 v_lit_nat (Z.to_string zn)
    | MV_abs_in e             -> s1   v_abs_in e
    | MV_add_nnn (e1, e2)     -> s2   v_add_nnn e1 e2
    | MV_mul_nnn (e1, e2)     -> s2   v_mul_nnn e1 e2
    | MV_shiftL_nnn (e1, e2)  -> s2   v_shiftL_nnn e1 e2
    | MV_shiftR_nnn (e1, e2)  -> s2   v_shiftR_nnn e1 e2
    | MV_and_nnn (e1, e2)     -> s2   v_and_nnn e1 e2
    | MV_and_inn (e1, e2)     -> s2   v_and_inn e1 e2
    | MV_or_nnn (e1, e2)      -> s2   v_or_nnn e1 e2
    | MV_xor_nnn (e1, e2)     -> s2   v_xor_nnn e1 e2
    | MV_size_s e             -> s1   v_size_s e
    | MV_size_m e             -> s1   v_size_m e
    | MV_size_l e             -> s1   v_size_l e
    | MV_size_str e           -> s1   v_size_str e
    | MV_size_b e             -> s1   v_size_b e
  
    (*************************************************************************)
    (* String                                                                *)
    (*************************************************************************)
    | MV_lit_string s         -> s0v1 v_lit_string s
    | MV_concat_sss (e1, e2)  -> s2   v_concat_sss e1 e2
    | MV_concat_list_s e      -> s1   v_concat_list_s e
  
    (*************************************************************************)
    (* Bytes                                                                 *)
    (*************************************************************************)
    | MV_lit_bytes s          -> s0v1 v_lit_bytes s
    | MV_concat_bbb (e1, e2)  -> s2   v_concat_bbb e1 e2
    | MV_concat_list_b e      -> s1   v_concat_list_b e
    | MV_pack e               -> s1   v_pack e
    | MV_blake2b e            -> s1   v_blake2b e
    | MV_sha256  e            -> s1   v_sha256 e
    | MV_sha512  e            -> s1   v_sha512 e
  
    (*************************************************************************)
    (* Mutez                                                                 *)
    (*************************************************************************)
    | MV_lit_mutez zn     -> s0v1 v_lit_mutez (Z.to_string zn)
    | MV_add_mmm (e1, e2) -> s2   v_add_mmm e1 e2
    | MV_sub_mmm (e1, e2) -> s2   v_sub_mmm e1 e2
    | MV_mul_mnm (e1, e2) -> s2   v_mul_mnm e1 e2
    | MV_mul_nmm (e1, e2) -> s2   v_mul_nmm e1 e2
  
    (*************************************************************************)
    (* Bool                                                                  *)
    (*************************************************************************)
    | MV_lit_bool b                 -> s0v1 v_lit_bool (string_of_bool b)
    | MV_not_bb e                   -> s1   v_not_bb e
    | MV_and_bbb (e1, e2)           -> s2   v_and_bbb e1 e2
    | MV_or_bbb  (e1, e2)           -> s2   v_or_bbb e1 e2
    | MV_xor_bbb (e1, e2)           -> s2   v_xor_bbb e1 e2
    | MV_eq_ib   (e1, e2)           -> s2   v_eq_ib e1 e2
    | MV_neq_ib  (e1, e2)           -> s2   v_neq_ib e1 e2
    | MV_lt_ib   (e1, e2)           -> s2   v_lt_ib e1 e2
    | MV_gt_ib   (e1, e2)           -> s2   v_gt_ib e1 e2
    | MV_leq_ib  (e1, e2)           -> s2   v_leq_ib e1 e2
    | MV_geq_ib  (e1, e2)           -> s2   v_geq_ib e1 e2
    | MV_mem_xsb (e1, e2)           -> s2   v_mem_xsb e1 e2
    | MV_mem_xmb (e1, e2)           -> s2   v_mem_xmb e1 e2
    | MV_mem_xbmb (e1, e2)          -> s2   v_mem_xbmb e1 e2
    | MV_check_signature (e1,e2,e3) -> s3   v_check_signature e1 e2 e3
  
    (*************************************************************************)
    (* Key Hash                                                              *)
    (*************************************************************************)
    | MV_lit_key_hash s -> s0v1 v_lit_key_hash s
    | MV_hash_key e     -> s1   v_hash_key e
  
    (*************************************************************************)
    (* Timestamp                                                             *)
    (*************************************************************************)
    | MV_lit_timestamp_str s  -> s0v1 v_lit_timestamp_str s
    | MV_lit_timestamp_sec zn -> s0v1 v_lit_timestamp_sec (Z.to_string zn)
    | MV_add_tit (e1, e2)     -> s2   v_add_tit e1 e2
    | MV_add_itt (e1, e2)     -> s2   v_add_itt e1 e2
    | MV_sub_tit (e1, e2)     -> s2   v_sub_tit e1 e2
  
    (*************************************************************************)
    (* Address                                                               *)
    (*************************************************************************)
    | MV_lit_address e          -> s1 v_lit_address e
    | MV_address_of_contract e  -> s1 v_address_of_contract e
  
    (*************************************************************************)
    (* Key                                                                   *)
    (*************************************************************************)
    | MV_lit_key s -> s0v1 v_lit_key s
  
    (*************************************************************************)
    (* Unit                                                                  *)
    (*************************************************************************)
    | MV_unit -> s0 v_unit
  
    (*************************************************************************)
    (* Signature                                                             *)
    (*************************************************************************)
    | MV_lit_signature_str s            -> s0v1 v_lit_signature_str s
    | MV_lit_signature_signed (e1, e2)  -> s2   v_lit_signature_signed e1 e2
  
    (*************************************************************************)
    (* Option                                                                *)
    (*************************************************************************)
    | MV_some e                     -> s1   v_some e
    | MV_none t                     -> s0t1 v_none t
    | MV_ediv_nnnn (e1, e2)         -> s2   v_ediv_nnnn e1 e2
    | MV_ediv_niin (e1, e2)         -> s2   v_ediv_niin e1 e2
    | MV_ediv_inin (e1, e2)         -> s2   v_ediv_inin e1 e2
    | MV_ediv_iiin (e1, e2)         -> s2   v_ediv_iiin e1 e2
    | MV_ediv_mnmm (e1, e2)         -> s2   v_ediv_mnmm e1 e2
    | MV_ediv_mmnm (e1, e2)         -> s2   v_ediv_mmnm e1 e2
    | MV_get_xmoy (e1, e2)          -> s2   v_get_xmoy e1 e2
    | MV_get_xbmo (e1, e2)          -> s2   v_get_xbmo e1 e2
    | MV_slice_nnso (e1, e2, e3)    -> s3   v_slice_nnso e1 e2 e3
    | MV_slice_nnbo (e1, e2, e3)    -> s3   v_slice_nnbo e1 e2 e3
    | MV_unpack (t, e)              -> t1s1 v_unpack t e
    | MV_contract_of_address (t,e)  -> t1s1 v_contract_of_address t e
    | MV_isnat e                    -> s1   v_isnat e
  
    (*************************************************************************)
    (* List                                                                  *)
    (*************************************************************************)
    | MV_lit_list (t, el) -> t1sl v_lit_list t (Core.List.map el ~f:cv_mvcc)
    | MV_nil t            -> s0t1 v_nil t
    | MV_cons (e1, e2)    -> s2   v_cons e1 e2
    | MV_tl_l e           -> s1   v_tl_l e
  
    (*************************************************************************)
    (* Set                                                                   *)
    (*************************************************************************)
    | MV_lit_set (elt, es)        -> t1sl v_lit_set elt (Core.Set.Poly.map es ~f:cv_mvcc |> Core.Set.Poly.to_list)
    | MV_empty_set elt            -> s0t1 v_empty_set elt
    | MV_update_xbss (e1, e2, e3) -> s3   v_update_xbss e1 e2 e3
  
    (*************************************************************************)
    (* Operation                                                             *)
    (*************************************************************************)
    | MV_create_contract (pt, st, e1, e2, e3, e4, e5) -> t2s5 v_create_contract pt st e1 e2 e3 e4 e5
    | MV_transfer_tokens (e1, e2, e3)                 -> s3   v_transfer_tokens e1 e2 e3
    | MV_set_delegate e                               -> s1   v_set_delegate e
  
    (*************************************************************************)
    (* Contract                                                              *)
    (*************************************************************************)
    | MV_lit_contract (t, e)  -> t1s1 v_lit_contract t e
    | MV_self t               -> s0t1 v_self t
    | MV_implicit_account e   -> s1   v_implicit_account e
  
    (*************************************************************************)
    (* Pair                                                                  *)
    (*************************************************************************)
    | MV_pair (e1, e2) -> s2 v_pair e1 e2
  
    (*************************************************************************)
    (* Or                                                                    *)
    (*************************************************************************)
    | MV_left (t, e)  -> t1s1 v_left t e
    | MV_right (t, e) -> t1s1 v_right t e
  
    (*************************************************************************)
    (* Lambda                                                                *)
    (*************************************************************************)
    | MV_lit_lambda (pt, rt, i)  -> t2i1  v_lit_lambda pt rt i
    | MV_lambda_unknown (pt, rt) -> s0t2  v_lambda_unknown pt rt
    | MV_lambda_closure (e1, e2) -> s2    v_lambda_closure e1 e2

    (*************************************************************************)
    (* Map                                                                   *)
    (*************************************************************************)
    | MV_lit_map (kt, vt, e)      -> t2sl v_lit_map kt vt (Core.Map.Poly.to_alist e |> Core.List.map ~f:(fun (k, v) -> s2 v_elt k v))
    | MV_empty_map (kt, vt)       -> s0t2 v_empty_map kt vt
    | MV_update_xomm (e1, e2, e3) -> s3   v_update_xomm e1 e2 e3

    (*************************************************************************)
    (* Big Map                                                               *)
    (*************************************************************************)
    | MV_lit_big_map (kt, vt, e)    -> t2sl v_lit_big_map kt vt (Core.Map.Poly.to_alist e |> Core.List.map ~f:(fun (k, v) -> s2 v_elt k v))
    | MV_empty_big_map (kt, vt)     -> s0t2 v_empty_big_map kt vt
    | MV_update_xobmbm (e1, e2, e3) -> s3   v_update_xobmbm e1 e2 e3

    (*************************************************************************)
    (* Chain Id                                                              *)
    (*************************************************************************)
    | MV_lit_chain_id s -> s0v1 v_lit_chain_id s

    (*************************************************************************)
    (* Custom Domain Value for Invariant Synthesis                           *)
    (*************************************************************************)
    | MV_sigma_lm (e1, e2) -> s2 v_sigma_lm e1 e2)
    |> gen_template cs_mich_v
  end (* function cv_mv end *)
  and cv_mi : Tz.mich_i -> csexp
  = let open Jc in
    let module CSexp = Core.Sexp in
    let module CList = Core.List in
    let a e = CSexp.Atom e in
    let l el = CSexp.List el in
    let s0 b = l [(a b);] in
    let s1 b i1 = l [(a b); (l [(cv_micc i1);]);] in
    let s0t1 b t1 = l [(a b); (l [(cv_mtcc t1);]);] in
    let s0v1 b v1 = l [(a b); (l [(a v1);]);] in
    let s2 b i1 i2 = l [(a b); (l [(cv_micc i1); (cv_micc i2);]);] in
    let s0t2 b t1 t2 = l [(a b); (l [(cv_mtcc t1); (cv_mtcc t2);]);] in
    let t1e1 b t1 e2 = l [(a b); (l [(cv_mtcc t1); (cv_mvcc e2);]);] in
    let v1s1 b v1 i2 = l [(a b); (l [(a v1); (cv_micc i2);]);] in
    let t2s1 b t1 t2 s3 = l [(a b); (l [(cv_mtcc t1); (cv_mtcc t2); (cv_micc s3)]);] in
    (* function cv_mi start *)
    fun x -> begin
    (match x with
    | MI_seq (i1,i2)                -> s2   i_seq i1 i2
    | MI_drop zn                    -> s0v1 i_drop (Z.to_string zn)
    | MI_dup zn                     -> s0v1 i_dup (Z.to_string zn)
    | MI_swap                       -> s0   i_swap
    | MI_dig zn                     -> s0v1 i_dig (Z.to_string zn)
    | MI_dug zn                     -> s0v1 i_dug (Z.to_string zn)
    | MI_push (t,v)                 -> t1e1 i_push t v
    | MI_some                       -> s0   i_some
    | MI_none t                     -> s0t1 i_none t
    | MI_unit                       -> s0   i_unit
    | MI_if_none (i1,i2)            -> s2   i_if_none i1 i2
    | MI_pair                       -> s0   i_pair
    | MI_car                        -> s0   i_car
    | MI_cdr                        -> s0   i_cdr
    | MI_left t                     -> s0t1 i_left t
    | MI_right t                    -> s0t1 i_right t
    | MI_if_left (i1,i2)            -> s2   i_if_left i1 i2
    | MI_nil t                      -> s0t1 i_nil t
    | MI_cons                       -> s0   i_cons
    | MI_if_cons (i1,i2)            -> s2   i_if_cons i1 i2
    | MI_size                       -> s0   i_size
    | MI_empty_set t                -> s0t1 i_empty_set t
    | MI_empty_map (t1,t2)          -> s0t2 i_empty_map t1 t2
    | MI_empty_big_map (t1,t2)      -> s0t2 i_empty_big_map t1 t2
    | MI_map (i)                    -> s1   i_map i
    | MI_iter (i)                   -> s1   i_iter i
    | MI_mem                        -> s0   i_mem
    | MI_get                        -> s0   i_get
    | MI_update                     -> s0   i_update
    | MI_if (i1,i2)                 -> s2   i_if i1 i2
    | MI_loop (i)                   -> s1   i_loop i
    | MI_loop_left (i)              -> s1   i_loop_left i
    | MI_lambda (t1,t2,i)           -> t2s1 i_lambda t1 t2 i
    | MI_exec                       -> s0   i_exec
    | MI_dip_n (zn, i)              -> v1s1 i_dip_n (Z.to_string zn) i
    | MI_failwith                   -> s0   i_failwith
    | MI_cast t                     -> s0t1 i_cast t
    | MI_rename                     -> s0   i_rename
    | MI_concat                     -> s0   i_concat
    | MI_slice                      -> s0   i_slice
    | MI_pack                       -> s0   i_pack
    | MI_unpack t                   -> s0t1 i_unpack t
    | MI_add                        -> s0   i_add
    | MI_sub                        -> s0   i_sub
    | MI_mul                        -> s0   i_mul
    | MI_ediv                       -> s0   i_ediv
    | MI_abs                        -> s0   i_abs
    | MI_isnat                      -> s0   i_isnat
    | MI_int                        -> s0   i_int
    | MI_neg                        -> s0   i_neg
    | MI_lsl                        -> s0   i_lsl
    | MI_lsr                        -> s0   i_lsr
    | MI_or                         -> s0   i_or
    | MI_and                        -> s0   i_and
    | MI_xor                        -> s0   i_xor
    | MI_not                        -> s0   i_not
    | MI_compare                    -> s0   i_compare
    | MI_eq                         -> s0   i_eq
    | MI_neq                        -> s0   i_neq
    | MI_lt                         -> s0   i_lt
    | MI_gt                         -> s0   i_gt
    | MI_le                         -> s0   i_le
    | MI_ge                         -> s0   i_ge
    | MI_self                       -> s0   i_self
    | MI_contract t                 -> s0t1 i_contract t
    | MI_transfer_tokens            -> s0   i_transfer_tokens
    | MI_set_delegate               -> s0   i_set_delegate
    | MI_create_account             -> s0   i_create_account
    | MI_create_contract (t1,t2,i)  -> t2s1 i_create_contract t1 t2 i
    | MI_implicit_account           -> s0   i_implicit_account
    | MI_now                        -> s0   i_now
    | MI_amount                     -> s0   i_amount
    | MI_balance                    -> s0   i_balance
    | MI_check_signature            -> s0   i_check_signature
    | MI_blake2b                    -> s0   i_blake2b
    | MI_sha256                     -> s0   i_sha256
    | MI_sha512                     -> s0   i_sha512
    | MI_hash_key                   -> s0   i_hash_key
    | MI_steps_to_quota             -> s0   i_steps_to_quota
    | MI_source                     -> s0   i_source
    | MI_sender                     -> s0   i_sender
    | MI_address                    -> s0   i_address
    | MI_chain_id                   -> s0   i_chain_id
    | MI_unpair                     -> s0   i_unpair
    | MI_micse_check (i)            -> s1   i_micse_check i)
    |> gen_template cs_mich_i
  end (* function cv_mi end *)
  and cv_mtcc : Tz.mich_t Tz.cc -> csexp
  = fun x -> gen_template Jc.cs_mich_t_cc (cv_cc cv_mt x) (* function cv_mtcc end *)
  and cv_mvcc : Tz.mich_v Tz.cc -> csexp
  = fun x -> gen_template Jc.cs_mich_v_cc (cv_cc cv_mv x) (* function cv_mvcc end *)
  and cv_micc : Tz.mich_i Tz.cc -> csexp
  = fun x -> gen_template Jc.cs_mich_i_cc (cv_cc cv_mi x) (* function cv_micc end *)

  let rec cv_mf : Tz.mich_f -> csexp
  = let open Jc in
    let module CSexp = Core.Sexp in
    let a e = CSexp.Atom e in
    let l el = CSexp.List el in
    let s0 b = l [(a b);] in
    let s1 b f1 = l [(a b); (l [(cv_mf f1);]);] in
    let s0e1 b e1 = l [(a b); (l [(cv_mvcc e1);]);] in
    let s2 b f1 f2 = l [(a b); (l [(cv_mf f1); (cv_mf f2);]);] in
    let s0e2 b e1 e2 = l [(a b); (l [(cv_mvcc e1); (cv_mvcc e2);]);] in
    let sl b sl = l [(a b); (l sl);] in
    (* function cv_mf start *)
    fun x -> begin
    (match x with
    | MF_true                             -> s0 f_true
    | MF_false                            -> s0 f_false
    | MF_not                    f         -> s1 f_not f
    | MF_and                    fl        -> sl f_and (Core.List.map fl ~f:cv_mf)
    | MF_or                     fl        -> sl f_or (Core.List.map fl ~f:cv_mf)
    | MF_eq                     (v1,v2)   -> s0e2 f_eq v1 v2
    | MF_imply                  (f1,f2)   -> s2 f_imply f1 f2
    | MF_is_true                v         -> s0e1 f_is_true v
    | MF_is_none                v         -> s0e1 f_is_none v
    | MF_is_left                v         -> s0e1 f_is_left v
    | MF_is_cons                v         -> s0e1 f_is_cons v
    | MF_add_mmm_no_overflow    (v1,v2)   -> s0e2 f_add_mmm_no_overflow v1 v2
    | MF_sub_mmm_no_underflow   (v1,v2)   -> s0e2 f_sub_mmm_no_underflow v1 v2
    | MF_mul_mnm_no_overflow    (v1,v2)   -> s0e2 f_mul_mnm_no_overflow v1 v2
    | MF_mul_nmm_no_overflow    (v1,v2)   -> s0e2 f_mul_nmm_no_overflow v1 v2
    | MF_shiftL_nnn_rhs_in_256  (v1,v2)   -> s0e2 f_shiftL_nnn_rhs_in_256 v1 v2
    | MF_shiftR_nnn_rhs_in_256  (v1,v2)   -> s0e2 f_shiftR_nnn_rhs_in_256 v1 v2)
    |> gen_template cs_mich_f
  end (* function cv_mf end *)

  let cv_mich_cut_category : Tz.mich_cut_category -> csexp
  = let open Jc in
    let module CSexp = Core.Sexp in
    let a e = CSexp.Atom e in
    (* function cv_mich_cut_category start *)
    fun x -> begin
    (match x with
    | MCC_trx_entry     -> a mcc_trx_entry
    | MCC_trx_exit      -> a mcc_trx_exit
    | MCC_ln_loop       -> a mcc_ln_loop
    | MCC_ln_loopleft   -> a mcc_ln_loopleft
    | MCC_ln_map        -> a mcc_ln_map
    | MCC_ln_iter       -> a mcc_ln_iter
    | MCC_lb_loop       -> a mcc_lb_loop
    | MCC_lb_loopleft   -> a mcc_lb_loopleft
    | MCC_lb_map        -> a mcc_lb_map
    | MCC_lb_iter       -> a mcc_lb_iter
    | MCC_query         -> a mcc_query)
    |> gen_template cs_mich_cut_category
  end (* function cv_mich_cut_category end *)

  let cv_mich_cut_info : Tz.mich_cut_info -> csexp
  = let open Jc in
    let module CSexp = Core.Sexp in
    let l el = CSexp.List el in
    (* function cv_mich_cut_info start *)
    fun x -> begin
    gen_template cs_mich_cut_info (l [(cv_loc x.mci_loc); (cv_mich_cut_category x.mci_cutcat)])
  end (* function cv_mich_cut_info end *)
end (* module T2CS end *)


(*****************************************************************************)
(*****************************************************************************)
(* Core.Sexp.t to Tz (No CC)                                                 *)
(*****************************************************************************)
(*****************************************************************************)

module CS2Tnocc = struct
  exception Error of string

  type csexp = Core.Sexp.t

  let get_body_in_atom_exn : csexp -> string
  = let module CSexp = Core.Sexp in
    (* function get_body_in_atom_exn start *)
    fun cs -> begin
    (match cs with
    | CSexp.Atom s -> s
    | _ -> Error ("get_body_in_atom_exn : _") |> Stdlib.raise)
  end (* function get_body_in_atom_exn end *)

  let get_body_in_list_exn : csexp -> csexp list
  = let module CSexp = Core.Sexp in
    (* function get_body_in_list_exn start *)
    fun cs -> begin
    (match cs with
    | CSexp.List sl -> sl
    | _ -> Error ("get_body_in_list_exn : _") |> Stdlib.raise)
  end (* function get_body_in_list_exn end *)

  let get_template_type_exn : csexp -> string
  = let module CSexp = Core.Sexp in
    (* function get_template_type_exn start *)
    fun cs -> begin
    (match cs with
    | CSexp.Atom s -> Error ("get_template_type_exn : Core.Sexp.Atom : " ^ s) |> Stdlib.raise
    | CSexp.List (idc::_) -> (
      match idc with
      | CSexp.Atom t -> t
      | _ -> Error ("get_template_type_exn : Core.Sexp.List : _") |> Stdlib.raise)
    | _ -> Error ("get_template_type_exn : _") |> Stdlib.raise)
  end (* function get_template_type_exn end *)

  let get_template_body_exn : string -> csexp -> csexp
  = let module CSexp = Core.Sexp in
    (* function get_template_body_exn start *)
    fun temp cs -> begin
    (match cs with
    | CSexp.Atom s -> Error ("get_template_body_exn " ^ temp ^ " : Core.Sexp.Atom : " ^ s) |> Stdlib.raise
    | CSexp.List (idc::body::[]) -> (
      match idc with
      | CSexp.Atom t when t = temp -> body
      | _ -> Error ("get_template_body_exn " ^ temp ^ " : Core.Sexp.List : _") |> Stdlib.raise)
    | _ -> Error ("get_template_body_exn " ^ temp ^ " : _") |> Stdlib.raise)
  end (* function get_template_body_exn end *)

  let cv_pos : csexp -> Tz.ccp_pos
  = let open Tz in
    let open Jc in
    let module CSexp = Core.Sexp in
    (* function cv_pos start *)
    fun x -> begin
    let body : csexp = get_template_body_exn cs_pos x in
    (match body with
    | CSexp.Atom s -> Error ("cv_pos : Core.Sexp.Atom : " ^ s) |> Stdlib.raise
    | CSexp.List (a1::a2::[]) -> (
      let lin_body : int = get_body_in_atom_exn a1 |> int_of_string in
      let col_body : int = get_body_in_atom_exn a2 |> int_of_string in
      { col=col_body; lin=lin_body; })
    | CSexp.List (s::_) -> Error ("cv_pos : Core.Sexp.List : " ^ (s |> get_body_in_atom_exn)) |> Stdlib.raise
    | _ -> Error ("cv_pos : _") |> Stdlib.raise)
  end (* function cv_pos end *)

  let cv_loc : csexp -> Tz.ccp_loc
  = let open Tz in
    let open Jc in
    let module CSexp = Core.Sexp in
    (* function cv_loc start *)
    fun x -> begin
    let body : csexp = get_template_body_exn cs_loc x in
    (match body with
    | CSexp.Atom s -> Error ("cv_loc : Core.Sexp.Atom : " ^ s) |> Stdlib.raise
    | CSexp.List (s0::[]) -> (
      match (get_body_in_atom_exn s0) with
      | s when s = cc_l_unk -> CCLOC_Unknown
      | s -> Error ("cv_mv : Core.Sexp.List s0 : " ^ s) |> Stdlib.raise)
    | CSexp.List (s2::a1::a2::[]) -> (
      match (get_body_in_atom_exn s2) with
      | s when s = cc_l_pos -> CCLOC_Pos ((cv_pos a1), (cv_pos a2))
      | s -> Error ("cv_mv : Core.Sexp.List s2 : " ^ s) |> Stdlib.raise)
    | CSexp.List (s::_) -> Error ("cv_loc : Core.Sexp.List : " ^ (s |> get_body_in_atom_exn)) |> Stdlib.raise
    | _ -> Error ("cv_loc : _") |> Stdlib.raise)
  end (* function cv_loc end *)

  let cv_annot : csexp -> Tz.ccp_annot
  = let open Tz in
    let open Jc in
    let module CSexp = Core.Sexp in
    (* function cv_annot start *)
    fun x -> begin
    let body : csexp = get_template_body_exn cs_annot x in
    (match body with
    | CSexp.Atom s -> Error ("cv_annot : Core.Sexp.Atom : " ^ s) |> Stdlib.raise
    | CSexp.List (s1::a1::[]) -> (
      match (get_body_in_atom_exn s1) with
      | s when s = cc_a_typ -> CCA_typ (get_body_in_atom_exn a1)
      | s when s = cc_a_var -> CCA_var (get_body_in_atom_exn a1)
      | s when s = cc_a_fld -> CCA_fld (get_body_in_atom_exn a1)
      | s -> Error ("cv_mv : Core.Sexp.List s1 : " ^ s) |> Stdlib.raise)
    | CSexp.List (s::_) -> Error ("cv_annot : Core.Sexp.List : " ^ (s |> get_body_in_atom_exn)) |> Stdlib.raise
    | _ -> Error ("cv_annot : _") |> Stdlib.raise)
  end (* function cv_annot end *)

  let rec cv_mt : csexp -> Tz.mich_t
  = let open Tz in
    let open Jc in
    let module CSexp = Core.Sexp in
    (* function cv_mt start *)
    fun x -> begin
    let body : csexp = get_template_body_exn cs_mich_t x in
    (match body with
    | CSexp.Atom s -> Error ("cv_mt : Core.Sexp.Atom : " ^ s) |> Stdlib.raise
    | CSexp.List (s0::[]) -> (
      match (get_body_in_atom_exn s0) with
      | s when s = t_key        -> MT_key
      | s when s = t_unit       -> MT_unit
      | s when s = t_signature  -> MT_signature
      | s when s = t_operation  -> MT_operation
      | s when s = t_chain_id   -> MT_chain_id
      | s when s = t_int        -> MT_int
      | s when s = t_nat        -> MT_nat
      | s when s = t_string     -> MT_string
      | s when s = t_bytes      -> MT_bytes
      | s when s = t_mutez      -> MT_mutez
      | s when s = t_bool       -> MT_bool
      | s when s = t_key_hash   -> MT_key_hash
      | s when s = t_timestamp  -> MT_timestamp
      | s when s = t_address    -> MT_address
      | s -> Error ("cv_mt : Core.Sexp.List s0 : " ^ s) |> Stdlib.raise)
    | CSexp.List (s1::a1::[]) -> (
      match (get_body_in_atom_exn s1) with
      | s when s = t_option   -> MT_option (cv_mtcc a1)
      | s when s = t_list     -> MT_list (cv_mtcc a1)
      | s when s = t_set      -> MT_set (cv_mtcc a1)
      | s when s = t_contract -> MT_contract (cv_mtcc a1)
      | s -> Error ("cv_mt : Core.Sexp.List s1 : " ^ s) |> Stdlib.raise)
    | CSexp.List (s2::a1::a2::[]) -> (
      match (get_body_in_atom_exn s2) with
      | s when s = t_pair     -> MT_pair ((cv_mtcc a1), (cv_mtcc a2))
      | s when s = t_or       -> MT_or ((cv_mtcc a1), (cv_mtcc a2))
      | s when s = t_lambda   -> MT_lambda ((cv_mtcc a1), (cv_mtcc a2))
      | s when s = t_map      -> MT_map ((cv_mtcc a1), (cv_mtcc a2))
      | s when s = t_big_map  -> MT_big_map ((cv_mtcc a1), (cv_mtcc a2))
      | s -> Error ("cv_mt : Core.Sexp.List s2 : " ^ s) |> Stdlib.raise)
    | CSexp.List (s::_) -> Error ("cv_mt : Core.Sexp.List : " ^ (s |> get_body_in_atom_exn)) |> Stdlib.raise
    | _ -> Error ("cv_mt : _") |> Stdlib.raise)
  end (* function cv_mt end *)
  and cv_mv : csexp -> Tz.mich_v
  = let open Tz in
    let open Jc in
    let module CSexp = Core.Sexp in
    let cv_elt : csexp -> (mich_v cc * mich_v cc)
    = (* function cv_elt start *)
      fun x -> begin
      let body : csexp = get_template_body_exn cs_mich_t x in
      (match body with
      | CSexp.Atom s -> Error ("cv_mv : cv_elt : Core.Sexp.Atom : " ^ s) |> Stdlib.raise
      | CSexp.List (s1::a1::a2::[]) when (get_body_in_atom_exn s1) = v_elt -> (
        (cv_mvcc a1), (cv_mvcc a2))
      | CSexp.List (s::_) -> Error ("cv_mv : cv_elt : Core.Sexp.List : " ^ (s |> get_body_in_atom_exn)) |> Stdlib.raise
      | _ -> Error ("cv_mv : cv_elt : _") |> Stdlib.raise)
    end in (* function cv_elt end *)
    (* function cv_mv start *)
    fun x -> begin
    let body : csexp = get_template_body_exn cs_mich_t x in
    (match body with
    | CSexp.Atom s -> Error ("cv_mv : Core.Sexp.Atom : " ^ s) |> Stdlib.raise
    | CSexp.List (s0::[]) -> (
      match (get_body_in_atom_exn s0) with
      (************************************************************************)
      (* Unit                                                                 *)
      (************************************************************************)
      | s when s = v_unit -> MV_unit
      (************************************************************************)
      (* Error                                                                *)
      (************************************************************************)
      | s -> Error ("cv_mv : Core.Sexp.List s0 : " ^ s) |> Stdlib.raise)
    | CSexp.List (s1::a1::[]) -> (
      match (get_body_in_atom_exn s1) with
      (************************************************************************)
      (* Symbol & Polymorphic                                                 *)
      (************************************************************************)
      | s when s = v_car            -> MV_car (cv_mvcc a1)
      | s when s = v_cdr            -> MV_cdr (cv_mvcc a1)
      | s when s = v_unlift_option  -> MV_unlift_option (cv_mvcc a1)
      | s when s = v_unlift_left    -> MV_unlift_left (cv_mvcc a1)
      | s when s = v_unlift_right   -> MV_unlift_right (cv_mvcc a1)
      | s when s = v_hd_l           -> MV_hd_l (cv_mvcc a1)
      (************************************************************************)
      (* Integer                                                              *)
      (************************************************************************)
      | s when s = v_lit_int    -> MV_lit_int (a1 |> get_body_in_atom_exn |> Z.of_string)
      | s when s = v_neg_ni     -> MV_neg_ni (cv_mvcc a1)
      | s when s = v_neg_ii     -> MV_neg_ii (cv_mvcc a1)
      | s when s = v_not_ni     -> MV_not_ni (cv_mvcc a1)
      | s when s = v_not_ii     -> MV_not_ii (cv_mvcc a1)
      | s when s = v_int_of_nat -> MV_int_of_nat (cv_mvcc a1)
      (************************************************************************)
      (* Natural Number                                                       *)
      (************************************************************************)
      | s when s = v_lit_nat  -> MV_lit_nat (a1 |> get_body_in_atom_exn |> Z.of_string)
      | s when s = v_abs_in   -> MV_abs_in (cv_mvcc a1)
      | s when s = v_size_s   -> MV_size_s (cv_mvcc a1)
      | s when s = v_size_m   -> MV_size_m (cv_mvcc a1)
      | s when s = v_size_l   -> MV_size_l (cv_mvcc a1)
      | s when s = v_size_str -> MV_size_str (cv_mvcc a1)
      | s when s = v_size_b   -> MV_size_b (cv_mvcc a1)
      (************************************************************************)
      (* String                                                               *)
      (************************************************************************)
      | s when s = v_lit_string     -> MV_lit_string (a1 |> get_body_in_atom_exn)
      | s when s = v_concat_list_s  -> MV_concat_list_s (cv_mvcc a1)
      (************************************************************************)
      (* Bytes                                                                *)
      (************************************************************************)
      | s when s = v_lit_bytes      -> MV_lit_bytes (a1 |> get_body_in_atom_exn)
      | s when s = v_concat_list_b  -> MV_concat_list_b (cv_mvcc a1)
      | s when s = v_pack           -> MV_pack (cv_mvcc a1)
      | s when s = v_blake2b        -> MV_blake2b (cv_mvcc a1)
      | s when s = v_sha256         -> MV_sha256 (cv_mvcc a1)
      | s when s = v_sha512         -> MV_sha512 (cv_mvcc a1)
      (************************************************************************)
      (* Mutez                                                                *)
      (************************************************************************)
      | s when s = v_lit_mutez -> MV_lit_mutez (a1 |> get_body_in_atom_exn |> Z.of_string)
      (************************************************************************)
      (* Bool                                                                 *)
      (************************************************************************)
      | s when s = v_lit_bool -> MV_lit_bool (a1 |> get_body_in_atom_exn |> bool_of_string)
      | s when s = v_not_bb   -> MV_not_bb (cv_mvcc a1)
      (************************************************************************)
      (* Key Hash                                                             *)
      (************************************************************************)
      | s when s = v_lit_key_hash -> MV_lit_key_hash (a1 |> get_body_in_atom_exn)
      | s when s = v_hash_key     -> MV_hash_key (cv_mvcc a1)
      (************************************************************************)
      (* Timestamp                                                            *)
      (************************************************************************)
      | s when s = v_lit_timestamp_str -> MV_lit_timestamp_str (a1 |> get_body_in_atom_exn)
      | s when s = v_lit_timestamp_sec -> MV_lit_timestamp_sec (a1 |> get_body_in_atom_exn |> Z.of_string)
      (************************************************************************)
      (* Address                                                              *)
      (************************************************************************)
      | s when s = v_lit_address          -> MV_lit_address (cv_mvcc a1)
      | s when s = v_address_of_contract  -> MV_address_of_contract (cv_mvcc a1)
      (************************************************************************)
      (* Key                                                                  *)
      (************************************************************************)
      | s when s = v_lit_key -> MV_lit_key (a1 |> get_body_in_atom_exn)
      (************************************************************************)
      (* Signature                                                            *)
      (************************************************************************)
      | s when s = v_lit_signature_str -> MV_lit_signature_str (a1 |> get_body_in_atom_exn)
      (************************************************************************)
      (* Option                                                               *)
      (************************************************************************)
      | s when s = v_some   -> MV_some (cv_mvcc a1)
      | s when s = v_none   -> MV_none (cv_mtcc a1)
      | s when s = v_isnat  -> MV_isnat (cv_mvcc a1)
      (************************************************************************)
      (* List                                                                 *)
      (************************************************************************)
      | s when s = v_nil  -> MV_nil (cv_mtcc a1)
      | s when s = v_tl_l -> MV_tl_l (cv_mvcc a1)
      (************************************************************************)
      (* Set                                                                  *)
      (************************************************************************)
      | s when s = v_empty_set -> MV_empty_set (cv_mtcc a1)
      (************************************************************************)
      (* Operation                                                            *)
      (************************************************************************)
      | s when s = v_set_delegate -> MV_set_delegate (cv_mvcc a1)
      (************************************************************************)
      (* Contract                                                             *)
      (************************************************************************)
      | s when s = v_self             -> MV_self (cv_mtcc a1)
      | s when s = v_implicit_account -> MV_implicit_account (cv_mvcc a1)
      (************************************************************************)
      (* Chain Id                                                             *)
      (************************************************************************)
      | s when s = v_lit_chain_id -> MV_lit_chain_id (a1 |> get_body_in_atom_exn)
      (************************************************************************)
      (* Error                                                                *)
      (************************************************************************)
      | s -> Error ("cv_mv : Core.Sexp.List s1 : " ^ s) |> Stdlib.raise)
    | CSexp.List (s2::a1::a2::[]) -> (
      match (get_body_in_atom_exn s2) with
      (************************************************************************)
      (* Symbol & Polymorphic                                                 *)
      (************************************************************************)
      | s when s = v_symbol -> MV_symbol ((cv_mtcc a1), (a2 |> get_body_in_atom_exn))
      (************************************************************************)
      (* Integer                                                              *)
      (************************************************************************)
      | s when s = v_add_nii -> MV_add_nii ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_add_ini -> MV_add_ini ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_add_iii -> MV_add_iii ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_sub_nni -> MV_sub_nni ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_sub_nii -> MV_sub_nii ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_sub_ini -> MV_sub_ini ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_sub_iii -> MV_sub_iii ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_sub_tti -> MV_sub_tti ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_mul_nii -> MV_mul_nii ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_mul_ini -> MV_mul_ini ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_mul_iii -> MV_mul_iii ((cv_mvcc a1), (cv_mvcc a2))
      (************************************************************************)
      (* Natural Number                                                       *)
      (************************************************************************)
      | s when s = v_add_nnn    -> MV_add_nnn ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_mul_nnn    -> MV_mul_nnn ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_shiftL_nnn -> MV_shiftL_nnn ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_shiftR_nnn -> MV_shiftR_nnn ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_and_nnn    -> MV_and_nnn ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_and_inn    -> MV_and_inn ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_or_nnn     -> MV_or_nnn ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_xor_nnn    -> MV_xor_nnn ((cv_mvcc a1), (cv_mvcc a2))
      (************************************************************************)
      (* String                                                               *)
      (************************************************************************)
      | s when s = v_concat_sss -> MV_concat_sss ((cv_mvcc a1), (cv_mvcc a2))
      (************************************************************************)
      (* Bytes                                                                *)
      (************************************************************************)
      | s when s = v_concat_bbb -> MV_concat_bbb ((cv_mvcc a1), (cv_mvcc a2))
      (************************************************************************)
      (* Mutez                                                                *)
      (************************************************************************)
      | s when s = v_add_mmm -> MV_add_mmm ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_sub_mmm -> MV_sub_mmm ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_mul_mnm -> MV_mul_mnm ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_mul_nmm -> MV_mul_nmm ((cv_mvcc a1), (cv_mvcc a2))
      (************************************************************************)
      (* Bool                                                                 *)
      (************************************************************************)
      | s when s = v_and_bbb  -> MV_and_bbb ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_or_bbb   -> MV_or_bbb ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_xor_bbb  -> MV_xor_bbb ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_eq_ib    -> MV_eq_ib ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_neq_ib   -> MV_neq_ib ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_lt_ib    -> MV_lt_ib ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_gt_ib    -> MV_gt_ib ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_leq_ib   -> MV_leq_ib ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_geq_ib   -> MV_geq_ib ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_mem_xsb  -> MV_mem_xsb ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_mem_xmb  -> MV_mem_xmb ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_mem_xbmb -> MV_mem_xbmb ((cv_mvcc a1), (cv_mvcc a2))
      (************************************************************************)
      (* Timestamp                                                            *)
      (************************************************************************)
      | s when s = v_add_tit              -> MV_add_tit ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_add_itt              -> MV_add_itt ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_sub_tit              -> MV_sub_tit ((cv_mvcc a1), (cv_mvcc a2))
      (************************************************************************)
      (* Option                                                               *)
      (************************************************************************)
      | s when s = v_ediv_nnnn            -> MV_ediv_nnnn ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_ediv_niin            -> MV_ediv_niin ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_ediv_inin            -> MV_ediv_inin ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_ediv_iiin            -> MV_ediv_iiin ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_ediv_mnmm            -> MV_ediv_mnmm ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_ediv_mmnm            -> MV_ediv_mmnm ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_get_xmoy             -> MV_get_xmoy ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_get_xbmo             -> MV_get_xbmo ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = v_unpack               -> MV_unpack ((cv_mtcc a1), (cv_mvcc a2))
      | s when s = v_contract_of_address  -> MV_contract_of_address ((cv_mtcc a1), (cv_mvcc a2))
      (************************************************************************)
      (* List                                                                 *)
      (************************************************************************)
      | s when s = v_lit_list -> MV_lit_list ((cv_mtcc a1), (a2 |> get_body_in_list_exn |> Core.List.map ~f:cv_mvcc))
      | s when s = v_cons     -> MV_cons ((cv_mvcc a1), (cv_mvcc a2))
      (************************************************************************)
      (* Set                                                                  *)
      (************************************************************************)
      | s when s = v_lit_set -> MV_lit_set ((cv_mtcc a1), (a2 |> get_body_in_list_exn |> Core.List.map ~f:cv_mvcc |> Core.Set.Poly.of_list))
      (************************************************************************)
      (* Contract                                                             *)
      (************************************************************************)
      | s when s = v_lit_contract -> MV_lit_contract ((cv_mtcc a1), (cv_mvcc a2))
      (************************************************************************)
      (* Pair                                                                 *)
      (************************************************************************)
      | s when s = v_pair -> MV_pair ((cv_mvcc a1), (cv_mvcc a2))
      (************************************************************************)
      (* Or                                                                   *)
      (************************************************************************)
      | s when s = v_left   -> MV_left ((cv_mtcc a1), (cv_mvcc a2))
      | s when s = v_right  -> MV_right ((cv_mtcc a1), (cv_mvcc a2))
      (************************************************************************)
      (* Lambda                                                               *)
      (************************************************************************)
      | s when s = v_lambda_unknown -> MV_lambda_unknown ((cv_mtcc a1), (cv_mtcc a2))
      | s when s = v_lambda_closure -> MV_lambda_closure ((cv_mvcc a1), (cv_mvcc a2))
      (************************************************************************)
      (* Map                                                                  *)
      (************************************************************************)
      | s when s = v_empty_map -> MV_empty_map ((cv_mtcc a1), (cv_mtcc a2))
      (************************************************************************)
      (* Big Map                                                              *)
      (************************************************************************)
      | s when s = v_empty_big_map -> MV_empty_big_map ((cv_mtcc a1), (cv_mtcc a2))
      (************************************************************************)
      (* Custom Domain Value for Invariant Synthesis                          *)
      (************************************************************************)
      | s when s = v_sigma_lm -> MV_sigma_lm ((cv_mvcc a1), (cv_mvcc a2))
      (************************************************************************)
      (* Error                                                                *)
      (************************************************************************)
      | s -> Error ("cv_mv : Core.Sexp.List s2 : " ^ s) |> Stdlib.raise)
    | CSexp.List (s2::a1::a2::a3::[]) -> (
      match (get_body_in_atom_exn s2) with
      (************************************************************************)
      (* Bool                                                                 *)
      (************************************************************************)
      | s when s = v_check_signature -> MV_check_signature ((cv_mvcc a1), (cv_mvcc a2), (cv_mvcc a3))
      (************************************************************************)
      (* Option                                                               *)
      (************************************************************************)
      | s when s = v_slice_nnso -> MV_slice_nnso ((cv_mvcc a1), (cv_mvcc a2), (cv_mvcc a3))
      | s when s = v_slice_nnbo -> MV_slice_nnbo ((cv_mvcc a1), (cv_mvcc a2), (cv_mvcc a3))
      (************************************************************************)
      (* Set                                                                  *)
      (************************************************************************)
      | s when s = v_update_xbss -> MV_update_xbss ((cv_mvcc a1), (cv_mvcc a2), (cv_mvcc a3))
      (************************************************************************)
      (* Operation                                                            *)
      (************************************************************************)
      | s when s = v_transfer_tokens -> MV_transfer_tokens ((cv_mvcc a1), (cv_mvcc a2), (cv_mvcc a3))
      (************************************************************************)
      (* Lambda                                                               *)
      (************************************************************************)
      | s when s = v_lit_lambda -> MV_lit_lambda ((cv_mtcc a1), (cv_mtcc a2), (cv_micc a3))
      (************************************************************************)
      (* Map                                                                  *)
      (************************************************************************)
      | s when s = v_lit_map    -> MV_lit_map ((cv_mtcc a1), (cv_mtcc a2), (a3 |> get_body_in_list_exn |> Core.List.map ~f:cv_elt |> Core.Map.Poly.of_alist_exn))
      | s when s = v_update_xomm  -> MV_update_xomm ((cv_mvcc a1), (cv_mvcc a2), (cv_mvcc a3))
      (************************************************************************)
      (* Big Map                                                              *)
      (************************************************************************)
      | s when s = v_lit_big_map    -> MV_lit_map ((cv_mtcc a1), (cv_mtcc a2), (a3 |> get_body_in_list_exn |> Core.List.map ~f:cv_elt |> Core.Map.Poly.of_alist_exn))
      | s when s = v_update_xobmbm  -> MV_update_xobmbm ((cv_mvcc a1), (cv_mvcc a2), (cv_mvcc a3))
      (************************************************************************)
      (* Error                                                                *)
      (************************************************************************)
      | s -> Error ("cv_mv : Core.Sexp.List s2 : " ^ s) |> Stdlib.raise)
    | CSexp.List (s2::a1::a2::a3::a4::a5::a6::a7::[]) -> (
      match (get_body_in_atom_exn s2) with
      (************************************************************************)
      (* Operation                                                            *)
      (************************************************************************)
      | s when s = v_create_contract -> MV_create_contract ((cv_mtcc a1), (cv_mtcc a2), (cv_mvcc a3), (cv_mvcc a4), (cv_mvcc a5), (cv_mvcc a6), (cv_mvcc a7))
      (************************************************************************)
      (* Error                                                                *)
      (************************************************************************)
      | s -> Error ("cv_mv : Core.Sexp.List s2 : " ^ s) |> Stdlib.raise)
    | CSexp.List (s::_) -> Error ("cv_mv : Core.Sexp.List : " ^ (s |> get_body_in_atom_exn)) |> Stdlib.raise
    | _ -> Error ("cv_mv : _") |> Stdlib.raise)
  end (* function cv_mv end *)
  and cv_mi : csexp -> Tz.mich_i
  = let open Tz in
    let open Jc in
    let module CSexp = Core.Sexp in
    (* function cv_mi start *)
    fun x -> begin
    let body : csexp = get_template_body_exn cs_mich_t x in
    (match body with
    | CSexp.Atom s -> Error ("cv_mi : Core.Sexp.Atom : " ^ s) |> Stdlib.raise
    | CSexp.List (s0::[]) -> (
      match (get_body_in_atom_exn s0) with
      | s when s = i_swap             -> MI_swap
      | s when s = i_some             -> MI_some
      | s when s = i_unit             -> MI_unit
      | s when s = i_pair             -> MI_pair
      | s when s = i_car              -> MI_car
      | s when s = i_cdr              -> MI_cdr
      | s when s = i_cons             -> MI_cons
      | s when s = i_size             -> MI_size
      | s when s = i_mem              -> MI_mem
      | s when s = i_get              -> MI_get
      | s when s = i_update           -> MI_update
      | s when s = i_exec             -> MI_exec
      | s when s = i_failwith         -> MI_failwith
      | s when s = i_rename           -> MI_rename
      | s when s = i_concat           -> MI_concat
      | s when s = i_slice            -> MI_slice
      | s when s = i_pack             -> MI_pack
      | s when s = i_add              -> MI_add
      | s when s = i_sub              -> MI_sub
      | s when s = i_mul              -> MI_mul
      | s when s = i_ediv             -> MI_ediv
      | s when s = i_abs              -> MI_abs
      | s when s = i_isnat            -> MI_isnat
      | s when s = i_int              -> MI_int
      | s when s = i_neg              -> MI_neg
      | s when s = i_lsl              -> MI_lsl
      | s when s = i_lsr              -> MI_lsr
      | s when s = i_or               -> MI_or
      | s when s = i_and              -> MI_and
      | s when s = i_xor              -> MI_xor
      | s when s = i_not              -> MI_not
      | s when s = i_compare          -> MI_compare
      | s when s = i_eq               -> MI_eq
      | s when s = i_neq              -> MI_neq
      | s when s = i_lt               -> MI_lt
      | s when s = i_gt               -> MI_gt
      | s when s = i_le               -> MI_le
      | s when s = i_ge               -> MI_ge
      | s when s = i_self             -> MI_self
      | s when s = i_transfer_tokens  -> MI_transfer_tokens
      | s when s = i_set_delegate     -> MI_set_delegate
      | s when s = i_create_account   -> MI_create_account
      | s when s = i_implicit_account -> MI_implicit_account
      | s when s = i_now              -> MI_now
      | s when s = i_amount           -> MI_amount
      | s when s = i_balance          -> MI_balance
      | s when s = i_check_signature  -> MI_check_signature
      | s when s = i_blake2b          -> MI_blake2b
      | s when s = i_sha256           -> MI_sha256
      | s when s = i_sha512           -> MI_sha512
      | s when s = i_hash_key         -> MI_hash_key
      | s when s = i_steps_to_quota   -> MI_steps_to_quota
      | s when s = i_source           -> MI_source
      | s when s = i_sender           -> MI_sender
      | s when s = i_address          -> MI_address
      | s when s = i_chain_id         -> MI_chain_id
      | s when s = i_unpair           -> MI_unpair
      | s -> Error ("cv_mi : Core.Sexp.List s0 : " ^ s) |> Stdlib.raise)
    | CSexp.List (s1::a1::[]) -> (
      match (get_body_in_atom_exn s1) with
      | s when s = i_drop         -> MI_drop (a1 |> get_body_in_atom_exn |> Z.of_string)
      | s when s = i_dup          -> MI_dup (a1 |> get_body_in_atom_exn |> Z.of_string)
      | s when s = i_dig          -> MI_dig (a1 |> get_body_in_atom_exn |> Z.of_string)
      | s when s = i_dug          -> MI_dug (a1 |> get_body_in_atom_exn |> Z.of_string)
      | s when s = i_none         -> MI_none (cv_mtcc a1)
      | s when s = i_left         -> MI_left (cv_mtcc a1)
      | s when s = i_right        -> MI_right (cv_mtcc a1)
      | s when s = i_nil          -> MI_nil (cv_mtcc a1)
      | s when s = i_empty_set    -> MI_empty_set (cv_mtcc a1)
      | s when s = i_map          -> MI_map (cv_micc a1)
      | s when s = i_iter         -> MI_iter (cv_micc a1)
      | s when s = i_loop         -> MI_loop (cv_micc a1)
      | s when s = i_loop_left    -> MI_loop_left (cv_micc a1)
      | s when s = i_cast         -> MI_cast (cv_mtcc a1)
      | s when s = i_unpack       -> MI_unpack (cv_mtcc a1)
      | s when s = i_contract     -> MI_contract (cv_mtcc a1)
      | s when s = i_micse_check  -> MI_micse_check (cv_micc a1)
      | s -> Error ("cv_mi : Core.Sexp.List s1 : " ^ s) |> Stdlib.raise)
    | CSexp.List (s2::a1::a2::[]) -> (
      match (get_body_in_atom_exn s2) with
      | s when s = i_seq            -> MI_seq ((cv_micc a1), (cv_micc a2))
      | s when s = i_push           -> MI_push ((cv_mtcc a1), (cv_mvcc a2))
      | s when s = i_if_none        -> MI_if_none ((cv_micc a1), (cv_micc a2))
      | s when s = i_if_left        -> MI_if_left ((cv_micc a1), (cv_micc a2))
      | s when s = i_if_cons        -> MI_if_cons ((cv_micc a1), (cv_micc a2))
      | s when s = i_empty_map      -> MI_empty_map ((cv_mtcc a1), (cv_mtcc a2))
      | s when s = i_empty_big_map  -> MI_empty_big_map ((cv_mtcc a1), (cv_mtcc a2))
      | s when s = i_if             -> MI_if ((cv_micc a1), (cv_micc a2))
      | s when s = i_dip_n          -> MI_dip_n ((a1 |> get_body_in_atom_exn |> Z.of_string), (cv_micc a2))
      | s -> Error ("cv_mi : Core.Sexp.List s2 : " ^ s) |> Stdlib.raise)
    | CSexp.List (s3::a1::a2::a3::[]) -> (
      match (get_body_in_atom_exn s3) with
      | s when s = i_lambda           -> MI_lambda ((cv_mtcc a1), (cv_mtcc a2), (cv_micc a3))
      | s when s = i_create_contract  -> MI_create_contract ((cv_mtcc a1), (cv_mtcc a2), (cv_micc a3))
      | s -> Error ("cv_mi : Core.Sexp.List s3 : " ^ s) |> Stdlib.raise)
    | CSexp.List (s::_) -> Error ("cv_mt : Core.Sexp.List : " ^ (s |> get_body_in_atom_exn)) |> Stdlib.raise
    | _ -> Error ("cv_mi : _") |> Stdlib.raise)
  end (* function cv_mi end *)

  and cv_mtcc : csexp -> Tz.mich_t Tz.cc
  = fun x -> x |> cv_mt |> Tz.gen_dummy_cc (* function cv_mtcc end *)
  and cv_mvcc : csexp -> Tz.mich_v Tz.cc
  = fun x -> x |> cv_mv |> Tz.gen_dummy_cc (* function cv_mvcc end *)
  and cv_micc : csexp -> Tz.mich_i Tz.cc
  = fun x -> x |> cv_mi |> Tz.gen_dummy_cc (* function cv_micc end *)

  let rec cv_mf : csexp -> Tz.mich_f
  = let open Tz in
    let open Jc in
    let module CSexp = Core.Sexp in
    (* function cv_mf start *)
    fun x -> begin
    let body : csexp = get_template_body_exn cs_mich_t x in
    (match body with
    | CSexp.Atom s -> Error ("cv_mf : Core.Sexp.Atom : " ^ s) |> Stdlib.raise
    | CSexp.List (s0::[]) -> (
      match (get_body_in_atom_exn s0) with
      | s when s = f_true   -> MF_true
      | s when s = f_false  -> MF_false
      | s -> Error ("cv_mf : Core.Sexp.List s0 : " ^ s) |> Stdlib.raise)
    | CSexp.List (s1::a1::[]) -> (
      match (get_body_in_atom_exn s1) with
      | s when s = f_not      -> MF_not (cv_mf a1)
      | s when s = f_and      -> MF_and (a1 |> get_body_in_list_exn |> Core.List.map ~f:cv_mf)
      | s when s = f_or       -> MF_or (a1 |> get_body_in_list_exn |> Core.List.map ~f:cv_mf)
      | s when s = f_is_true  -> MF_is_true (cv_mvcc a1)
      | s when s = f_is_none  -> MF_is_none (cv_mvcc a1)
      | s when s = f_is_left  -> MF_is_left (cv_mvcc a1)
      | s when s = f_is_cons  -> MF_is_cons (cv_mvcc a1)
      | s -> Error ("cv_mf : Core.Sexp.List s1 : " ^ s) |> Stdlib.raise)
    | CSexp.List (s2::a1::a2::[]) -> (
      match (get_body_in_atom_exn s2) with
      | s when s = f_eq -> MF_eq ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = f_imply -> MF_imply ((cv_mf a1), (cv_mf a2))
      | s when s = f_add_mmm_no_overflow -> MF_add_mmm_no_overflow ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = f_sub_mmm_no_underflow -> MF_sub_mmm_no_underflow ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = f_mul_mnm_no_overflow -> MF_mul_mnm_no_overflow ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = f_mul_nmm_no_overflow -> MF_mul_nmm_no_overflow ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = f_shiftL_nnn_rhs_in_256 -> MF_shiftL_nnn_rhs_in_256 ((cv_mvcc a1), (cv_mvcc a2))
      | s when s = f_shiftR_nnn_rhs_in_256 -> MF_shiftR_nnn_rhs_in_256 ((cv_mvcc a1), (cv_mvcc a2))
      | s -> Error ("cv_mf : Core.Sexp.List s2 : " ^ s) |> Stdlib.raise)
    | CSexp.List (s::_) -> Error ("cv_mf : Core.Sexp.List : " ^ (s |> get_body_in_atom_exn)) |> Stdlib.raise
    | _ -> Error ("cv_mf : _") |> Stdlib.raise)
  end (* function cv_mf end *)

  let cv_mich_cut_category : csexp -> Tz.mich_cut_category
  = let open Tz in
    let open Jc in
    let module CSexp = Core.Sexp in
    (* function cv_mich_cut_category start *)
    fun x -> begin
    let body : csexp = get_template_body_exn cs_mich_cut_category x in
    (match body with
    | CSexp.Atom s -> (
      match s with
      | s when s = mcc_trx_entry    -> MCC_trx_entry
      | s when s = mcc_trx_exit     -> MCC_trx_exit
      | s when s = mcc_ln_loop      -> MCC_ln_loop
      | s when s = mcc_ln_loopleft  -> MCC_ln_loopleft
      | s when s = mcc_ln_map       -> MCC_ln_map
      | s when s = mcc_ln_iter      -> MCC_ln_iter
      | s when s = mcc_lb_loop      -> MCC_lb_loop
      | s when s = mcc_lb_loopleft  -> MCC_lb_loopleft
      | s when s = mcc_lb_map       -> MCC_lb_map
      | s when s = mcc_lb_iter      -> MCC_lb_iter
      | s when s = mcc_query        -> MCC_query
      | s -> Error ("cv_mich_cut_category : Core.Sexp.List s : " ^ s) |> Stdlib.raise)
    | CSexp.List (s::_) -> Error ("cv_mich_cut_category : Core.Sexp.List : " ^ (s |> get_body_in_atom_exn)) |> Stdlib.raise
    | _ -> Error ("cv_mich_cut_category : _") |> Stdlib.raise)
  end (* function cv_mich_cut_category end *)

  let cv_mich_cut_info : csexp -> Tz.mich_cut_info
  = let open Tz in
    let open Jc in
    let module CSexp = Core.Sexp in
    (* function cv_mich_cut_info start *)
    fun x -> begin
    let body : csexp = get_template_body_exn cs_mich_cut_info x in
    (match body with
    | CSexp.Atom s -> Error ("cv_mich_cut_info : Core.Sexp.Atom : " ^ s) |> Stdlib.raise
    | CSexp.List (a1::a2::[]) -> { mci_loc=(cv_loc a1); mci_cutcat=(cv_mich_cut_category a2); }
    | CSexp.List (s::_) -> Error ("cv_mich_cut_info : Core.Sexp.List : " ^ (s |> get_body_in_atom_exn)) |> Stdlib.raise
    | _ -> Error ("cv_mich_cut_info : _") |> Stdlib.raise)
  end (* function cv_mich_cut_info end *)
end (* module CS2T end *)