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

