(* Sexp - Sexpression of Tz *)


(******************************************************************************)
(******************************************************************************)
(* Tz to Core.Sexp.t                                                          *)
(******************************************************************************)
(******************************************************************************)

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
    (**************************************************************************)
    (* Symbol & Polymorphic                                                   *)
    (**************************************************************************)
    | MV_symbol (t,v)     -> t1v1 v_symbol t v
    | MV_car e            -> s1   v_car e
    | MV_cdr e            -> s1   v_cdr e
    | MV_unlift_option e  -> s1   v_unlift_option e
    | MV_unlift_left e    -> s1   v_unlift_left e
    | MV_unlift_right e   -> s1   v_unlift_right e
    | MV_hd_l e           -> s1   v_hd_l e
  
    (**************************************************************************)
    (* Integer                                                                *)
    (**************************************************************************)
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
  
    (**************************************************************************)
    (* Natural Number                                                         *)
    (**************************************************************************)
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
  
    (**************************************************************************)
    (* String                                                                 *)
    (**************************************************************************)
    | MV_lit_string s         -> s0v1 v_lit_string s
    | MV_concat_sss (e1, e2)  -> s2   v_concat_sss e1 e2
    | MV_concat_list_s e      -> s1   v_concat_list_s e
  
    (**************************************************************************)
    (* Bytes                                                                  *)
    (**************************************************************************)
    | MV_lit_bytes s          -> s0v1 v_lit_bytes s
    | MV_concat_bbb (e1, e2)  -> s2   v_concat_bbb e1 e2
    | MV_concat_list_b e      -> s1   v_concat_list_b e
    | MV_pack e               -> s1   v_pack e
    | MV_blake2b e            -> s1   v_blake2b e
    | MV_sha256  e            -> s1   v_sha256 e
    | MV_sha512  e            -> s1   v_sha512 e
  
    (**************************************************************************)
    (* Mutez                                                                  *)
    (**************************************************************************)
    | MV_lit_mutez zn     -> s0v1 v_lit_mutez (Z.to_string zn)
    | MV_add_mmm (e1, e2) -> s2   v_add_mmm e1 e2
    | MV_sub_mmm (e1, e2) -> s2   v_sub_mmm e1 e2
    | MV_mul_mnm (e1, e2) -> s2   v_mul_mnm e1 e2
    | MV_mul_nmm (e1, e2) -> s2   v_mul_nmm e1 e2
  
    (**************************************************************************)
    (* Bool                                                                   *)
    (**************************************************************************)
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
  
    (**************************************************************************)
    (* Key Hash                                                               *)
    (**************************************************************************)
    | MV_lit_key_hash s -> s0v1 v_lit_key_hash s
    | MV_hash_key e     -> s1   v_hash_key e
  
    (**************************************************************************)
    (* Timestamp                                                              *)
    (**************************************************************************)
    | MV_lit_timestamp_str s  -> s0v1 v_lit_timestamp_str s
    | MV_lit_timestamp_sec zn -> s0v1 v_lit_timestamp_sec (Z.to_string zn)
    | MV_add_tit (e1, e2)     -> s2   v_add_tit e1 e2
    | MV_add_itt (e1, e2)     -> s2   v_add_itt e1 e2
    | MV_sub_tit (e1, e2)     -> s2   v_sub_tit e1 e2
  
    (**************************************************************************)
    (* Address                                                                *)
    (**************************************************************************)
    | MV_lit_address e          -> s1 v_lit_address e
    | MV_address_of_contract e  -> s1 v_address_of_contract e
  
    (**************************************************************************)
    (* Key                                                                    *)
    (**************************************************************************)
    | MV_lit_key s -> s0v1 v_lit_key s
  
    (**************************************************************************)
    (* Unit                                                                   *)
    (**************************************************************************)
    | MV_unit -> s0 v_unit
  
    (**************************************************************************)
    (* Signature                                                              *)
    (**************************************************************************)
    | MV_lit_signature_str s            -> s0v1 v_lit_signature_str s
    | MV_lit_signature_signed (e1, e2)  -> s2   v_lit_signature_signed e1 e2
  
    (**************************************************************************)
    (* Option                                                                 *)
    (**************************************************************************)
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
  
    (**************************************************************************)
    (* List                                                                   *)
    (**************************************************************************)
    | MV_lit_list (t, el) -> t1sl v_lit_list t (Core.List.map el ~f:cv_mvcc)
    | MV_nil t            -> s0t1 v_nil t
    | MV_cons (e1, e2)    -> s2   v_cons e1 e2
    | MV_tl_l e           -> s1   v_tl_l e
  
    (**************************************************************************)
    (* Set                                                                    *)
    (**************************************************************************)
    | MV_lit_set (elt, es)        -> t1sl v_lit_set elt (Core.Set.Poly.map es ~f:cv_mvcc |> Core.Set.Poly.to_list)
    | MV_empty_set elt            -> s0t1 v_empty_set elt
    | MV_update_xbss (e1, e2, e3) -> s3   v_update_xbss e1 e2 e3
  
    (**************************************************************************)
    (* Operation                                                              *)
    (**************************************************************************)
    | MV_create_contract (pt, st, e1, e2, e3, e4, e5) -> t2s5 v_create_contract pt st e1 e2 e3 e4 e5
    | MV_transfer_tokens (e1, e2, e3)                 -> s3   v_transfer_tokens e1 e2 e3
    | MV_set_delegate e                               -> s1   v_set_delegate e
  
    (**************************************************************************)
    (* Contract                                                               *)
    (**************************************************************************)
    | MV_lit_contract (t, e)  -> t1s1 v_lit_contract t e
    | MV_self t               -> s0t1 v_self t
    | MV_implicit_account e   -> s1   v_implicit_account e
  
    (**************************************************************************)
    (* Pair                                                                   *)
    (**************************************************************************)
    | MV_pair (e1, e2) -> s2 v_pair e1 e2
  
    (**************************************************************************)
    (* Or                                                                     *)
    (**************************************************************************)
    | MV_left (t, e)  -> t1s1 v_left t e
    | MV_right (t, e) -> t1s1 v_right t e
  
    (**************************************************************************)
    (* Lambda                                                                 *)
    (**************************************************************************)
    | MV_lit_lambda (pt, rt, i)  -> t2i1  v_lit_lambda pt rt i
    | MV_lambda_unknown (pt, rt) -> s0t2  v_lambda_unknown pt rt
    | MV_lambda_closure (e1, e2) -> s2    v_lambda_closure e1 e2

    (**************************************************************************)
    (* Map                                                                    *)
    (**************************************************************************)
    | MV_lit_map (kt, vt, e)      -> t2sl v_lit_map kt vt (Core.Map.Poly.to_alist e |> Core.List.map ~f:(fun (k, v) -> s2 v_elt k v))
    | MV_empty_map (kt, vt)       -> s0t2 v_empty_map kt vt
    | MV_update_xomm (e1, e2, e3) -> s3   v_update_xomm e1 e2 e3

    (**************************************************************************)
    (* Big Map                                                                *)
    (**************************************************************************)
    | MV_lit_big_map (kt, vt, e)    -> t2sl v_lit_big_map kt vt (Core.Map.Poly.to_alist e |> Core.List.map ~f:(fun (k, v) -> s2 v_elt k v))
    | MV_empty_big_map (kt, vt)     -> s0t2 v_empty_big_map kt vt
    | MV_update_xobmbm (e1, e2, e3) -> s3   v_update_xobmbm e1 e2 e3

    (**************************************************************************)
    (* Chain Id                                                               *)
    (**************************************************************************)
    | MV_lit_chain_id s -> s0v1 v_lit_chain_id s

    (**************************************************************************)
    (* Custom Domain Value for Invariant Synthesis                            *)
    (**************************************************************************)
    | MV_sigma_tmplm e1 -> s1 v_sigma_tmplm e1)
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
    | MI_apply                      -> s0   i_apply
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


(******************************************************************************)
(******************************************************************************)
(* Tz to Core.Sexp.t (No CC)                                                  *)
(******************************************************************************)
(******************************************************************************)

module T2CSnocc = struct
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
    (**************************************************************************)
    (* Symbol & Polymorphic                                                   *)
    (**************************************************************************)
    | MV_symbol (t,v)     -> t1v1 v_symbol t v
    | MV_car e            -> s1   v_car e
    | MV_cdr e            -> s1   v_cdr e
    | MV_unlift_option e  -> s1   v_unlift_option e
    | MV_unlift_left e    -> s1   v_unlift_left e
    | MV_unlift_right e   -> s1   v_unlift_right e
    | MV_hd_l e           -> s1   v_hd_l e
  
    (**************************************************************************)
    (* Integer                                                                *)
    (**************************************************************************)
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
  
    (**************************************************************************)
    (* Natural Number                                                         *)
    (**************************************************************************)
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
  
    (**************************************************************************)
    (* String                                                                 *)
    (**************************************************************************)
    | MV_lit_string s         -> s0v1 v_lit_string s
    | MV_concat_sss (e1, e2)  -> s2   v_concat_sss e1 e2
    | MV_concat_list_s e      -> s1   v_concat_list_s e
  
    (**************************************************************************)
    (* Bytes                                                                  *)
    (**************************************************************************)
    | MV_lit_bytes s          -> s0v1 v_lit_bytes s
    | MV_concat_bbb (e1, e2)  -> s2   v_concat_bbb e1 e2
    | MV_concat_list_b e      -> s1   v_concat_list_b e
    | MV_pack e               -> s1   v_pack e
    | MV_blake2b e            -> s1   v_blake2b e
    | MV_sha256  e            -> s1   v_sha256 e
    | MV_sha512  e            -> s1   v_sha512 e
  
    (**************************************************************************)
    (* Mutez                                                                  *)
    (**************************************************************************)
    | MV_lit_mutez zn     -> s0v1 v_lit_mutez (Z.to_string zn)
    | MV_add_mmm (e1, e2) -> s2   v_add_mmm e1 e2
    | MV_sub_mmm (e1, e2) -> s2   v_sub_mmm e1 e2
    | MV_mul_mnm (e1, e2) -> s2   v_mul_mnm e1 e2
    | MV_mul_nmm (e1, e2) -> s2   v_mul_nmm e1 e2
  
    (**************************************************************************)
    (* Bool                                                                   *)
    (**************************************************************************)
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
  
    (**************************************************************************)
    (* Key Hash                                                               *)
    (**************************************************************************)
    | MV_lit_key_hash s -> s0v1 v_lit_key_hash s
    | MV_hash_key e     -> s1   v_hash_key e
  
    (**************************************************************************)
    (* Timestamp                                                              *)
    (**************************************************************************)
    | MV_lit_timestamp_str s  -> s0v1 v_lit_timestamp_str s
    | MV_lit_timestamp_sec zn -> s0v1 v_lit_timestamp_sec (Z.to_string zn)
    | MV_add_tit (e1, e2)     -> s2   v_add_tit e1 e2
    | MV_add_itt (e1, e2)     -> s2   v_add_itt e1 e2
    | MV_sub_tit (e1, e2)     -> s2   v_sub_tit e1 e2
  
    (**************************************************************************)
    (* Address                                                                *)
    (**************************************************************************)
    | MV_lit_address e          -> s1 v_lit_address e
    | MV_address_of_contract e  -> s1 v_address_of_contract e
  
    (**************************************************************************)
    (* Key                                                                    *)
    (**************************************************************************)
    | MV_lit_key s -> s0v1 v_lit_key s
  
    (**************************************************************************)
    (* Unit                                                                   *)
    (**************************************************************************)
    | MV_unit -> s0 v_unit
  
    (**************************************************************************)
    (* Signature                                                              *)
    (**************************************************************************)
    | MV_lit_signature_str s            -> s0v1 v_lit_signature_str s
    | MV_lit_signature_signed (e1, e2)  -> s2   v_lit_signature_signed e1 e2
  
    (**************************************************************************)
    (* Option                                                                 *)
    (**************************************************************************)
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
  
    (**************************************************************************)
    (* List                                                                   *)
    (**************************************************************************)
    | MV_lit_list (t, el) -> t1sl v_lit_list t (Core.List.map el ~f:cv_mvcc)
    | MV_nil t            -> s0t1 v_nil t
    | MV_cons (e1, e2)    -> s2   v_cons e1 e2
    | MV_tl_l e           -> s1   v_tl_l e
  
    (**************************************************************************)
    (* Set                                                                    *)
    (**************************************************************************)
    | MV_lit_set (elt, es)        -> t1sl v_lit_set elt (Core.Set.Poly.map es ~f:cv_mvcc |> Core.Set.Poly.to_list)
    | MV_empty_set elt            -> s0t1 v_empty_set elt
    | MV_update_xbss (e1, e2, e3) -> s3   v_update_xbss e1 e2 e3
  
    (**************************************************************************)
    (* Operation                                                              *)
    (**************************************************************************)
    | MV_create_contract (pt, st, e1, e2, e3, e4, e5) -> t2s5 v_create_contract pt st e1 e2 e3 e4 e5
    | MV_transfer_tokens (e1, e2, e3)                 -> s3   v_transfer_tokens e1 e2 e3
    | MV_set_delegate e                               -> s1   v_set_delegate e
  
    (**************************************************************************)
    (* Contract                                                               *)
    (**************************************************************************)
    | MV_lit_contract (t, e)  -> t1s1 v_lit_contract t e
    | MV_self t               -> s0t1 v_self t
    | MV_implicit_account e   -> s1   v_implicit_account e
  
    (**************************************************************************)
    (* Pair                                                                   *)
    (**************************************************************************)
    | MV_pair (e1, e2) -> s2 v_pair e1 e2
  
    (**************************************************************************)
    (* Or                                                                     *)
    (**************************************************************************)
    | MV_left (t, e)  -> t1s1 v_left t e
    | MV_right (t, e) -> t1s1 v_right t e
  
    (**************************************************************************)
    (* Lambda                                                                 *)
    (**************************************************************************)
    | MV_lit_lambda (pt, rt, i)  -> t2i1  v_lit_lambda pt rt i
    | MV_lambda_unknown (pt, rt) -> s0t2  v_lambda_unknown pt rt
    | MV_lambda_closure (e1, e2) -> s2    v_lambda_closure e1 e2

    (**************************************************************************)
    (* Map                                                                    *)
    (**************************************************************************)
    | MV_lit_map (kt, vt, e)      -> t2sl v_lit_map kt vt (Core.Map.Poly.to_alist e |> Core.List.map ~f:(fun (k, v) -> s2 v_elt k v))
    | MV_empty_map (kt, vt)       -> s0t2 v_empty_map kt vt
    | MV_update_xomm (e1, e2, e3) -> s3   v_update_xomm e1 e2 e3

    (**************************************************************************)
    (* Big Map                                                                *)
    (**************************************************************************)
    | MV_lit_big_map (kt, vt, e)    -> t2sl v_lit_big_map kt vt (Core.Map.Poly.to_alist e |> Core.List.map ~f:(fun (k, v) -> s2 v_elt k v))
    | MV_empty_big_map (kt, vt)     -> s0t2 v_empty_big_map kt vt
    | MV_update_xobmbm (e1, e2, e3) -> s3   v_update_xobmbm e1 e2 e3

    (**************************************************************************)
    (* Chain Id                                                               *)
    (**************************************************************************)
    | MV_lit_chain_id s -> s0v1 v_lit_chain_id s

    (**************************************************************************)
    (* Custom Domain Value for Invariant Synthesis                            *)
    (**************************************************************************)
    | MV_sigma_tmplm e1 -> s1 v_sigma_tmplm e1)
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
    | MI_apply                      -> s0   i_apply
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
  = fun x -> cv_mt x.cc_v (* function cv_mtcc end *)
  and cv_mvcc : Tz.mich_v Tz.cc -> csexp
  = fun x -> cv_mv x.cc_v (* function cv_mvcc end *)
  and cv_micc : Tz.mich_i Tz.cc -> csexp
  = fun x -> cv_mi x.cc_v (* function cv_micc end *)

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


(******************************************************************************)
(******************************************************************************)
(* Core.Sexp.t to Tz (No CC)                                                  *)
(******************************************************************************)
(******************************************************************************)

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
      (* Custom Domain Value for Invariant Synthesis                          *)
      (************************************************************************)
      | s when s = v_sigma_tmplm -> MV_sigma_tmplm (cv_mvcc a1)
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
      | s when s = i_apply            -> MI_apply
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