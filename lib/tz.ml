(* Tz : MicSE's Michelson representation *)

open Core

(*****************************************************************************)
(*****************************************************************************)
(* Code Component                                                            *)
(*****************************************************************************)
(*****************************************************************************)

type ccp_pos = {
  col : int;
  lin : int;
}
[@@deriving sexp, compare, equal]

type ccp_loc =
  | CCLOC_Unknown
  | CCLOC_Pos     of ccp_pos * ccp_pos
[@@deriving sexp, compare, equal]

type ccp_annot =
  (* :type_annot  *)
  | CCA_typ of string
  (* @var_annot   *)
  | CCA_var of string
  (* %field_annot *)
  | CCA_fld of string
[@@deriving sexp, compare, equal]

type 'a cc = {
  (* code component *)
  cc_loc : (ccp_loc[@sexp.opaque]);
  cc_anl : (ccp_annot list[@sexp.opaque]);
  cc_v : 'a;
}
[@@deriving sexp, compare, equal]

let gen_dummy_cc : 'a -> 'a cc =
  (fun x -> { cc_loc = CCLOC_Unknown; cc_anl = []; cc_v = x })

(*****************************************************************************)
(*****************************************************************************)
(* Tezos Types                                                               *)
(*****************************************************************************)
(*****************************************************************************)

type mich_t =
  | MT_key
  | MT_unit
  | MT_signature
  | MT_option    of mich_t cc
  | MT_list      of mich_t cc
  | MT_set       of mich_t cc
  | MT_operation
  | MT_contract  of mich_t cc
  | MT_pair      of mich_t cc * mich_t cc
  | MT_or        of mich_t cc * mich_t cc
  | MT_lambda    of mich_t cc * mich_t cc
  | MT_map       of mich_t cc * mich_t cc
  | MT_big_map   of mich_t cc * mich_t cc
  | MT_chain_id
  | MT_int
  | MT_nat
  | MT_string
  | MT_bytes
  | MT_mutez
  | MT_bool
  | MT_key_hash
  | MT_timestamp
  | MT_address

(*****************************************************************************)
(*****************************************************************************)
(* Michelson Values & Instructions                                           *)
(*****************************************************************************)
(*****************************************************************************)
and mich_v =
  (* Michelson Value *)

  (*************************************************************************)
  (* Symbol & Polymorphic                                                  *)
  (*************************************************************************)
  | MV_symbol               of (mich_t cc * string)
  | MV_car                  of mich_v cc (* ('a, 'b) pair -> 'a *)
  | MV_cdr                  of mich_v cc (* ('a, 'b) pair -> 'b *)
  | MV_unlift_option        of mich_v cc (* 'a option -> 'a *)
  | MV_unlift_left          of mich_v cc (* ('a, 'b) or -> 'a *)
  | MV_unlift_right         of mich_v cc (* ('a, 'b) or -> 'b *)
  | MV_hd_l                 of mich_v cc (* 'a list -> 'a *)
  (*************************************************************************)
  (* Integer                                                               *)
  (*************************************************************************)
  | MV_lit_int              of Bigint.t
  | MV_neg_ni               of mich_v cc (* nat -> int *)
  | MV_neg_ii               of mich_v cc (* int -> int *)
  | MV_not_ni               of mich_v cc (* nat -> int *)
  | MV_not_ii               of mich_v cc (* int -> int *)
  | MV_add_nii              of mich_v cc * mich_v cc (* nat * int -> int *)
  | MV_add_ini              of mich_v cc * mich_v cc (* int * nat -> int *)
  | MV_add_iii              of mich_v cc * mich_v cc (* int * int -> int *)
  | MV_sub_nni              of mich_v cc * mich_v cc (* nat * nat -> int *)
  | MV_sub_nii              of mich_v cc * mich_v cc (* nat * int -> int *)
  | MV_sub_ini              of mich_v cc * mich_v cc (* int * nat -> int *)
  | MV_sub_iii              of mich_v cc * mich_v cc (* int * int -> int *)
  | MV_sub_tti              of mich_v cc * mich_v cc (* timestamp * timestamp -> int *)
  | MV_mul_nii              of mich_v cc * mich_v cc (* nat * int -> int *)
  | MV_mul_ini              of mich_v cc * mich_v cc (* int * nat -> int *)
  | MV_mul_iii              of mich_v cc * mich_v cc (* int * int -> int *)
  | MV_compare              of mich_v cc * mich_v cc (* 'a * 'a -> int *)
  | MV_int_of_nat           of mich_v cc (* nat -> int *)
  (*************************************************************************)
  (* Natural Number                                                        *)
  (*************************************************************************)
  | MV_lit_nat              of Bigint.t
  | MV_abs_in               of mich_v cc (* int -> nat *)
  | MV_add_nnn              of mich_v cc * mich_v cc (* nat * nat -> nat *)
  | MV_mul_nnn              of mich_v cc * mich_v cc (* nat * nat -> nat *)
  | MV_shiftL_nnn           of mich_v cc * mich_v cc (* nat * nat -> nat *)
  | MV_shiftR_nnn           of mich_v cc * mich_v cc (* nat * nat -> nat *)
  | MV_and_nnn              of mich_v cc * mich_v cc (* nat * nat -> nat *)
  | MV_and_inn              of mich_v cc * mich_v cc (* int * nat -> nat *)
  | MV_or_nnn               of mich_v cc * mich_v cc (* nat * nat -> nat *)
  | MV_xor_nnn              of mich_v cc * mich_v cc (* nat * nat -> nat *)
  | MV_size_s               of mich_v cc (* 'a set -> nat *)
  | MV_size_m               of mich_v cc (* ('k, 'v) map -> nat *)
  | MV_size_l               of mich_v cc (* 'a list -> nat *)
  | MV_size_str             of mich_v cc (* string -> nat *)
  | MV_size_b               of mich_v cc (* bytes -> nat *)
  (*************************************************************************)
  (* String                                                                *)
  (*************************************************************************)
  | MV_lit_string           of string
  | MV_concat_sss           of mich_v cc * mich_v cc (* string * string -> string *)
  | MV_concat_list_s        of mich_v cc (* string list -> string *)
  (*************************************************************************)
  (* Bytes                                                                 *)
  (*************************************************************************)
  | MV_lit_bytes            of string
  | MV_concat_bbb           of mich_v cc * mich_v cc (* bytes * bytes -> bytes *)
  | MV_concat_list_b        of mich_v cc (* bytes list -> bytes *)
  | MV_pack                 of mich_v cc (* 'a -> bytes *)
  | MV_blake2b              of mich_v cc (* bytes -> bytes *)
  | MV_sha256               of mich_v cc (* bytes -> bytes *)
  | MV_sha512               of mich_v cc (* bytes -> bytes *)
  (*************************************************************************)
  (* Mutez                                                                 *)
  (*************************************************************************)
  | MV_lit_mutez            of Bigint.t
  | MV_add_mmm              of mich_v cc * mich_v cc (* mutez * mutez -> mutez *)
  | MV_sub_mmm              of mich_v cc * mich_v cc (* mutez * mutez -> mutez *)
  | MV_mul_mnm              of mich_v cc * mich_v cc (* mutez * nat -> mutez *)
  | MV_mul_nmm              of mich_v cc * mich_v cc (* nat * mutez -> mutez *)
  (*************************************************************************)
  (* Bool                                                                  *)
  (*************************************************************************)
  | MV_lit_bool             of bool
  | MV_not_bb               of mich_v cc (* bool -> bool *)
  | MV_and_bbb              of mich_v cc * mich_v cc (* bool * bool -> bool *)
  | MV_or_bbb               of mich_v cc * mich_v cc (* bool * bool -> bool *)
  | MV_xor_bbb              of mich_v cc * mich_v cc (* bool * bool -> bool *)
  | MV_eq_ib                of mich_v cc * mich_v cc (* int -> int -> bool *)
  | MV_neq_ib               of mich_v cc * mich_v cc (* int -> int -> bool *)
  | MV_lt_ib                of mich_v cc * mich_v cc (* int -> int -> bool *)
  | MV_gt_ib                of mich_v cc * mich_v cc (* int -> int -> bool *)
  | MV_leq_ib               of mich_v cc * mich_v cc (* int -> int -> bool *)
  | MV_geq_ib               of mich_v cc * mich_v cc (* int -> int -> bool *)
  | MV_mem_xsb              of mich_v cc * mich_v cc (* 'a * 'a set -> bool *)
  | MV_mem_xmb              of mich_v cc * mich_v cc (* 'k * ('k, 'v) map -> bool *)
  | MV_mem_xbmb             of mich_v cc * mich_v cc (* 'k * ('k, 'v) big_map -> bool *)
  | MV_check_signature      of mich_v cc * mich_v cc * mich_v cc (* key * signature * bytes -> bool *)
  (*************************************************************************)
  (* Key Hash                                                              *)
  (*************************************************************************)
  | MV_lit_key_hash         of string
  | MV_hash_key             of mich_v cc (* key -> key_hash *)
  (*************************************************************************)
  (* Timestamp                                                             *)
  (*************************************************************************)
  | MV_lit_timestamp_str    of string
  | MV_lit_timestamp_sec    of Bigint.t
  | MV_add_tit              of mich_v cc * mich_v cc (* timestamp * int -> timestamp *)
  | MV_add_itt              of mich_v cc * mich_v cc (* int * timestamp -> timestamp *)
  | MV_sub_tit              of mich_v cc * mich_v cc (* timestamp * int -> timestamp *)
  (*************************************************************************)
  (* Address                                                               *)
  (*************************************************************************)
  | MV_lit_address          of mich_v cc (* key_hash -> address *)
  | MV_address_of_contract  of mich_v cc (* 'a contract -> address *)
  (*************************************************************************)
  (* Key                                                                   *)
  (*************************************************************************)
  | MV_lit_key              of string
  (*************************************************************************)
  (* Unit                                                                  *)
  (*************************************************************************)
  | MV_unit
  (*************************************************************************)
  (* Signature                                                             *)
  (*************************************************************************)
  | MV_lit_signature_str    of string
  | MV_lit_signature_signed of mich_v cc * mich_v cc (* key * bytes -> signature *)
  (*************************************************************************)
  (* Option                                                                *)
  (*************************************************************************)
  | MV_some                 of mich_v cc (* 'a -> 'a option *)
  | MV_none                 of mich_t cc (* ('a) -> 'a option *)
  | MV_ediv_nnnn            of mich_v cc * mich_v cc (* nat * nat -> (nat, nat) pair option *)
  | MV_ediv_niin            of mich_v cc * mich_v cc (* nat * int -> (int, nat) pair option *)
  | MV_ediv_inin            of mich_v cc * mich_v cc (* int * nat -> (int, nat) pair option *)
  | MV_ediv_iiin            of mich_v cc * mich_v cc (* int * int -> (int, nat) pair option *)
  | MV_ediv_mnmm            of mich_v cc * mich_v cc (* mutez * nat -> (mutez, mutez) pair option *)
  | MV_ediv_mmnm            of mich_v cc * mich_v cc (* mutez * mutez -> (nat, mutez) pair option *)
  | MV_get_xmoy             of mich_v cc * mich_v cc (* 'k * ('k, 'v) map -> 'v option *)
  | MV_get_xbmo             of mich_v cc * mich_v cc (* 'k * ('k, 'v) big_map -> 'v option *)
  | MV_slice_nnso           of mich_v cc * mich_v cc * mich_v cc (* nat * nat * string -> string option *)
  | MV_slice_nnbo           of mich_v cc * mich_v cc * mich_v cc (* nat * nat * bytes -> bytes option *)
  | MV_unpack               of mich_t cc * mich_v cc (* ('a) * bytes -> 'a option *)
  | MV_contract_of_address  of mich_t cc * mich_v cc (* ('a) -> address -> 'a contract option *)
  | MV_isnat                of mich_v cc (* int -> nat option *)
  (*************************************************************************)
  (* List                                                                  *)
  (*************************************************************************)
  | MV_lit_list             of mich_t cc * mich_v cc list (* ('a) * list-literal -> 'a list *)
  | MV_nil                  of mich_t cc (* ('a) -> 'a list *)
  | MV_cons                 of mich_v cc * mich_v cc (* 'a * 'a list -> 'a list *)
  | MV_tl_l                 of mich_v cc (* 'a list -> 'a list *)
  (*************************************************************************)
  (* Set                                                                   *)
  (*************************************************************************)
  | MV_lit_set              of mich_t cc * mich_v cc Core.List.t (* ('a) * set-literal (list) -> 'a set *)
  | MV_empty_set            of mich_t cc (* ('a) -> 'a set *)
  | MV_update_xbss          of mich_v cc * mich_v cc * mich_v cc (* 'a * bool * 'a set -> 'a set *)
  (*************************************************************************)
  (* Operation                                                             *)
  (*************************************************************************)
  | MV_create_contract      of
      mich_t cc
      * mich_t cc
      * mich_v cc
      * mich_v cc
      * mich_v cc
      * mich_v cc
      * mich_v cc (* ('param) * ('strg) * (('param, 'strg) pair, (operation list, 'strg) pair) lambda * key_hash option * mutez * 'strg * address -> operation *)
  | MV_transfer_tokens      of mich_v cc * mich_v cc * mich_v cc (* 'a * mutez * 'a contract -> operation *)
  | MV_set_delegate         of mich_v cc (* key_hash option -> operation *)
  (*************************************************************************)
  (* Contract                                                              *)
  (*************************************************************************)
  | MV_lit_contract         of mich_t cc * mich_v cc (* ('a) * address -> 'a contract *)
  | MV_self                 of mich_t cc (* 'a -> 'a contract *)
  | MV_implicit_account     of mich_v cc (* key_hash -> unit contract *)
  (*************************************************************************)
  (* Pair                                                                  *)
  (*************************************************************************)
  | MV_pair                 of mich_v cc * mich_v cc (* 'a * 'b -> ('a, 'b) pair *)
  (*************************************************************************)
  (* Or                                                                    *)
  (*************************************************************************)
  | MV_left                 of mich_t cc * mich_v cc (* (('a, 'b) or) * 'a -> ('a, 'b) or *)
  | MV_right                of mich_t cc * mich_v cc (* (('a, 'b) or) * 'b -> ('a, 'b) or *)
  (*************************************************************************)
  (* Lambda                                                                *)
  (*************************************************************************)
  | MV_lit_lambda           of mich_t cc * mich_t cc * mich_i cc (* ('param) * ('ret) * ('param, 'ret) Mich.inst Mich.t -> ('param, 'ret) lambda *)
  (* embedded code with LAMBDA Michelson-instruction should be expressed with V_lambda_id, not V_lit_lambda *)
  | MV_lambda_unknown       of mich_t cc * mich_t cc (* ('param) * ('ret) -> ('param, 'ret) lambda *)
  | MV_lambda_closure       of mich_v cc * mich_v cc (* (('p1, 'p2) pair, 'ret) lambda * 'p1 -> ('p2, 'ret) lambda *)
  (*************************************************************************)
  (* Map                                                                   *)
  (*************************************************************************)
  | MV_lit_map              of
      mich_t cc * mich_t cc * (mich_v cc * mich_v cc) list (* ('k) * ('v) * map-literal(alist) -> ('k, 'v) map *)
  | MV_empty_map            of mich_t cc * mich_t cc (* ('k) * ('v) -> ('k, 'v) map *)
  | MV_update_xomm          of mich_v cc * mich_v cc * mich_v cc (* 'k * 'v option * ('k, 'v) map -> ('k, 'v) map *)
  (*************************************************************************)
  (* Big Map                                                               *)
  (*************************************************************************)
  | MV_lit_big_map          of
      mich_t cc * mich_t cc * (mich_v cc * mich_v cc) list (* ('k) * ('v) * map-literal -> ('k, 'v) big_map *)
  | MV_empty_big_map        of mich_t cc * mich_t cc (* ('k) * ('v) -> ('k, 'v) big_map *)
  | MV_update_xobmbm        of mich_v cc * mich_v cc * mich_v cc (* 'k * 'v option * ('k, 'v) big_map -> ('k, 'v) big_map *)
  (*************************************************************************)
  (* Chain Id                                                              *)
  (*************************************************************************)
  | MV_lit_chain_id         of string
  (*************************************************************************)
  (* Custom Domain Value for Invariant Synthesis                           *)
  (*************************************************************************)
  | MV_sigma_tmplm          of mich_v cc
(* (timestamp * mutez) list -> mutez *)

and mich_i =
  (* Michelson Instruction *)
  | MI_seq              of mich_i cc * mich_i cc
  | MI_drop             of Bigint.t
  | MI_dup              of Bigint.t
  | MI_swap
  | MI_dig              of Bigint.t
  | MI_dug              of Bigint.t
  | MI_push             of mich_t cc * mich_v cc
  | MI_some
  | MI_none             of mich_t cc
  | MI_unit
  | MI_if_none          of mich_i cc * mich_i cc
  | MI_pair
  | MI_car
  | MI_cdr
  | MI_left             of mich_t cc
  | MI_right            of mich_t cc
  | MI_if_left          of mich_i cc * mich_i cc
  | MI_nil              of mich_t cc
  | MI_cons
  | MI_if_cons          of mich_i cc * mich_i cc
  | MI_size
  | MI_empty_set        of mich_t cc
  | MI_empty_map        of mich_t cc * mich_t cc
  | MI_empty_big_map    of mich_t cc * mich_t cc
  | MI_map              of mich_i cc
  | MI_iter             of mich_i cc
  | MI_mem
  | MI_get
  | MI_update
  | MI_if               of mich_i cc * mich_i cc
  | MI_loop             of mich_i cc
  | MI_loop_left        of mich_i cc
  | MI_lambda           of mich_t cc * mich_t cc * mich_i cc
  | MI_exec
  | MI_apply
  | MI_dip_n            of Bigint.t * mich_i cc
  | MI_failwith
  | MI_cast             of mich_t cc
  | MI_rename
  | MI_concat
  | MI_slice
  | MI_pack
  | MI_unpack           of mich_t cc
  | MI_add
  | MI_sub
  | MI_mul
  | MI_ediv
  | MI_abs
  | MI_isnat
  | MI_int
  | MI_neg
  | MI_lsl
  | MI_lsr
  | MI_or
  | MI_and
  | MI_xor
  | MI_not
  | MI_compare
  | MI_eq
  | MI_neq
  | MI_lt
  | MI_gt
  | MI_le
  | MI_ge
  | MI_self
  | MI_contract         of mich_t cc
  | MI_transfer_tokens
  | MI_set_delegate
  | MI_create_account
  | MI_create_contract  of mich_t cc * mich_t cc * mich_i cc
  | MI_implicit_account
  | MI_now
  | MI_amount
  | MI_balance
  | MI_check_signature
  | MI_blake2b
  | MI_sha256
  | MI_sha512
  | MI_hash_key
  | MI_steps_to_quota
  | MI_source
  | MI_sender
  | MI_address
  | MI_chain_id
  | MI_unpair
  (* Non-Standard Instruction : Special Comment : MicSE user defined safety property *)
  | MI_micse_check      of mich_i cc
[@@deriving sexp, compare, equal]
(* WARNING: I_check instruction is not in Michelson standard. It is for MicSE formatted-comment *)

(*****************************************************************************)
(*****************************************************************************)
(* Michelson to Tz                                                           *)
(*****************************************************************************)
(*****************************************************************************)

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
