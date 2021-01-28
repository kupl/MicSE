(* Tz for Tezos *)


(*****************************************************************************)
(*****************************************************************************)
(* Common Datatypes                                                          *)
(*****************************************************************************)
(*****************************************************************************)

module PSet = Core.Set.Poly
module PMap = Core.Map.Poly


(*****************************************************************************)
(*****************************************************************************)
(* Code Component                                                            *)
(*****************************************************************************)
(*****************************************************************************)

(* ccp for Code Component *)
type ccp_pos = { col : int; lin : int; }
type ccp_loc = Unknown | Pos of ccp_pos * ccp_pos
type ccp_annot = 
  | CCA_typ of string (* :type_annot  *)
  | CCA_var of string (* @var_annot   *)
  | CCA_fld of string (* %field_annot *)
type 'a cc = {
  (* code component *)
  cc_loc : ccp_loc;
  cc_anl : ccp_annot list;
  cc_v : 'a;
}


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

(* type mty_stack = mich_ty cc list *)


(*****************************************************************************)
(*****************************************************************************)
(* Tezos Values                                                              *)
(*****************************************************************************)
(*****************************************************************************)

(* Following components are entangled,
  - Michelson Values
  - Michelson Instruction
  - Stack in Michelson
*)

type address = string
and chain_id = string
and contract = {
  ctrt_address : address;
  ctrt_balance : mutez;
  ctrt_delegate : key_hash option;
  ctrt_param_ty : mich_t cc;
  ctrt_storage_ty : mich_t cc;
  ctrt_storage : mich_v cc;
  ctrt_code : mich_i cc;
}
and key_hash = string
and mutez = Int64.t
and operation =
  | TO_create_contract of ((key_hash option * mutez * mich_v cc) * (mich_t cc * mich_t cc * mich_i cc))
  | TO_transfer_tokens of (mich_v cc * mutez * contract)
  | TO_set_delegate of (key_hash option)

and mich_v = 
  (* Michelson Value *)

  (*************************************************************************)
  (* Symbol & Polymorphic                                                  *)
  (*************************************************************************)
  | MV_symbol of (mich_t cc * string)
  | MV_car of mich_v cc  (* ('a, 'b) pair -> 'a *)
  | MV_cdr of mich_v cc  (* ('a, 'b) pair -> 'b *)
  | MV_unlift_option of mich_v cc  (* 'a option -> 'a *)
  | MV_unlift_left of mich_v cc  (* ('a, 'b) or -> 'a *)
  | MV_unlift_right of mich_v cc (* ('a, 'b) or -> 'b *) 
  | MV_hd_l of mich_v cc (* 'a list -> 'a *)
  | MV_exec of mich_v cc * mich_v cc (* 'a * ('a, 'b) lambda -> 'b *)

  (*************************************************************************)
  (* Integer                                                               *)
  (*************************************************************************)
  | MV_lit_int of Z.t
  | MV_neg_ni of mich_v cc (* nat -> int *)
  | MV_neg_ii of mich_v cc (* int -> int *)
  | MV_not_ni of mich_v cc (* nat -> int *)
  | MV_not_ii of mich_v cc (* int -> int *)
  | MV_add_nii of mich_v cc * mich_v cc  (* nat * int -> int *)
  | MV_add_ini of mich_v cc * mich_v cc  (* int * nat -> int *)
  | MV_add_iii of mich_v cc * mich_v cc  (* int * int -> int *)
  | MV_sub_nni of mich_v cc * mich_v cc  (* nat * nat -> int *)
  | MV_sub_nii of mich_v cc * mich_v cc  (* nat * int -> int *)
  | MV_sub_ini of mich_v cc * mich_v cc  (* int * nat -> int *)
  | MV_sub_iii of mich_v cc * mich_v cc  (* int * int -> int *)
  | MV_sub_tti of mich_v cc * mich_v cc  (* timestamp * timestamp -> int *)
  | MV_mul_nii of mich_v cc * mich_v cc  (* nat * int -> int *)
  | MV_mul_ini of mich_v cc * mich_v cc  (* int * nat -> int *)
  | MV_mul_iii of mich_v cc * mich_v cc  (* int * int -> int *)
  | MV_compare of mich_v cc * mich_v cc  (* 'a * 'a -> int *)
  | MV_int_of_nat of mich_v cc (* nat -> int *)

  (*************************************************************************)
  (* Natural Number                                                        *)
  (*************************************************************************)
  | MV_lit_nat of Z.t
  | MV_abs_in of mich_v cc (* int -> nat *)
  | MV_add_nnn of mich_v cc * mich_v cc  (* nat * nat -> nat *)
  | MV_mul_nnn of mich_v cc * mich_v cc  (* nat * nat -> nat *)
  | MV_shiftL_nnn of mich_v cc * mich_v cc (* nat * nat -> nat *)
  | MV_shiftR_nnn of mich_v cc * mich_v cc (* nat * nat -> nat *)
  | MV_and_nnn   of mich_v cc * mich_v cc  (* nat * nat -> nat *)
  | MV_and_inn   of mich_v cc * mich_v cc  (* int * nat -> nat *)
  | MV_or_nnn    of mich_v cc * mich_v cc (* nat * nat -> nat *)
  | MV_xor_nnn   of mich_v cc * mich_v cc (* nat * nat -> nat *)
  | MV_size_s    of mich_v cc  (* 'a set -> nat *)
  | MV_size_m    of mich_v cc  (* ('k, 'v) map -> nat *)
  | MV_size_l    of mich_v cc  (* 'a list -> nat *)
  | MV_size_str  of mich_v cc  (* string -> nat *)
  | MV_size_b    of mich_v cc  (* bytes -> nat *)

  (*************************************************************************)
  (* String                                                                *)
  (*************************************************************************)
  | MV_lit_string of string
  | MV_concat_sss of mich_v cc * mich_v cc (* string * string -> string *)
  | MV_concat_list_s of mich_v cc  (* string list -> string *)

  (*************************************************************************)
  (* Bytes                                                                 *)
  (*************************************************************************)
  | MV_lit_bytes of string
  | MV_concat_bbb of mich_v cc * mich_v cc (* bytes * bytes -> bytes *)
  | MV_concat_list_b of mich_v cc  (* bytes list -> bytes *)
  | MV_pack    of mich_v cc (* 'a -> bytes *)
  | MV_blake2b of mich_v cc  (* bytes -> bytes *)
  | MV_sha256  of mich_v cc (* bytes -> bytes *)
  | MV_sha512  of mich_v cc (* bytes -> bytes *)

  (*************************************************************************)
  (* Mutez                                                                 *)
  (*************************************************************************)
  | MV_lit_mutez of Z.t
  | MV_add_mmm of mich_v cc * mich_v cc  (* mutez * mutez -> mutez *)
  | MV_sub_mmm of mich_v cc * mich_v cc  (* mutez * mutez -> mutez *)
  | MV_mul_mnm of mich_v cc * mich_v cc  (* mutez * nat -> mutez *)
  | MV_mul_nmm of mich_v cc * mich_v cc  (* nat * mutez -> mutez *)

  (*************************************************************************)
  (* Bool                                                                  *)
  (*************************************************************************)
  | MV_lit_bool of bool
  | MV_not_bb  of mich_v cc (* bool -> bool *)
  | MV_and_bbb of mich_v cc * mich_v cc  (* bool * bool -> bool *)
  | MV_or_bbb  of mich_v cc * mich_v cc  (* bool * bool -> bool *)
  | MV_xor_bbb of mich_v cc * mich_v cc  (* bool * bool -> bool *)
  | MV_eq_ib   of mich_v cc * mich_v cc  (* int -> int -> bool *)
  | MV_neq_ib  of mich_v cc * mich_v cc  (* int -> int -> bool *)
  | MV_lt_ib   of mich_v cc * mich_v cc  (* int -> int -> bool *)
  | MV_gt_ib   of mich_v cc * mich_v cc  (* int -> int -> bool *)
  | MV_leq_ib  of mich_v cc * mich_v cc  (* int -> int -> bool *)
  | MV_geq_ib  of mich_v cc * mich_v cc  (* int -> int -> bool *)
  | MV_mem_xsb of mich_v cc * mich_v cc  (* 'a * 'a set -> bool *)
  | MV_mem_xmb of mich_v cc * mich_v cc  (* 'k * ('k, 'v) map -> bool *)
  | MV_mem_xbmb of mich_v cc * mich_v cc  (* 'k * ('k, 'v) big_map -> bool *)
  | MV_check_signature of mich_v cc * mich_v cc * mich_v cc (* key * signature * bytes -> bool *)

  (*************************************************************************)
  (* Key Hash                                                              *)
  (*************************************************************************)
  | MV_lit_key_hash of string 
  | MV_hash_key of mich_v cc (* key -> key_hash *)

  (*************************************************************************)
  (* Timestamp                                                             *)
  (*************************************************************************)
  | MV_lit_timestamp_str of string 
  | MV_lit_timestamp_sec of Z.t
  | MV_add_tit of mich_v cc * mich_v cc  (* timestamp * int -> timestamp *)
  | MV_add_itt of mich_v cc * mich_v cc  (* int * timestamp -> timestamp *)
  | MV_sub_tit of mich_v cc * mich_v cc  (* timestamp * int -> timestamp *)

  (*************************************************************************)
  (* Address                                                               *)
  (*************************************************************************)
  | MV_lit_address of mich_v cc (* key_hash -> address *)
  | MV_address_of_contract of mich_v cc (* 'a contract -> address *)

  (*************************************************************************)
  (* Key                                                                   *)
  (*************************************************************************)
  | MV_lit_key of string

  (*************************************************************************)
  (* Unit                                                                  *)
  (*************************************************************************)
  | MV_unit

  (*************************************************************************)
  (* Signature                                                             *)
  (*************************************************************************)
  | MV_lit_signature_str of string
  | MV_lit_signature_signed of mich_v cc * mich_v cc  (* key * bytes -> signature *)

  (*************************************************************************)
  (* Option                                                                *)
  (*************************************************************************)
  | MV_some of mich_v cc (* 'a -> 'a option *)
  | MV_none of mich_t cc (* ('a) -> 'a option *)
  | MV_ediv_nnnn of mich_v cc * mich_v cc  (* nat * nat -> (nat, nat) pair option *)
  | MV_ediv_niin of mich_v cc * mich_v cc  (* nat * int -> (int, nat) pair option *)
  | MV_ediv_inin of mich_v cc * mich_v cc  (* int * nat -> (int, nat) pair option *)
  | MV_ediv_iiin of mich_v cc * mich_v cc  (* int * int -> (int, nat) pair option *)
  | MV_ediv_mnmm of mich_v cc * mich_v cc  (* mutez * nat -> (mutez, mutez) pair option *)
  | MV_ediv_mmnm of mich_v cc * mich_v cc  (* mutez * mutez -> (nat, mutez) pair option *)
  | MV_get_xmoy  of mich_v cc * mich_v cc  (* 'k * ('k, 'v) map -> 'v option *)
  | MV_get_xbmo  of mich_v cc * mich_v cc  (* 'k * ('k, 'v) big_map -> 'v option *)
  | MV_slice_nnso of mich_v cc * mich_v cc * mich_v cc (* nat * nat * string -> string option *)
  | MV_slice_nnbo of mich_v cc * mich_v cc * mich_v cc (* nat * nat * bytes -> bytes option *)
  | MV_unpack of mich_t cc * mich_v cc (* ('a) * bytes -> 'a option *)
  | MV_contract_of_address of mich_t cc * mich_v cc (* ('a) -> address -> 'a contract option *)
  | MV_isnat of mich_v cc (* int -> nat option *)

  (*************************************************************************)
  (* List                                                                  *)
  (*************************************************************************)
  | MV_lit_list of mich_t cc * mich_v cc list  (* ('a) * list-literal -> 'a list *)
  | MV_nil of mich_t cc  (* ('a) -> 'a list *)
  | MV_cons of mich_v cc * mich_v cc (* 'a * 'a list -> 'a list *)
  | MV_tl_l of mich_v cc (* 'a list -> 'a list *)

  (*************************************************************************)
  (* Set                                                                   *)
  (*************************************************************************)
  | MV_lit_set of mich_t cc * mich_v cc PSet.t  (* ('a) * set-literal -> 'a set *)
  | MV_empty_set of mich_t cc (* ('a) -> 'a set *)
  | MV_update_xbss of mich_v cc * mich_v cc * mich_v cc  (* 'a * bool * 'a set -> 'a set *)

  (*************************************************************************)
  (* Operation                                                             *)
  (*************************************************************************)
  | MV_create_contract of mich_t cc * mich_t cc * mich_v cc * mich_v cc * mich_v cc * mich_v cc (* ('param) * ('strg) * (('param, 'strg) pair, (operation list, 'strg) pair) lambda * key_hash option * mutez * 'strg -> operation *)
  | MV_transfer_tokens of mich_v cc * mich_v cc * mich_v cc  (* 'a * mutez * 'a contract -> operation *)
  | MV_set_delegate of mich_v cc (* key_hash option -> operation *)

  (*************************************************************************)
  (* Contract                                                              *)
  (*************************************************************************)
  | MV_lit_contract of contract
  | MV_self of mich_t cc  (* 'a -> 'a contract *)
  | MV_implicit_account of mich_v cc (* key_hash -> unit contract *)

  (*************************************************************************)
  (* Pair                                                                  *)
  (*************************************************************************)
  | MV_pair of mich_v cc * mich_v cc  (* 'a * 'b -> ('a, 'b) pair *)

  (*************************************************************************)
  (* Or                                                                    *)
  (*************************************************************************)
  | MV_left of mich_t cc * mich_v cc (* (('a, 'b) or) * 'a -> ('a, 'b) or *)
  | MV_right of mich_t cc * mich_v cc (* (('a, 'b) or) * 'b -> ('a, 'b) or *)

  (*************************************************************************)
  (* Lambda                                                                *)
  (*************************************************************************)
  | MV_lit_lambda of mich_t cc * mich_t cc * mich_i cc (* ('param) * ('ret) * ('param, 'ret) Mich.inst Mich.t -> ('param, 'ret) lambda *) (* embedded code with LAMBDA Michelson-instruction should be expressed with V_lambda_id, not V_lit_lambda *)
  | MV_lambda_unknown of mich_t cc * mich_t cc (* ('param) * ('ret) -> ('param, 'ret) lambda *)
  | MV_lambda_closure of mich_v cc * mich_v cc (* (('p1, 'p2) pair, 'ret) lambda * 'p1 -> ('p2, 'ret) lambda *)

  (*************************************************************************)
  (* Map                                                                   *)
  (*************************************************************************)
  | MV_lit_map of mich_t cc * mich_t cc * (mich_v cc, mich_v cc) PMap.t (* ('k) * ('v) * map-literal -> ('k, 'v) map *)
  | MV_empty_map of mich_t cc * mich_t cc (* ('k) * ('v) -> ('k, 'v) map *)
  | MV_update_xomm of mich_v cc * mich_v cc * mich_v cc (* 'k * 'v option * ('k, 'v) map -> ('k, 'v) map *)

  (*************************************************************************)
  (* Big Map                                                               *)
  (*************************************************************************)
  | MV_lit_big_map of mich_t cc * mich_t cc * (mich_v cc, mich_v cc) PMap.t (* ('k) * ('v) * map-literal -> ('k, 'v) big_map *)
  | MV_empty_big_map of mich_t cc * mich_t cc  (* ('k) * ('v) -> ('k, 'v) big_map *)
  | MV_update_xobmbm of mich_v cc * mich_v cc * mich_v cc  (* 'k * 'v option * ('k, 'v) big_map -> ('k, 'v) big_map *)

  (*************************************************************************)
  (* Chain Id                                                              *)
  (*************************************************************************)
  | MV_lit_chain_id of string


and mich_i = 
  (* Michelson Instruction *)
  | MI_seq           of mich_i cc * mich_i cc
  | MI_drop          of Z.t
  | MI_dup           of Z.t
  | MI_swap
  | MI_dig           of Z.t
  | MI_dug           of Z.t
  | MI_push          of mich_t cc * mich_v cc
  | MI_some
  | MI_none          of mich_t cc
  | MI_unit
  | MI_if_none       of mich_i cc * mich_i cc
  | MI_pair
  | MI_car
  | MI_cdr
  | MI_left          of mich_t cc
  | MI_right         of mich_t cc
  | MI_if_left       of mich_i cc * mich_i cc
  | MI_nil           of mich_t cc
  | MI_cons
  | MI_if_cons       of mich_i cc * mich_i cc
  | MI_size
  | MI_empty_set     of mich_t cc
  | MI_empty_map     of mich_t cc * mich_t cc
  | MI_empty_big_map of mich_t cc * mich_t cc
  | MI_map           of mich_i cc
  | MI_iter          of mich_i cc
  | MI_mem
  | MI_get
  | MI_update
  | MI_if            of mich_i cc * mich_i cc
  | MI_loop          of mich_i cc
  | MI_loop_left     of mich_i cc
  | MI_lambda        of mich_t cc * mich_t cc * mich_i cc
  | MI_exec
  | MI_dip_n         of Z.t * mich_i cc
  | MI_failwith
  | MI_cast          of mich_t cc
  | MI_rename
  | MI_concat
  | MI_slice
  | MI_pack
  | MI_unpack        of mich_t cc
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
  | MI_contract      of mich_t cc
  | MI_transfer_tokens
  | MI_set_delegate
  | MI_create_account
  | MI_create_contract of mich_t cc * mich_t cc * mich_i cc
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
  | MI_micse_check of mich_i cc   (* WARNING: I_check instruction is not in Michelson standard. It is for MicSE formatted-comment *)





(*****************************************************************************)
(*****************************************************************************)
(* Blockchain                                                                *)
(*****************************************************************************)
(*****************************************************************************)

module Bc = struct
  (* blockchain *)
  type t = {
    contracts : (address, contract) PMap.t;
    chain_id : chain_id;
    last_time : Z.t;
  }
end (* module Blockchain end *)


(*****************************************************************************)
(*****************************************************************************)
(* Formula                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

type mich_f =
  (* Logical Formula *)
  | MF_true
  | MF_false
  | MF_not of mich_f
  | MF_and of mich_f list
  | MF_or of mich_f list
  | MF_eq of mich_v cc * mich_v cc  (* 'a * 'a -> formula *)
  | MF_imply of mich_f * mich_f
  (* MicSE-Cfg Pattern Matching *)  
  | MF_is_none of mich_v cc (* 'a option -> formula *)
  | MF_is_left of mich_v cc (* ('a, 'b) or -> formula *)
  | MF_is_cons of mich_v cc (* 'a list -> formula *)
  (* Custom Formula for verifiying *)
  | MF_add_mmm_no_overflow of (mich_v cc * mich_v cc)
  | MF_sub_mmm_no_underflow of (mich_v cc * mich_v cc)
  | MF_mul_mnm_no_overflow of (mich_v cc * mich_v cc)
  | MF_mul_nmm_no_overflow of (mich_v cc * mich_v cc)
  | MF_shiftL_nnn_rhs_in_256 of (mich_v cc * mich_v cc)
  | MF_shiftR_nnn_rhs_in_256 of (mich_v cc * mich_v cc)
  (* Custom Domain Formula for Invariant Generation *)
  | MF_sigma_equal of (mich_v cc * mich_v cc)


(*****************************************************************************)
(*****************************************************************************)
(* Symbolic Stack                                                            *)
(*****************************************************************************)
(*****************************************************************************)

(* type sym_stack = (mich_v list) *)
