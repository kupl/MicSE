(* Tz : MicSE's Michelson representation *)

exception TzError of string

open Core

(******************************************************************************)
(******************************************************************************)
(* Code Component                                                             *)
(******************************************************************************)
(******************************************************************************)

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
  cc_loc : (ccp_loc[@sexp.opaque] [@ignore]);
  cc_anl : (ccp_annot list[@sexp.opaque] [@ignore]);
  cc_v : 'a;
}
[@@deriving sexp, compare, equal]

(******************************************************************************)
(******************************************************************************)
(* Tezos Types                                                                *)
(******************************************************************************)
(******************************************************************************)

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

(******************************************************************************)
(******************************************************************************)
(* Michelson Values & Instructions                                            *)
(******************************************************************************)
(******************************************************************************)
and mich_sym_category =
  | MSC_contract
  | MSC_source
  | MSC_sender
  | MSC_param
  | MSC_amount
  | MSC_time
  | MSC_balance
  | MSC_bc_balance
  | MSC_mich_stack      of int
  | MSC_dip_stack       of int
  | MSC_map_entry_stack of int
  | MSC_map_exit_stack  of int
  | MSC_iter_stack      of int

and mich_sym_ctxt = int list

(* MV_inv_symbol should be replaced to other mich_v expression.
   If some un-replaced MV_inv_symbol remained formula block in precondition of
   verification condition, that formula block should be replaced to MF_true.
   Caution: Free variable should not be allowed in precondition of formula. *)
and mich_invsym_category =
  | MIC_balance
  | MIC_bc_balance
  | MIC_iter_cont
  | MIC_map_entry_cont
  | MIC_map_exit_cont
  | MIC_not_implemented

and mich_v =
  (* Michelson Value *)

  (****************************************************************************)
  (* Symbol & Polymorphic                                                     *)
  (****************************************************************************)
  | MV_symbol               of
      (mich_t cc * mich_sym_category * (mich_sym_ctxt[@sexp.opaque] [@ignore]))
  | MV_car                  of mich_v cc (* ('a, 'b) pair -> 'a *)
  | MV_cdr                  of mich_v cc (* ('a, 'b) pair -> 'b *)
  | MV_unlift_option        of mich_v cc (* 'a option -> 'a *)
  | MV_unlift_left          of mich_v cc (* ('a, 'b) or -> 'a *)
  | MV_unlift_right         of mich_v cc (* ('a, 'b) or -> 'b *)
  | MV_hd_l                 of mich_v cc (* 'a list -> 'a *)
  (****************************************************************************)
  (* Integer                                                                  *)
  (****************************************************************************)
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
  (****************************************************************************)
  (* Natural Number                                                           *)
  (****************************************************************************)
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
  (****************************************************************************)
  (* String                                                                   *)
  (****************************************************************************)
  | MV_lit_string           of string
  | MV_concat_sss           of mich_v cc * mich_v cc (* string * string -> string *)
  | MV_concat_list_s        of mich_v cc (* string list -> string *)
  (****************************************************************************)
  (* Bytes                                                                    *)
  (****************************************************************************)
  | MV_lit_bytes            of string
  | MV_concat_bbb           of mich_v cc * mich_v cc (* bytes * bytes -> bytes *)
  | MV_concat_list_b        of mich_v cc (* bytes list -> bytes *)
  | MV_pack                 of mich_v cc (* 'a -> bytes *)
  | MV_blake2b              of mich_v cc (* bytes -> bytes *)
  | MV_sha256               of mich_v cc (* bytes -> bytes *)
  | MV_sha512               of mich_v cc (* bytes -> bytes *)
  (****************************************************************************)
  (* Mutez                                                                    *)
  (****************************************************************************)
  | MV_lit_mutez            of Bigint.t
  | MV_add_mmm              of mich_v cc * mich_v cc (* mutez * mutez -> mutez *)
  | MV_sub_mmm              of mich_v cc * mich_v cc (* mutez * mutez -> mutez *)
  | MV_mul_mnm              of mich_v cc * mich_v cc (* mutez * nat -> mutez *)
  | MV_mul_nmm              of mich_v cc * mich_v cc (* nat * mutez -> mutez *)
  | MV_mtz_of_op_list       of mich_v cc (* operation list -> mutz *)
  (****************************************************************************)
  (* Bool                                                                     *)
  (****************************************************************************)
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
  (****************************************************************************)
  (* Key Hash                                                                 *)
  (****************************************************************************)
  | MV_lit_key_hash         of string
  | MV_hash_key             of mich_v cc (* key -> key_hash *)
  (****************************************************************************)
  (* Timestamp                                                                *)
  (****************************************************************************)
  | MV_lit_timestamp_str    of string
  | MV_lit_timestamp_sec    of Bigint.t
  | MV_add_tit              of mich_v cc * mich_v cc (* timestamp * int -> timestamp *)
  | MV_add_itt              of mich_v cc * mich_v cc (* int * timestamp -> timestamp *)
  | MV_sub_tit              of mich_v cc * mich_v cc (* timestamp * int -> timestamp *)
  (****************************************************************************)
  (* Address                                                                  *)
  (****************************************************************************)
  | MV_lit_address          of mich_v cc (* key_hash -> address *)
  | MV_address_of_contract  of mich_v cc (* 'a contract -> address *)
  (****************************************************************************)
  (* Key                                                                      *)
  (****************************************************************************)
  | MV_lit_key              of string
  (****************************************************************************)
  (* Unit                                                                     *)
  (****************************************************************************)
  | MV_unit
  (****************************************************************************)
  (* Signature                                                                *)
  (****************************************************************************)
  | MV_lit_signature_str    of string
  | MV_lit_signature_signed of mich_v cc * mich_v cc (* key * bytes -> signature *)
  (****************************************************************************)
  (* Option                                                                   *)
  (****************************************************************************)
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
  (****************************************************************************)
  (* List                                                                     *)
  (****************************************************************************)
  | MV_lit_list             of mich_t cc * mich_v cc list (* ('a) * list-literal -> 'a list *)
  | MV_nil                  of mich_t cc (* ('a) -> 'a list *)
  | MV_cons                 of mich_v cc * mich_v cc (* 'a * 'a list -> 'a list *)
  | MV_tl_l                 of mich_v cc (* 'a list -> 'a list *)
  (****************************************************************************)
  (* Set                                                                      *)
  (****************************************************************************)
  | MV_lit_set              of mich_t cc * mich_v cc Core.List.t (* ('a) * set-literal (list) -> 'a set *)
  | MV_empty_set            of mich_t cc (* ('a) -> 'a set *)
  | MV_update_xbss          of mich_v cc * mich_v cc * mich_v cc (* 'a * bool * 'a set -> 'a set *)
  (****************************************************************************)
  (* Operation                                                                *)
  (****************************************************************************)
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
  (****************************************************************************)
  (* Contract                                                                 *)
  (****************************************************************************)
  | MV_lit_contract         of mich_t cc * mich_v cc (* ('a) * address -> 'a contract *)
  | MV_self                 of mich_t cc (* 'a -> 'a contract *)
  | MV_implicit_account     of mich_v cc (* key_hash -> unit contract *)
  (****************************************************************************)
  (* Pair                                                                     *)
  (****************************************************************************)
  | MV_pair                 of mich_v cc * mich_v cc (* 'a * 'b -> ('a, 'b) pair *)
  (****************************************************************************)
  (* Or                                                                       *)
  (****************************************************************************)
  | MV_left                 of mich_t cc * mich_v cc (* (('a, 'b) or) * 'a -> ('a, 'b) or *)
  | MV_right                of mich_t cc * mich_v cc (* (('a, 'b) or) * 'b -> ('a, 'b) or *)
  (****************************************************************************)
  (* Lambda                                                                   *)
  (****************************************************************************)
  | MV_lit_lambda           of mich_t cc * mich_t cc * mich_i cc (* ('param) * ('ret) * ('param, 'ret) Mich.inst Mich.t -> ('param, 'ret) lambda *)
  (* embedded code with LAMBDA Michelson-instruction should be expressed with V_lambda_id, not V_lit_lambda *)
  | MV_lambda_unknown       of mich_t cc * mich_t cc (* ('param) * ('ret) -> ('param, 'ret) lambda *)
  | MV_lambda_closure       of mich_v cc * mich_v cc (* (('p1, 'p2) pair, 'ret) lambda * 'p1 -> ('p2, 'ret) lambda *)
  (****************************************************************************)
  (* Map                                                                      *)
  (****************************************************************************)
  | MV_lit_map              of
      mich_t cc * mich_t cc * (mich_v cc * mich_v cc) list (* ('k) * ('v) * map-literal(alist) -> ('k, 'v) map *)
  | MV_empty_map            of mich_t cc * mich_t cc (* ('k) * ('v) -> ('k, 'v) map *)
  | MV_update_xomm          of mich_v cc * mich_v cc * mich_v cc (* 'k * 'v option * ('k, 'v) map -> ('k, 'v) map *)
  (****************************************************************************)
  (* Big Map                                                                  *)
  (****************************************************************************)
  | MV_lit_big_map          of
      mich_t cc * mich_t cc * (mich_v cc * mich_v cc) list (* ('k) * ('v) * map-literal -> ('k, 'v) big_map *)
  | MV_empty_big_map        of mich_t cc * mich_t cc (* ('k) * ('v) -> ('k, 'v) big_map *)
  | MV_update_xobmbm        of mich_v cc * mich_v cc * mich_v cc (* 'k * 'v option * ('k, 'v) big_map -> ('k, 'v) big_map *)
  (****************************************************************************)
  (* Chain Id                                                                 *)
  (****************************************************************************)
  | MV_lit_chain_id         of string
  (****************************************************************************)
  (* Custom Domain Value for Invariant Synthesis                              *)
  (****************************************************************************)
  | MV_inv_symbol           of (mich_t cc * mich_invsym_category)
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

module MichTCC_cmp = struct
  type t = mich_t cc

  let compare = compare_cc compare_mich_t

  let t_of_sexp = cc_of_sexp mich_t_of_sexp

  let sexp_of_t = sexp_of_cc sexp_of_mich_t
end

module MichVCC_cmp = struct
  type t = mich_v cc

  let compare = compare_cc compare_mich_v

  let t_of_sexp = cc_of_sexp mich_v_of_sexp

  let sexp_of_t = sexp_of_cc sexp_of_mich_v
end

(******************************************************************************)
(******************************************************************************)
(* Formula                                                                    *)
(******************************************************************************)
(******************************************************************************)

type mich_f =
  (* Logical Formula *)
  | MF_true
  | MF_false
  | MF_not                   of mich_f
  | MF_and                   of mich_f list
  | MF_or                    of mich_f list
  | MF_eq                    of mich_v cc * mich_v cc (* 'a * 'a -> formula *)
  | MF_imply                 of mich_f * mich_f
  (* MicSE Branch *)
  | MF_is_true               of mich_v cc (* bool -> formula *)
  | MF_is_none               of mich_v cc (* 'a option -> formula *)
  | MF_is_left               of mich_v cc (* ('a, 'b) or -> formula *)
  | MF_is_cons               of mich_v cc (* 'a list -> formula *)
  (* MicSE Datatype Constraint *)
  | MF_mutez_bound           of mich_v cc (* (integer arithmetic) 'a -> formula *)
  | MF_nat_bound             of mich_v cc (* (integer arithmetic) 'a -> formula *)
  (* Custom Formula for verifiying *)
  | MF_add_mmm_no_overflow   of (mich_v cc * mich_v cc)
  | MF_sub_mmm_no_underflow  of (mich_v cc * mich_v cc)
  | MF_mul_mnm_no_overflow   of (mich_v cc * mich_v cc)
  | MF_mul_nmm_no_overflow   of (mich_v cc * mich_v cc)
  | MF_shiftL_nnn_rhs_in_256 of (mich_v cc * mich_v cc)
  | MF_shiftR_nnn_rhs_in_256 of (mich_v cc * mich_v cc)
[@@deriving sexp, compare, equal]

(******************************************************************************)
(******************************************************************************)
(* Symbolic State                                                             *)
(******************************************************************************)
(******************************************************************************)

type query_category =
  (* Each of them are indicator of "State -> Formula" function *)
  | Q_mutez_add_no_overflow
  | Q_mutez_sub_no_underflow
  | Q_mutez_mul_mnm_no_overflow
  | Q_mutez_mul_nmm_no_overflow
  | Q_shiftleft_safe
  | Q_shiftright_safe
  | Q_assertion
[@@deriving sexp, compare, equal]

type mich_cut_category =
  | MCC_trx_entry
  | MCC_trx_exit
  | MCC_ln_loop (* non-body of the loop location *)
  | MCC_ln_loopleft (* non-body of the loop location *)
  | MCC_ln_map (* non-body of the loop location *)
  | MCC_ln_iter (* non-body of the loop location *)
  | MCC_lb_loop (* body of the loop *)
  | MCC_lb_loopleft (* body of the loop *)
  | MCC_lb_map (* body of the loop *)
  | MCC_lb_iter (* body of the loop *)
  | MCC_query       of query_category
[@@deriving sexp, compare, equal]

(* reduced mich_cut_category *)
type r_mich_cut_category =
  | RMCC_trx
  | RMCC_loop
  | RMCC_loopleft
  | RMCC_map
  | RMCC_iter
  | RMCC_query    of query_category
[@@deriving sexp, compare, equal]

type mich_cut_info = {
  mci_loc : ccp_loc;
  mci_cutcat : mich_cut_category;
}
[@@deriving sexp, compare, equal]

(* reduced mich_cut_info *)
type r_mich_cut_info = {
  rmci_loc : ccp_loc;
  rmci_cutcat : r_mich_cut_category;
}
[@@deriving sexp, compare, equal]

module RMichCutInfo_cmp = struct
  type t = r_mich_cut_info

  let compare = compare_r_mich_cut_info

  let t_of_sexp = r_mich_cut_info_of_sexp

  let sexp_of_t = sexp_of_r_mich_cut_info
end

(* Transaction parameter value container *)
type trx_image = {
  (* THIS CONTRACT (not address, since SELF instruction returns contract, not address. *)
  ti_contract : mich_v cc;
  (* transaction source & sender (addresses) *)
  ti_source : mich_v cc;
  ti_sender : mich_v cc;
  (* smart contract parameter value *)
  ti_param : mich_v cc;
  (* amount value *)
  ti_amount : mich_v cc;
  (* timestamp value *)
  ti_time : mich_v cc;
}
[@@deriving sexp, compare, equal]

(* symbolic moment snapshot (for single contract verifciation) *)
type sym_image = {
  (* michelson main stack *)
  si_mich : mich_v cc list;
  (* DIP instruction stack *)
  si_dip : mich_v cc list;
  (* container stack for MAP instruction - Entry & Exit *)
  si_map_entry : mich_v cc list;
  si_map_exit : mich_v cc list;
  (* container stack for ITER instruction *)
  si_iter : mich_v cc list;
  (* contract balance snapshot *)
  si_balance : mich_v cc;
  (* blockchain's balance (excluding tp_amount & si_balance) *)
  si_bc_balance : mich_v cc;
  (* parameter used in this image *)
  si_param : trx_image;
}
[@@deriving sexp, compare, equal]

type sym_state_id = mich_sym_ctxt [@@deriving sexp, compare, equal]

(* entire blockchain symbolic state - designed for single contract verification *)
type sym_state = {
  ss_id : sym_state_id;
  (* location and category where and when the state starts/blocked a symbolic execution *)
  ss_start_mci : mich_cut_info;
  ss_block_mci : mich_cut_info;
  (* symbolic stack image where the state starts/blocked *)
  ss_start_si : sym_image;
  ss_block_si : sym_image;
  (* parameter & amount & timestamp history between start/blocked
     - information duplicated with sym_image's si_param
  *)
  ss_param_history : trx_image list;
  (* AND-connected logical formula to constrain this state *)
  ss_constraints : mich_f list;
}
[@@deriving sexp, compare, equal]

module MichCutInfo_cmp = struct
  type t = mich_cut_info

  let compare = compare_mich_cut_info

  let t_of_sexp = mich_cut_info_of_sexp

  let sexp_of_t = sexp_of_mich_cut_info
end

module SymState_cmp = struct
  type t = sym_state

  let compare = compare_sym_state

  let t_of_sexp = sym_state_of_sexp

  let sexp_of_t = sexp_of_sym_state
end

(******************************************************************************)
(******************************************************************************)
(* Utility Functions for Tz                                                   *)
(******************************************************************************)
(******************************************************************************)

(******************************************************************************)
(* Code Component                                                             *)
(******************************************************************************)

let gen_dummy_cc : 'a -> 'a cc =
  (fun x -> { cc_loc = CCLOC_Unknown; cc_anl = []; cc_v = x })

let gen_custom_cc : 'ccbase cc -> 'a -> 'a cc =
  (fun base x -> { base with cc_v = x })

(******************************************************************************)
(* Tezos Type                                                                 *)
(******************************************************************************)

let typ_of_val : mich_v cc -> mich_t cc =
   let rec typ_of_val_i : mich_v cc -> mich_t cc =
     fun v ->
     let err : unit -> 'a =
       fun () ->
       TzError
         ("typ_of_val : typ_of_val_i : "
         ^ Sexp.to_string (sexp_of_cc sexp_of_mich_v v)
         )
       |> Stdlib.raise
     in
     let gen_cc : mich_t -> mich_t cc = (fun t -> gen_custom_cc v t) in
     match v.cc_v with
     (**************************************************************************)
     (* Symbol & Polymorphic                                                   *)
     (**************************************************************************)
     | MV_symbol (t, _, _) -> t
     | MV_car v1 -> (
       (typ_of_val_i v1).cc_v
       |> function
       | MT_pair (t1, _) -> t1
       | _               -> err ()
     )
     | MV_cdr v1 -> (
       (typ_of_val_i v1).cc_v
       |> function
       | MT_pair (_, t2) -> t2
       | _               -> err ()
     )
     | MV_unlift_option v1 -> (
       (typ_of_val_i v1).cc_v
       |> function
       | MT_pair (_, t2) -> t2
       | _               -> err ()
     )
     | MV_unlift_left v1 -> (
       (typ_of_val_i v1).cc_v
       |> function
       | MT_option t1 -> t1
       | _            -> err ()
     )
     | MV_unlift_right v1 -> (
       (typ_of_val_i v1).cc_v
       |> function
       | MT_or (t1, _) -> t1
       | _             -> err ()
     )
     | MV_hd_l v1 -> (
       (typ_of_val_i v1).cc_v
       |> function
       | MT_or (_, t2) -> t2
       | _             -> err ()
     )
     (****************************************************************************)
     (* Integer                                                                  *)
     (****************************************************************************)
     | MV_lit_int _
     | MV_neg_ni _
     | MV_neg_ii _
     | MV_not_ni _
     | MV_not_ii _
     | MV_add_nii _
     | MV_add_ini _
     | MV_add_iii _
     | MV_sub_nni _
     | MV_sub_nii _
     | MV_sub_ini _
     | MV_sub_iii _
     | MV_sub_tti _
     | MV_mul_nii _
     | MV_mul_ini _
     | MV_mul_iii _
     | MV_compare _
     | MV_int_of_nat _ ->
       gen_cc MT_int
     (****************************************************************************)
     (* Natural Number                                                           *)
     (****************************************************************************)
     | MV_lit_nat _
     | MV_abs_in _
     | MV_add_nnn _
     | MV_mul_nnn _
     | MV_shiftL_nnn _
     | MV_shiftR_nnn _
     | MV_and_nnn _
     | MV_and_inn _
     | MV_or_nnn _
     | MV_xor_nnn _
     | MV_size_s _
     | MV_size_m _
     | MV_size_l _
     | MV_size_str _
     | MV_size_b _ ->
       gen_cc MT_nat
     (****************************************************************************)
     (* String                                                                   *)
     (****************************************************************************)
     | MV_lit_string _
     | MV_concat_sss _
     | MV_concat_list_s _ ->
       gen_cc MT_string
     (****************************************************************************)
     (* Bytes                                                                    *)
     (****************************************************************************)
     | MV_lit_bytes _
     | MV_concat_bbb _
     | MV_concat_list_b _
     | MV_pack _
     | MV_blake2b _
     | MV_sha256 _
     | MV_sha512 _ ->
       gen_cc MT_bytes
     (****************************************************************************)
     (* Mutez                                                                    *)
     (****************************************************************************)
     | MV_lit_mutez _
     | MV_add_mmm _
     | MV_sub_mmm _
     | MV_mul_mnm _
     | MV_mul_nmm _
     | MV_mtz_of_op_list _ ->
       gen_cc MT_mutez
     (****************************************************************************)
     (* Bool                                                                     *)
     (****************************************************************************)
     | MV_lit_bool _
     | MV_not_bb _
     | MV_and_bbb _
     | MV_or_bbb _
     | MV_xor_bbb _
     | MV_eq_ib _
     | MV_neq_ib _
     | MV_lt_ib _
     | MV_gt_ib _
     | MV_leq_ib _
     | MV_geq_ib _
     | MV_mem_xsb _
     | MV_mem_xmb _
     | MV_mem_xbmb _
     | MV_check_signature _ ->
       gen_cc MT_bool
     (****************************************************************************)
     (* Key Hash                                                                 *)
     (****************************************************************************)
     | MV_lit_key_hash _
     | MV_hash_key _ ->
       gen_cc MT_key_hash
     (****************************************************************************)
     (* Timestamp                                                                *)
     (****************************************************************************)
     | MV_lit_timestamp_str _
     | MV_lit_timestamp_sec _
     | MV_add_tit _
     | MV_add_itt _
     | MV_sub_tit _ ->
       gen_cc MT_timestamp
     (****************************************************************************)
     (* Address                                                                  *)
     (****************************************************************************)
     | MV_lit_address _
     | MV_address_of_contract _ ->
       gen_cc MT_address
     (****************************************************************************)
     (* Key                                                                      *)
     (****************************************************************************)
     | MV_lit_key _ -> gen_cc MT_key
     (****************************************************************************)
     (* Unit                                                                     *)
     (****************************************************************************)
     | MV_unit -> gen_cc MT_unit
     (****************************************************************************)
     (* Signature                                                                *)
     (****************************************************************************)
     | MV_lit_signature_str _
     | MV_lit_signature_signed _ ->
       gen_cc MT_signature
     (****************************************************************************)
     (* Option                                                                   *)
     (****************************************************************************)
     | MV_some v1 -> gen_cc (MT_option (typ_of_val_i v1))
     | MV_none t1 -> gen_cc (MT_option t1)
     | MV_ediv_nnnn _ ->
       gen_cc (MT_option (gen_cc (MT_pair (gen_cc MT_nat, gen_cc MT_nat))))
     | MV_ediv_niin _ ->
       gen_cc (MT_option (gen_cc (MT_pair (gen_cc MT_int, gen_cc MT_nat))))
     | MV_ediv_inin _ ->
       gen_cc (MT_option (gen_cc (MT_pair (gen_cc MT_int, gen_cc MT_nat))))
     | MV_ediv_iiin _ ->
       gen_cc (MT_option (gen_cc (MT_pair (gen_cc MT_int, gen_cc MT_nat))))
     | MV_ediv_mnmm _ ->
       gen_cc (MT_option (gen_cc (MT_pair (gen_cc MT_mutez, gen_cc MT_mutez))))
     | MV_ediv_mmnm _ ->
       gen_cc (MT_option (gen_cc (MT_pair (gen_cc MT_nat, gen_cc MT_mutez))))
     | MV_get_xmoy (_, v2) -> (
       (typ_of_val_i v2).cc_v
       |> function
       | MT_map (_, t2) -> gen_cc (MT_option t2)
       | _              -> err ()
     )
     | MV_get_xbmo (_, v2) -> (
       (typ_of_val_i v2).cc_v
       |> function
       | MT_big_map (_, t2) -> gen_cc (MT_option t2)
       | _                  -> err ()
     )
     | MV_slice_nnso _ -> gen_cc (MT_option (gen_cc MT_string))
     | MV_slice_nnbo _ -> gen_cc (MT_option (gen_cc MT_bytes))
     | MV_unpack (t1, _) -> gen_cc (MT_option t1)
     | MV_contract_of_address (t1, _) ->
       gen_cc (MT_option (gen_cc (MT_contract t1)))
     | MV_isnat _ -> gen_cc (MT_option (gen_cc MT_nat))
     (****************************************************************************)
     (* List                                                                     *)
     (****************************************************************************)
     | MV_lit_list (t1, _) -> gen_cc (MT_list t1)
     | MV_nil t1 -> gen_cc (MT_list t1)
     | MV_cons (v1, _) -> gen_cc (MT_list (typ_of_val_i v1))
     | MV_tl_l v1 -> typ_of_val_i v1
     (****************************************************************************)
     (* Set                                                                      *)
     (****************************************************************************)
     | MV_lit_set (t1, _) -> gen_cc (MT_set t1)
     | MV_empty_set t1 -> gen_cc (MT_set t1)
     | MV_update_xbss (v1, _, _) -> gen_cc (MT_set (typ_of_val_i v1))
     (****************************************************************************)
     (* Operation                                                                *)
     (****************************************************************************)
     | MV_create_contract _
     | MV_transfer_tokens _
     | MV_set_delegate _ ->
       gen_cc MT_operation
     (****************************************************************************)
     (* Contract                                                                 *)
     (****************************************************************************)
     | MV_lit_contract (t1, _) -> gen_cc (MT_contract t1)
     | MV_self t1 -> gen_cc (MT_contract t1)
     | MV_implicit_account _ -> gen_cc (MT_contract (gen_cc MT_unit))
     (****************************************************************************)
     (* Pair                                                                     *)
     (****************************************************************************)
     | MV_pair (v1, v2) -> gen_cc (MT_pair (typ_of_val_i v1, typ_of_val_i v2))
     (****************************************************************************)
     (* Or                                                                       *)
     (****************************************************************************)
     | MV_left (t1, _)
     | MV_right (t1, _) ->
       t1
     (****************************************************************************)
     (* Lambda                                                                   *)
     (****************************************************************************)
     | MV_lit_lambda (t1, t2, _) -> gen_cc (MT_lambda (t1, t2))
     | MV_lambda_unknown (t1, t2) -> gen_cc (MT_lambda (t1, t2))
     | MV_lambda_closure (v1, _) -> (
       (typ_of_val_i v1).cc_v
       |> function
       | MT_lambda (t1, t2) -> (
         match t1.cc_v with
         | MT_pair (_, t12) -> gen_cc (MT_lambda (t12, t2))
         | _                -> err ()
       )
       | _                  -> err ()
     )
     (****************************************************************************)
     (* Map                                                                      *)
     (****************************************************************************)
     | MV_lit_map (t1, t2, _) -> gen_cc (MT_map (t1, t2))
     | MV_empty_map (t1, t2) -> gen_cc (MT_map (t1, t2))
     | MV_update_xomm (v1, v2, _) -> (
       (typ_of_val_i v1, (typ_of_val_i v2).cc_v)
       |> function
       | (t1, MT_option t2) -> gen_cc (MT_map (t1, t2))
       | _                  -> err ()
     )
     (****************************************************************************)
     (* Big Map                                                                  *)
     (****************************************************************************)
     | MV_lit_big_map (t1, t2, _) -> gen_cc (MT_map (t1, t2))
     | MV_empty_big_map (t1, t2) -> gen_cc (MT_map (t1, t2))
     | MV_update_xobmbm (v1, v2, _) -> (
       (typ_of_val_i v1, (typ_of_val_i v2).cc_v)
       |> function
       | (t1, MT_option t2) -> gen_cc (MT_map (t1, t2))
       | _                  -> err ()
     )
     (****************************************************************************)
     (* Chain Id                                                                 *)
     (****************************************************************************)
     | MV_lit_chain_id _ -> gen_cc MT_chain_id
     (****************************************************************************)
     (* Custom Domain Value for Invariant Synthesis                              *)
     (****************************************************************************)
     | MV_inv_symbol (t1, _) -> t1
     | MV_sigma_tmplm _ -> gen_cc MT_mutez
     (* inner-function typ_of_val_i end *)
   in
   (fun v -> typ_of_val_i v)
(* function typ_of_val end *)

let get_innertyp : mich_t cc -> mich_t cc =
  fun ttt ->
  match ttt.cc_v with
  | MT_option t
  | MT_list t
  | MT_set t
  | MT_contract t ->
    t
  | _ ->
    TzError ("get_innertyp : " ^ Sexp.to_string (sexp_of_cc sexp_of_mich_t ttt))
    |> Stdlib.raise
(* function get_innertyp end *)

let get_innertyp2 : mich_t cc -> mich_t cc * mich_t cc =
  fun ttt ->
  match ttt.cc_v with
  | MT_pair (t1, t2)
  | MT_or (t1, t2)
  | MT_lambda (t1, t2)
  | MT_map (t1, t2)
  | MT_big_map (t1, t2) ->
    (t1, t2)
  | _ ->
    TzError ("get_innertyp2 : " ^ Sexp.to_string (sexp_of_cc sexp_of_mich_t ttt))
    |> Stdlib.raise
(* function get_innertyp2 end *)

(******************************************************************************)
(* Michelson Cut Information                                                  *)
(******************************************************************************)

let lb_of_ln_mci : mich_cut_info -> mich_cut_info option =
  fun mci ->
  let (lb_mcc, flag) : mich_cut_category * bool =
     match mci.mci_cutcat with
     | MCC_ln_loop     -> (MCC_lb_loop, true)
     | MCC_ln_loopleft -> (MCC_lb_loopleft, true)
     | MCC_ln_map      -> (MCC_lb_map, true)
     | MCC_ln_iter     -> (MCC_lb_iter, true)
     | _ as m          -> (m, false)
  in
  if flag then Some { mci with mci_cutcat = lb_mcc } else None
(* function lb_of_ln_mci end *)

let lb_of_ln_exn : mich_cut_info -> mich_cut_info =
  fun mci ->
  lb_of_ln_mci mci
  |> function
  | Some bbb -> bbb
  | None     -> TzError "lb_of_ln_exn" |> Stdlib.raise
(* function lb_of_ln_exn end *)

let is_ln_mcc : mich_cut_category -> bool = function
| MCC_trx_entry -> false
| MCC_trx_exit -> false
| MCC_ln_loop
| MCC_ln_loopleft
| MCC_ln_map
| MCC_ln_iter ->
  true
| MCC_lb_loop
| MCC_lb_loopleft
| MCC_lb_map
| MCC_lb_iter ->
  false
| MCC_query _ -> false
(* function is_ln_mcc end *)

let is_ln_mci : mich_cut_info -> bool = (fun mci -> is_ln_mcc mci.mci_cutcat)

let ln_of_lb_mci : mich_cut_info -> mich_cut_info option =
  fun mci ->
  let (ln_mcc, flag) : mich_cut_category * bool =
     match mci.mci_cutcat with
     | MCC_lb_loop     -> (MCC_ln_loop, true)
     | MCC_lb_loopleft -> (MCC_ln_loopleft, true)
     | MCC_lb_map      -> (MCC_ln_map, true)
     | MCC_lb_iter     -> (MCC_ln_iter, true)
     | _ as m          -> (m, false)
  in
  if flag then Some { mci with mci_cutcat = ln_mcc } else None
(* function ln_of_lb_mci end *)

let ln_of_lb_exn : mich_cut_info -> mich_cut_info =
  fun mci ->
  ln_of_lb_mci mci
  |> function
  | Some nnn -> nnn
  | None     -> TzError "ln_of_lb_exn" |> Stdlib.raise
(* function ln_of_lb_exn end *)

let is_lb_mcc : mich_cut_category -> bool = function
| MCC_trx_entry -> false
| MCC_trx_exit -> false
| MCC_ln_loop
| MCC_ln_loopleft
| MCC_ln_map
| MCC_ln_iter ->
  false
| MCC_lb_loop
| MCC_lb_loopleft
| MCC_lb_map
| MCC_lb_iter ->
  true
| MCC_query _ -> false
(* function is_lb_mcc end *)

let is_lb_mci : mich_cut_info -> bool = (fun mci -> is_lb_mcc mci.mci_cutcat)

let exit_of_entry_mci : mich_cut_info -> mich_cut_info option =
  fun mci ->
  let (exit_mcc, flag) : mich_cut_category * bool =
     match mci.mci_cutcat with
     | MCC_trx_entry -> (MCC_trx_exit, true)
     | _ as m        -> (m, false)
  in
  if flag then Some { mci with mci_cutcat = exit_mcc } else None
(* function exit_of_entry_mci end *)

let exit_of_entry_exn : mich_cut_info -> mich_cut_info =
  fun mci ->
  exit_of_entry_mci mci
  |> function
  | Some xxx -> xxx
  | None     -> TzError "exit_of_entry_exn" |> Stdlib.raise
(* function exit_of_entry_exn end *)

let is_exit_mcc : mich_cut_category -> bool = function
| MCC_trx_entry -> false
| MCC_trx_exit -> true
| MCC_ln_loop
| MCC_ln_loopleft
| MCC_ln_map
| MCC_ln_iter ->
  false
| MCC_lb_loop
| MCC_lb_loopleft
| MCC_lb_map
| MCC_lb_iter ->
  false
| MCC_query _ -> false
(* function is_exit_mcc end *)

let is_exit_mci : mich_cut_info -> bool = (fun mci -> is_exit_mcc mci.mci_cutcat)

let entry_of_exit_mci : mich_cut_info -> mich_cut_info option =
  fun mci ->
  let (entry_mcc, flag) : mich_cut_category * bool =
     match mci.mci_cutcat with
     | MCC_trx_exit -> (MCC_trx_entry, true)
     | _ as m       -> (m, false)
  in
  if flag then Some { mci with mci_cutcat = entry_mcc } else None
(* function entry_of_exit_mci end *)

let entry_of_exit_exn : mich_cut_info -> mich_cut_info =
  fun mci ->
  entry_of_exit_mci mci
  |> function
  | Some xxx -> xxx
  | None     -> TzError "entry_of_exit_exn" |> Stdlib.raise
(* function entry_of_exit_exn end *)

let is_entry_mcc : mich_cut_category -> bool = function
| MCC_trx_entry -> true
| MCC_trx_exit -> false
| MCC_ln_loop
| MCC_ln_loopleft
| MCC_ln_map
| MCC_ln_iter ->
  false
| MCC_lb_loop
| MCC_lb_loopleft
| MCC_lb_map
| MCC_lb_iter ->
  false
| MCC_query _ -> false
(* function is_entry_mcc end *)

let is_entry_mci : mich_cut_info -> bool =
  (fun mci -> is_entry_mcc mci.mci_cutcat)

let get_reduced_mcc : mich_cut_category -> r_mich_cut_category = function
| MCC_trx_entry
| MCC_trx_exit ->
  RMCC_trx
| MCC_ln_loop
| MCC_lb_loop ->
  RMCC_loop
| MCC_ln_loopleft
| MCC_lb_loopleft ->
  RMCC_loopleft
| MCC_ln_map
| MCC_lb_map ->
  RMCC_map
| MCC_ln_iter
| MCC_lb_iter ->
  RMCC_iter
| MCC_query qc -> RMCC_query qc
(* function get_reduced_mcc end *)

let get_reduced_mci : mich_cut_info -> r_mich_cut_info =
  fun mci ->
  { rmci_loc = mci.mci_loc; rmci_cutcat = get_reduced_mcc mci.mci_cutcat }

(******************************************************************************)
(******************************************************************************)
(* Michelson to Tz                                                            *)
(******************************************************************************)
(******************************************************************************)

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
