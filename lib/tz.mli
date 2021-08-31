(* Tz : MicSE's Michelson representation *)

exception TzError of string

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
  | MSC_mich_stack       of int
  | MSC_dip_stack        of int
  | MSC_map_entry_stack  of int
  | MSC_map_exit_stack   of int
  | MSC_map_mapkey_stack of int
  | MSC_iter_stack       of int

and mich_sym_ctxt = int list

(* MV_inv_symbol should be replaced to other mich_v expression.
   If some un-replaced MV_inv_symbol remained formula block in precondition of
   verification condition, that formula block should be replaced to MF_true.
   Caution: Free variable should not be allowed in precondition of formula. *)
(* MV_inv_symbol is now deprecated. use MV_ref or MV_ref_cont instead. *)
and mich_contsym_category =
  | MCSC_iter_cont
  | MCSC_map_entry_cont
  | MCSC_map_exit_cont
  | MCSC_not_implemented

and mich_v =
  (* Michelson Value *)

  (*************************************************************************)
  (* Symbol & Polymorphic                                                  *)
  (*************************************************************************)
  | MV_symbol               of (mich_t cc * mich_sym_category * mich_sym_ctxt)
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
  | MV_mtz_of_op_list       of mich_v cc (* operation list -> mutz *)
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
  | MV_lit_set              of mich_t cc * mich_v cc list (* ('a) * set-literal (list) -> 'a set *)
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
  | MV_ref                  of (mich_t cc * mich_sym_category)
  | MV_ref_cont             of (mich_t cc * mich_contsym_category)
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

module MichTCC_cmp : sig
  type t = mich_t cc [@@deriving compare, sexp]
end

module MichVCC_cmp : sig
  type t = mich_v cc [@@deriving compare, sexp]
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

module MichF_cmp : sig
  type t = mich_f [@@deriving compare, sexp]
end

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

module MichCutInfo_cmp : sig
  type t = mich_cut_info [@@deriving compare, sexp]
end

(* reduced mich_cut_info *)
type r_mich_cut_info = {
  rmci_loc : ccp_loc;
  rmci_cutcat : r_mich_cut_category;
}
[@@deriving sexp, compare, equal]

module RMichCutInfo_cmp : sig
  type t = r_mich_cut_info [@@deriving compare, sexp]
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
  (* container stack for MAP instruction - Entry & Exit & Key *)
  si_map_entry : mich_v cc list;
  si_map_exit : mich_v cc list;
  si_map_mapkey : mich_v cc list;
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

(* entire blockchain symbolic state - designed for single contract verification *)
type sym_state_id = mich_sym_ctxt [@@deriving sexp, compare, equal]

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

module SymState_cmp : sig
  type t = sym_state [@@deriving compare, sexp]
end
