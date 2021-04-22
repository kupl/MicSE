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
type ccp_loc = CCLOC_Unknown | CCLOC_Pos of ccp_pos * ccp_pos
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
  | MV_symbol of (mich_t cc * string)
  | MV_car of mich_v cc  (* ('a, 'b) pair -> 'a *)
  | MV_cdr of mich_v cc  (* ('a, 'b) pair -> 'b *)
  | MV_unlift_option of mich_v cc  (* 'a option -> 'a *)
  | MV_unlift_left of mich_v cc  (* ('a, 'b) or -> 'a *)
  | MV_unlift_right of mich_v cc (* ('a, 'b) or -> 'b *) 
  | MV_hd_l of mich_v cc (* 'a list -> 'a *)

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
  | MV_create_contract of mich_t cc * mich_t cc * mich_v cc * mich_v cc * mich_v cc * mich_v cc * mich_v cc (* ('param) * ('strg) * (('param, 'strg) pair, (operation list, 'strg) pair) lambda * key_hash option * mutez * 'strg * address -> operation *)
  | MV_transfer_tokens of mich_v cc * mich_v cc * mich_v cc  (* 'a * mutez * 'a contract -> operation *)
  | MV_set_delegate of mich_v cc (* key_hash option -> operation *)

  (*************************************************************************)
  (* Contract                                                              *)
  (*************************************************************************)
  | MV_lit_contract of mich_t cc * mich_v cc (* ('a) * address -> 'a contract *)
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
(* Tezos Values                                                              *)
(*****************************************************************************)
(*****************************************************************************)

and blockchain = {
  bc_storage : (mich_v cc, mich_v cc) PMap.t; (* It should not be expressed dynamically, until the whole mich_v can be encoded to single sort in SMT. *)  
  bc_code : (mich_v cc, mich_i cc) PMap.t; (* (address, ((param*strg), (oplist*strg)) lambda) PMap.t *)
  (* (address, contract) map *)
  bc_balance : mich_v cc; (* (address, mutez) map *)
  bc_delegate : mich_v cc; (* (address, address option) map *)
  (* others *)
  bc_chain_id : mich_v cc; (* chain-id *)
  bc_last_blocktime : mich_v cc; (* timestamp *)
}

and explicit_operation =
  | EXOP_transfer_token of (mich_v cc * mich_v cc * mich_v cc * mich_v cc) (* address of the target contract, source, amount, parameter *)

and oper_transfertoken = {
  optt_addr : mich_v cc; (* address *)
  optt_source : mich_v cc; (* address *)
  optt_sender : mich_v cc; (* address *)
  optt_amount : mich_v cc; (* mutez *)
  optt_param : mich_v cc; (* 'param *)
  optt_now : mich_v cc; (* timestamp *)
}


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
  | MF_is_true of mich_v cc (* bool -> formula *)
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
(* Symbolic State                                                            *)
(*****************************************************************************)
(*****************************************************************************)

type mich_cut_category =
| MCC_trx_entry
| MCC_trx_exit
| MCC_ln_loop       (* non-body of the loop location *)
| MCC_ln_loopleft   (* non-body of the loop location *)
| MCC_ln_map        (* non-body of the loop location *)
| MCC_ln_iter       (* non-body of the loop location *)
| MCC_lb_loop       (* body of the loop *)
| MCC_lb_loopleft   (* body of the loop *)
| MCC_lb_map        (* body of the loop *)
| MCC_lb_iter       (* body of the loop *)
| MCC_query

type mich_cut_info = {
  mci_loc : ccp_loc;
  mci_cutcat : mich_cut_category;
}

type sym_state = {
  ss_fixchain : blockchain; (* fixed, concrete blockchain *)
  (* the state for an explicitly called operation *)
  ss_exop : explicit_operation;  (* explicitly called operation *)
  ss_dynchain : blockchain; (* dynamic, unwritten blockchain *)
  ss_exec_addrs : mich_v cc; (* address set of executed contracts *)
  ss_oper_queue : mich_v cc; (* operation list, WARNING: MICSE does not support this feature strictly *)
  (* the state for a single contract call *)
  ss_optt : oper_transfertoken; (* transfer-token operation *)
  ss_entry_mci : mich_cut_info; (* location and category where and when the state starts a (symbolic) execution *)
  ss_entry_symstack : mich_v cc list; (* symbolic stack where the state starts while executing a contract *)
  ss_block_mci : mich_cut_info; (* location and category where and when the state blocked *)
  ss_symstack : mich_v cc list; (* symbolic stack *)
  (* overall constraint *)
  ss_constraints : mich_f list;  (* AND-connected logical formula to constrain this state. *)
}


(*****************************************************************************)
(*****************************************************************************)
(* Utilities                                                                 *)
(*****************************************************************************)
(*****************************************************************************)


(*****************************************************************************)
(* Common Datatype                                                           *)
(*****************************************************************************)

let pmap_of_pset : 'a PSet.t -> key_f:('a -> 'key) -> data_f:('a -> 'data) -> ('key, 'data PSet.t) PMap.t
= fun set ~key_f ~data_f -> begin
  PSet.fold set ~init:(PMap.empty)
    ~f:(fun accmap elem ->
      let (key, data) = (key_f elem, data_f elem) in
      PMap.change accmap key 
        ~f:(function | None -> Some (PSet.singleton data) | Some s -> Some (PSet.add s data))
    )
end (* function pmap_of_pset end *)

let pmap_find_exn : ('a, 'b) PMap.t -> 'a -> ?debug:(string) -> 'b
= fun map key ?debug:(debug="") -> 
  try PMap.find_exn map key with | _ -> Stdlib.failwith debug

let pmap_find_dft : ('a, 'b) PMap.t -> 'a -> default:('b) -> 'b
= fun map key ~default -> (match PMap.find map key with | Some s -> s | None -> default)

let psetmap_update_union : ('a, 'b PSet.t) PMap.t -> 'a -> 'b PSet.t -> ('a, 'b PSet.t) PMap.t
= fun map key set -> PMap.update map key ~f:(function | None -> set | Some s -> PSet.union set s)


(*****************************************************************************)
(* Code Component                                                            *)
(*****************************************************************************)

(* gen_dummy_cc wraps the given value with "Unknown" location and "[]" annotations *)
let gen_dummy_cc : 'a -> 'a cc
= fun x -> {
  cc_loc = CCLOC_Unknown;
  cc_anl = [];
  cc_v = x;
}

let gen_custom_cc : 'ccbase cc -> 'a -> 'a cc
= fun base x -> {base with cc_v=x}


(*****************************************************************************)
(* Type                                                                      *)
(*****************************************************************************)

let rec typ_of_val : mich_v cc -> mich_t cc
= fun v -> begin
  let gen_cc : 'a -> 'a cc = fun x -> {v with cc_v=x} in
  typ_of_val_i gen_cc v.cc_v
end

and typ_of_val_i : (mich_t -> mich_t cc) -> mich_v -> mich_t cc
= fun gen_cc v -> begin
  let err (line : int) = Stdlib.failwith ( "typ_of_val_i : LINE = " ^ (Stdlib.string_of_int line) ) in
  match v with
  (*************************************************************************)
  (* Symbol & Polymorphic                                                  *)
  (*************************************************************************)
  | MV_symbol (t,_) -> t
  | MV_car v -> (function | MT_pair (t1,_) -> t1 | _ -> err Stdlib.__LINE__) ((typ_of_val v).cc_v)
  | MV_cdr v -> (function | MT_pair (_,t2) -> t2 | _ -> err Stdlib.__LINE__) ((typ_of_val v).cc_v)
  | MV_unlift_option v -> (function | MT_option t -> t | _ -> err Stdlib.__LINE__) ((typ_of_val v).cc_v)
  | MV_unlift_left v -> (function | MT_or (t1,_) -> t1 | _ -> err Stdlib.__LINE__) ((typ_of_val v).cc_v)
  | MV_unlift_right v -> (function | MT_or (_,t2) -> t2 | _ -> err Stdlib.__LINE__) ((typ_of_val v).cc_v)
  | MV_hd_l v -> (function | MT_list t -> t | _ -> err Stdlib.__LINE__) ((typ_of_val v).cc_v)
  (*************************************************************************)
  (* Integer                                                               *)
  (*************************************************************************)
  | MV_lit_int _ -> MT_int |> gen_cc
  | MV_neg_ni _ -> MT_int |> gen_cc
  | MV_neg_ii _ -> MT_int |> gen_cc
  | MV_not_ni _ -> MT_int |> gen_cc
  | MV_not_ii _ -> MT_int |> gen_cc
  | MV_add_nii _ -> MT_int |> gen_cc
  | MV_add_ini _ -> MT_int |> gen_cc
  | MV_add_iii _ -> MT_int |> gen_cc
  | MV_sub_nni _ -> MT_int |> gen_cc
  | MV_sub_nii _ -> MT_int |> gen_cc
  | MV_sub_ini _ -> MT_int |> gen_cc
  | MV_sub_iii _ -> MT_int |> gen_cc
  | MV_sub_tti _ -> MT_int |> gen_cc
  | MV_mul_nii _ -> MT_int |> gen_cc
  | MV_mul_ini _ -> MT_int |> gen_cc
  | MV_mul_iii _ -> MT_int |> gen_cc
  | MV_compare _ -> MT_int |> gen_cc
  | MV_int_of_nat _ -> MT_int |> gen_cc
  (*************************************************************************)
  (* Natural Number                                                        *)
  (*************************************************************************)
  | MV_lit_nat _ -> MT_nat |> gen_cc
  | MV_abs_in _ -> MT_nat |> gen_cc
  | MV_add_nnn _ -> MT_nat |> gen_cc
  | MV_mul_nnn _ -> MT_nat |> gen_cc
  | MV_shiftL_nnn _ -> MT_nat |> gen_cc
  | MV_shiftR_nnn _ -> MT_nat |> gen_cc
  | MV_and_nnn _ -> MT_nat |> gen_cc
  | MV_and_inn _ -> MT_nat |> gen_cc
  | MV_or_nnn _ -> MT_nat |> gen_cc
  | MV_xor_nnn _ -> MT_nat |> gen_cc
  | MV_size_s _ -> MT_nat |> gen_cc
  | MV_size_m _ -> MT_nat |> gen_cc
  | MV_size_l _ -> MT_nat |> gen_cc
  | MV_size_str _ -> MT_nat |> gen_cc
  | MV_size_b _ -> MT_nat |> gen_cc
  (*************************************************************************)
  (* String                                                                *)
  (*************************************************************************)
  | MV_lit_string _ -> MT_string |> gen_cc
  | MV_concat_sss _ -> MT_string |> gen_cc
  | MV_concat_list_s _ -> MT_string |> gen_cc
  (*************************************************************************)
  (* Bytes                                                                 *)
  (*************************************************************************)
  | MV_lit_bytes _ -> MT_bytes |> gen_cc
  | MV_concat_bbb _ -> MT_bytes |> gen_cc
  | MV_concat_list_b _ -> MT_bytes |> gen_cc
  | MV_pack _ -> MT_bytes |> gen_cc
  | MV_blake2b _ -> MT_bytes |> gen_cc
  | MV_sha256 _ -> MT_bytes |> gen_cc
  | MV_sha512 _ -> MT_bytes |> gen_cc
  (*************************************************************************)
  (* Mutez                                                                 *)
  (*************************************************************************)
  | MV_lit_mutez _ -> MT_mutez |> gen_cc
  | MV_add_mmm _ -> MT_mutez |> gen_cc
  | MV_sub_mmm _ -> MT_mutez |> gen_cc
  | MV_mul_mnm _ -> MT_mutez |> gen_cc
  | MV_mul_nmm _ -> MT_mutez |> gen_cc
  (*************************************************************************)
  (* Bool                                                                  *)
  (*************************************************************************)
  | MV_lit_bool _ -> MT_bool |> gen_cc
  | MV_not_bb _ -> MT_bool |> gen_cc
  | MV_and_bbb _ -> MT_bool |> gen_cc
  | MV_or_bbb _ -> MT_bool |> gen_cc
  | MV_xor_bbb _ -> MT_bool |> gen_cc
  | MV_eq_ib _ -> MT_bool |> gen_cc
  | MV_neq_ib _ -> MT_bool |> gen_cc
  | MV_lt_ib _ -> MT_bool |> gen_cc
  | MV_gt_ib _ -> MT_bool |> gen_cc
  | MV_leq_ib _ -> MT_bool |> gen_cc
  | MV_geq_ib _ -> MT_bool |> gen_cc
  | MV_mem_xsb _ -> MT_bool |> gen_cc
  | MV_mem_xmb _ -> MT_bool |> gen_cc
  | MV_mem_xbmb _ -> MT_bool |> gen_cc
  | MV_check_signature _ -> MT_bool |> gen_cc
  (*************************************************************************)
  (* Key Hash                                                              *)
  (*************************************************************************)
  | MV_lit_key_hash _ -> MT_key_hash |> gen_cc
  | MV_hash_key _ -> MT_key_hash |> gen_cc
  (*************************************************************************)
  (* Timestamp                                                             *)
  (*************************************************************************)
  | MV_lit_timestamp_str _ -> MT_timestamp |> gen_cc
  | MV_lit_timestamp_sec _ -> MT_timestamp |> gen_cc
  | MV_add_tit _ -> MT_timestamp |> gen_cc
  | MV_add_itt _ -> MT_timestamp |> gen_cc
  | MV_sub_tit _ -> MT_timestamp |> gen_cc
  (*************************************************************************)
  (* Address                                                               *)
  (*************************************************************************)
  | MV_lit_address _ -> MT_address |> gen_cc
  | MV_address_of_contract _ -> MT_address |> gen_cc
  (*************************************************************************)
  (* Key                                                                   *)
  (*************************************************************************)
  | MV_lit_key _ -> MT_key |> gen_cc
  (*************************************************************************)
  (* Unit                                                                  *)
  (*************************************************************************)
  | MV_unit -> MT_unit |> gen_cc
  (*************************************************************************)
  (* Signature                                                             *)
  (*************************************************************************)
  | MV_lit_signature_str _ -> MT_signature |> gen_cc
  | MV_lit_signature_signed _ -> MT_signature |> gen_cc
  (*************************************************************************)
  (* Option                                                                *)
  (*************************************************************************)
  | MV_some v -> MT_option (typ_of_val v) |> gen_cc
  | MV_none t -> MT_option t |> gen_cc
  | MV_ediv_nnnn _ -> MT_option (MT_pair (gen_cc MT_nat, gen_cc MT_nat) |> gen_cc) |> gen_cc
  | MV_ediv_niin _ -> MT_option (MT_pair (gen_cc MT_int, gen_cc MT_nat) |> gen_cc) |> gen_cc
  | MV_ediv_inin _ -> MT_option (MT_pair (gen_cc MT_int, gen_cc MT_nat) |> gen_cc) |> gen_cc
  | MV_ediv_iiin _ -> MT_option (MT_pair (gen_cc MT_int, gen_cc MT_nat) |> gen_cc) |> gen_cc
  | MV_ediv_mnmm _ -> MT_option (MT_pair (gen_cc MT_mutez, gen_cc MT_mutez) |> gen_cc) |> gen_cc
  | MV_ediv_mmnm _ -> MT_option (MT_pair (gen_cc MT_nat, gen_cc MT_mutez) |> gen_cc) |> gen_cc
  | MV_get_xmoy (_,v2) -> (function | MT_map(_,t2) -> MT_option t2 | _ -> err Stdlib.__LINE__) ((typ_of_val v2).cc_v) |> gen_cc
  | MV_get_xbmo (_,v2) -> (function | MT_big_map(_,t2) -> MT_option t2 | _ -> err Stdlib.__LINE__) ((typ_of_val v2).cc_v) |> gen_cc
  | MV_slice_nnso _ -> MT_option (MT_string |> gen_cc) |> gen_cc
  | MV_slice_nnbo _ -> MT_option (MT_bytes |> gen_cc) |> gen_cc
  | MV_unpack (t,_) -> MT_option t |> gen_cc
  | MV_contract_of_address (t,_) -> MT_option (MT_contract t |> gen_cc) |> gen_cc
  | MV_isnat _ -> MT_option (MT_nat |> gen_cc) |> gen_cc
  (*************************************************************************)
  (* List                                                                  *)
  (*************************************************************************)
  | MV_lit_list (t,_) -> MT_list t |> gen_cc
  | MV_nil t -> MT_list t |> gen_cc
  | MV_cons (v,_) -> MT_list (typ_of_val v) |> gen_cc
  | MV_tl_l v -> typ_of_val v
  (*************************************************************************)
  (* Set                                                                   *)
  (*************************************************************************)
  | MV_lit_set (t,_) -> MT_set t |> gen_cc
  | MV_empty_set t -> MT_set t |> gen_cc
  | MV_update_xbss (v,_,_) -> MT_set (typ_of_val v) |> gen_cc
  (*************************************************************************)
  (* Operation                                                             *)
  (*************************************************************************)
  | MV_create_contract _ -> MT_operation |> gen_cc
  | MV_transfer_tokens _ -> MT_operation |> gen_cc
  | MV_set_delegate _ -> MT_operation |> gen_cc
  (*************************************************************************)
  (* Contract                                                              *)
  (*************************************************************************)
  | MV_lit_contract (t,_) -> MT_contract t |> gen_cc
  | MV_self t -> MT_contract t |> gen_cc
  | MV_implicit_account _ -> MT_contract (gen_cc MT_unit) |> gen_cc
  (*************************************************************************)
  (* Pair                                                                  *)
  (*************************************************************************)
  | MV_pair (v1,v2) -> MT_pair (typ_of_val v1, typ_of_val v2) |> gen_cc
  (*************************************************************************)
  (* Or                                                                    *)
  (*************************************************************************)
  | MV_left (t,_) -> t
  | MV_right (t,_) -> t
  (*************************************************************************)
  (* Lambda                                                                *)
  (*************************************************************************)
  | MV_lit_lambda (t1,t2,_) -> MT_lambda (t1,t2) |> gen_cc
  | MV_lambda_unknown (t1,t2) -> MT_lambda (t1,t2) |> gen_cc
  | MV_lambda_closure (v1,_) -> (
    (match ((typ_of_val v1).cc_v) with
      | MT_lambda (t1,t3) -> (match t1.cc_v with | MT_pair (_,t12) -> MT_lambda (t12,t3) | _ -> err Stdlib.__LINE__)
      | _ -> err Stdlib.__LINE__)
    |> gen_cc
  )
  (*************************************************************************)
  (* Map                                                                   *)
  (*************************************************************************)
  | MV_lit_map (t1,t2,_) -> MT_map (t1,t2) |> gen_cc
  | MV_empty_map (t1,t2) -> MT_map (t1,t2) |> gen_cc
  | MV_update_xomm (v1,v2,_) -> (
    let t1 = (typ_of_val v1).cc_v |> gen_cc in
    (match ((typ_of_val v2).cc_v) with
      | MT_option t2 -> MT_map (t1,t2)
      | _ -> err Stdlib.__LINE__)
    |> gen_cc
  )
  (*************************************************************************)
  (* Big Map                                                               *)
  (*************************************************************************)
  | MV_lit_big_map (t1,t2,_) -> MT_big_map (t1,t2) |> gen_cc
  | MV_empty_big_map (t1,t2) -> MT_big_map (t1,t2) |> gen_cc
  | MV_update_xobmbm (v1,v2,_) -> (
    let t1 = (typ_of_val v1).cc_v |> gen_cc in
    (match ((typ_of_val v2).cc_v) with
      | MT_option t2 -> MT_big_map (t1,t2)
      | _ -> err Stdlib.__LINE__)
    |> gen_cc
  )
  (*************************************************************************)
  (* Chain Id                                                              *)
  (*************************************************************************)
  | MV_lit_chain_id _ -> MT_chain_id |> gen_cc
end (* function typ_of_val_i end *)

let get_innertyp : mich_t cc -> mich_t cc
= (fun ttt -> match ttt.cc_v with
  | MT_option t | MT_list t | MT_set t | MT_contract t -> t
  | _ -> Stdlib.failwith "Tz.get_innertyp"
)

let get_innertyp2 : mich_t cc -> (mich_t cc * mich_t cc)
= (fun ttt -> match ttt.cc_v with
  | MT_pair (t1, t2) | MT_or (t1, t2) | MT_lambda (t1, t2) | MT_map (t1, t2) | MT_big_map (t1, t2) -> (t1, t2)
  | _ -> Stdlib.failwith "Tz.get_innertyp2"
)


(*****************************************************************************)
(* Michelson Cut Category & Cut Info                                         *)
(*****************************************************************************)

let lb_of_ln_mci : mich_cut_info -> mich_cut_info option
= fun mci -> begin
  let lb_mcc, flag =
    (match mci.mci_cutcat with
    | MCC_ln_loop     -> MCC_lb_loop    , true
    | MCC_ln_loopleft -> MCC_lb_loopleft, true
    | MCC_ln_map      -> MCC_lb_map     , true
    | MCC_ln_iter     -> MCC_lb_iter    , true
    | _ as m          -> m              , false) 
  in 
  if flag then Some {mci with mci_cutcat=lb_mcc;} else None
end

let lb_of_ln_exn : mich_cut_info -> debug:(string) -> mich_cut_info
= fun mci ~debug -> begin
  match lb_of_ln_mci mci with | Some s -> s | None -> Stdlib.failwith debug
end

let is_ln_mcc : mich_cut_category -> bool =
  (function
  | MCC_ln_loop       -> true
  | MCC_ln_loopleft   -> true
  | MCC_ln_map        -> true
  | MCC_ln_iter       -> true
  | MCC_trx_entry     -> false
  | MCC_trx_exit      -> false
  | MCC_lb_loop       -> false
  | MCC_lb_loopleft   -> false
  | MCC_lb_map        -> false
  | MCC_lb_iter       -> false
  | MCC_query         -> false
  )


(*****************************************************************************)
(* Symbol & Symbolic Stack                                                   *)
(*****************************************************************************)

let new_symbol_counter = ref Z.zero

let gen_new_symname_s s = ((new_symbol_counter := Z.succ !new_symbol_counter); s ^ (Z.to_string !new_symbol_counter))
let gen_new_symname () = gen_new_symname_s "v"

let gen_new_symval_t = fun t -> {t with cc_v=(MV_symbol(t, gen_new_symname ()))}
let gen_new_symval_v = fun v -> {v with cc_v=(MV_symbol(typ_of_val v, gen_new_symname ()))}
let gen_new_symval_ts = fun t s -> {t with cc_v=(MV_symbol(t, gen_new_symname_s s))}

let gen_newvar_symstack_ts : (mich_t cc list) -> (mich_v cc list)
= fun tlist -> List.map gen_new_symval_t tlist
let gen_newvar_symstack_vs : (mich_v cc list) -> (mich_v cc list)
= fun vlist -> gen_newvar_symstack_ts (List.map typ_of_val vlist)

(* "stack_eq_fmla s1 s2" Precondition : two stacks should have the same length *)
let stack_eq_fmla : (mich_v cc list) -> (mich_v cc list) -> (mich_f list)
= fun s1 s2 -> List.map2 (fun v1 v2 -> MF_eq (v1, v2)) s1 s2


(*****************************************************************************)
(* OCaml container -> Tz container                                           *)
(*****************************************************************************)

let pmap_to_mtmap : mich_t cc * mich_t cc * (mich_v cc, mich_v cc) PMap.t -> mich_v cc
= let gdcc = gen_dummy_cc in
  fun (kt, vt, pm) -> begin
  PMap.fold
    pm
    ~init:(MV_empty_map (kt,vt) |> gdcc)
    ~f:(fun ~key ~data acc -> MV_update_xomm (key, MV_some data |> gdcc, acc) |> gdcc)
end (* function pmap_to_mtmap end *)


(*****************************************************************************)
(* Invariant Application form for each mich_cut_category                     *)
(*****************************************************************************)

(* Currently, we regard the invariant uses the base stack elements only *)
let inv_app_guide_entry : (mich_v cc list -> mich_f) -> (sym_state) -> mich_f
= let module CList = Core.List in
  fun inv_f ss -> begin
  let sstack : mich_v cc list = ss.ss_entry_symstack in
  match ss.ss_entry_mci.mci_cutcat with
  | MCC_trx_entry     -> inv_f sstack
  | MCC_trx_exit      -> Stdlib.failwith "inv_app_guide_entry : MCC_trx_exit : unexpected"
  | MCC_ln_loop       -> inv_f sstack
  | MCC_ln_loopleft   -> inv_f (CList.tl_exn sstack)
  | MCC_ln_map        -> inv_f (CList.tl_exn sstack)
  | MCC_ln_iter       -> inv_f sstack
  | MCC_lb_loop       -> inv_f sstack
  | MCC_lb_loopleft   -> inv_f (CList.tl_exn sstack)
  | MCC_lb_map        -> inv_f (CList.tl_exn sstack)
  | MCC_lb_iter       -> inv_f (CList.tl_exn sstack)
  | MCC_query         -> Stdlib.failwith "inv_app_guide_entry : MCC_query : unexpected"
end (* function inv_app_guide_entry end *)
let inv_app_guide_block : (mich_v cc list -> mich_f) -> (sym_state) -> mich_f
= let module CList = Core.List in
  fun inv_f ss -> begin
  let sstack : mich_v cc list = ss.ss_symstack in
  match ss.ss_block_mci.mci_cutcat with
  | MCC_trx_entry     -> Stdlib.failwith "inv_app_guide_block : MCC_trx_entry : unexpected"
  | MCC_trx_exit      -> inv_f sstack
  | MCC_ln_loop       -> inv_f (CList.tl_exn sstack)
  | MCC_ln_loopleft   -> inv_f (CList.tl_exn sstack)
  | MCC_ln_map        -> inv_f (CList.tl_exn sstack)
  | MCC_ln_iter       -> inv_f (CList.tl_exn sstack)
  | MCC_lb_loop       -> inv_f (CList.tl_exn sstack)
  | MCC_lb_loopleft   -> inv_f (CList.tl_exn sstack)
  | MCC_lb_map        -> inv_f (CList.tl_exn sstack)
  | MCC_lb_iter       -> inv_f sstack
  | MCC_query         -> Stdlib.failwith "inv_app_guide_block : MCC_query : unexpected"
end (* function inv_app_guide_exit end *)
