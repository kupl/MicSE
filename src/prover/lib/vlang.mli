(*****************************************************************************)
(*****************************************************************************)
(* Formula Representation                                                    *)
(*****************************************************************************)
(*****************************************************************************)

module Ty : sig
    type t =
      | T_int
      | T_nat
      | T_string
      | T_bytes
      | T_mutez
      | T_bool
      | T_key_hash
      | T_timestamp
      | T_address
      | T_key
      | T_unit
      | T_signature
      | T_option    of t
      | T_list      of t
      | T_set       of t
      | T_operation
      | T_contract  of t
      | T_pair      of t * t
      | T_or        of t * t
      | T_lambda    of t * t
      | T_map       of t * t
      | T_big_map   of t * t
      | T_chain_id
  end
  
  
  module Expr : sig
  
    type var = PreLib.Cfg.ident (* string *)
    type typ = Ty.t
  
    type raw_code = PreLib.Mich.inst PreLib.Mich.t
    type raw_program = PreLib.Mich.program
    type lambda_ident = Core.Int.t
    
    type t =
      (*************************************************************************)
      (* Variable & Polymorphic                                                *)
      (*************************************************************************)
      | V_var of (typ * var)
      | V_car of t  (* ('a, 'b) pair -> 'a *)
      | V_cdr of t  (* ('a, 'b) pair -> 'b *)
      | V_unlift_option of t  (* 'a option -> 'a *)
      | V_unlift_left of t  (* ('a, 'b) or -> 'a *)
      | V_unlift_right of t (* ('a, 'b) or -> 'b *) 
      | V_hd_l of t (* 'a list -> 'a *)
      | V_hd_s of t (* 'a set -> 'a *)
      | V_exec of t * t (* 'a * ('a, 'b) lambda -> 'b *)
      | V_dup  of t   (* 'a -> 'a *)
      | V_itself of t (* 'a -> 'a *)
  
      (*************************************************************************)
      (* Integer                                                               *)
      (*************************************************************************)
      | V_lit_int of Z.t
      | V_neg_ni of t (* nat -> int *)
      | V_neg_ii of t (* int -> int *)
      | V_not_ni of t (* nat -> int *)
      | V_not_ii of t (* int -> int *)
      | V_add_nii of t * t  (* nat * int -> int *)
      | V_add_ini of t * t  (* int * nat -> int *)
      | V_add_iii of t * t  (* int * int -> int *)
      | V_sub_nni of t * t  (* nat * nat -> int *)
      | V_sub_nii of t * t  (* nat * int -> int *)
      | V_sub_ini of t * t  (* int * nat -> int *)
      | V_sub_iii of t * t  (* int * int -> int *)
      | V_sub_tti of t * t  (* timestamp * timestamp -> int *)
      | V_mul_nii of t * t  (* nat * int -> int *)
      | V_mul_ini of t * t  (* int * nat -> int *)
      | V_mul_iii of t * t  (* int * int -> int *)
      | V_compare of t * t  (* 'a * 'a -> int *)
      | V_int_of_nat of t (* nat -> int *)
  
      (*************************************************************************)
      (* Natural Number                                                        *)
      (*************************************************************************)
      | V_lit_nat of Z.t
      | V_abs_in of t (* int -> nat *)
      | V_add_nnn of t * t  (* nat * nat -> nat *)
      | V_mul_nnn of t * t  (* nat * nat -> nat *)
      | V_shiftL_nnn of t * t (* nat * nat -> nat *)
      | V_shiftR_nnn of t * t (* nat * nat -> nat *)
      | V_and_nnn   of t * t  (* nat * nat -> nat *)
      | V_and_inn   of t * t  (* int * nat -> nat *)
      | V_or_nnn    of t * t (* nat * nat -> nat *)
      | V_xor_nnn   of t * t (* nat * nat -> nat *)
      | V_size_s    of t  (* 'a set -> nat *)
      | V_size_m    of t  (* ('k, 'v) map -> nat *)
      | V_size_l    of t  (* 'a list -> nat *)
      | V_size_str  of t  (* string -> nat *)
      | V_size_b    of t  (* bytes -> nat *)
  
      (*************************************************************************)
      (* String                                                                *)
      (*************************************************************************)
      | V_lit_string of string
      | V_concat_sss of t * t (* string * string -> string *)
      | V_concat_list_s of t  (* string list -> string *)
  
      (*************************************************************************)
      (* Bytes                                                                 *)
      (*************************************************************************)
      | V_lit_bytes of string
      | V_concat_bbb of t * t (* bytes * bytes -> bytes *)
      | V_concat_list_b of t  (* bytes list -> bytes *)
      | V_pack    of t (* 'a -> bytes *)
      | V_blake2b of t  (* bytes -> bytes *)
      | V_sha256  of t (* bytes -> bytes *)
      | V_sha512  of t (* bytes -> bytes *)
  
      (*************************************************************************)
      (* Mutez                                                                 *)
      (*************************************************************************)
      | V_lit_mutez of Z.t
      | V_amount
      | V_balance
      | V_add_mmm of t * t  (* mutez * mutez -> mutez *)
      | V_sub_mmm of t * t  (* mutez * mutez -> mutez *)
      | V_mul_mnm of t * t  (* mutez * nat -> mutez *)
      | V_mul_nmm of t * t  (* nat * mutez -> mutez *)
  
      (*************************************************************************)
      (* Bool                                                                  *)
      (*************************************************************************)
      | V_lit_bool of bool
      | V_not_bb  of t (* bool -> bool *)
      | V_and_bbb of t * t  (* bool * bool -> bool *)
      | V_or_bbb  of t * t  (* bool * bool -> bool *)
      | V_xor_bbb of t * t  (* bool * bool -> bool *)
      | V_eq_ib   of t  (* int -> bool *)
      | V_neq_ib  of t  (* int -> bool *)
      | V_lt_ib   of t  (* int -> bool *)
      | V_gt_ib   of t  (* int -> bool *)
      | V_leq_ib  of t  (* int -> bool *)
      | V_geq_ib  of t  (* int -> bool *)
      | V_mem_xsb of t * t  (* 'a * 'a set -> bool *)
      | V_mem_xmb of t * t  (* 'k * ('k, 'v) map -> bool *)
      | V_mem_xbmb of t * t  (* 'k * ('k, 'v) big_map -> bool *)
      | V_check_signature of t * t * t (* key * signature * bytes -> bool *)
  
      (*************************************************************************)
      (* Key Hash                                                              *)
      (*************************************************************************)
      | V_lit_key_hash of string 
      | V_hash_key of t (* key -> key_hash *)
  
      (*************************************************************************)
      (* Timestamp                                                             *)
      (*************************************************************************)
      | V_lit_timestamp_str of string 
      | V_lit_timestamp_sec of Z.t
      | V_now
      | V_add_tit of t * t  (* timestamp * int -> timestamp *)
      | V_add_itt of t * t  (* int * timestamp -> timestamp *)
      | V_sub_tit of t * t  (* timestamp * int -> timestamp *)
  
      (*************************************************************************)
      (* Address                                                               *)
      (*************************************************************************)
      | V_lit_address of t (* key_hash -> address *)
      | V_source
      | V_sender
      | V_address_of_contract of t (* 'a contract -> address *)
  
      (*************************************************************************)
      (* Key                                                                   *)
      (*************************************************************************)
      | V_lit_key of string
  
      (*************************************************************************)
      (* Unit                                                                  *)
      (*************************************************************************)
      (* | V_lit_unit : t_unit t *) (* V_unit has the same feature. *)
      | V_unit
  
      (*************************************************************************)
      (* Signature                                                             *)
      (*************************************************************************)
      | V_lit_signature_str of string
      | V_lit_signature_signed of t * t  (* key * bytes -> signature *)
  
      (*************************************************************************)
      (* Option                                                                *)
      (*************************************************************************)
      (* | V_lit_option : 'a t option -> 'a t_option t *) (* V_some and V_none has the same feature. *)
      | V_some of t (* 'a -> 'a option *)
      | V_none of typ (* ('a) -> 'a option *)
      | V_ediv_nnnn of t * t  (* nat * nat -> (nat, nat) pair option *)
      | V_ediv_niin of t * t  (* nat * int -> (int, nat) pair option *)
      | V_ediv_inin of t * t  (* int * nat -> (int, nat) pair option *)
      | V_ediv_iiin of t * t  (* int * int -> (int, nat) pair option *)
      | V_ediv_mnmm of t * t  (* mutez * nat -> (mutez, mutez) pair option *)
      | V_ediv_mmnm of t * t  (* mutez * mutez -> (nat, mutez) pair option *)
      | V_get_xmoy  of t * t  (* 'k * ('k, 'v) map -> 'v option *)
      | V_get_xbmo  of t * t  (* 'k * ('k, 'v) big_map -> 'v option *)
      | V_slice_nnso of t * t * t (* nat * nat * string -> string option *)
      | V_slice_nnbo of t * t * t (* nat * nat * bytes -> bytes option *)
      | V_unpack of typ * t (* ('a) * bytes -> 'a option *)
      | V_contract_of_address of typ * t (* ('a) -> address -> 'a contract option *)
      | V_isnat of t (* int -> nat option *)
  
      (*************************************************************************)
      (* List                                                                  *)
      (*************************************************************************)
      | V_lit_list of typ * t list  (* ('a) * list-literal -> 'a list *)
      | V_nil of typ  (* ('a) -> 'a list *)
      | V_cons of t * t (* 'a * 'a list -> 'a list *)
      | V_tl_l of t (* 'a list -> 'a list *)
  
      (*************************************************************************)
      (* Set                                                                   *)
      (*************************************************************************)
      | V_lit_set of typ * t Core.Set.Poly.t  (* ('a) * set-literal -> 'a set *)
      | V_empty_set of typ (* ('a) -> 'a set *)
      | V_update_xbss of t * t * t  (* 'a * bool * 'a set -> 'a set *)
      | V_tl_s of t (* 'a set -> 'a set *)
  
      (*************************************************************************)
      (* Operation                                                             *)
      (*************************************************************************)
      (* | V_lit_operation of t_operation t *) (* V_create_contract, V_transfer_tokens, V_set_delegate has the same feature. *)
      | V_create_contract of typ * typ * t * t * t * t (* ('param) * ('strg) * (('param, 'strg) pair, (operation list, 'strg) pair) lambda * key_hash option * mutez * 'strg -> operation *)
      | V_transfer_tokens of t * t * t  (* 'a * mutez * 'a contract -> operation *)
      | V_set_delegate of t (* key_hash option -> operation *)
  
      (*************************************************************************)
      (* Contract                                                              *)
      (*************************************************************************)
      | V_lit_contract of t * t * t * t * t (* addres * mutez * key_hash option * 'strg * (('param, 'strg) pair, (operation list, 'strg) pair) lambda -> 'param contract *) (* address * balance * delegate * storage * program -> contract *)
      | V_self of typ  (* 'a -> 'a contract *)
      | V_implicit_account of t (* key_hash -> unit contract *)
  
      (*************************************************************************)
      (* Pair                                                                  *)
      (*************************************************************************)
      (* | V_lit_pair : 'a t * 'b t -> ('a, 'b) t_pair t *) (* V_pair has the same feature *)
      | V_pair of t * t  (* 'a * 'b -> ('a, 'b) pair *)
      | V_hd_m of t (* ('k, 'v) map -> ('k, 'v) pair *)
      | V_hd_bm of t  (* ('k, 'v) big_map -> ('k, 'v) big_map *)
  
      (*************************************************************************)
      (* Or                                                                    *)
      (*************************************************************************)
      (* | V_lit_or *) (* It cannot construct any value, use V_left or V_right instead *)
      | V_left of typ * t (* (('a, 'b) or) * 'a -> ('a, 'b) or *)
      | V_right of typ * t (* (('a, 'b) or) * 'b -> ('a, 'b) or *)
  
      (*************************************************************************)
      (* Lambda                                                                *)
      (*************************************************************************)
      | V_lit_program of raw_program  (* ('param, 'strg) Mich.program -> (('param, 'strg) pair, (operation list, 'strg) pair) lambda *)
      | V_lambda_id of typ * typ * lambda_ident (* ('param) * ('strg) * lambda_identifier -> ('param, 'strg) lambda *)
      | V_lit_lambda of typ * typ * raw_code (* ('param) * ('ret) * ('param, 'ret) Mich.inst Mich.t -> ('param, 'ret) lambda *) (* embedded code with LAMBDA Michelson-instruction should be expressed with V_lambda_id, not V_lit_lambda *)
      | V_lambda_unknown of typ * typ (* ('param) * ('ret) -> ('param, 'ret) lambda *)
      | V_lambda_closure of t * t (* (('p1, 'p2) pair, 'ret) lambda * 'p1 -> ('p2, 'ret) lambda *)
  
      (*************************************************************************)
      (* Map                                                                   *)
      (*************************************************************************)
      | V_lit_map of typ * typ * (t, t) Core.Map.Poly.t (* ('k) * ('v) * map-literal -> ('k, 'v) map *)
      | V_empty_map of typ * typ (* ('k) * ('v) -> ('k, 'v) map *)
      | V_update_xomm of t * t * t (* 'k * 'v option * ('k, 'v) map -> ('k, 'v) map *)
      | V_tl_m of t (* ('k, 'v) map -> ('k, 'v) map *)
  
      (*************************************************************************)
      (* Big Map                                                               *)
      (*************************************************************************)
      | V_lit_big_map of typ * typ * (t, t) Core.Map.Poly.t (* ('k) * ('v) * map-literal -> ('k, 'v) big_map *)
      | V_empty_big_map of typ * typ  (* ('k) * ('v) -> ('k, 'v) big_map *)
      | V_update_xobmbm of t * t * t  (* 'k * 'v option * ('k, 'v) big_map -> ('k, 'v) big_map *)
      | V_tl_bm of t (* ('k, 'v) big_map -> ('k, 'v) big_map *)
  
      (*************************************************************************)
      (* Chain Id                                                              *)
      (*************************************************************************)
      | V_lit_chain_id of string
      | V_chain_id (* chain_id -> chain_id *)
  
  end (* module Expr end *)
  
  
  module Formula : sig
  
    type t =
    (* Logical Formula *)
    | VF_true
    | VF_false
    | VF_not of t
    | VF_and of t list
    | VF_or of t list
    | VF_eq of Expr.t * Expr.t  (* 'a * 'a -> formula *)
    | VF_imply of t * t
    (* MicSE-Cfg Specific Boolean *)  
    | VF_mich_if of Expr.t (* bool -> formula *)
    | VF_mich_if_none of Expr.t (* 'a option -> formula *)
    | VF_mich_if_left of Expr.t (* ('a, 'b) or -> formula *)
    | VF_mich_if_cons of Expr.t (* 'a list -> formula *)
    | VF_mich_loop of Expr.t (* bool -> formula*)
    | VF_mich_loop_left of Expr.t (* ('a, 'b) or -> formula*)
    | VF_mich_map_l of Expr.t (* 'a list -> formula *)
    | VF_mich_map_m of Expr.t (* ('k, 'v) map -> formula *)
    | VF_mich_iter_l of Expr.t (* 'a list -> formula *)
    | VF_mich_iter_s of Expr.t (* 'a set -> formula *)
    | VF_mich_iter_m of Expr.t (* ('k, 'v) map -> formula *)
    | VF_mich_micse_check_value of Expr.t (* bool -> formula *)
  
  end (* module Formula end *)
  
  
  type t = Formula.t
  type typ = Ty.t
  
  type v_formula = t  (* legacy *)
  (*type typ = Pre.Lib.Adt.typ (* legacy *)*)
  type var = Expr.var (* legacy *)
  type v_exp = Expr.t (* legacy *)
  
  
  (*****************************************************************************)
  (*****************************************************************************)
  (* Type Utility                                                              *)
  (*****************************************************************************)
  (*****************************************************************************)
  
  module TypeUtil : sig
    type michtyp = PreLib.Mich.typ
    type mty = PreLib.Mich.typ PreLib.Mich.t
    
    exception InvalidTyp of typ
    val invalidtyp : typ -> 'a
  
    val ty_of_michtyp : michtyp -> typ
    val ty_of_mty : mty -> typ
  
    (* WARNING: "ty_of_expr" does not check type validity of given expression *)
    val ty_of_expr : Expr.t -> typ
  
  end (* module TypeUtil end *)
  
  
  (*****************************************************************************)
  (*****************************************************************************)
  (* Utility                                                                   *)
  (*****************************************************************************)
  (*****************************************************************************)
  
  val string_of_formula : t -> string
  