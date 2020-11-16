(*****************************************************************************)
(*****************************************************************************)
(* Formula Representation                                                    *)
(*****************************************************************************)
(*****************************************************************************)

module Ty = struct
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


module Expr = struct

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
    | V_create_contract of typ * typ * t * t * t * t (* ('param) * ('strg) * lambda * key_hash option * mutez * 'strg -> operation *)
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
    | V_lit_big_map of typ * typ * (t, t) Core.Map.Poly.t (* ('k) * ('v) * map-literal -> ('k, 'v) map *)
    | V_empty_big_map of typ * typ  (* ('k) * ('v) -> ('k, 'v) big_map *)
    | V_update_xobmbm of t * t * t  (* 'k * 'v option * ('k, 'v) big_map -> ('k, 'v) big_map *)
    | V_tl_bm of t (* ('k, 'v) big_map -> ('k, 'v) big_map *)

    (*************************************************************************)
    (* Chain Id                                                              *)
    (*************************************************************************)
    | V_lit_chain_id of string
    | V_chain_id

end (* module Expr end *)


module Formula = struct

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

module TypeUtil = struct
  type mty = PreLib.Mich.typ PreLib.Mich.t
  type michtyp = PreLib.Mich.typ

  exception InvalidTyp of typ
  let invalidtyp t = Stdlib.raise (InvalidTyp t)

  let rec ty_of_mty : mty -> typ
  =fun mtt -> begin
    ty_of_michtyp (PreLib.Mich.get_d mtt)
  end
  and ty_of_michtyp : michtyp -> typ
  = let open Ty in
    let open PreLib in
    (function
    | Mich.T_key                -> T_key
    | Mich.T_unit               -> T_unit
    | Mich.T_signature          -> T_signature
    | Mich.T_option    t1       -> T_option (ty_of_mty t1)
    | Mich.T_list      t1       -> T_list (ty_of_mty t1)
    | Mich.T_set       t1       -> T_set (ty_of_mty t1)
    | Mich.T_operation          -> T_operation
    | Mich.T_contract  t1       -> T_contract (ty_of_mty t1)
    | Mich.T_pair      (t1, t2) -> T_pair (ty_of_mty t1, ty_of_mty t2)
    | Mich.T_or        (t1, t2) -> T_or (ty_of_mty t1, ty_of_mty t2)
    | Mich.T_lambda    (t1, t2) -> T_lambda (ty_of_mty t1, ty_of_mty t2)
    | Mich.T_map       (t1, t2) -> T_map (ty_of_mty t1, ty_of_mty t2)
    | Mich.T_big_map   (t1, t2) -> T_big_map (ty_of_mty t1, ty_of_mty t2)
    | Mich.T_chain_id           -> T_chain_id
    | Mich.T_int                -> T_int
    | Mich.T_nat                -> T_nat
    | Mich.T_string             -> T_string
    | Mich.T_bytes              -> T_bytes
    | Mich.T_mutez              -> T_mutez
    | Mich.T_bool               -> T_bool
    | Mich.T_key_hash           -> T_key_hash
    | Mich.T_timestamp          -> T_timestamp
    | Mich.T_address            -> T_address
    )

  (* WARNING: "ty_of_expr" does not check type validity of given expression *)
  let rec ty_of_expr : Expr.t -> typ
  = let open Ty in
    let open Expr in
    let toe = ty_of_expr in (* syntax sugar *)
    fun eee -> begin
    match eee with
    (*************************************************************************)
    (* Variable & Polymorphic                                                *)
    (*************************************************************************)
    | V_var (t, _) -> t
    | V_car e -> (match toe e with | T_pair (tt, _) -> tt | _ as tt -> invalidtyp tt)
    | V_cdr e -> (match toe e with | T_pair (_, tt) -> tt | _ as tt -> invalidtyp tt)
    | V_unlift_option e -> (match toe e with | T_option tt -> tt | _ as tt -> invalidtyp tt)
    | V_unlift_left e -> (match toe e with | T_or (tt, _) -> tt | _ as tt -> invalidtyp tt)
    | V_unlift_right e -> (match toe e with | T_or (_, tt) -> tt | _ as tt -> invalidtyp tt)
    | V_hd_l e -> (match toe e with | T_list tt -> tt | _ as tt -> invalidtyp tt)
    | V_hd_s e -> (match toe e with | T_set tt -> tt | _ as tt -> invalidtyp tt)
    | V_exec (e1, _) -> (match toe e1 with | T_lambda (_, t2) -> t2 | _ as tt -> invalidtyp tt)
    | V_dup e -> toe e
    | V_itself e -> toe e

    (*************************************************************************)
    (* Integer                                                               *)
    (*************************************************************************)
    | V_lit_int     _
    | V_neg_ni      _
    | V_neg_ii      _
    | V_not_ni      _
    | V_not_ii      _
    | V_add_nii     _
    | V_add_ini     _
    | V_add_iii     _
    | V_sub_nni     _
    | V_sub_nii     _
    | V_sub_ini     _
    | V_sub_iii     _
    | V_sub_tti     _
    | V_mul_nii     _
    | V_mul_ini     _
    | V_mul_iii     _
    | V_compare     _
    | V_int_of_nat  _ -> T_int

    (*************************************************************************)
    (* Natural Number                                                        *)
    (*************************************************************************)
    | V_lit_nat     _
    | V_abs_in      _
    | V_add_nnn     _
    | V_mul_nnn     _
    | V_shiftL_nnn  _
    | V_shiftR_nnn  _
    | V_and_nnn     _
    | V_and_inn     _
    | V_or_nnn      _
    | V_xor_nnn     _
    | V_size_s      _
    | V_size_m      _
    | V_size_l      _
    | V_size_str    _
    | V_size_b      _ -> T_nat

    (*************************************************************************)
    (* String                                                                *)
    (*************************************************************************)
    | V_lit_string    _
    | V_concat_sss    _
    | V_concat_list_s _ -> T_string

    (*************************************************************************)
    (* Bytes                                                                 *)
    (*************************************************************************)
    | V_lit_bytes     _
    | V_concat_bbb    _
    | V_concat_list_b _
    | V_pack          _
    | V_blake2b       _
    | V_sha256        _
    | V_sha512        _ -> T_bytes

    (*************************************************************************)
    (* Mutez                                                                 *)
    (*************************************************************************)
    | V_lit_mutez _
    | V_amount
    | V_balance
    | V_add_mmm   _
    | V_sub_mmm   _
    | V_mul_mnm   _
    | V_mul_nmm   _ -> T_mutez

    (*************************************************************************)
    (* Bool                                                                  *)
    (*************************************************************************)
    | V_lit_bool  _
    | V_not_bb    _
    | V_and_bbb   _
    | V_or_bbb    _
    | V_xor_bbb   _
    | V_eq_ib     _
    | V_neq_ib    _
    | V_lt_ib     _
    | V_gt_ib     _
    | V_leq_ib    _
    | V_geq_ib    _
    | V_mem_xsb   _
    | V_mem_xmb   _
    | V_mem_xbmb  _
    | V_check_signature _ -> T_bool

    (*************************************************************************)
    (* Key Hash                                                              *)
    (*************************************************************************)
    | V_lit_key_hash  _
    | V_hash_key      _ -> T_key_hash

    (*************************************************************************)
    (* Timestamp                                                             *)
    (*************************************************************************)
    | V_lit_timestamp_str _
    | V_lit_timestamp_sec _
    | V_now
    | V_add_tit           _
    | V_add_itt           _
    | V_sub_tit           _ -> T_timestamp

    (*************************************************************************)
    (* Address                                                               *)
    (*************************************************************************)
    | V_lit_address _
    | V_source
    | V_sender
    | V_address_of_contract _ -> T_address

    (*************************************************************************)
    (* Key                                                                   *)
    (*************************************************************************)
    | V_lit_key _ -> T_string

    (*************************************************************************)
    (* Unit                                                                  *)
    (*************************************************************************)
    (* | V_lit_unit : t_unit t *) (* V_unit has the same feature. *)
    | V_unit -> T_unit

    (*************************************************************************)
    (* Signature                                                             *)
    (*************************************************************************)
    | V_lit_signature_str _
    | V_lit_signature_signed _ -> T_signature

    (*************************************************************************)
    (* Option                                                                *)
    (*************************************************************************)
    (* | V_lit_option : 'a t option -> 'a t_option t *) (* V_some and V_none has the same feature. *)
    | V_some e -> T_option (toe e)
    | V_none t -> T_option t
    | V_ediv_nnnn _ -> T_option (T_pair (T_nat, T_nat))
    | V_ediv_niin _ -> T_option (T_pair (T_int, T_nat))
    | V_ediv_inin _ -> T_option (T_pair (T_int, T_nat))
    | V_ediv_iiin _ -> T_option (T_pair (T_int, T_nat))
    | V_ediv_mnmm _ -> T_option (T_pair (T_mutez, T_mutez))
    | V_ediv_mmnm _ -> T_option (T_pair (T_nat, T_mutez))
    | V_get_xmoy (_, e2) -> (match toe e2 with | T_map (_, tt) -> T_option tt | _ as tt -> invalidtyp tt)
    | V_get_xbmo (_, e2) -> (match toe e2 with | T_big_map (_, tt) -> T_option tt | _ as tt -> invalidtyp tt)
    | V_slice_nnso _ -> T_option T_string
    | V_slice_nnbo _ -> T_option T_bytes
    | V_unpack (t, _) -> T_option t
    | V_contract_of_address (t, _) -> T_option (T_contract t)
    | V_isnat _ -> T_option T_int

    (*************************************************************************)
    (* List                                                                  *)
    (*************************************************************************)
    | V_lit_list (t, _) -> T_list t
    | V_nil t -> T_list t
    | V_cons (e1, _) -> T_list (toe e1)
    | V_tl_l e -> toe e

    (*************************************************************************)
    (* Set                                                                   *)
    (*************************************************************************)
    | V_lit_set (t, _) -> T_set t
    | V_empty_set t -> T_set t
    | V_update_xbss (_, _, e3) -> toe e3
    | V_tl_s e -> toe e

    (*************************************************************************)
    (* Operation                                                             *)
    (*************************************************************************)
    (* | V_lit_operation of t_operation t *) (* V_create_contract, V_transfer_tokens, V_set_delegate has the same feature. *)
    | V_create_contract _
    | V_transfer_tokens _
    | V_set_delegate    _ -> T_operation

    (*************************************************************************)
    (* Contract                                                              *)
    (*************************************************************************)
    | V_lit_contract (_, _, _, _, e5) -> (match toe e5 with | T_lambda (T_pair (pt, _), _) -> T_contract pt | _ as tt -> invalidtyp tt)
    | V_self t -> T_contract t
    | V_implicit_account _ -> T_contract T_unit

    (*************************************************************************)
    (* Pair                                                                  *)
    (*************************************************************************)
    (* | V_lit_pair : 'a t * 'b t -> ('a, 'b) t_pair t *) (* V_pair has the same feature *)
    | V_pair (e1, e2) -> T_pair (toe e1, toe e2)
    | V_hd_m e -> (match toe e with | T_map (kt, vt) -> T_pair (kt, vt) | _ as tt -> invalidtyp tt)
    | V_hd_bm e -> (match toe e with | T_big_map (kt, vt) -> T_pair (kt, vt) | _ as tt -> invalidtyp tt)

    (*************************************************************************)
    (* Or                                                                    *)
    (*************************************************************************)
    (* | V_lit_or *) (* It cannot construct any value, use V_left or V_right instead *)
    | V_left (t, _) -> t
    | V_right (t, _) -> t

    (*************************************************************************)
    (* Lambda                                                                *)
    (*************************************************************************)
    | V_lit_program r_p -> (
        let pt_mic, st_mic = r_p.PreLib.Mich.param, r_p.PreLib.Mich.storage in
        let (pt, st) : typ * typ = ty_of_mty pt_mic, ty_of_mty st_mic in
        T_lambda (T_pair (pt, st), T_pair (T_list T_operation, st))
      )
    | V_lambda_id (t1, t2, _) -> T_lambda (t1, t2)
    | V_lit_lambda (t1, t2, _) -> T_lambda (t1, t2)
    | V_lambda_unknown (t1, t2) -> T_lambda (t1, t2)
    | V_lambda_closure (e1, e2) -> (match toe e1, toe e2 with | T_lambda (T_pair(p1, p2), rett), pt when p1 = pt -> T_lambda(p2, rett) | _, tt2 -> invalidtyp tt2)
    (*************************************************************************)
    (* Map                                                                   *)
    (*************************************************************************)
    | V_lit_map (kt, vt, _) -> T_map (kt, vt)
    | V_empty_map (kt, vt) -> T_map (kt, vt)
    | V_update_xomm (_, _, e3) -> toe e3
    | V_tl_m e -> toe e

    (*************************************************************************)
    (* Big Map                                                               *)
    (*************************************************************************)
    | V_lit_big_map (kt, vt, _) -> T_big_map (kt, vt)
    | V_empty_big_map (kt, vt) -> T_big_map (kt, vt)
    | V_update_xobmbm (_, _, e) -> toe e
    | V_tl_bm e -> toe e
    
    (*************************************************************************)
    (* Chain Id                                                              *)
    (*************************************************************************)
    | V_lit_chain_id  _
    | V_chain_id -> T_chain_id

  end (* function ty_of_expr end *)

  (* option & list & set & contract *)
  let get_innertyp : typ -> typ =
    function
    | T_option it
    | T_list it
    | T_set it
    | T_contract it -> it
    | _ as tt -> invalidtyp tt

  (* pair & or & lambda & map & big_map *)
  let get_innertyp2 : typ -> (typ * typ) = 
    function
    | T_pair (t1, t2)
    | T_or (t1, t2)
    | T_lambda (t1, t2)
    | T_map (t1, t2)
    | T_big_map (t1, t2) -> (t1, t2)
    | _ as tt -> invalidtyp tt

end (* module TypeUtil end *)

(*****************************************************************************)
(*****************************************************************************)
(* Utility                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

let string_of_formula : t -> string = fun _ -> "Vlang.string_of_formula.UNIMPLEMENTED_TODO" (* TODO *)
