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

  let rec to_string : t -> string
  = let ts = to_string in (* syntax sugar *)
    fun t -> begin
    match t with
    | T_int               -> "Int"
    | T_nat               -> "Nat"
    | T_string            -> "String"
    | T_bytes             -> "Bytes"
    | T_mutez             -> "Mutez"
    | T_bool              -> "Bool"
    | T_key_hash          -> "Key_Hash"
    | T_timestamp         -> "Timestamp"
    | T_address           -> "Address"
    | T_key               -> "Key"
    | T_unit              -> "Unit"
    | T_signature         -> "Signature"
    | T_option t1         -> "Option("    ^ (t1 |> ts) ^ ")"
    | T_list t1           -> "List("      ^ (t1 |> ts) ^ ")"
    | T_set t1            -> "Set("       ^ (t1 |> ts) ^ ")"
    | T_operation         -> "Operation"
    | T_contract t1       -> "Contract("  ^ (t1 |> ts) ^ ")"
    | T_pair (t1, t2)     -> "Pair("      ^ (t1 |> ts) ^ "," ^ (t2 |> ts) ^ ")"
    | T_or (t1, t2)       -> "Or("        ^ (t1 |> ts) ^ "," ^ (t2 |> ts) ^ ")"
    | T_lambda (t1, t2)   -> "Lambda("    ^ (t1 |> ts) ^ "," ^ (t2 |> ts) ^ ")"
    | T_map (t1, t2)      -> "Map("       ^ (t1 |> ts) ^ "," ^ (t2 |> ts) ^ ")"
    | T_big_map (t1, t2)  -> "Big_Map("   ^ (t1 |> ts) ^ "," ^ (t2 |> ts) ^ ")"
    | T_chain_id          -> "Chain_Id"
  end
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
    | V_append_l of t * t (* 'a -> 'a list -> 'a list *)

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
    | V_hdtl_l of t (* 'a list -> ('a, 'a list) pair *)
    | V_hdtl_s of t (* 'a set -> ('a, 'a set) pair *)
    | V_hdtl_m of t (* ('k, 'v) map -> (('k, 'v) pair, ('k, 'v) map) pair *)

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

  let rec to_string : t -> string
  = let ts = to_string in (* syntax sugar *)
    fun e -> begin
      match e with
      (*************************************************************************)
      (* Variable & Polymorphic                                                *)
      (*************************************************************************)
      | V_var (_, v)        -> ""           ^ v
      | V_car e1            -> "CAR("       ^ (e1 |> ts) ^ ")"
      | V_cdr e1            -> "CDR("       ^ (e1 |> ts) ^ ")"
      | V_unlift_option e1  -> "UNOPTION("  ^ (e1 |> ts) ^ ")"
      | V_unlift_left e1    -> "UNLEFT("    ^ (e1 |> ts) ^ ")"
      | V_unlift_right e1   -> "UNRIGHT("   ^ (e1 |> ts) ^ ")"
      | V_hd_l e1           -> "HD"         ^ (e1 |> ts) ^ ")"
      | V_hd_s e1           -> "HD"         ^ (e1 |> ts) ^ ")"
      | V_exec (e1, e2)     -> "EXEC"       ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_dup e1            -> ""           ^ (e1 |> ts)
      | V_itself e1         -> ""           ^ (e1 |> ts)
  
      (*************************************************************************)
      (* Integer                                                               *)
      (*************************************************************************)
      | V_lit_int zn        -> "I_"   ^ (zn |> Z.to_string)
      | V_neg_ni e1         -> "NEG(" ^ (e1 |> ts) ^ ")"
      | V_neg_ii e1         -> "NEG(" ^ (e1 |> ts) ^ ")"
      | V_not_ni e1         -> "NOT(" ^ (e1 |> ts) ^ ")"
      | V_not_ii e1         -> "NOT(" ^ (e1 |> ts) ^ ")"
      | V_add_nii (e1, e2)  -> "ADD(" ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_add_ini (e1, e2)  -> "ADD(" ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_add_iii (e1, e2)  -> "ADD(" ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_sub_nni (e1, e2)  -> "SUB(" ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_sub_nii (e1, e2)  -> "SUB(" ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_sub_ini (e1, e2)  -> "SUB(" ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_sub_iii (e1, e2)  -> "SUB(" ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_sub_tti (e1, e2)  -> "SUB(" ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_mul_nii (e1, e2)  -> "MUL(" ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_mul_ini (e1, e2)  -> "MUL(" ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_mul_iii (e1, e2)  -> "MUL(" ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_compare (e1, e2)  -> "CMP(" ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_int_of_nat e1     -> "INT(" ^ (e1 |> ts) ^ ")"
  
      (*************************************************************************)
      (* Natural Number                                                        *)
      (*************************************************************************)
      | V_lit_nat zn          -> "N_"       ^ (zn |> Z.to_string)
      | V_abs_in e1           -> "ABS("     ^ (e1 |> ts) ^ ")"
      | V_add_nnn (e1, e2)    -> "ADD("     ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_mul_nnn (e1, e2)    -> "MUL("     ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_shiftL_nnn (e1, e2) -> "SHIFTL("  ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_shiftR_nnn (e1, e2) -> "SHIFTR("  ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_and_nnn (e1, e2)    -> "AND("     ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_and_inn (e1, e2)    -> "AND("     ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_or_nnn (e1, e2)     -> "OR("      ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_xor_nnn (e1, e2)    -> "XOR("     ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_size_s e1           -> "SIZE("    ^ (e1 |> ts) ^ ")"
      | V_size_m e1           -> "SIZE("    ^ (e1 |> ts) ^ ")"
      | V_size_l e1           -> "SIZE("    ^ (e1 |> ts) ^ ")"
      | V_size_str e1         -> "SIZE("    ^ (e1 |> ts) ^ ")"
      | V_size_b e1           -> "SIZE("    ^ (e1 |> ts) ^ ")"
  
      (*************************************************************************)
      (* String                                                                *)
      (*************************************************************************)
      | V_lit_string str      -> "S_"       ^ str
      | V_concat_sss (e1, e2) -> "CONCAT("  ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_concat_list_s e1    -> "CONCAT("  ^ (e1 |> ts) ^ ")"
  
      (*************************************************************************)
      (* Bytes                                                                 *)
      (*************************************************************************)
      | V_lit_bytes str       -> "B_"       ^ str
      | V_concat_bbb (e1, e2) -> "CONCAT("  ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_concat_list_b e1    -> "CONCAT("  ^ (e1 |> ts) ^ ")"
      | V_pack e1             -> "PACK("    ^ (e1 |> ts) ^ ")"
      | V_blake2b e1          -> "BLAKE2B(" ^ (e1 |> ts) ^ ")"
      | V_sha256 e1           -> "SHA256("  ^ (e1 |> ts) ^ ")"
      | V_sha512 e1           -> "SHA512("  ^ (e1 |> ts) ^ ")"
  
      (*************************************************************************)
      (* Mutez                                                                 *)
      (*************************************************************************)
      | V_lit_mutez zn      -> "M_"       ^ (zn |> Z.to_string)
      | V_amount            -> "AMOUNT"
      | V_balance           -> "BALANCE"
      | V_add_mmm (e1, e2)  -> "ADD("     ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_sub_mmm (e1, e2)  -> "SUB("     ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_mul_mnm (e1, e2)  -> "MUL("     ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_mul_nmm (e1, e2)  -> "MUL("     ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
  
      (*************************************************************************)
      (* Bool                                                                  *)
      (*************************************************************************)
      | V_lit_bool b                    -> "B_"               ^ (b |> string_of_bool)
      | V_not_bb e1                     -> "NOT("             ^ (e1 |> ts) ^ ")"
      | V_and_bbb (e1, e2)              -> "AND("             ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_or_bbb (e1, e2)               -> "OR("              ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_xor_bbb (e1, e2)              -> "XOR("             ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_eq_ib e1                      -> "EQ("              ^ (e1 |> ts) ^ ")"
      | V_neq_ib e1                     -> "NEQ("             ^ (e1 |> ts) ^ ")"
      | V_lt_ib e1                      -> "LT("              ^ (e1 |> ts) ^ ")"
      | V_gt_ib e1                      -> "GT("              ^ (e1 |> ts) ^ ")"
      | V_leq_ib e1                     -> "LEQ("             ^ (e1 |> ts) ^ ")"
      | V_geq_ib e1                     -> "GEQ("             ^ (e1 |> ts) ^ ")"
      | V_mem_xsb (e1, e2)              -> "MEM("             ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_mem_xmb (e1, e2)              -> "MEM("             ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_mem_xbmb (e1, e2)             -> "MEM("             ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_check_signature (e1, e2, e3)  -> "CHECK_SIGNATURE(" ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ "," ^ (e3 |> ts) ^ ")"
  
      (*************************************************************************)
      (* Key Hash                                                              *)
      (*************************************************************************)
      | V_lit_key_hash str  -> "K_"         ^ str
      | V_hash_key e1       -> "HASH_KEY("  ^ (e1 |> ts) ^ ")"
  
      (*************************************************************************)
      (* Timestamp                                                             *)
      (*************************************************************************)
      | V_lit_timestamp_str str -> "T_"   ^ str
      | V_lit_timestamp_sec zn  -> "T_"   ^ (zn |> Z.to_string)
      | V_now                   -> "NOW"
      | V_add_tit (e1, e2)      -> "ADD(" ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_add_itt (e1, e2)      -> "ADD(" ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_sub_tit (e1, e2)      -> "SUB(" ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
  
      (*************************************************************************)
      (* Address                                                               *)
      (*************************************************************************)
      | V_lit_address e1          -> "ADDRESS(" ^ (e1 |> ts) ^ ")"
      | V_source                  -> "SOURCE"
      | V_sender                  -> "SENDER"
      | V_address_of_contract e1  -> "ADDRESS(" ^ (e1 |> ts) ^ ")"
  
      (*************************************************************************)
      (* Key                                                                   *)
      (*************************************************************************)
      | V_lit_key str -> "K_" ^ str
  
      (*************************************************************************)
      (* Unit                                                                  *)
      (*************************************************************************)
      (* | V_lit_unit : t_unit t *) (* V_unit has the same feature. *)
      | V_unit -> "UNIT"
  
      (*************************************************************************)
      (* Signature                                                             *)
      (*************************************************************************)
      | V_lit_signature_str str         -> "S_"         ^ str
      | V_lit_signature_signed (e1, e2) -> "SIGNATURE(" ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
  
      (*************************************************************************)
      (* Option                                                                *)
      (*************************************************************************)
      (* | V_lit_option : 'a t option -> 'a t_option t *) (* V_some and V_none has the same feature. *)
      | V_some e1                     -> "SOME("      ^ (e1 |> ts) ^ ")"
      | V_none _                      -> "NONE"
      | V_ediv_nnnn (e1, e2)          -> "EDIV("      ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_ediv_niin (e1, e2)          -> "EDIV("      ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_ediv_inin (e1, e2)          -> "EDIV("      ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_ediv_iiin (e1, e2)          -> "EDIV("      ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_ediv_mnmm (e1, e2)          -> "EDIV("      ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_ediv_mmnm (e1, e2)          -> "EDIV("      ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_get_xmoy (e1, e2)           -> "GET("       ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_get_xbmo (e1, e2)           -> "GET("       ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_slice_nnso (e1, e2, e3)     -> "SLICE("     ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ "," ^ (e3 |> ts) ^ ")"
      | V_slice_nnbo (e1, e2, e3)     -> "SLICE("     ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ "," ^ (e3 |> ts) ^ ")"
      | V_unpack (_, e1)              -> "UNPACK("    ^ (e1 |> ts) ^ ")"
      | V_contract_of_address (_, e1) -> "CONTRACT("  ^ (e1 |> ts) ^ ")"
      | V_isnat e1                    -> "ISNAT("     ^ (e1 |> ts) ^ ")"
  
      (*************************************************************************)
      (* List                                                                  *)
      (*************************************************************************)
      | V_lit_list (_, el)  -> "L_["      ^ (el |> Core.List.map ~f:ts |> Core.String.concat ~sep: "; ") ^ "]"
      | V_nil _             -> "L_[]"
      | V_cons (e1, e2)     -> "CONS("    ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_tl_l e1           -> "TL("      ^ (e1 |> ts) ^ ")"
      | V_append_l (e1, e2) -> "APPEND("  ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
  
      (*************************************************************************)
      (* Set                                                                   *)
      (*************************************************************************)
      | V_lit_set (_, es)           -> "S_{"      ^ (es |> Core.Set.Poly.to_list |> Core.List.map ~f:ts |> Core.String.concat ~sep: "; ") ^ "}"
      | V_empty_set _               -> "S_{}"
      | V_update_xbss (e1, e2, e3)  -> "UPDATE("  ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ "," ^ (e3 |> ts) ^ ")"
      | V_tl_s e1                   -> "TL("      ^ (e1 |> ts) ^ ")"
  
      (*************************************************************************)
      (* Operation                                                             *)
      (*************************************************************************)
      | V_create_contract (_, _, e1, e2, e3, e4)  -> "CREATE_CONTRACT(" ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ "," ^ (e3 |> ts) ^ "," ^ (e4 |> ts) ^ ")"
      | V_transfer_tokens (e1, e2, e3)            -> "TRANSFER_TOKEN("  ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ "," ^ (e3 |> ts) ^ ")"
      | V_set_delegate e1                         -> "SET_DELEGATE"     ^ (e1 |> ts)
  
      (*************************************************************************)
      (* Contract                                                              *)
      (*************************************************************************)
      | V_lit_contract (e1, e2, e3, e4, e5) -> "CONTRACT("        ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ "," ^ (e3 |> ts) ^ "," ^ (e4 |> ts) ^ "," ^ (e5 |> ts) ^ ")"
      | V_self _                            -> "SELF"
      | V_implicit_account e1               -> "IMPLICIT_ACCOUNT" ^ (e1 |> ts)
  
      (*************************************************************************)
      (* Pair                                                                  *)
      (*************************************************************************)
      | V_pair (e1, e2) -> "PAIR("  ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
      | V_hd_m e1       -> "HD("    ^ (e1 |> ts) ^ ")"
      | V_hd_bm e1      -> "HD("    ^ (e1 |> ts) ^ ")"
      | V_hdtl_l e1     -> "HDTL("  ^ (e1 |> ts) ^ ")"
      | V_hdtl_s e1     -> "HDTL("  ^ (e1 |> ts) ^ ")"
      | V_hdtl_m e1     -> "HDTL("  ^ (e1 |> ts) ^ ")"
  
      (*************************************************************************)
      (* Or                                                                    *)
      (*************************************************************************)
      | V_left (_, e1)  -> "LEFT("  ^ (e1 |> ts) ^ ")"
      | V_right (_, e1) -> "RIGHT(" ^ (e1 |> ts) ^ ")"
  
      (*************************************************************************)
      (* Lambda                                                                *)
      (*************************************************************************)
      | V_lit_program pgm         -> "LAMBDA("          ^ (pgm |> PreLib.Mich.string_of_pgm_ol) ^ ")"
      | V_lambda_id (_, _, id)    -> "LAMBDA("          ^ (id |> string_of_int) ^ ")"
      | V_lit_lambda (_, _, code) -> "LAMBDA("          ^ (code |> PreLib.Mich.string_of_instt_ol) ^ ")"
      | V_lambda_unknown (_, _)   -> "LAMBDA(UNKNOWN)"
      | V_lambda_closure (e1, e2) -> "LAMBDA("          ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ ")"
  
      (*************************************************************************)
      (* Map                                                                   *)
      (*************************************************************************)
      | V_lit_map (_, _, em)        -> "M_{"      ^ (em |> Core.Map.Poly.to_alist |> Core.List.map ~f:(fun (k, v) -> (k |> ts) ^ "|->" ^ (v |> ts)) |> Core.String.concat ~sep:"; ") ^ "}"
      | V_empty_map (_, _)          -> "M_{}"
      | V_update_xomm (e1, e2, e3)  -> "UPDATE("  ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ "," ^ (e3 |> ts) ^ ")"
      | V_tl_m e1                   -> "TL("      ^ (e1 |> ts) ^ ")"
  
      (*************************************************************************)
      (* Big Map                                                               *)
      (*************************************************************************)
      | V_lit_big_map  (_, _, em)    -> "M_{"     ^ (em |> Core.Map.Poly.to_alist |> Core.List.map ~f:(fun (k, v) -> (k |> ts) ^ "|->" ^ (v |> ts)) |> Core.String.concat ~sep:"; ") ^ "}"
      | V_empty_big_map (_, _)       -> "M_{}"
      | V_update_xobmbm (e1, e2, e3) -> "UPDATE(" ^ (e1 |> ts) ^ "," ^ (e2 |> ts) ^ "," ^ (e3 |> ts) ^ ")"
      | V_tl_bm e1                   -> "TL("     ^ (e1 |> ts) ^ ")"
  
      (*************************************************************************)
      (* Chain Id                                                              *)
      (*************************************************************************)
      | V_lit_chain_id str  -> "C_"       ^ str
      | V_chain_id          -> "CHAIN_ID"
    end
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
  (* Custom Formula for verifiying *)
  | VF_add_mmm_no_overflow of (Expr.t * Expr.t)
  | VF_sub_mmm_no_underflow of (Expr.t * Expr.t)
  | VF_mul_mnm_no_overflow of (Expr.t * Expr.t)
  | VF_mul_nmm_no_overflow of (Expr.t * Expr.t)
  (* Custom Domain Formula for Invariant Generation *)
  | VF_sigma_equal of ([`Pre | `Post] * Expr.t * Expr.t)

  let rec to_string : t -> string
  = let ts = to_string in   (* syntax sugar *)
    let ets = Expr.to_string in  (* syntax sugar *)
    fun f -> begin
      match f with
      (* Logical Formula *)
      | VF_true           -> "True"
      | VF_false          -> "False"
      | VF_not f1         -> "!"      ^ (f1 |> ts) ^ ""
      | VF_and fl         -> ""       ^ (fl |> Core.List.map ~f:ts |> Core.String.concat ~sep:" /\\ ")
      | VF_or fl          -> ""       ^ (fl |> Core.List.map ~f:ts |> Core.String.concat ~sep:" \\/ ")
      | VF_eq (e1, e2)    -> "("      ^ (e1 |> ets) ^ "=" ^ (e2 |> ets) ^ ")"
      | VF_imply (e1, e2) -> "("      ^ (e1 |> ts) ^ " -> " ^ (e2 |> ts) ^ ")"
      (* MicSE-Cfg Specific Boolean *)  
      | VF_mich_if e1                 -> "("  ^ (e1 |> ets) ^ "=" ^ "B_True"     ^ ")"
      | VF_mich_if_none e1            -> "("  ^ (e1 |> ets) ^ "=" ^ "NONE"       ^ ")"
      | VF_mich_if_left e1            -> "("  ^ (e1 |> ets) ^ "IS_LEFT"          ^ ")"
      | VF_mich_if_cons e1            -> "("  ^ (e1 |> ets) ^ "IS_CONS"          ^ ")"
      | VF_mich_loop e1               -> "("  ^ (e1 |> ets) ^ "=" ^ "B_True"     ^ ")"
      | VF_mich_loop_left e1          -> "("  ^ (e1 |> ets) ^ "IS_LEFT"          ^ ")"
      | VF_mich_map_l e1              -> "!(" ^ (e1 |> ets) ^ "=" ^ "L_[]"       ^ ")"
      | VF_mich_map_m e1              -> "!(" ^ (e1 |> ets) ^ "=" ^ "M_{}"       ^ ")"
      | VF_mich_iter_l e1             -> "!(" ^ (e1 |> ets) ^ "=" ^ "L_[]"       ^ ")"
      | VF_mich_iter_s e1             -> "!(" ^ (e1 |> ets) ^ "=" ^ "S_{}"       ^ ")"
      | VF_mich_iter_m e1             -> "!(" ^ (e1 |> ets) ^ "=" ^ "M_{}"       ^ ")"
      | VF_mich_micse_check_value e1  -> "("  ^ (e1 |> ets) ^ "=" ^ "True"       ^ ")"
      (* Custom Formula for verifiying *)
      | VF_add_mmm_no_overflow (e1, e2)   -> "NoOverflow_ADD("    ^ (e1 |> ets) ^ "," ^ (e2 |> ets) ^ ")"
      | VF_sub_mmm_no_underflow (e1, e2)  -> "NoUnderflow_SUB("  ^ (e1 |> ets) ^ "," ^ (e2 |> ets) ^ ")"
      | VF_mul_mnm_no_overflow (e1, e2)   -> "NoOverflow_MUL("    ^ (e1 |> ets) ^ "," ^ (e2 |> ets) ^ ")"
      | VF_mul_nmm_no_overflow (e1, e2)   -> "NoOverflow_MUL("    ^ (e1 |> ets) ^ "," ^ (e2 |> ets) ^ ")"
      (* Custom Domain Formula for Invariant Generation *)
      | VF_sigma_equal (pos, e1, e2) -> (match pos with `Pre -> "Pre_" | `Post -> "Post_") ^ "Sigma(" ^ (e1 |> ets) ^ ")=(" ^ (e2 |> ets) ^ ")"
    end
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

  exception Error of string

  let invalid_typ_of_expr : typ -> Expr.t -> func:string -> 'a
  =fun t e ~func -> Error (func ^ ": Invalid type [" ^ (t |> Ty.to_string) ^ "] of expression [" ^ (e |> Expr.to_string) ^ "]") |> raise

  let invalid_typ : typ -> func:string -> 'a
  =fun t ~func -> Error (func ^ ": Invalid type [" ^ (t |> Ty.to_string) ^ "]") |> raise

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
    let invalidtyp = invalid_typ_of_expr ~func:"ty_of_expr" in
    let toe = ty_of_expr in (* syntax sugar *)
    fun eee -> begin
    match eee with
    (*************************************************************************)
    (* Variable & Polymorphic                                                *)
    (*************************************************************************)
    | V_var (t, _) -> t
    | V_car e -> (match toe e with | T_pair (tt, _) -> tt | _ as tt -> invalidtyp tt eee)
    | V_cdr e -> (match toe e with | T_pair (_, tt) -> tt | _ as tt -> invalidtyp tt eee)
    | V_unlift_option e -> (match toe e with | T_option tt -> tt | _ as tt -> invalidtyp tt eee)
    | V_unlift_left e -> (match toe e with | T_or (tt, _) -> tt | _ as tt -> invalidtyp tt eee)
    | V_unlift_right e -> (match toe e with | T_or (_, tt) -> tt | _ as tt -> invalidtyp tt eee)
    | V_hd_l e -> (match toe e with | T_list tt -> tt | _ as tt -> invalidtyp tt eee)
    | V_hd_s e -> (match toe e with | T_set tt -> tt | _ as tt -> invalidtyp tt eee)
    | V_exec (e1, _) -> (match toe e1 with | T_lambda (_, t2) -> t2 | _ as tt -> invalidtyp tt eee)
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
    | V_get_xmoy (_, e2) -> (match toe e2 with | T_map (_, tt) -> T_option tt | _ as tt -> invalidtyp tt eee)
    | V_get_xbmo (_, e2) -> (match toe e2 with | T_big_map (_, tt) -> T_option tt | _ as tt -> invalidtyp tt eee)
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
    | V_append_l (e1, _) -> T_list (toe e1)

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
    | V_lit_contract (_, _, _, _, e5) -> (match toe e5 with | T_lambda (T_pair (pt, _), _) -> T_contract pt | _ as tt -> invalidtyp tt eee)
    | V_self t -> T_contract t
    | V_implicit_account _ -> T_contract T_unit

    (*************************************************************************)
    (* Pair                                                                  *)
    (*************************************************************************)
    (* | V_lit_pair : 'a t * 'b t -> ('a, 'b) t_pair t *) (* V_pair has the same feature *)
    | V_pair (e1, e2) -> T_pair (toe e1, toe e2)
    | V_hd_m e -> (match toe e with | T_map (kt, vt) -> T_pair (kt, vt) | _ as tt -> invalidtyp tt eee)
    | V_hd_bm e -> (match toe e with | T_big_map (kt, vt) -> T_pair (kt, vt) | _ as tt -> invalidtyp tt eee)
    | V_hdtl_l e -> (match toe e with | T_list elt -> T_pair (elt, T_list elt) | _ as tt -> invalidtyp tt eee)
    | V_hdtl_s e -> (match toe e with | T_set elt -> T_pair (elt, T_set elt) | _ as tt -> invalidtyp tt eee)
    | V_hdtl_m e -> (match toe e with | T_map (kt, vt) -> T_pair (T_pair (kt, vt), T_map (kt, vt)) | _ as tt -> invalidtyp tt eee)

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
    | V_lambda_closure (e1, e2) -> (match toe e1, toe e2 with | T_lambda (T_pair(p1, p2), rett), pt when p1 = pt -> T_lambda(p2, rett) | _, tt2 -> invalidtyp tt2 eee)
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
    | _ as tt -> invalid_typ tt ~func:"get_innertyp"

  (* pair & or & lambda & map & big_map *)
  let get_innertyp2 : typ -> (typ * typ) = 
    function
    | T_pair (t1, t2)
    | T_or (t1, t2)
    | T_lambda (t1, t2)
    | T_map (t1, t2)
    | T_big_map (t1, t2) -> (t1, t2)
    | _ as tt -> invalid_typ tt ~func:"get_innertyp2"

end (* module TypeUtil end *)

(*****************************************************************************)
(*****************************************************************************)
(* Utility                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

let string_of_formula : t -> string = Formula.to_string

module RecursiveMappingExprTemplate = struct
  (* it maps expr from outer, if outer data-constructor (e.g. V_some _ ) matched, it ignores inside *)
  (* WARNING: It maps something in lambda value too *)
  let rec map_expr_outer : (Expr.t -> bool) -> (Expr.t -> Expr.t) -> Expr.t -> Expr.t
  = let open Expr in
    fun is_wanted_constructor_f mapf eee -> begin
    let rf e = map_expr_outer is_wanted_constructor_f mapf e in (* syntax sugar *)
    if is_wanted_constructor_f eee then mapf eee else
    match eee with
    (*************************************************************************)
    (* Variable & Polymorphic                                                *)
    (*************************************************************************)
    | V_var _ -> eee
    | V_car e -> V_car (rf e)
    | V_cdr e -> V_cdr (rf e)
    | V_unlift_option e -> V_unlift_option (rf e)
    | V_unlift_left e -> V_unlift_left (rf e)
    | V_unlift_right e -> V_unlift_right (rf e)
    | V_hd_l e -> V_hd_l (rf e)
    | V_hd_s e -> V_hd_s (rf e)
    | V_exec (e1, e2) -> V_exec (rf e1, rf e2)
    | V_dup e -> V_dup (rf e)
    | V_itself e -> V_itself (rf e)

    (*************************************************************************)
    (* Integer                                                               *)
    (*************************************************************************)
    | V_lit_int _ -> eee
    | V_neg_ni e -> V_neg_ni (rf e)
    | V_neg_ii e -> V_neg_ii (rf e)
    | V_not_ni e -> V_not_ni (rf e)
    | V_not_ii e -> V_not_ii (rf e)
    | V_add_nii (e1, e2) -> V_add_nii (rf e1, rf e2)
    | V_add_ini (e1, e2) -> V_add_ini (rf e1, rf e2)
    | V_add_iii (e1, e2) -> V_add_iii (rf e1, rf e2)
    | V_sub_nni (e1, e2) -> V_sub_iii (rf e1, rf e2)
    | V_sub_nii (e1, e2) -> V_sub_nii (rf e1, rf e2)
    | V_sub_ini (e1, e2) -> V_sub_ini (rf e1, rf e2)
    | V_sub_iii (e1, e2) -> V_sub_iii (rf e1, rf e2)
    | V_sub_tti (e1, e2) -> V_sub_tti (rf e1, rf e2)
    | V_mul_nii (e1, e2) -> V_mul_nii (rf e1, rf e2)
    | V_mul_ini (e1, e2) -> V_mul_ini (rf e1, rf e2)
    | V_mul_iii (e1, e2) -> V_mul_iii (rf e1, rf e2)
    | V_compare (e1, e2) -> V_compare (rf e1, rf e2)
    | V_int_of_nat e -> V_int_of_nat (rf e)

    (*************************************************************************)
    (* Natural Number                                                        *)
    (*************************************************************************)
    | V_lit_nat _ -> eee
    | V_abs_in e -> V_abs_in (rf e)
    | V_add_nnn (e1, e2) -> V_add_nnn (rf e1, rf e2)
    | V_mul_nnn (e1, e2) -> V_mul_nnn (rf e1, rf e2)    
    | V_shiftL_nnn (e1, e2) -> V_shiftL_nnn (rf e1, rf e2)
    | V_shiftR_nnn (e1, e2) -> V_shiftR_nnn (rf e1, rf e2) 
    | V_and_nnn (e1, e2) -> V_and_nnn (rf e1, rf e2)
    | V_and_inn (e1, e2) -> V_and_inn (rf e1, rf e2)
    | V_or_nnn  (e1, e2) -> V_or_nnn (rf e1, rf e2)
    | V_xor_nnn (e1, e2) -> V_xor_nnn (rf e1, rf e2)
    | V_size_s e -> V_size_s (rf e)
    | V_size_m e -> V_size_m (rf e)
    | V_size_l e -> V_size_l (rf e)
    | V_size_str e -> V_size_str (rf e)
    | V_size_b e -> V_size_b (rf e)

    (*************************************************************************)
    (* String                                                                *)
    (*************************************************************************)
    | V_lit_string _ -> eee
    | V_concat_sss (e1, e2) -> V_concat_sss (rf e1, rf e2)
    | V_concat_list_s e -> V_concat_list_s (rf e)

    (*************************************************************************)
    (* Bytes                                                                 *)
    (*************************************************************************)
    | V_lit_bytes _ -> eee
    | V_concat_bbb (e1, e2) -> V_concat_bbb (rf e1, rf e2)
    | V_concat_list_b e -> V_concat_list_b (rf e)
    | V_pack    e -> V_pack (rf e)
    | V_blake2b e -> V_blake2b (rf e)
    | V_sha256  e -> V_sha256 (rf e)
    | V_sha512  e -> V_sha512 (rf e)

    (*************************************************************************)
    (* Mutez                                                                 *)
    (*************************************************************************)
    | V_lit_mutez _ -> eee
    | V_amount      -> eee
    | V_balance     -> eee
    | V_add_mmm (e1, e2) -> V_add_mmm (rf e1, rf e2)
    | V_sub_mmm (e1, e2) -> V_sub_mmm (rf e1, rf e2)
    | V_mul_mnm (e1, e2) -> V_mul_mnm (rf e1, rf e2)
    | V_mul_nmm (e1, e2) -> V_mul_nmm (rf e1, rf e2)

    (*************************************************************************)
    (* Bool                                                                  *)
    (*************************************************************************)
    | V_lit_bool  _ -> eee
    | V_not_bb    e -> V_not_bb (rf e)
    | V_and_bbb   (e1, e2) -> V_and_bbb (rf e1, rf e2)
    | V_or_bbb    (e1, e2) -> V_or_bbb (rf e1, rf e2)
    | V_xor_bbb   (e1, e2) -> V_xor_bbb (rf e1, rf e2)
    | V_eq_ib     e -> V_eq_ib (rf e)
    | V_neq_ib    e -> V_neq_ib (rf e)
    | V_lt_ib     e -> V_lt_ib (rf e)
    | V_gt_ib     e -> V_gt_ib (rf e)
    | V_leq_ib    e -> V_leq_ib (rf e)
    | V_geq_ib    e -> V_geq_ib (rf e)
    | V_mem_xsb   (e1, e2) -> V_mem_xsb (rf e1, rf e2)
    | V_mem_xmb   (e1, e2) -> V_mem_xmb (rf e1, rf e2)
    | V_mem_xbmb  (e1, e2) -> V_mem_xbmb (rf e1, rf e2)
    | V_check_signature (e1, e2, e3) -> V_check_signature (rf e1, rf e2, rf e3)

    (*************************************************************************)
    (* Key Hash                                                              *)
    (*************************************************************************)
    | V_lit_key_hash  _ -> eee
    | V_hash_key      e -> V_hash_key (rf e)

    (*************************************************************************)
    (* Timestamp                                                             *)
    (*************************************************************************)
    | V_lit_timestamp_str _ -> eee
    | V_lit_timestamp_sec _ -> eee
    | V_now                 -> eee
    | V_add_tit           (e1, e2) -> V_add_tit (rf e1, rf e2)
    | V_add_itt           (e1, e2) -> V_add_itt (rf e1, rf e2)
    | V_sub_tit           (e1, e2) -> V_sub_tit (rf e1, rf e2)

    (*************************************************************************)
    (* Address                                                               *)
    (*************************************************************************)
    | V_lit_address e -> V_lit_address (rf e)
    | V_source  -> eee
    | V_sender  -> eee
    | V_address_of_contract e -> V_address_of_contract (rf e)

    (*************************************************************************)
    (* Key                                                                   *)
    (*************************************************************************)
    | V_lit_key _ -> eee

    (*************************************************************************)
    (* Unit                                                                  *)
    (*************************************************************************)
    (* | V_lit_unit : t_unit t *) (* V_unit has the same feature. *)
    | V_unit -> eee

    (*************************************************************************)
    (* Signature                                                             *)
    (*************************************************************************)
    | V_lit_signature_str _ -> eee
    | V_lit_signature_signed (e1, e2) -> V_lit_signature_signed (rf e1, rf e2)

    (*************************************************************************)
    (* Option                                                                *)
    (*************************************************************************)
    (* | V_lit_option : 'a t option -> 'a t_option t *) (* V_some and V_none has the same feature. *)
    | V_some e -> V_some (rf e)
    | V_none _ -> eee
    | V_ediv_nnnn (e1, e2) -> V_ediv_nnnn (rf e1, rf e2)
    | V_ediv_niin (e1, e2) -> V_ediv_niin (rf e1, rf e2)
    | V_ediv_inin (e1, e2) -> V_ediv_inin (rf e1, rf e2)
    | V_ediv_iiin (e1, e2) -> V_ediv_iiin (rf e1, rf e2)
    | V_ediv_mnmm (e1, e2) -> V_ediv_mnmm (rf e1, rf e2)
    | V_ediv_mmnm (e1, e2) -> V_ediv_mmnm (rf e1, rf e2)
    | V_get_xmoy  (e1, e2) -> V_get_xmoy (rf e1, rf e2)
    | V_get_xbmo  (e1, e2) -> V_get_xbmo (rf e1, rf e2)
    | V_slice_nnso (e1, e2, e3) -> V_slice_nnso (rf e1, rf e2, rf e3)
    | V_slice_nnbo (e1, e2, e3) -> V_slice_nnbo (rf e1, rf e2, rf e3)
    | V_unpack (t, e) -> V_unpack (t, rf e)
    | V_contract_of_address (t, e) -> V_contract_of_address (t, rf e)
    | V_isnat e -> V_isnat (rf e)

    (*************************************************************************)
    (* List                                                                  *)
    (*************************************************************************)
    | V_lit_list (t, el) -> V_lit_list (t, (List.map rf el))
    | V_nil _ -> eee
    | V_cons (e1, e2) -> V_cons (rf e1, rf e2)
    | V_tl_l e -> V_tl_l (rf e)
    | V_append_l (e1, e2) -> V_append_l (rf e1, rf e2)

    (*************************************************************************)
    (* Set                                                                   *)
    (*************************************************************************)
    | V_lit_set (t, es) -> V_lit_set (t, Core.Set.Poly.map es ~f:rf)
    | V_empty_set _ -> eee
    | V_update_xbss (e1, e2, e3) -> V_update_xbss (rf e1, rf e2, rf e3)
    | V_tl_s e -> V_tl_s (rf e)

    (*************************************************************************)
    (* Operation                                                             *)
    (*************************************************************************)
    (* | V_lit_operation of t_operation t *) (* V_create_contract, V_transfer_tokens, V_set_delegate has the same feature. *)
    | V_create_contract (t1, t2, e1, e2, e3, e4) -> V_create_contract (t1, t2, rf e1, rf e2, rf e3, rf e4)
    | V_transfer_tokens (e1, e2, e3) -> V_transfer_tokens (rf e1, rf e2, rf e3)
    | V_set_delegate e -> V_set_delegate (rf e)

    (*************************************************************************)
    (* Contract                                                              *)
    (*************************************************************************)
    | V_lit_contract (e1, e2, e3, e4, e5) -> V_lit_contract (rf e1, rf e2, rf e3, rf e4, rf e5)
    | V_self _ -> eee
    | V_implicit_account e -> V_implicit_account (rf e)

    (*************************************************************************)
    (* Pair                                                                  *)
    (*************************************************************************)
    (* | V_lit_pair : 'a t * 'b t -> ('a, 'b) t_pair t *) (* V_pair has the same feature *)
    | V_pair (e1, e2) -> V_pair (rf e1, rf e2)
    | V_hd_m e -> V_hd_m (rf e)
    | V_hd_bm e -> V_hd_bm (rf e)
    | V_hdtl_l e -> V_hdtl_l (rf e)
    | V_hdtl_s e -> V_hdtl_s (rf e)
    | V_hdtl_m e -> V_hdtl_m (rf e)

    (*************************************************************************)
    (* Or                                                                    *)
    (*************************************************************************)
    (* | V_lit_or *) (* It cannot construct any value, use V_left or V_right instead *)
    | V_left (t, e) -> V_left (t, rf e)
    | V_right (t, e) -> V_right (t, rf e)

    (*************************************************************************)
    (* Lambda                                                                *)
    (*************************************************************************)
    | V_lit_program _ -> eee
    | V_lambda_id _ -> eee
    | V_lit_lambda _ -> eee
    | V_lambda_unknown _ -> eee
    | V_lambda_closure (e1, e2) -> V_lambda_closure (rf e1, rf e2)
    (*************************************************************************)
    (* Map                                                                   *)
    (*************************************************************************)
    | V_lit_map (kt, vt, em) -> V_lit_map (kt, vt, Core.Map.Poly.map em ~f:rf)
    | V_empty_map _ -> eee
    | V_update_xomm (e1, e2, e3) -> V_update_xomm (rf e1, rf e2, rf e3)
    | V_tl_m e -> V_tl_m (rf e)

    (*************************************************************************)
    (* Big Map                                                               *)
    (*************************************************************************)
    | V_lit_big_map (kt, vt, em) -> V_lit_big_map (kt, vt, Core.Map.Poly.map em ~f:rf)
    | V_empty_big_map _ -> eee
    | V_update_xobmbm (e1, e2, e3) -> V_update_xobmbm (rf e1, rf e2, rf e3)
    | V_tl_bm e -> V_tl_bm (rf e)
    
    (*************************************************************************)
    (* Chain Id                                                              *)
    (*************************************************************************)
    | V_lit_chain_id _ -> eee
    | V_chain_id -> eee
  end (* function map_expr_outer end *)

  let rec map_expr_inner : expr_f:(Expr.t -> Expr.t) -> Expr.t -> Expr.t
  = let open Expr in
    fun ~expr_f eee -> begin
    let rf e = map_expr_inner ~expr_f:expr_f e in (* syntax sugar *)
    (match eee with
    (*************************************************************************)
    (* Variable & Polymorphic                                                *)
    (*************************************************************************)
    | V_var _ -> eee
    | V_car e -> V_car (rf e)
    | V_cdr e -> V_cdr (rf e)
    | V_unlift_option e -> V_unlift_option (rf e)
    | V_unlift_left e -> V_unlift_left (rf e)
    | V_unlift_right e -> V_unlift_right (rf e)
    | V_hd_l e -> V_hd_l (rf e)
    | V_hd_s e -> V_hd_s (rf e)
    | V_exec (e1, e2) -> V_exec (rf e1, rf e2)
    | V_dup e -> V_dup (rf e)
    | V_itself e -> V_itself (rf e)

    (*************************************************************************)
    (* Integer                                                               *)
    (*************************************************************************)
    | V_lit_int _ -> eee
    | V_neg_ni e -> V_neg_ni (rf e)
    | V_neg_ii e -> V_neg_ii (rf e)
    | V_not_ni e -> V_not_ni (rf e)
    | V_not_ii e -> V_not_ii (rf e)
    | V_add_nii (e1, e2) -> V_add_nii (rf e1, rf e2)
    | V_add_ini (e1, e2) -> V_add_ini (rf e1, rf e2)
    | V_add_iii (e1, e2) -> V_add_iii (rf e1, rf e2)
    | V_sub_nni (e1, e2) -> V_sub_iii (rf e1, rf e2)
    | V_sub_nii (e1, e2) -> V_sub_nii (rf e1, rf e2)
    | V_sub_ini (e1, e2) -> V_sub_ini (rf e1, rf e2)
    | V_sub_iii (e1, e2) -> V_sub_iii (rf e1, rf e2)
    | V_sub_tti (e1, e2) -> V_sub_tti (rf e1, rf e2)
    | V_mul_nii (e1, e2) -> V_mul_nii (rf e1, rf e2)
    | V_mul_ini (e1, e2) -> V_mul_ini (rf e1, rf e2)
    | V_mul_iii (e1, e2) -> V_mul_iii (rf e1, rf e2)
    | V_compare (e1, e2) -> V_compare (rf e1, rf e2)
    | V_int_of_nat e -> V_int_of_nat (rf e)

    (*************************************************************************)
    (* Natural Number                                                        *)
    (*************************************************************************)
    | V_lit_nat _ -> eee
    | V_abs_in e -> V_abs_in (rf e)
    | V_add_nnn (e1, e2) -> V_add_nnn (rf e1, rf e2)
    | V_mul_nnn (e1, e2) -> V_mul_nnn (rf e1, rf e2)    
    | V_shiftL_nnn (e1, e2) -> V_shiftL_nnn (rf e1, rf e2)
    | V_shiftR_nnn (e1, e2) -> V_shiftR_nnn (rf e1, rf e2) 
    | V_and_nnn (e1, e2) -> V_and_nnn (rf e1, rf e2)
    | V_and_inn (e1, e2) -> V_and_inn (rf e1, rf e2)
    | V_or_nnn  (e1, e2) -> V_or_nnn (rf e1, rf e2)
    | V_xor_nnn (e1, e2) -> V_xor_nnn (rf e1, rf e2)
    | V_size_s e -> V_size_s (rf e)
    | V_size_m e -> V_size_m (rf e)
    | V_size_l e -> V_size_l (rf e)
    | V_size_str e -> V_size_str (rf e)
    | V_size_b e -> V_size_b (rf e)

    (*************************************************************************)
    (* String                                                                *)
    (*************************************************************************)
    | V_lit_string _ -> eee
    | V_concat_sss (e1, e2) -> V_concat_sss (rf e1, rf e2)
    | V_concat_list_s e -> V_concat_list_s (rf e)

    (*************************************************************************)
    (* Bytes                                                                 *)
    (*************************************************************************)
    | V_lit_bytes _ -> eee
    | V_concat_bbb (e1, e2) -> V_concat_bbb (rf e1, rf e2)
    | V_concat_list_b e -> V_concat_list_b (rf e)
    | V_pack    e -> V_pack (rf e)
    | V_blake2b e -> V_blake2b (rf e)
    | V_sha256  e -> V_sha256 (rf e)
    | V_sha512  e -> V_sha512 (rf e)

    (*************************************************************************)
    (* Mutez                                                                 *)
    (*************************************************************************)
    | V_lit_mutez _ -> eee
    | V_amount      -> eee
    | V_balance     -> eee
    | V_add_mmm (e1, e2) -> V_add_mmm (rf e1, rf e2)
    | V_sub_mmm (e1, e2) -> V_sub_mmm (rf e1, rf e2)
    | V_mul_mnm (e1, e2) -> V_mul_mnm (rf e1, rf e2)
    | V_mul_nmm (e1, e2) -> V_mul_nmm (rf e1, rf e2)

    (*************************************************************************)
    (* Bool                                                                  *)
    (*************************************************************************)
    | V_lit_bool  _ -> eee
    | V_not_bb    e -> V_not_bb (rf e)
    | V_and_bbb   (e1, e2) -> V_and_bbb (rf e1, rf e2)
    | V_or_bbb    (e1, e2) -> V_or_bbb (rf e1, rf e2)
    | V_xor_bbb   (e1, e2) -> V_xor_bbb (rf e1, rf e2)
    | V_eq_ib     e -> V_eq_ib (rf e)
    | V_neq_ib    e -> V_neq_ib (rf e)
    | V_lt_ib     e -> V_lt_ib (rf e)
    | V_gt_ib     e -> V_gt_ib (rf e)
    | V_leq_ib    e -> V_leq_ib (rf e)
    | V_geq_ib    e -> V_geq_ib (rf e)
    | V_mem_xsb   (e1, e2) -> V_mem_xsb (rf e1, rf e2)
    | V_mem_xmb   (e1, e2) -> V_mem_xmb (rf e1, rf e2)
    | V_mem_xbmb  (e1, e2) -> V_mem_xbmb (rf e1, rf e2)
    | V_check_signature (e1, e2, e3) -> V_check_signature (rf e1, rf e2, rf e3)

    (*************************************************************************)
    (* Key Hash                                                              *)
    (*************************************************************************)
    | V_lit_key_hash  _ -> eee
    | V_hash_key      e -> V_hash_key (rf e)

    (*************************************************************************)
    (* Timestamp                                                             *)
    (*************************************************************************)
    | V_lit_timestamp_str _ -> eee
    | V_lit_timestamp_sec _ -> eee
    | V_now                 -> eee
    | V_add_tit           (e1, e2) -> V_add_tit (rf e1, rf e2)
    | V_add_itt           (e1, e2) -> V_add_itt (rf e1, rf e2)
    | V_sub_tit           (e1, e2) -> V_sub_tit (rf e1, rf e2)

    (*************************************************************************)
    (* Address                                                               *)
    (*************************************************************************)
    | V_lit_address e -> V_lit_address (rf e)
    | V_source  -> eee
    | V_sender  -> eee
    | V_address_of_contract e -> V_address_of_contract (rf e)

    (*************************************************************************)
    (* Key                                                                   *)
    (*************************************************************************)
    | V_lit_key _ -> eee

    (*************************************************************************)
    (* Unit                                                                  *)
    (*************************************************************************)
    (* | V_lit_unit : t_unit t *) (* V_unit has the same feature. *)
    | V_unit -> eee

    (*************************************************************************)
    (* Signature                                                             *)
    (*************************************************************************)
    | V_lit_signature_str _ -> eee
    | V_lit_signature_signed (e1, e2) -> V_lit_signature_signed (rf e1, rf e2)

    (*************************************************************************)
    (* Option                                                                *)
    (*************************************************************************)
    (* | V_lit_option : 'a t option -> 'a t_option t *) (* V_some and V_none has the same feature. *)
    | V_some e -> V_some (rf e)
    | V_none _ -> eee
    | V_ediv_nnnn (e1, e2) -> V_ediv_nnnn (rf e1, rf e2)
    | V_ediv_niin (e1, e2) -> V_ediv_niin (rf e1, rf e2)
    | V_ediv_inin (e1, e2) -> V_ediv_inin (rf e1, rf e2)
    | V_ediv_iiin (e1, e2) -> V_ediv_iiin (rf e1, rf e2)
    | V_ediv_mnmm (e1, e2) -> V_ediv_mnmm (rf e1, rf e2)
    | V_ediv_mmnm (e1, e2) -> V_ediv_mmnm (rf e1, rf e2)
    | V_get_xmoy  (e1, e2) -> V_get_xmoy (rf e1, rf e2)
    | V_get_xbmo  (e1, e2) -> V_get_xbmo (rf e1, rf e2)
    | V_slice_nnso (e1, e2, e3) -> V_slice_nnso (rf e1, rf e2, rf e3)
    | V_slice_nnbo (e1, e2, e3) -> V_slice_nnbo (rf e1, rf e2, rf e3)
    | V_unpack (t, e) -> V_unpack (t, rf e)
    | V_contract_of_address (t, e) -> V_contract_of_address (t, rf e)
    | V_isnat e -> V_isnat (rf e)

    (*************************************************************************)
    (* List                                                                  *)
    (*************************************************************************)
    | V_lit_list (t, el) -> V_lit_list (t, (List.map rf el))
    | V_nil _ -> eee
    | V_cons (e1, e2) -> V_cons (rf e1, rf e2)
    | V_tl_l e -> V_tl_l (rf e)
    | V_append_l (e1, e2) -> V_append_l (rf e1, rf e2)

    (*************************************************************************)
    (* Set                                                                   *)
    (*************************************************************************)
    | V_lit_set (t, es) -> V_lit_set (t, Core.Set.Poly.map es ~f:rf)
    | V_empty_set _ -> eee
    | V_update_xbss (e1, e2, e3) -> V_update_xbss (rf e1, rf e2, rf e3)
    | V_tl_s e -> V_tl_s (rf e)

    (*************************************************************************)
    (* Operation                                                             *)
    (*************************************************************************)
    (* | V_lit_operation of t_operation t *) (* V_create_contract, V_transfer_tokens, V_set_delegate has the same feature. *)
    | V_create_contract (t1, t2, e1, e2, e3, e4) -> V_create_contract (t1, t2, rf e1, rf e2, rf e3, rf e4)
    | V_transfer_tokens (e1, e2, e3) -> V_transfer_tokens (rf e1, rf e2, rf e3)
    | V_set_delegate e -> V_set_delegate (rf e)

    (*************************************************************************)
    (* Contract                                                              *)
    (*************************************************************************)
    | V_lit_contract (e1, e2, e3, e4, e5) -> V_lit_contract (rf e1, rf e2, rf e3, rf e4, rf e5)
    | V_self _ -> eee
    | V_implicit_account e -> V_implicit_account (rf e)

    (*************************************************************************)
    (* Pair                                                                  *)
    (*************************************************************************)
    (* | V_lit_pair : 'a t * 'b t -> ('a, 'b) t_pair t *) (* V_pair has the same feature *)
    | V_pair (e1, e2) -> V_pair (rf e1, rf e2)
    | V_hd_m e -> V_hd_m (rf e)
    | V_hd_bm e -> V_hd_bm (rf e)
    | V_hdtl_l e -> V_hdtl_l (rf e)
    | V_hdtl_s e -> V_hdtl_s (rf e)
    | V_hdtl_m e -> V_hdtl_m (rf e)

    (*************************************************************************)
    (* Or                                                                    *)
    (*************************************************************************)
    (* | V_lit_or *) (* It cannot construct any value, use V_left or V_right instead *)
    | V_left (t, e) -> V_left (t, rf e)
    | V_right (t, e) -> V_right (t, rf e)

    (*************************************************************************)
    (* Lambda                                                                *)
    (*************************************************************************)
    | V_lit_program _ -> eee
    | V_lambda_id _ -> eee
    | V_lit_lambda _ -> eee
    | V_lambda_unknown _ -> eee
    | V_lambda_closure (e1, e2) -> V_lambda_closure (rf e1, rf e2)
    (*************************************************************************)
    (* Map                                                                   *)
    (*************************************************************************)
    | V_lit_map (kt, vt, em) -> V_lit_map (kt, vt, Core.Map.Poly.map em ~f:rf)
    | V_empty_map _ -> eee
    | V_update_xomm (e1, e2, e3) -> V_update_xomm (rf e1, rf e2, rf e3)
    | V_tl_m e -> V_tl_m (rf e)

    (*************************************************************************)
    (* Big Map                                                               *)
    (*************************************************************************)
    | V_lit_big_map (kt, vt, em) -> V_lit_big_map (kt, vt, Core.Map.Poly.map em ~f:rf)
    | V_empty_big_map _ -> eee
    | V_update_xobmbm (e1, e2, e3) -> V_update_xobmbm (rf e1, rf e2, rf e3)
    | V_tl_bm e -> V_tl_bm (rf e)
    
    (*************************************************************************)
    (* Chain Id                                                              *)
    (*************************************************************************)
    | V_lit_chain_id _ -> eee
    | V_chain_id -> eee) |> expr_f
  end (* function map_expr_inner end *)

  let rec map_formula_outer : (Expr.t -> bool) -> (Expr.t -> Expr.t) -> t -> t
  = let open Formula in
    fun is_wanted_constructor_f mapf eee -> begin
    let rf f = map_formula_outer is_wanted_constructor_f mapf f in (* syntax sugar *)
    let re e = map_expr_outer is_wanted_constructor_f mapf e in (* syntax sugar *)
    match eee with
    | VF_true -> eee
    | VF_false -> eee
    | VF_not f -> VF_not (rf f)
    | VF_and fl -> VF_and (List.map rf fl)
    | VF_or fl -> VF_or (List.map rf fl)
    | VF_eq (e1, e2) -> VF_eq (re e1, re e2)
    | VF_imply (f1, f2) -> VF_imply (rf f1, rf f2)
    (* MicSE-Cfg Specific Boolean *)  
    | VF_mich_if e -> VF_mich_if (re e)
    | VF_mich_if_none e -> VF_mich_if_none (re e)
    | VF_mich_if_left e -> VF_mich_if_left (re e)
    | VF_mich_if_cons e -> VF_mich_if_cons (re e)
    | VF_mich_loop e -> VF_mich_loop (re e)
    | VF_mich_loop_left e -> VF_mich_loop_left (re e)
    | VF_mich_map_l e -> VF_mich_map_l (re e)
    | VF_mich_map_m e -> VF_mich_map_m (re e)
    | VF_mich_iter_l e -> VF_mich_iter_l (re e)
    | VF_mich_iter_s e -> VF_mich_iter_s (re e)
    | VF_mich_iter_m e -> VF_mich_iter_m (re e)
    | VF_mich_micse_check_value e -> VF_mich_micse_check_value (re e)
    (* Custom Formula for verifiying *)
    | VF_add_mmm_no_overflow (e1, e2) -> VF_add_mmm_no_overflow ((re e1), (re e2))
    | VF_sub_mmm_no_underflow (e1, e2) -> VF_sub_mmm_no_underflow ((re e1), (re e2))
    | VF_mul_mnm_no_overflow (e1, e2) -> VF_mul_mnm_no_overflow ((re e1), (re e2))
    | VF_mul_nmm_no_overflow (e1, e2) -> VF_mul_nmm_no_overflow ((re e1), (re e2))
    (* Custom Domain Formula for Invariant Generation *)
    | VF_sigma_equal (pos, e1, e2) -> VF_sigma_equal (pos, (re e1), (re e2))
  end (* function map_formula_outer end *)


  let rec map_formula_inner : ?formula_f:(Formula.t -> Formula.t) -> ?expr_f:(Expr.t -> Expr.t) -> t -> t
  = let open Formula in
    let id_f = fun x -> x in (* syntax sugar *)
    fun ?(formula_f=id_f) ?(expr_f=id_f) fff -> begin
    let rf f = map_formula_inner ~formula_f:formula_f ~expr_f:expr_f f in (* syntax sugar *)
    let re e = map_expr_inner ~expr_f:expr_f e in (* syntax sugar *)
    (match fff with
    | VF_true -> fff
    | VF_false -> fff
    | VF_not f -> VF_not (rf f)
    | VF_and fl -> VF_and (List.map rf fl)
    | VF_or fl -> VF_or (List.map rf fl)
    | VF_eq (e1, e2) -> VF_eq (re e1, re e2)
    | VF_imply (f1, f2) -> VF_imply (rf f1, rf f2)
    (* MicSE-Cfg Specific Boolean *)  
    | VF_mich_if e -> VF_mich_if (re e)
    | VF_mich_if_none e -> VF_mich_if_none (re e)
    | VF_mich_if_left e -> VF_mich_if_left (re e)
    | VF_mich_if_cons e -> VF_mich_if_cons (re e)
    | VF_mich_loop e -> VF_mich_loop (re e)
    | VF_mich_loop_left e -> VF_mich_loop_left (re e)
    | VF_mich_map_l e -> VF_mich_map_l (re e)
    | VF_mich_map_m e -> VF_mich_map_m (re e)
    | VF_mich_iter_l e -> VF_mich_iter_l (re e)
    | VF_mich_iter_s e -> VF_mich_iter_s (re e)
    | VF_mich_iter_m e -> VF_mich_iter_m (re e)
    | VF_mich_micse_check_value e -> VF_mich_micse_check_value (re e)
    (* Custom Formula for verifiying *)
    | VF_add_mmm_no_overflow (e1, e2) -> VF_add_mmm_no_overflow ((re e1), (re e2))
    | VF_sub_mmm_no_underflow (e1, e2) -> VF_sub_mmm_no_underflow ((re e1), (re e2))
    | VF_mul_mnm_no_overflow (e1, e2) -> VF_mul_mnm_no_overflow ((re e1), (re e2))
    | VF_mul_nmm_no_overflow (e1, e2) -> VF_mul_nmm_no_overflow ((re e1), (re e2))
    (* Custom Domain Formula for Invariant Generation *)
    | VF_sigma_equal (pos, e1, e2) -> VF_sigma_equal (pos, (re e1), (re e2))
    ) |> formula_f
  end (* function map_formula_outer end *)

end (* module RecursiveMappingExprTemplate end *)


module Renaming = struct
  
  let is_var : Expr.t -> bool = (function | Expr.V_var _ -> true | _ -> false)

  let var_in_expr : Expr.var -> Expr.var -> Expr.t -> Expr.t
  =fun old_var new_var e -> begin
    let rename_var : Expr.t -> Expr.t = (fun e -> match e with | Expr.V_var (t, s) when s = old_var -> Expr.V_var (t, new_var) | _ -> e) in
    RecursiveMappingExprTemplate.map_expr_outer is_var rename_var e
  end (* function var_in_expr end *)

  let var_in_expr_formula : Expr.var -> Expr.var -> t -> t
  =fun old_var new_var f -> begin
    let rename_var : Expr.t -> Expr.t = (fun e -> match e with | Expr.V_var (t, s) when s = old_var -> Expr.V_var (t, new_var) | _ -> e) in
    RecursiveMappingExprTemplate.map_formula_outer is_var rename_var f
  end (* function var_in_expr_formula end *) 

end (* module Renaming end *)