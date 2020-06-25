(*****************************************************************************)
(*****************************************************************************)
(* Formula                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

type var = Cfg.ident
  (*
    type indent = string
  *)
type formula = 
  | F_true  | F_false
  | F_is_true of var
  | F_is_none of var
  | F_is_left of var
  | F_is_cons of var
  | F_and of formula * formula
  | F_or of formula * formula
  | F_not of formula
  | F_imply of formula * formula
  | F_iff of formula * formula
  | F_forall of var list * formula
  | F_exists of var list * formula

val create_formula_is_true : var -> formula

val create_formula_is_none : var -> formula

val create_formula_is_left : var -> formula

val create_formula_is_cons : var -> formula

val create_formula_not : formula -> formula

val string_of_formula : formula -> string


(*****************************************************************************)
(*****************************************************************************)
(* Instruction                                                               *)
(*****************************************************************************)
(*****************************************************************************)

type exp = Cfg.expr
  (*
    type expr =
    | E_push of data * typ
    | E_car of string
    | E_cdr of string
    | E_abs of string
    | E_neg of string
    | E_not of string
    | E_add of string * string
    | E_sub of string * string
    | E_mul of string * string
    | E_div of string * string
    | E_mod of string * string
    | E_shiftL of string * string
    | E_shiftR of string * string
    | E_and of string * string
    | E_or of string * string
    | E_xor of string * string
    | E_eq of string
    | E_neq of string
    | E_lt of string
    | E_gt of string
    | E_leq of string
    | E_geq of string
    | E_compare of string * string
    | E_cons of string * string
    | E_operation of operation
    | E_unit
    | E_pair of string * string
    | E_left of string * typ
    | E_right of string * typ
    | E_some of string
    | E_none of typ
    | E_mem of string * string
    | E_get of string * string
    | E_update of string * string * string
    | E_cast of string
    | E_concat of string * string
    | E_concat_list of string
    | E_slice of string * string * string
    | E_pack of string
    | E_unpack of typ * string
    | E_self
    | E_contract_of_address of string
    | E_implicit_account of string
    | E_now
    | E_amount
    | E_balance
    | E_check_signature of string * string * string
    | E_blake2b of string
    | E_sha256 of string
    | E_sha512 of string
    | E_hash_key of string
    | E_steps_to_quota
    | E_source
    | E_sender
    | E_address_of_contract of string
    | E_create_contract_address of operation
    | E_unlift_option of string
    | E_unlift_or of string
    | E_hd of string
    | E_tl of string
    | E_size of string
    | E_isnat of string
    | E_int_of_nat of string
    | E_chain_id
    | E_create_account_address of operation
    | E_lambda of typ * typ * func
    | E_exec of string * string
    | E_dup of string
    | E_nil of typ
    | E_empty_set of typ
    | E_empty_map of typ * typ
    | E_empty_big_map of typ * typ
    | E_append of string * string
    | E_special_nil_list
    | E_phi of string * string
  *)
type inst =
  | I_assume of formula
  | I_assign of var * exp
  | I_skip

val create_inst_assume : formula -> inst

val create_inst_assign : (var * exp) -> inst

val create_inst_skip : unit -> inst

val string_of_inst : inst -> string


(*****************************************************************************)
(*****************************************************************************)
(* Invariants                                                                *)
(*****************************************************************************)
(*****************************************************************************)

type vertex = Cfg.vertex
  (*
    type vertex = int
  *)
type inv = { id: vertex; formula: formula option }
type inv_map = (vertex, formula) Core.Hashtbl.t

val create_dummy_inv : vertex -> inv

val string_of_inv : inv -> string


(*****************************************************************************)
(*****************************************************************************)
(* Basic path                                                                *)
(*****************************************************************************)
(*****************************************************************************)

type t = { pre: inv; body: inst list; post: inv }

val create_new_bp : vertex -> vertex -> t

val create_cut_bp : t -> vertex -> (t * t)

val update_body : t -> inst -> t

val to_string : t -> string