(*****************************************************************************)
(*****************************************************************************)
(* Condition                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

type typ = Adt.typ

type var = Cfg.ident
  (*
    type indent = string
  *)

and exp = Cfg.expr
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
    | E_ediv of string * string
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
    | E_lambda_id of int
    | E_exec of string * string
    | E_dup of string
    | E_nil of typ
    | E_empty_set of typ
    | E_empty_map of typ * typ
    | E_empty_big_map of typ * typ
    | E_append of string * string
    | E_special_nil_list
    | E_phi of string * string
    | E_itself of string
  *)

type cond =
  | BC_is_true of var
  | BC_is_none of var
  | BC_is_left of var
  | BC_is_cons of var
  | BC_no_overflow of exp * typ
  | BC_no_underflow of exp * typ
  | BC_not of cond

val create_cond_is_true : var -> cond

val create_cond_is_none : var -> cond

val create_cond_is_left : var -> cond

val create_cond_is_cons : var -> cond

val create_cond_no_overflow : exp -> typ -> cond

val create_cond_no_underflow : exp -> typ -> cond

val create_cond_not : cond -> cond

val string_of_cond : cond -> string


(*****************************************************************************)
(*****************************************************************************)
(* Instruction                                                               *)
(*****************************************************************************)
(*****************************************************************************)

type inst =
  | BI_assume of cond
  | BI_assert of cond
  | BI_assign of var * exp
  | BI_skip

val create_inst_assume : cond -> inst

val create_inst_assert : cond -> inst

val create_inst_assign : (var * exp) -> inst

val create_inst_skip : inst

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

type formula = Vlang.t

type inv = { id: vertex; formula: formula option }
and inv_map = (vertex, formula) Core.Hashtbl.t

val create_dummy_inv : vertex -> inv

val create_inv : vertex -> formula -> inv

val string_of_inv : inv -> string


(*****************************************************************************)
(*****************************************************************************)
(* Basic path                                                                *)
(*****************************************************************************)
(*****************************************************************************)

type t = { pre: inv; body: inst list; post: inv }
and raw_t_list = { bps: t list; trx_inv_vtx: vertex list; loop_inv_vtx: vertex list }

val create_new_bp : vertex -> vertex -> t

val create_cut_bp : t -> vertex -> (t * t)

val update_body : t -> inst -> t

val to_string : t -> string