type typ = Michelson.Adt.typ Michelson.Adt.t

type data = Michelson.Adt.data Michelson.Adt.t

type operation =
  | O_create_contract of Michelson.Adt.program * string * string * string
  | O_transfer_tokens of string * string * string
  | O_set_delegate of string
  | O_create_account of string * string * string * string

and expr =
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
  | E_itself of string  (* E_itself is not defined at Michelson, but added for MicSE internal usage. 
                            "Cfg_assign (new_varname, (E_itself old_varname)" == "new_varname = old_varname"
                        *)

and stmt_t =
  | S_seq of stmt * stmt
  | S_assign of string * expr
  | S_skip
  | S_drop of string list
  | S_swap
  | S_dig
  | S_dug
  | S_if of string * stmt * stmt
  | S_if_none of string * stmt * stmt
  | S_if_left of string * stmt * stmt
  | S_if_cons of string * stmt * stmt
  | S_loop of string * (string * string) * stmt
  | S_loop_left of string * (string * string) * stmt
  | S_map of (string * (string * string)) * (string * (string * string)) * stmt
  | S_iter of string * (string * string) * stmt
  | S_failwith of string

and stmt = { id : int; stm : stmt_t }

and func = stmt * string

and program = typ * typ * func

val create_stmt : stmt_t -> stmt

val simpl : stmt -> stmt

val create_typ : Michelson.Adt.typ -> Michelson.Adt.typ Michelson.Adt.t
