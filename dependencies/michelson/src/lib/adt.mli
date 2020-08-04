type typ =
  | T_key
  | T_unit
  | T_signature
  | T_option of typ t
  | T_list of typ t
  | T_set of typ t
  | T_operation
  | T_contract of typ t
  | T_pair of typ t * typ t
  | T_or of typ t * typ t
  | T_lambda of typ t * typ t
  | T_map of typ t * typ t
  | T_big_map of typ t * typ t
  | T_chain_id
  | T_int
  | T_nat
  | T_string
  | T_bytes
  | T_mutez
  | T_bool
  | T_key_hash
  | T_timestamp
  | T_address

and inst =
  | I_micse_check of inst t   (* WARNING: I_check instruction is not in Michelson standard. It is for MicSE formatted-comment *)
  | I_seq of inst t * inst t
  | I_drop
  | I_drop_n of Z.t
  | I_dup
  | I_swap
  | I_dig of Z.t
  | I_dug of Z.t
  | I_push of typ t * data t
  | I_some
  | I_none of typ t
  | I_unit
  | I_if_none of inst t * inst t
  | I_if_some of inst t * inst t
  | I_pair
  | I_car
  | I_cdr
  | I_left of typ t
  | I_right of typ t
  | I_if_left of inst t * inst t
  | I_if_right of inst t * inst t
  | I_nil of typ t
  | I_cons
  | I_if_cons of inst t * inst t
  | I_size
  | I_empty_set of typ t
  | I_empty_map of typ t * typ t
  | I_empty_big_map of typ t * typ t
  | I_map of inst t
  | I_iter of inst t
  | I_mem
  | I_get
  | I_update
  | I_if of inst t * inst t
  | I_loop of inst t
  | I_loop_left of inst t
  | I_lambda of typ t * typ t * inst t
  | I_exec
  | I_dip of inst t
  | I_dip_n of Z.t * inst t
  | I_failwith
  | I_cast of typ t
  | I_rename
  | I_concat
  | I_slice
  | I_pack
  | I_unpack of typ t
  | I_add
  | I_sub
  | I_mul
  | I_ediv
  | I_abs
  | I_isnat
  | I_int
  | I_neg
  | I_lsl
  | I_lsr
  | I_or
  | I_and
  | I_xor
  | I_not
  | I_compare
  | I_eq
  | I_neq
  | I_lt
  | I_gt
  | I_le
  | I_ge
  | I_self
  | I_contract of typ t
  | I_transfer_tokens
  | I_set_delegate
  | I_create_account
  | I_create_contract of program
  | I_implicit_account
  | I_now
  | I_amount
  | I_balance
  | I_check_signature
  | I_blake2b
  | I_sha256
  | I_sha512
  | I_hash_key
  | I_steps_to_quota
  | I_source
  | I_sender
  | I_address
  | I_chain_id
  | I_noop
  | I_unpair

and data =
  | D_int of Z.t
  | D_string of string
  | D_bytes of string
  | D_unit
  | D_bool of bool
  | D_pair of data t * data t
  | D_left of data t
  | D_right of data t
  | D_some of data t
  | D_none
  | D_elt of data t * data t
  | D_list of data t list

and program = { param : typ t; storage : typ t; code : inst t }

and 'a t = { pos : Location.t; d : 'a }

(* val data_of_parser_data : typ -> parser_data -> data *)

val num_of_string : string -> Z.t

val num_of_int : int -> Z.t

val is_comparable_type : typ t -> bool

val assert_type : data t -> typ t -> bool
