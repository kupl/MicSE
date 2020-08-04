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

let rec is_comparable_type t =
  let is_simple_comparable_type t =
    match t.d with
    | T_int | T_nat | T_string | T_bytes | T_mutez | T_bool | T_key_hash
    | T_timestamp | T_address ->
        true
    | _ -> false
  in
  match t.d with
  | T_pair (t_1, t_2) -> is_simple_comparable_type t_1 && is_comparable_type t_2
  | _ -> is_simple_comparable_type t

(* let rec data_of_parser_data (t, _) d =
  match d with
  | P_int d -> (
      match t with
      | T_comparable (T_simple_comparable_type T_int) -> D_int d
      | T_comparable (T_simple_comparable_type T_nat) -> D_nat d
      | T_comparable (T_simple_comparable_type T_mutez) -> D_mutez d
      | _ -> assert false )
  | P_string s -> (
      match t with
      | T_comparable (T_simple_comparable_type T_string) -> D_string s
      | T_comparable (T_simple_comparable_type T_key_hash) -> D_key_hash s
      | T_comparable (T_simple_comparable_type T_timestamp) -> D_timestamp s
      | T_comparable (T_simple_comparable_type T_address) -> D_address s
      | _ -> assert false )
  | P_bytes s -> (
      match t with
      | T_comparable (T_simple_comparable_type T_bytes) -> D_bytes s
      | _ -> assert false )
  | P_bool b -> (
      match t with
      | T_comparable (T_simple_comparable_type T_bool) -> D_bool b
      | _ -> assert false )
  | P_pair (d_1, d_2) -> (
      match t with
      | T_comparable (T_comparable_pair ((t_1, a_1), (t_2, a_2))) ->
          D_pair
            ( data_of_parser_data
                (T_comparable (T_simple_comparable_type t_1), a_1)
                d_1,
              data_of_parser_data (T_comparable t_2, a_2) d_2 )
      | T_pair (t_1, t_2) ->
          D_pair (data_of_parser_data t_1 d_1, data_of_parser_data t_2 d_2)
      | _ -> assert false )
  | P_left d -> (
      match t with
      | T_or (t, _) -> D_left (data_of_parser_data t d)
      | _ -> assert false )
  | P_right d -> (
      match t with
      | T_or (_, t) -> D_right (data_of_parser_data t d)
      | _ -> assert false )
  | P_some d -> (
      match t with
      | T_option t -> D_some (data_of_parser_data t d)
      | _ -> assert false )
  | P_none -> ( match t with T_option t -> D_none t | _ -> assert false )
  | P_list d -> (
      match t with
      | T_list t -> D_list (t, List.map (data_of_parser_data t) d)
      | T_set (t, a) ->
          let t' = (T_comparable t, a) in
          D_set ((t, a), List.map (data_of_parser_data t') d)
      | _ -> assert false )
  | P_map d -> (
      match t with
      | T_map ((t_1, a_1), t_2) ->
          D_map
            ( ((t_1, a_1), t_2),
              List.map
                (fun (k, v) ->
                  ( data_of_parser_data (T_comparable t_1, a_1) k,
                    data_of_parser_data t_2 v ))
                d )
      | _ -> assert false )
  | P_unit -> ( match t with T_unit -> D_unit | _ -> assert false ) *)

let num_of_string = Z.of_string

let num_of_int = Z.of_int

let rec assert_type d t =
  match (d.d, t.d) with
  | D_int _, (T_int | T_nat | T_mutez)
  | D_unit, T_unit
  | D_none, T_option _
  | ( D_string _,
      (T_string | T_key | T_key_hash | T_signature | T_address | T_timestamp) )
  | D_bytes _, T_bytes
  | D_bool _, T_bool ->
      true
  | D_pair (d_1, d_2), T_pair (t_1, t_2) ->
      assert_type d_1 t_1 && assert_type d_2 t_2
  | D_left d', T_or (t', _) | D_right d', T_or (_, t') | D_some d', T_option t'
    ->
      assert_type d' t'
  | D_list l, (T_list t' | T_set t') ->
      List.for_all (fun d' -> assert_type d' t') l
  | D_list l, (T_map (k, v) | T_big_map (k, v)) ->
      let assert_type_map d k v =
        match d.d with
        | D_elt (d_k, d_v) -> assert_type d_k k && assert_type d_v v
        | _ -> false
      in
      List.for_all (fun d' -> assert_type_map d' k v) l
  | _ -> false
