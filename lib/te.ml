(* type execution *)

open! Core

exception Failwith_signal

(* nth [1;2;3] 0 => 1 *)
(* nth [1;2;3] 1 => 2 *)
let nth : 'a list -> int -> 'a = (fun st n -> List.nth_exn st n)

(* znth [1;2;3] Bigint.zero => 1 *)
(* znth [1;2;3] Bigint.one => 2 *)
let znth : 'a list -> Bigint.t -> 'a =
  (fun st zn -> nth st (Bigint.to_int_exn zn))

(* nth_hd [1;2;3] 1 => [1] *)
(* nth_hd [1;2;3] 2 => [1;2] *)
let nth_hd : 'a list -> int -> 'a list = (fun st n -> List.split_n st n |> fst)

(* nth_tl [1;2;3] 1 => [2;3] *)
(* nth_tl [1;2;3] 2 => [3] *)
let nth_tl : 'a list -> int -> 'a list = (fun st n -> List.split_n st n |> snd)

let rec typ_run_inst :
    param_t:Tz.mich_t Tz.cc ->
    Tz.mich_i Tz.cc ->
    Tz.mich_t Tz.cc list ->
    Tz.mich_t Tz.cc list =
   let open Tz in
   let open TzUtil in
   let z2i = Bigint.to_int_exn in
   let (hd, tl) = (List.hd_exn, List.tl_exn) in
   let gdc = gen_dummy_cc in
   let fw inst_str = failwith ("typ_run_inst : " ^ inst_str) in
   fun ~param_t inst st ->
   let tri = typ_run_inst ~param_t in
   match inst.cc_v with
   | MI_seq (i1, i2) -> st |> tri i1 |> tri i2
   | MI_drop zn -> nth_tl st (z2i zn)
   | MI_dup zn -> nth st (z2i zn - 1) :: st
   | MI_swap -> nth st 1 :: nth st 0 :: nth_tl st 2
   | MI_dig zn -> znth st zn :: (nth_hd st (z2i zn) @ nth_tl st (z2i zn + 1))
   | MI_dug zn ->
     tl (nth_hd st (z2i zn + 1)) @ [ hd st ] @ nth_tl st (z2i zn + 1)
   | MI_push (t, _) -> t :: st
   | MI_some -> gdc (MT_option (hd st)) :: tl st
   | MI_none t -> gdc (MT_option t) :: st
   | MI_unit -> gdc MT_unit :: st
   | MI_if_none (i1, i2) -> (
     try tri i1 (tl st) with
     | Failwith_signal -> (
       match (hd st).cc_v with
       | MT_option t -> tri i2 (t :: tl st)
       | _           -> fw "MI_if_none"
     )
   )
   | MI_pair -> gdc (MT_pair (hd st, nth st 1)) :: nth_tl st 2
   | MI_car -> (
     match (hd st).cc_v with
     | MT_pair (t1, _) -> t1 :: tl st
     | _               -> fw "MI_car"
   )
   | MI_cdr -> (
     match (hd st).cc_v with
     | MT_pair (_, t2) -> t2 :: tl st
     | _               -> fw "MI_cdr"
   )
   | MI_left t -> gdc (MT_or (hd st, t)) :: tl st
   | MI_right t -> gdc (MT_or (t, hd st)) :: tl st
   | MI_if_left (i1, i2) -> (
     try
       match (hd st).cc_v with
       | MT_or (t, _) -> tri i1 (t :: tl st)
       | _            -> fw "MI_if_left 1"
     with
     | Failwith_signal -> (
       match (hd st).cc_v with
       | MT_or (_, t) -> tri i2 (t :: tl st)
       | _            -> fw "MI_if_left 2"
     )
   )
   | MI_nil t -> gdc (MT_list t) :: st
   | MI_cons -> nth st 1 :: nth_tl st 2
   | MI_if_cons (i1, i2) -> (
     try tri i2 (tl st) with
     | Failwith_signal -> (
       match (hd st).cc_v with
       | MT_list t -> tri i1 (t :: hd st :: tl st)
       | _         -> fw "MI_if_cons"
     )
   )
   | MI_size -> gdc MT_nat :: tl st
   | MI_empty_set t -> gdc (MT_set t) :: st
   | MI_empty_map (t1, t2) -> gdc (MT_map (t1, t2)) :: st
   | MI_empty_big_map (t1, t2) -> gdc (MT_big_map (t1, t2)) :: st
   | MI_map i -> (
     match (hd st).cc_v with
     | MT_list t       ->
       let bodyst = tri i (t :: tl st) in
       gdc (MT_list (hd bodyst)) :: tl bodyst
     | MT_map (t1, t2) ->
       let bodyst = tri i (gdc (MT_pair (t1, t2)) :: tl st) in
       gdc (MT_map (t1, hd bodyst)) :: tl bodyst
     | _               -> fw "MI_map"
   )
   | MI_iter _ -> tl st
   | MI_mem -> gdc MT_bool :: tl st
   | MI_get -> (
     match (nth st 1).cc_v with
     | MT_map (_, t)     -> gdc (MT_option t) :: nth_tl st 2
     | MT_big_map (_, t) -> gdc (MT_option t) :: nth_tl st 2
     | _                 -> fw "MI_get"
   )
   | MI_update -> nth_tl st 2
   | MI_if (i1, i2) -> (
     try tri i1 (tl st) with
     | Failwith_signal -> tri i2 (tl st)
   )
   | MI_loop _ -> tl st
   | MI_loop_left _ -> (
     match (hd st).cc_v with
     | MT_or (_, t) -> t :: tl st
     | _            -> fw "MI_loop_left"
   )
   | MI_lambda (_, t2, _) -> t2 :: tl st
   | MI_exec -> (
     match (nth st 1).cc_v with
     | MT_lambda (_, t) -> t :: nth_tl st 2
     | _                -> fw "MI_exec"
   )
   | MI_apply -> (
     match (nth st 1).cc_v with
     | MT_lambda (p, r) -> (
       match p.cc_v with
       | MT_pair (_, p2) -> gdc (MT_lambda (p2, r)) :: nth_tl st 2
       | _               -> fw "MI_apply 2"
     )
     | _                -> fw "MI_apply 1"
   )
   | MI_dip_n (zn, i) -> nth_hd st (z2i zn) @ tri i (nth_tl st (z2i zn))
   | MI_failwith -> raise Failwith_signal
   | MI_cast t -> t :: tl st
   | MI_rename -> st
   | MI_concat -> (
     match (hd st).cc_v with
     | MT_list t -> (
       match t.cc_v with
       | MT_string
       | MT_bytes ->
         t :: tl st
       | _ -> fw "MI_concat 2"
     )
     | MT_string -> gdc MT_string :: nth_tl st 2
     | MT_bytes  -> gdc MT_bytes :: nth_tl st 2
     | _         -> fw "MI_concat 1"
   )
   | MI_slice -> (
     let nth2 = nth st 2 in
     match nth2.cc_v with
     | MT_string
     | MT_bytes ->
       gdc (MT_option nth2) :: nth_tl st 3
     | _ -> fw "MI_slice"
   )
   | MI_pack -> gdc MT_bytes :: tl st
   | MI_unpack t -> t :: tl st
   | MI_add ->
     let vt =
        match ((hd st).cc_v, (nth st 1).cc_v) with
        | (MT_nat, MT_nat)       -> MT_nat
        | (MT_nat, MT_int)       -> MT_int
        | (MT_int, MT_nat)       -> MT_int
        | (MT_int, MT_int)       -> MT_int
        | (MT_timestamp, MT_int) -> MT_timestamp
        | (MT_int, MT_timestamp) -> MT_timestamp
        | (MT_mutez, MT_mutez)   -> MT_mutez
        | _                      -> fw "MI_add"
     in
     gdc vt :: nth_tl st 2
   | MI_sub ->
     let vt =
        match ((hd st).cc_v, (nth st 1).cc_v) with
        | (MT_nat, MT_nat)       -> MT_int
        | (MT_nat, MT_int)       -> MT_int
        | (MT_int, MT_nat)       -> MT_int
        | (MT_int, MT_int)       -> MT_int
        | (MT_timestamp, MT_int) -> MT_timestamp
        | (MT_int, MT_timestamp) -> MT_int
        | (MT_mutez, MT_mutez)   -> MT_mutez
        | _                      -> fw "MI_sub"
     in
     gdc vt :: nth_tl st 2
   | MI_mul ->
     let vt =
        match ((hd st).cc_v, (nth st 1).cc_v) with
        | (MT_nat, MT_nat)   -> MT_nat
        | (MT_nat, MT_int)   -> MT_int
        | (MT_int, MT_nat)   -> MT_int
        | (MT_int, MT_int)   -> MT_int
        | (MT_mutez, MT_nat) -> MT_mutez
        | (MT_nat, MT_mutez) -> MT_mutez
        | _                  -> fw "MI_mul"
     in
     gdc vt :: nth_tl st 2
   | MI_ediv ->
     let vt =
        match ((hd st).cc_v, (nth st 1).cc_v) with
        | (MT_nat, MT_nat)   ->
          MT_option (gdc (MT_pair (gdc MT_nat, gdc MT_nat)))
        | (MT_nat, MT_int)   ->
          MT_option (gdc (MT_pair (gdc MT_int, gdc MT_nat)))
        | (MT_int, MT_nat)   ->
          MT_option (gdc (MT_pair (gdc MT_int, gdc MT_nat)))
        | (MT_int, MT_int)   ->
          MT_option (gdc (MT_pair (gdc MT_int, gdc MT_nat)))
        | (MT_mutez, MT_nat) ->
          MT_option (gdc (MT_pair (gdc MT_mutez, gdc MT_mutez)))
        | (MT_nat, MT_mutez) ->
          MT_option (gdc (MT_pair (gdc MT_nat, gdc MT_mutez)))
        | _                  -> fw "MI_ediv"
     in
     gdc vt :: nth_tl st 2
   | MI_abs -> gdc MT_nat :: tl st
   | MI_isnat -> gdc (MT_option (gdc MT_nat)) :: tl st
   | MI_int -> gdc MT_int :: tl st
   | MI_neg -> gdc MT_int :: tl st
   | MI_lsl -> gdc MT_nat :: nth_tl st 2
   | MI_lsr -> gdc MT_nat :: nth_tl st 2
   | MI_or -> (
     match (hd st).cc_v with
     | MT_bool
     | MT_nat ->
       hd st :: nth_tl st 2
     | _ -> fw "MI_or"
   )
   | MI_and -> (
     match (hd st).cc_v with
     | MT_bool -> hd st :: nth_tl st 2
     | MT_nat
     | MT_int ->
       nth st 1 :: nth_tl st 2
     | _ -> fw "MI_and"
   )
   | MI_xor -> (
     match (hd st).cc_v with
     | MT_bool
     | MT_nat ->
       hd st :: nth_tl st 2
     | _ -> fw "MI_xor"
   )
   | MI_not -> (
     match (hd st).cc_v with
     | MT_bool -> hd st :: tl st
     | MT_nat
     | MT_int ->
       gdc MT_int :: tl st
     | _ -> fw "MI_not"
   )
   | MI_compare -> gdc MT_int :: nth_tl st 2
   | MI_eq -> gdc MT_bool :: tl st
   | MI_neq -> gdc MT_bool :: tl st
   | MI_lt -> gdc MT_bool :: tl st
   | MI_gt -> gdc MT_bool :: tl st
   | MI_le -> gdc MT_bool :: tl st
   | MI_ge -> gdc MT_bool :: tl st
   | MI_self -> gdc (MT_contract param_t) :: st
   | MI_contract t -> gdc (MT_option (gdc (MT_contract t))) :: tl st
   | MI_transfer_tokens -> gdc MT_operation :: nth_tl st 3
   | MI_set_delegate -> gdc MT_operation :: tl st
   | MI_create_account -> gdc MT_operation :: gdc MT_address :: nth_tl st 4
   | MI_create_contract (_, _, _) ->
     gdc MT_operation :: gdc MT_address :: nth_tl st 3
   | MI_implicit_account -> gdc (MT_contract (gdc MT_unit)) :: tl st
   | MI_now -> gdc MT_timestamp :: st
   | MI_amount -> gdc MT_mutez :: st
   | MI_balance -> gdc MT_mutez :: st
   | MI_check_signature -> gdc MT_bool :: nth_tl st 3
   | MI_blake2b -> st
   | MI_sha256 -> st
   | MI_sha512 -> st
   | MI_hash_key -> gdc MT_key_hash :: tl st
   | MI_steps_to_quota -> gdc MT_nat :: st
   | MI_source -> gdc MT_address :: st
   | MI_sender -> gdc MT_address :: st
   | MI_address -> gdc MT_address :: tl st
   | MI_chain_id -> gdc MT_chain_id :: st
   | MI_unpair -> (
     match (hd st).cc_v with
     | MT_pair (t1, t2) -> t1 :: t2 :: tl st
     | _                -> fw "MI_unpair"
   )
   (* Non-Standard Instruction : Special Comment : MicSE user defined safety property *)
   | MI_micse_check _ -> st

(* let rec typ_run_inst :
    Tz.mich_i Tz.cc -> Tz.mich_t Tz.cc list -> Tz.mich_t Tz.cc list =
   let open Tz in
   fun inst st ->
   match inst.cc_v with
   | MI_seq (i1, i2)
   | MI_drop zn
   | MI_dup zn
   | MI_swap
   | MI_dig zn
   | MI_dug zn
   | MI_push (t, _)
   | MI_some
   | MI_none t
   | MI_unit
   | MI_if_none (i1, i2)
   | MI_pair
   | MI_car
   | MI_cdr
   | MI_left t
   | MI_right t
   | MI_if_left (i1, i2)
   | MI_nil t
   | MI_cons
   | MI_if_cons (i1, i2)
   | MI_size
   | MI_empty_set t
   | MI_empty_map (t1, t2)
   | MI_empty_big_map (t1, t2)
   | MI_map i
   | MI_iter i
   | MI_mem
   | MI_get
   | MI_update
   | MI_if (i1, i2)
   | MI_loop i
   | MI_loop_left i
   | MI_lambda (t1, t2, i)
   | MI_exec
   | MI_apply
   | MI_dip_n (zn, i)
   | MI_failwith
   | MI_cast t
   | MI_rename
   | MI_concat
   | MI_slice
   | MI_pack
   | MI_unpack t
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
   | MI_contract t
   | MI_transfer_tokens
   | MI_set_delegate
   | MI_create_account
   | MI_create_contract (t1, t2, i)
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
   | MI_micse_check i ->
     failwith "typ_run_inst" *)
