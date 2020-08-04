open Env

let join counter env_t env_f =
  let open Adt in
  match (env_t, env_f) with
  | Failed, env | env, Failed -> (env, create_stmt S_skip)
  | Stack env_t, Stack env_f ->
      let env_after =
        List.map2
          (fun (v, v_t) (f, _) ->
            if v = f then (v, v_t) else (next_var counter, v_t))
          env_t env_f
      in
      let rec phi acc env_after env_t env_f =
        match (env_after, env_t, env_f) with
        | [], [], [] -> acc
        | (after, _) :: env_after, (t, _) :: env_t, (f, _) :: env_f when t <> f
          ->
            let s = create_stmt (S_assign (after, E_phi (t, f))) in
            phi (create_stmt (S_seq (s, acc))) env_after env_t env_f
        | _ :: env_after, _ :: env_t, _ :: env_f ->
            phi acc env_after env_t env_f
        | _ -> assert false
      in
      let phis =
        phi (create_stmt S_skip) (List.rev env_after) (List.rev env_t)
          (List.rev env_f)
      in
      (Stack env_after, phis)

let unlift_option_t t =
  let open Michelson.Adt in
  match t.d with
  | T_option t -> t
  | _ ->
      let () =
        Format.fprintf Format.err_formatter "Expected: option 'a but got %a\n"
          Pp.typ t
      in
      assert false

let car_t t =
  let open Michelson.Adt in
  match t.d with
  | T_pair (t, _) -> t
  | _ ->
      let () =
        Format.fprintf Format.err_formatter "Expected: pair 'a 'b but got %a\n"
          Pp.typ t
      in
      assert false

let cdr_t t =
  let open Michelson.Adt in
  match t.d with
  | T_pair (_, t) -> t
  | _ ->
      let () =
        Format.fprintf Format.err_formatter "Expected: pair 'a 'b but got %a\n"
          Pp.typ t
      in
      assert false

let unlift_left_t t =
  let open Michelson.Adt in
  match t.d with
  | T_or (t, _) -> t
  | _ ->
      let () =
        Format.fprintf Format.err_formatter "Expected: or 'a 'b but got %a\n"
          Pp.typ t
      in
      assert false

let unlift_right_t t =
  let open Michelson.Adt in
  match t.d with
  | T_or (_, t) -> t
  | _ ->
      let () =
        Format.fprintf Format.err_formatter "Expected: or 'a 'b but got %a\n"
          Pp.typ t
      in
      assert false

let list_elem_t t =
  let open Michelson.Adt in
  match t.d with
  | T_list t -> t
  | _ ->
      let () =
        Format.fprintf Format.err_formatter "Expected: list 'a but got %a\n"
          Pp.typ t
      in
      assert false

let map_iter_elem_t t =
  let open Michelson.Adt in
  match t.d with
  | T_list t -> t
  | T_set t -> t
  | T_map (k, v) | T_big_map (k, v) -> Adt.create_typ (T_pair (k, v))
  | _ ->
      let () =
        Format.fprintf Format.err_formatter
          "Expected: list 'a or set 'a or map 'a 'b but got %a" Pp.typ t
      in
      assert false

let lambda_t t =
  let open Michelson.Adt in
  match t.d with
  | T_lambda (_, t) -> t
  | _ ->
      let () =
        Format.fprintf Format.err_formatter
          "Expected: lambda 'a 'b but got %a\n" Pp.typ t
      in
      assert false

let rec inst_to_stmt contract_t counter env =
  let inst_to_stmt = inst_to_stmt contract_t in
  let open Michelson.Adt in
  let open Adt in
  let loop_n f =
    let rec loop acc n =
      if Z.(n = zero) then acc else loop (f acc n) Z.(n - one)
    in
    loop
  in
  let next_var () = next_var counter in
  let create_assign e =
    let v = next_var () in
    (v, create_stmt (S_assign (v, e)))
  in
  let rec aux i =
    try
      match i.d with
      | I_push (t, x) ->
          assert (Michelson.Adt.assert_type x t);
          let e = E_push (x, t) in
          let v, assign = create_assign e in
          (assign, push (v, t) env)
      | I_seq (i_1, i_2) ->
          let s_1, env_1 = inst_to_stmt counter env i_1 in
          let s_2, env_2 = inst_to_stmt counter env_1 i_2 in
          (create_stmt (S_seq (s_1, s_2)), env_2)
      | I_drop ->
          let (x, _), env' = pop env in
          (create_stmt (S_drop [ x ]), env')
      | I_drop_n n ->
          let env', l =
            loop_n
              (fun (env, l) _ ->
                let (x, _), env = pop env in
                (env, x :: l))
              (env, []) n
          in
          (create_stmt (S_drop l), env')
      | I_dup ->
          let x, t = peek env in
          let v, assign = create_assign (E_dup x) in
          (assign, push (v, t) env)
      | I_dig n -> (create_stmt S_dig, dig env n)
      | I_dug n -> (create_stmt S_dug, dug env n)
      | I_swap ->
          let env' = swap env in
          (create_stmt S_swap, env')
      | I_some ->
          let (x, t), env' = pop env in
          let v, assign = create_assign (E_some x) in
          (assign, push (v, create_typ (T_option t)) env')
      | I_none t ->
          let v, assign = create_assign (E_none t) in
          (assign, push (v, create_typ (T_option t)) env)
      | I_unit ->
          let v, assign = create_assign E_unit in
          (assign, push (v, create_typ T_unit) env)
      | I_if_none (i_t, i_f) ->
          let (x, t), env' = pop env in
          let s_t, env_t = inst_to_stmt counter env' i_t in
          let v, assign = create_assign (E_unlift_option x) in
          let t = unlift_option_t t in
          let s_f, env_f = inst_to_stmt counter (push (v, t) env') i_f in
          let env', phis = join counter env_t env_f in
          let s_f = create_stmt (S_seq (assign, s_f)) in
          let s =
            create_stmt (S_seq (create_stmt (S_if_none (x, s_t, s_f)), phis))
          in
          (s, env')
      | I_if_some (i_t, i_f) -> aux { i with d = I_if_none (i_f, i_t) }
      | I_pair ->
          let (x_1, t_1), env' = pop env in
          let (x_2, t_2), env' = pop env' in
          let v, assign = create_assign (E_pair (x_1, x_2)) in
          (assign, push (v, create_typ (T_pair (t_1, t_2))) env')
      | I_car ->
          let (x, t), env' = pop env in
          let t = car_t t in
          let v, assign = create_assign (E_car x) in
          (assign, push (v, t) env')
      | I_cdr ->
          let (x, t), env' = pop env in
          let t = cdr_t t in
          let v, assign = create_assign (E_cdr x) in
          (assign, push (v, t) env')
      | I_left t_r ->
          let (x, t_l), env' = pop env in
          let v, assign = create_assign (E_left (x, t_r)) in
          (assign, push (v, create_typ (T_or (t_l, t_r))) env')
      | I_right t_l ->
          let (x, t_r), env' = pop env in
          let v, assign = create_assign (E_right (x, t_l)) in
          (assign, push (v, create_typ (T_or (t_l, t_r))) env')
      | I_if_left (i_t, i_f) ->
          let (x, t), env' = pop env in
          let e = E_unlift_or x in
          let v_l, assign_l = create_assign e in
          let v_r, assign_r = create_assign e in
          let t_l, t_r = (unlift_left_t t, unlift_right_t t) in
          let env_t = push (v_l, t_l) env' in
          let env_f = push (v_r, t_r) env' in
          let s_t, env_t = inst_to_stmt counter env_t i_t in
          let s_f, env_f = inst_to_stmt counter env_f i_f in
          let env', phis = join counter env_t env_f in
          let s_t = create_stmt (S_seq (assign_l, s_t)) in
          let s_f = create_stmt (S_seq (assign_r, s_f)) in
          let s =
            create_stmt (S_seq (create_stmt (S_if_left (x, s_t, s_f)), phis))
          in
          (s, env')
      | I_if_right (i_t, i_f) ->
          inst_to_stmt counter env { i with d = I_if_left (i_f, i_t) }
      | I_nil t ->
          let v, assign = create_assign (E_nil t) in
          (assign, push (v, create_typ (T_list t)) env)
      | I_cons ->
          let (x_1, _), env' = pop env in
          let (x_2, t), env' = pop env' in
          let v, assign = create_assign (E_cons (x_1, x_2)) in
          (assign, push (v, t) env')
      | I_if_cons (i_t, i_f) ->
          let (c, t_l), env' = pop env in
          let t_hd = list_elem_t t_l in
          let v_hd, assign_hd =
            let e_hd = E_hd c in
            create_assign e_hd
          in
          let v_tl, assign_tl =
            let e_tl = E_tl c in
            create_assign e_tl
          in
          let env_t = push (v_hd, t_hd) (push (v_tl, t_l) env') in
          let env_f = env' in
          let s_t, env_t = inst_to_stmt counter env_t i_t in
          let s_f, env_f = inst_to_stmt counter env_f i_f in
          let env', phis = join counter env_t env_f in
          let s_t =
            create_stmt
              (S_seq (assign_hd, create_stmt (S_seq (assign_tl, s_t))))
          in
          let s =
            create_stmt (S_seq (create_stmt (S_if_cons (c, s_t, s_f)), phis))
          in
          (s, env')
      | I_size ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_size x) in
          (assign, push (v, create_typ T_nat) env')
      | I_empty_set t ->
          let v, assign = create_assign (E_empty_set t) in
          (assign, push (v, create_typ (T_set t)) env)
      | I_empty_map (t_k, t_v) ->
          let v, assign = create_assign (E_empty_map (t_k, t_v)) in
          (assign, push (v, create_typ (T_map (t_k, t_v))) env)
      | I_empty_big_map (t_k, t_v) ->
          let v, assign = create_assign (E_empty_big_map (t_k, t_v)) in
          (assign, push (v, create_typ (T_big_map (t_k, t_v))) env)
      | I_map b ->
          let (c, t_l), env' = pop env in
          let t_hd = map_iter_elem_t t_l in
          let empty_list, empty_list_assign =
            create_assign E_special_nil_list
          in
          let loop_var = next_var () in
          let result = next_var () in
          let hd, assign_hd = create_assign (E_hd loop_var) in
          let body, env' =
            let body_env = push (hd, t_hd) env' in
            inst_to_stmt counter body_env b
          in
          let tl, assign_tl = create_assign (E_tl loop_var) in
          let (x, t), env' = pop env' in
          let append, assign_append = create_assign (E_append (result, x)) in
          let body =
            create_stmt
              (S_seq
                 ( assign_hd,
                   create_stmt
                     (S_seq
                        (body, create_stmt (S_seq (assign_append, assign_tl))))
                 ))
          in
          let s =
            create_stmt
              (S_seq
                 ( empty_list_assign,
                   create_stmt
                     (S_map
                        ( (loop_var, (c, tl)),
                          (result, (empty_list, append)),
                          body )) ))
          in
          (s, push (result, t) env')
      | I_iter b ->
          let (c, t_l), env' = pop env in
          let t_hd = map_iter_elem_t t_l in
          let loop_var = next_var () in
          let hd, assign_hd = create_assign (E_hd loop_var) in
          let body, env' =
            let body_env = push (hd, t_hd) env' in
            inst_to_stmt counter body_env b
          in
          let tl, assign_tl = create_assign (E_tl loop_var) in
          let body =
            create_stmt
              (S_seq (assign_hd, create_stmt (S_seq (body, assign_tl))))
          in
          let s = create_stmt (S_iter (loop_var, (c, tl), body)) in
          (s, env')
      | I_mem ->
          let (elt, _), env' = pop env in
          let (set, _), env' = pop env' in
          let v, assign = create_assign (E_mem (elt, set)) in
          (assign, push (v, create_typ T_bool) env')
      | I_get ->
          let (key, _), env' = pop env in
          let (map, t), env' = pop env' in
          let t =
            match t.d with
            | T_map (_, t) | T_big_map (_, t) -> t
            | _ -> assert false
          in
          let v, assign = create_assign (E_get (key, map)) in
          (assign, push (v, create_typ (T_option t)) env')
      | I_update ->
          let (key, _), env' = pop env in
          let (value, _), env' = pop env' in
          let (map, t), env' = pop env' in
          let v, assign = create_assign (E_update (key, value, map)) in
          (assign, push (v, t) env')
      | I_if (i_t, i_f) ->
          let (c, _), env' = pop env in
          let s_t, env_t = inst_to_stmt counter env' i_t in
          let s_f, env_f = inst_to_stmt counter env' i_f in
          let env', phis = join counter env_t env_f in
          let s =
            create_stmt (S_seq (create_stmt (S_if (c, s_t, s_f)), phis))
          in
          (s, env')
      | I_loop i ->
          let (c, _), env' = pop env in
          let loop_var = next_var () in
          let body, env' = inst_to_stmt counter env' i in
          let (loop_result, _), env' = pop env' in
          let s = create_stmt (S_loop (loop_var, (c, loop_result), body)) in
          (s, env')
      | I_loop_left i ->
          let (c, t), env' = pop env in
          let t_l, t_r = (unlift_left_t t, unlift_right_t t) in
          let loop_var = next_var () in
          let e = E_unlift_or loop_var in
          let v, assign_unlift = create_assign e in
          let body, env' =
            let body_env = push (v, t_l) env' in
            inst_to_stmt counter body_env i
          in
          let (loop_result, _), env' = pop env' in
          let body = create_stmt (S_seq (assign_unlift, body)) in
          let post_loop_unlift = E_unlift_or loop_var in
          let v_post_loop, post_loop_assign_unlift =
            create_assign post_loop_unlift
          in
          let s =
            create_stmt
              (S_seq
                 ( create_stmt (S_loop_left (loop_var, (c, loop_result), body)),
                   post_loop_assign_unlift ))
          in
          let env' = push (v_post_loop, t_r) env' in
          (s, env')
      | I_lambda (t_1, t_2, i) ->
          let b, lambda_env =
            inst_to_stmt counter
              (push ("param_storage", create_typ (T_pair (t_1, t_2))) empty_env)
              i
          in
          let (r, _), _ = pop lambda_env in
          let v, assign = create_assign (E_lambda (t_1, t_2, (b, r))) in
          (assign, push (v, create_typ (T_lambda (t_1, t_2))) env)
      | I_exec ->
          let (param, _), env' = pop env in
          let (lambda, t), env' = pop env' in
          let t = lambda_t t in
          let v, assign = create_assign (E_exec (lambda, param)) in
          (assign, push (v, t) env')
      | I_dip i ->
          let x, env' = pop env in
          let s, env' = inst_to_stmt counter env' i in
          (s, push x env')
      | I_dip_n (n, i) ->
          let xl, env' = dip env n in
          let s, env' = inst_to_stmt counter env' i in
          let env' = List.fold_left (fun acc x -> push x acc) env' xl in
          (s, env')
      | I_failwith ->
          let (x, _), _ = pop env in
          (create_stmt (S_failwith x), Failed)
      | I_cast _ -> (create_stmt S_skip, env)
      | I_rename -> (create_stmt S_skip, env)
      | I_concat ->
          let (s, t), env' = pop env in
          let (v, assign), env' =
            match t.d with
            | T_list { d = T_string; _ } ->
                (create_assign (E_concat_list s), env')
            | T_string ->
                let (s_2, _), env' = pop env' in
                (create_assign (E_concat (s, s_2)), env')
            | _ -> assert false
          in
          (assign, push (v, create_typ T_string) env')
      | I_slice ->
          let (offset, _), env' = pop env in
          let (length, _), env' = pop env' in
          let (x, _), env' = pop env' in
          let v, assign = create_assign (E_slice (offset, length, x)) in
          (assign, push (v, create_typ (T_option (create_typ T_bytes))) env')
      | I_pack ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_pack x) in
          (assign, push (v, create_typ T_bytes) env')
      | I_unpack t ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_unpack (t, x)) in
          (assign, push (v, create_typ (T_option t)) env')
      | I_add ->
          let (x_1, t_1), env' = pop env in
          let (x_2, t_2), env' = pop env' in
          let t =
            match (t_1.d, t_2.d) with
            | T_timestamp, T_int | T_int, T_timestamp -> T_timestamp
            | T_mutez, T_mutez -> T_mutez
            | T_int, _ | _, T_int -> T_int
            | T_nat, T_nat -> T_nat
            | _ -> assert false
          in
          let v, assign = create_assign (E_add (x_1, x_2)) in
          (assign, push (v, create_typ t) env')
      | I_sub ->
          let (x_1, t_1), env' = pop env in
          let (x_2, t_2), env' = pop env' in
          let t =
            match (t_1.d, t_2.d) with
            | T_timestamp, T_int -> T_timestamp
            | T_timestamp, T_timestamp -> T_int
            | T_mutez, T_mutez -> T_mutez
            | (T_int | T_nat), (T_int | T_nat) -> T_int
            | _ -> assert false
          in
          let v, assign = create_assign (E_sub (x_1, x_2)) in
          (assign, push (v, create_typ t) env')
      | I_mul ->
          let (x_1, t_1), env' = pop env in
          let (x_2, t_2), env' = pop env' in
          let t =
            match (t_1.d, t_2.d) with
            | T_mutez, T_nat -> T_mutez
            | T_nat, T_mutez -> T_mutez
            | T_int, _ | _, T_int -> T_int
            | T_nat, T_nat -> T_nat
            | _ -> assert false
          in
          let v, assign = create_assign (E_mul (x_1, x_2)) in
          (assign, push (v, create_typ t) env')
      | I_ediv ->
          let (x_1, t_1), env' = pop env in
          let (x_2, t_2), env' = pop env' in
          let t_1, t_2 =
            match (t_1.d, t_2.d) with
            | T_mutez, T_nat -> (T_mutez, T_mutez)
            | T_mutez, T_mutez -> (T_nat, T_mutez)
            | T_int, _ | _, T_int -> (T_int, T_nat)
            | T_nat, T_nat -> (T_nat, T_nat)
            | _ -> assert false
          in
          let v, assign = create_assign (E_div (x_1, x_2)) in
          ( assign,
            push
              ( v,
                create_typ
                  (T_option
                     (create_typ (T_pair (create_typ t_1, create_typ t_2)))) )
              env' )
      | I_abs ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_abs x) in
          (assign, push (v, create_typ T_nat) env')
      | I_neg ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_neg x) in
          (assign, push (v, create_typ T_int) env')
      | I_lsl ->
          let (x_1, _), env' = pop env in
          let (x_2, _), env' = pop env' in
          let v, assign = create_assign (E_shiftL (x_1, x_2)) in
          (assign, push (v, create_typ T_nat) env')
      | I_lsr ->
          let (x_1, _), env' = pop env in
          let (x_2, _), env' = pop env' in
          let v, assign = create_assign (E_shiftR (x_1, x_2)) in
          (assign, push (v, create_typ T_nat) env')
      | I_or ->
          let (x_1, _), env' = pop env in
          let (x_2, _), env' = pop env' in
          let v, assign = create_assign (E_or (x_1, x_2)) in
          (assign, push (v, create_typ T_nat) env')
      | I_and ->
          let (x_1, _), env' = pop env in
          let (x_2, _), env' = pop env' in
          let v, assign = create_assign (E_and (x_1, x_2)) in
          (assign, push (v, create_typ T_nat) env')
      | I_xor ->
          let (x_1, _), env' = pop env in
          let (x_2, _), env' = pop env' in
          let v, assign = create_assign (E_xor (x_1, x_2)) in
          (assign, push (v, create_typ T_nat) env')
      | I_not ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_not x) in
          (assign, push (v, create_typ T_int) env')
      | I_compare ->
          let (x_1, _), env' = pop env in
          let (x_2, _), env' = pop env' in
          let v, assign = create_assign (E_compare (x_1, x_2)) in
          (assign, push (v, create_typ T_int) env')
      | I_eq ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_eq x) in
          (assign, push (v, create_typ T_bool) env')
      | I_neq ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_neq x) in
          (assign, push (v, create_typ T_bool) env')
      | I_lt ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_lt x) in
          (assign, push (v, create_typ T_bool) env')
      | I_gt ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_gt x) in
          (assign, push (v, create_typ T_bool) env')
      | I_le ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_leq x) in
          (assign, push (v, create_typ T_bool) env')
      | I_ge ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_geq x) in
          (assign, push (v, create_typ T_bool) env')
      | I_self ->
          let v, assign = create_assign E_self in
          (assign, push (v, contract_t) env)
      | I_contract t ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_contract_of_address x) in
          (assign, push (v, create_typ (T_option t)) env')
      | I_transfer_tokens ->
          let (x, _), env' = pop env in
          let (amount, _), env' = pop env' in
          let (contract, _), env' = pop env' in
          let operation = O_transfer_tokens (x, amount, contract) in
          let v, assign = create_assign (E_operation operation) in
          (assign, push (v, create_typ T_operation) env')
      | I_set_delegate ->
          let (x, _), env' = pop env in
          let o = O_set_delegate x in
          let v, assign = create_assign (E_operation o) in
          (assign, push (v, create_typ T_operation) env')
      | I_create_account ->
          let (manager, _), env' = pop env in
          let (delegate, _), env' = pop env' in
          let (delegatable, _), env' = pop env' in
          let (amount, _), env' = pop env' in
          let o = O_create_account (manager, delegate, delegatable, amount) in
          let v_o, assign_o = create_assign (E_operation o) in
          let v_a, assign_a = create_assign (E_create_account_address o) in
          ( create_stmt (S_seq (assign_o, assign_a)),
            push
              (v_o, create_typ T_operation)
              (push (v_a, create_typ T_address) env') )
      | I_create_contract c ->
          let (delegate, _), env' = pop env in
          let (amount, _), env' = pop env' in
          let (storage, _), env' = pop env' in
          let o = O_create_contract (c, delegate, amount, storage) in
          let v_o, assign_o = create_assign (E_operation o) in
          let v_a, assign_a = create_assign (E_create_account_address o) in
          let env' =
            push
              (v_o, create_typ T_operation)
              (push (v_a, create_typ T_address) env')
          in
          (create_stmt (S_seq (assign_o, assign_a)), env')
      | I_implicit_account ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_implicit_account x) in
          (assign, push (v, create_typ (T_contract (create_typ T_unit))) env')
      | I_now ->
          let v, assign = create_assign E_now in
          (assign, push (v, create_typ T_timestamp) env)
      | I_amount ->
          let v, assign = create_assign E_amount in
          (assign, push (v, create_typ T_mutez) env)
      | I_balance ->
          let v, assign = create_assign E_balance in
          (assign, push (v, create_typ T_mutez) env)
      | I_check_signature ->
          let (key, _), env' = pop env in
          let (signature, _), env' = pop env' in
          let (bytes, _), env' = pop env' in
          let v, assign =
            create_assign (E_check_signature (key, signature, bytes))
          in
          (assign, push (v, create_typ T_bool) env')
      | I_blake2b ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_blake2b x) in
          (assign, push (v, create_typ T_bytes) env')
      | I_sha256 ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_sha256 x) in
          (assign, push (v, create_typ T_bytes) env')
      | I_sha512 ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_sha512 x) in
          (assign, push (v, create_typ T_bytes) env')
      | I_hash_key ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_hash_key x) in
          (assign, push (v, create_typ T_key_hash) env')
      | I_steps_to_quota ->
          let v, assign = create_assign E_steps_to_quota in
          (assign, push (v, create_typ T_nat) env)
      | I_source ->
          let v, assign = create_assign E_source in
          (assign, push (v, create_typ T_address) env)
      | I_sender ->
          let v, assign = create_assign E_sender in
          (assign, push (v, create_typ T_address) env)
      | I_address ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_address_of_contract x) in
          (assign, push (v, create_typ T_address) env')
      | I_isnat ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_isnat x) in
          (assign, push (v, create_typ (T_option (create_typ T_nat))) env')
      | I_int ->
          let (x, _), env' = pop env in
          let v, assign = create_assign (E_int_of_nat x) in
          (assign, push (v, create_typ T_int) env')
      | I_chain_id ->
          let v, assign = create_assign E_chain_id in
          (assign, push (v, create_typ T_chain_id) env)
      | I_noop -> (create_stmt S_skip, env)
      | I_unpair ->
          let (x, t), env' = pop env in
          let v_1, assign_1 = create_assign (E_car x) in
          let v_2, assign_2 = create_assign (E_cdr x) in
          let t_1, t_2 = (car_t t, cdr_t t) in
          ( create_stmt (S_seq (assign_1, assign_2)),
            push (v_1, t_1) (push (v_2, t_2) env') )
      | I_micse_check _ -> (create_stmt S_skip, env)
    with exn -> (
      match i.pos with
      | Michelson.Location.Pos (s, e) ->
          Printf.fprintf stderr "line: %d col: %d-%d\n" s.lin s.col e.col;
          raise exn
      | _ -> raise exn )
  in
  aux

let convert_program counter p =
  let open Michelson.Adt in
  let env =
    Env.push
      ("parameter_storage", Adt.create_typ (T_pair (p.param, p.storage)))
      Env.empty_env
  in
  fst (inst_to_stmt p.param counter env p.code) |> Adt.simpl
