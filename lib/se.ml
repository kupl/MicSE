(* Se is a symbolic execution module based on Tz.sym_state definition *)

open! Core

(* Set of Tz.sym_state & Set of Tz.mich_cut_info *)
module SSet = Core.Set.Make (Tz.SymState_cmp)
module MciSet = Core.Set.Make (Tz.MichCutInfo_cmp)
(* module SSet = Core.Set.Make(struct type t = Tz.sym_state [@@deriving sexp, compare, equal] end)
   module MciSet = Core.Set.Make(struct type t = Tz.mich_cut_info [@@deriving sexp, compare, equal] end) *)

type se_result = {
  (* symbolic states *)
  sr_running : SSet.t;
  sr_blocked : SSet.t;
  sr_queries : SSet.t;
  sr_terminated : SSet.t;
  (* caches - accumulates which loop/lambdas passed *)
  sr_entered_loops : MciSet.t;
  sr_entered_lmbds : MciSet.t;
  (* caches - count integer to assign sym_state_id (start with 0) *)
  sr_sid_counter : int;
}
[@@deriving sexp, compare, equal]

let se_result_empty : se_result =
   {
     sr_running = SSet.empty;
     sr_blocked = SSet.empty;
     sr_queries = SSet.empty;
     sr_terminated = SSet.empty;
     sr_entered_loops = MciSet.empty;
     sr_entered_lmbds = MciSet.empty;
     sr_sid_counter = 0;
   }

let se_result_pointwise_union : se_result -> se_result -> se_result =
  fun r1 r2 ->
  {
    sr_running = SSet.union r1.sr_running r2.sr_running;
    sr_blocked = SSet.union r1.sr_blocked r2.sr_blocked;
    sr_queries = SSet.union r1.sr_queries r2.sr_queries;
    sr_terminated = SSet.union r1.sr_terminated r2.sr_terminated;
    sr_entered_loops = MciSet.union r1.sr_entered_loops r2.sr_entered_loops;
    sr_entered_lmbds = MciSet.union r1.sr_entered_lmbds r2.sr_entered_lmbds;
    sr_sid_counter = max r1.sr_sid_counter r2.sr_sid_counter;
  }

let run_inst_initial_se_result :
    Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc -> se_result =
   let open Tz in
   fun (param_tcc, strg_tcc, code) ->
   (* sid_counter & sym_ctxt *)
   let scounter = 0 in
   let sctxt = [ scounter ] in
   (* mich_t cc values *)
   let cur_contract_tcc = MT_contract param_tcc |> gen_dummy_cc
   and addr_tcc = MT_address |> gen_dummy_cc
   and mutez_tcc = MT_mutez |> gen_dummy_cc
   and time_tcc = MT_timestamp |> gen_dummy_cc
   and paramstrg_tcc = MT_pair (param_tcc, strg_tcc) |> gen_dummy_cc in
   (* initial mich_cut_info *)
   let init_mci = { mci_loc = code.cc_loc; mci_cutcat = MCC_trx_entry } in
   (* beginning trx-image *)
   let beginning_ti : trx_image =
      {
        ti_contract =
          MV_symbol (cur_contract_tcc, MSC_contract, sctxt) |> gen_dummy_cc;
        ti_source = MV_symbol (addr_tcc, MSC_source, sctxt) |> gen_dummy_cc;
        ti_sender = MV_symbol (addr_tcc, MSC_sender, sctxt) |> gen_dummy_cc;
        ti_param = MV_symbol (param_tcc, MSC_param, sctxt) |> gen_dummy_cc;
        ti_amount = MV_symbol (mutez_tcc, MSC_amount, sctxt) |> gen_dummy_cc;
        ti_time = MV_symbol (time_tcc, MSC_time, sctxt) |> gen_dummy_cc;
      }
   in
   (* beginning sym-image *)
   let beginning_si : sym_image =
      {
        si_mich =
          [ MV_symbol (paramstrg_tcc, MSC_mich_stack 0, sctxt) |> gen_dummy_cc ];
        si_dip = [];
        si_map_entry = [];
        si_map_exit = [];
        si_iter = [];
        si_balance = MV_symbol (mutez_tcc, MSC_balance, sctxt) |> gen_dummy_cc;
        si_bc_balance =
          MV_symbol (mutez_tcc, MSC_bc_balance, sctxt) |> gen_dummy_cc;
        si_param = beginning_ti;
      }
   in
   (* blocking sym-image *)
   let blocking_si : sym_image =
      {
        beginning_si with
        si_balance =
          MV_add_mmm (beginning_si.si_balance, beginning_ti.ti_amount)
          |> gen_dummy_cc;
      }
   in
   let initial_sym_state : sym_state =
      {
        ss_id = sctxt;
        ss_start_mci = init_mci;
        ss_block_mci = init_mci;
        ss_start_si = beginning_si;
        ss_block_si = blocking_si;
        ss_param_history = [ beginning_ti ];
        ss_constraints =
          [
            (* 1. first stack's CAR is parameter-value *)
            MF_eq
              ( beginning_ti.ti_param,
                MV_car (List.hd_exn beginning_si.si_mich) |> gen_dummy_cc
              );
            (* 2. amount, balance, and bc_balance are mutez values *)
            MF_mutez_bound beginning_ti.ti_amount;
            MF_mutez_bound beginning_si.si_balance;
            MF_mutez_bound beginning_si.si_bc_balance;
            (* 3. amount is less-or-equal than bc_balance *)
            MF_is_true
              (MV_leq_ib (beginning_ti.ti_amount, beginning_si.si_bc_balance)
              |> gen_dummy_cc
              );
            (* 4. (balance + bc_balance) is also mutez value *)
            MF_mutez_bound
              (MV_add_mmm (beginning_si.si_balance, beginning_si.si_bc_balance)
              |> gen_dummy_cc
              );
            (* 5. (balance + bc_balance) is equal to total-mutez-amount *)
            (* (let lit_total_mutez_amount =
                   MV_lit_mutez (Bigint.of_int64 Int64.max_value) |> gen_dummy_cc
                in
                MF_eq
                  ( MV_add_mmm (beginning_si.si_balance, beginning_si.si_bc_balance)
                    |> gen_dummy_cc,
                    lit_total_mutez_amount
                  )
               ); *)
          ];
      }
   in
   let initial_se_result : se_result =
      {
        se_result_empty with
        sr_running = SSet.singleton initial_sym_state;
        sr_sid_counter = scounter + 1;
      }
   in
   initial_se_result
(* function run_inst_initial_se_result end *)

let rec run_inst : Tz.mich_i Tz.cc -> se_result -> se_result =
  fun inst sr ->
  SSet.fold sr.sr_running ~init:{ sr with sr_running = SSet.empty }
    ~f:(fun acc_sr ss ->
      se_result_pointwise_union (run_inst_i inst (acc_sr, ss)) acc_sr
  )

and run_inst_i : Tz.mich_i Tz.cc -> se_result * Tz.sym_state -> se_result =
   let open Tz in
   let err : mich_i cc -> 'a =
     fun inst ->
     failwith ("unexpected : " ^ (sexp_of_mich_i inst.cc_v |> string_of_sexp))
   in
   (* utilties : bmstack : blocked-mich-stack *)
   let get_bmstack : sym_state -> mich_v cc list =
     (fun ss -> ss.ss_block_si.si_mich)
   in
   let get_bmstack_2 : sym_state -> mich_v cc * mich_v cc =
     fun ss ->
     match get_bmstack ss with
     | h1 :: h2 :: _ -> (h1, h2)
     | _             -> failwith "get_bmstack_2 : unexpected"
   in
   let set_bmstack : sym_state -> mich_v cc list -> sym_state =
     fun ss st ->
     { ss with ss_block_si = { ss.ss_block_si with si_mich = st } }
   in
   let update_bmstack :
       f:(mich_v cc list -> mich_v cc list) -> sym_state -> sym_state =
     (fun ~f ss -> get_bmstack ss |> f |> set_bmstack ss)
   in
   let push_bmstack : v:mich_v cc -> sym_state -> sym_state =
     (fun ~v ss -> update_bmstack ~f:(List.cons v) ss)
   in
   let update_top_1_bmstack :
       f:(mich_v cc -> mich_v cc list) -> sym_state -> sym_state =
     fun ~f ss ->
     match get_bmstack ss with
     | hd :: tl -> f hd @ tl |> set_bmstack ss
     | _        -> failwith "update_top_1_bmstack : unexpected"
   in
   let update_top_2_bmstack :
       f:(mich_v cc * mich_v cc -> mich_v cc list) -> sym_state -> sym_state =
     fun ~f ss ->
     match get_bmstack ss with
     | h1 :: h2 :: tl -> f (h1, h2) @ tl |> set_bmstack ss
     | _              -> failwith "update_top_2_bmstack : unexpected"
   in
   let update_top_3_bmstack :
       f:(mich_v cc * mich_v cc * mich_v cc -> mich_v cc list) ->
       sym_state ->
       sym_state =
     fun ~f ss ->
     match get_bmstack ss with
     | h1 :: h2 :: h3 :: tl -> f (h1, h2, h3) @ tl |> set_bmstack ss
     | _                    -> failwith "update_top_e_bmstack : unexpected"
   in
   let set_bmstack_and_constraint :
       sym_state -> mich_v cc list -> mich_f list -> sym_state =
     fun ss st cs ->
     {
       ss with
       ss_block_si = { ss.ss_block_si with si_mich = st };
       ss_constraints = cs;
     }
   in
   (* let update_top_1_bmstack_and_constraint :
          f:(mich_v cc -> mich_v cc list * mich_f list) -> sym_state -> sym_state =
        fun ~f ss ->
        match get_bmstack ss with
        | hd :: tl ->
          let (st, cs) = f hd in
          set_bmstack_and_constraint ss (st @ tl) (cs @ ss.ss_constraints)
        | _        -> failwith "update_top_1_bmstack_and_constraint : unexpected"
      in *)
   let update_top_2_bmstack_and_constraint :
       f:(mich_v cc * mich_v cc -> mich_v cc list * mich_f list) ->
       sym_state ->
       sym_state =
     fun ~f ss ->
     match get_bmstack ss with
     | h1 :: h2 :: tl ->
       let (st, cs) = f (h1, h2) in
       set_bmstack_and_constraint ss (st @ tl) (cs @ ss.ss_constraints)
     | _              ->
       failwith "update_top_2_bmstack_and_constraint : unexpected"
   in
   (* let update_top_3_bmstack_and_constraint :
          f:(mich_v cc * mich_v cc * mich_v cc -> mich_v cc list * mich_f list) ->
          sym_state ->
          sym_state =
        fun ~f ss ->
        match get_bmstack ss with
        | h1 :: h2 :: h3 :: tl ->
          let (st, cs) = f (h1, h2, h3) in
          set_bmstack_and_constraint ss (st @ tl) (cs @ ss.ss_constraints)
        | _                    ->
          failwith "update_top_3_bmstack_and_constraint : unexpected"
      in *)
   (* utilities : constraint *)
   let add_constraints : c:mich_f list -> sym_state -> sym_state =
     (fun ~c ss -> { ss with ss_constraints = c @ ss.ss_constraints })
   in
   let mtz_constriant_if_it_is_or_true : tv:mich_t cc * mich_v cc -> mich_f =
     fun ~tv:(t, v) ->
     if equal_mich_t t.cc_v MT_mutez then MF_mutez_bound v else MF_true
   in
   let add_mtz_constraint_if_it_is :
       tv:mich_t cc * mich_v cc -> sym_state -> sym_state =
     fun ~tv ss ->
     add_constraints ~c:[ mtz_constriant_if_it_is_or_true ~tv ] ss
   in
   let nat_constriant_if_it_is_or_true : tv:mich_t cc * mich_v cc -> mich_f =
     fun ~tv:(t, v) ->
     if equal_mich_t t.cc_v MT_nat then MF_nat_bound v else MF_true
   in
   let add_nat_constraint_if_it_is :
       tv:mich_t cc * mich_v cc -> sym_state -> sym_state =
     fun ~tv ss ->
     add_constraints ~c:[ nat_constriant_if_it_is_or_true ~tv ] ss
   in
   (* utilities : sym_state <-> se_result *)
   let running_ss_to_sr : sym_state -> se_result =
     (fun ss -> { se_result_empty with sr_running = SSet.singleton ss })
   in
   (* utilities : context-se_result update *)
   let ctxt_sr_update : se_result -> se_result -> se_result =
     fun ctxt_sr new_sr ->
     {
       ctxt_sr with
       sr_entered_loops =
         MciSet.union ctxt_sr.sr_entered_loops new_sr.sr_entered_loops;
       sr_entered_lmbds =
         MciSet.union ctxt_sr.sr_entered_lmbds new_sr.sr_entered_lmbds;
     }
   in
   (* FUNCTION BEGIN *)
   fun inst (ctxt_sr, ss) ->
   match inst.cc_v with
   | MI_seq (i1, i2) -> run_inst_i i1 (ctxt_sr, ss) |> run_inst i2
   | MI_drop zn ->
     update_bmstack ss ~f:(fun x -> List.split_n x (Bigint.to_int_exn zn) |> snd)
     |> running_ss_to_sr
   | MI_dup zn ->
     update_bmstack ss ~f:(fun x ->
         List.nth_exn x (Bigint.to_int_exn zn - 1) :: x
     )
     |> running_ss_to_sr
   | MI_swap ->
     update_bmstack ss ~f:(function
     | h1 :: h2 :: tl -> h2 :: h1 :: tl
     | _              -> err inst
     )
     |> running_ss_to_sr
   | MI_dig zn ->
     update_bmstack ss ~f:(fun x ->
         match List.split_n x (Bigint.to_int_exn zn) with
         | (hdlst, tlhd :: tltl) -> (tlhd :: hdlst) @ tltl
         | _                     -> err inst
     )
     |> running_ss_to_sr
   | MI_dug zn ->
     update_bmstack ss ~f:(fun x ->
         match List.split_n x (Bigint.to_int_exn zn + 1) with
         | (hdhd :: hdtl, tl) -> hdtl @ (hdhd :: tl)
         | _                  -> err inst
     )
     |> running_ss_to_sr
   | MI_push (t, v) ->
     push_bmstack ss ~v
     |> add_mtz_constraint_if_it_is ~tv:(t, v)
     |> add_nat_constraint_if_it_is ~tv:(t, v)
     |> running_ss_to_sr
   | MI_some ->
     update_top_1_bmstack ~f:(fun x -> [ MV_some x |> gen_custom_cc inst ]) ss
     |> running_ss_to_sr
   | MI_none t ->
     push_bmstack ss ~v:(MV_none t |> gen_custom_cc inst) |> running_ss_to_sr
   | MI_unit ->
     push_bmstack ss ~v:(MV_unit |> gen_custom_cc inst) |> running_ss_to_sr
   | MI_if_none (i1, i2) ->
     let cond_value : mich_v cc = List.hd_exn (get_bmstack ss) in
     let cond_constraint : mich_f = MF_is_none cond_value in
     (* then branch *)
     let then_br_sr : se_result =
        update_top_1_bmstack ~f:(fun _ -> []) ss
        |> add_constraints ~c:[ cond_constraint ]
        |> (fun ssss -> run_inst_i i1 (ctxt_sr, ssss))
     in
     (* IMPORTANT: ctxt_sr name shadowing *)
     let ctxt_sr = ctxt_sr_update ctxt_sr then_br_sr in
     (* else branch *)
     let else_br_sr : se_result =
        let unlifted_cond_value =
           MV_unlift_option cond_value |> gen_custom_cc inst
        in
        update_top_1_bmstack ~f:(fun _ -> [ unlifted_cond_value ]) ss
        |> add_constraints ~c:[ MF_not cond_constraint ]
        |> add_mtz_constraint_if_it_is
             ~tv:(typ_of_val unlifted_cond_value, unlifted_cond_value)
        |> add_nat_constraint_if_it_is
             ~tv:(typ_of_val unlifted_cond_value, unlifted_cond_value)
        |> (fun ssss -> run_inst_i i2 (ctxt_sr, ssss))
     in
     se_result_pointwise_union then_br_sr else_br_sr
   | MI_pair ->
     update_top_2_bmstack
       ~f:(fun (x, y) -> [ MV_pair (x, y) |> gen_custom_cc inst ])
       ss
     |> running_ss_to_sr
   | MI_car ->
     update_top_1_bmstack ~f:(fun x -> [ MV_car x |> gen_custom_cc inst ]) ss
     |> running_ss_to_sr
   | MI_cdr ->
     update_top_1_bmstack ~f:(fun x -> [ MV_cdr x |> gen_custom_cc inst ]) ss
     |> running_ss_to_sr
   (* | MI_left t -> TODO *)
   (* | MI_right t -> TODO *)
   | MI_if_left (i1, i2) ->
     let cond_value : mich_v cc = List.hd_exn (get_bmstack ss) in
     let cond_constraint : mich_f = MF_is_left cond_value in
     (* then branch *)
     let then_br_sr : se_result =
        let unlifted_cond_value =
           MV_unlift_left cond_value |> gen_custom_cc inst
        in
        update_top_1_bmstack ~f:(fun _ -> [ unlifted_cond_value ]) ss
        |> add_constraints ~c:[ cond_constraint ]
        |> add_mtz_constraint_if_it_is
             ~tv:(typ_of_val unlifted_cond_value, unlifted_cond_value)
        |> add_nat_constraint_if_it_is
             ~tv:(typ_of_val unlifted_cond_value, unlifted_cond_value)
        |> (fun ssss -> run_inst_i i1 (ctxt_sr, ssss))
     in
     (* IMPORTANT: ctxt_sr name shadowing *)
     let ctxt_sr = ctxt_sr_update ctxt_sr then_br_sr in
     (* else branch *)
     let else_br_sr : se_result =
        let unlifted_cond_value =
           MV_unlift_right cond_value |> gen_custom_cc inst
        in
        update_top_1_bmstack ~f:(fun _ -> [ unlifted_cond_value ]) ss
        |> add_constraints ~c:[ MF_not cond_constraint ]
        |> add_mtz_constraint_if_it_is
             ~tv:(typ_of_val unlifted_cond_value, unlifted_cond_value)
        |> add_nat_constraint_if_it_is
             ~tv:(typ_of_val unlifted_cond_value, unlifted_cond_value)
        |> (fun ssss -> run_inst_i i2 (ctxt_sr, ssss))
     in
     se_result_pointwise_union then_br_sr else_br_sr
   | MI_nil t ->
     push_bmstack ~v:(MV_nil t |> gen_custom_cc inst) ss |> running_ss_to_sr
   | MI_cons ->
     update_top_2_bmstack
       ~f:(fun (x, y) -> [ MV_cons (x, y) |> gen_custom_cc inst ])
       ss
     |> running_ss_to_sr
   | MI_if_cons (i1, i2) ->
     (* IF_CONS receives list-container only *)
     let cond_value : mich_v cc = List.hd_exn (get_bmstack ss) in
     let cond_constraint : mich_f = MF_is_cons cond_value in
     (* then branch *)
     let then_br_sr : se_result =
        let unlifted_cond_value_hd = MV_hd_l cond_value |> gen_custom_cc inst in
        let unlifted_cond_value_tl = MV_tl_l cond_value |> gen_custom_cc inst in
        update_top_1_bmstack
          ~f:(fun _ -> [ unlifted_cond_value_hd; unlifted_cond_value_tl ])
          ss
        |> add_constraints ~c:[ cond_constraint ]
        |> add_mtz_constraint_if_it_is
             ~tv:(typ_of_val unlifted_cond_value_hd, unlifted_cond_value_hd)
        |> add_nat_constraint_if_it_is
             ~tv:(typ_of_val unlifted_cond_value_hd, unlifted_cond_value_hd)
        |> (fun ssss -> run_inst_i i1 (ctxt_sr, ssss))
     in
     (* IMPORTANT: ctxt_sr name shadowing *)
     let ctxt_sr = ctxt_sr_update ctxt_sr then_br_sr in
     (* else branch *)
     let else_br_sr : se_result =
        update_top_1_bmstack ~f:(fun _ -> []) ss
        |> add_constraints ~c:[ MF_not cond_constraint ]
        |> (fun ssss -> run_inst_i i2 (ctxt_sr, ssss))
     in
     se_result_pointwise_union then_br_sr else_br_sr
   (* | MI_size -> TODO *)
   (* | MI_empty_set t -> TODO *)
   (* | MI_empty_map (t1, t2) -> TODO *)
   (* | MI_empty_big_map (t1, t2) -> TODO *)
   (* | MI_map i -> TODO *)
   (* | MI_iter -> TODO *)
   | MI_mem ->
     update_top_2_bmstack
       ~f:(fun (h, h2) ->
         let nv =
            match (typ_of_val h2).cc_v with
            | MT_set _     -> MV_mem_xsb (h, h2)
            | MT_map _     -> MV_mem_xmb (h, h2)
            | MT_big_map _ -> MV_mem_xbmb (h, h2)
            | _            -> failwith "run_inst_i : MI_mem : unexpected"
         in
         [ gen_custom_cc inst nv ])
       ss
     |> running_ss_to_sr
   | MI_get ->
     update_top_2_bmstack_and_constraint
       ~f:(fun (h, h2) ->
         match (typ_of_val h2).cc_v with
         | MT_map (_, t2)     ->
           let nv = MV_get_xmoy (h, h2) |> gen_custom_cc inst in
           ( [ nv ],
             [
               mtz_constriant_if_it_is_or_true ~tv:(t2, nv);
               nat_constriant_if_it_is_or_true ~tv:(t2, nv);
             ]
           )
         | MT_big_map (_, t2) ->
           let nv = MV_get_xbmo (h, h2) |> gen_custom_cc inst in
           ( [ nv ],
             [
               mtz_constriant_if_it_is_or_true ~tv:(t2, nv);
               nat_constriant_if_it_is_or_true ~tv:(t2, nv);
             ]
           )
         | _                  -> failwith "run_inst_i : MI_get : unexpected")
       ss
     |> running_ss_to_sr
   | MI_update ->
     update_top_3_bmstack
       ~f:(fun (h, h2, h3) ->
         let nv =
            match (typ_of_val h3).cc_v with
            | MT_set _     -> MV_update_xbss (h, h2, h3)
            | MT_map _     -> MV_update_xomm (h, h2, h3)
            | MT_big_map _ -> MV_update_xobmbm (h, h2, h3)
            | _            -> failwith "run_inst_i : MI_update : unexpected"
         in
         [ gen_custom_cc inst nv ])
       ss
     |> running_ss_to_sr
   | MI_if (i1, i2) ->
     (* let cond_value : mich_v cc = List.hd_exn (get_bmstack ss) in *)
     let cond_constraint : mich_f = MF_is_true (List.hd_exn (get_bmstack ss)) in
     (* then branch *)
     let then_br_sr : se_result =
        update_top_1_bmstack ~f:(fun _ -> []) ss
        |> add_constraints ~c:[ cond_constraint ]
        |> (fun ssss -> run_inst_i i1 (ctxt_sr, ssss))
     in
     (* IMPORTANT: ctxt_sr name shadowing *)
     let ctxt_sr = ctxt_sr_update ctxt_sr then_br_sr in
     (* else branch *)
     let else_br_sr : se_result =
        update_top_1_bmstack ~f:(fun _ -> []) ss
        |> add_constraints ~c:[ MF_not cond_constraint ]
        |> (fun ssss -> run_inst_i i2 (ctxt_sr, ssss))
     in
     se_result_pointwise_union then_br_sr else_br_sr
   (* | MI_loop i -> TODO *)
   (* | MI_loop_left i -> TODO *)
   (* | MI_lambda (t1, t2, i) -> TODO *)
   (* | MI_exec -> TODO *)
   (* | MI_apply -> TODO *)
   (* | MI_dip_n (zn, i) -> TODO *)
   | MI_failwith ->
     (* 1. set block_mci
        2. enroll this sym_state to sr_blocked
     *)
     let bmci = { mci_loc = inst.cc_loc; mci_cutcat = MCC_trx_exit } in
     {
       se_result_empty with
       sr_blocked = SSet.singleton { ss with ss_block_mci = bmci };
     }
   (* | MI_cast t -> TODO *)
   | MI_rename ->
     update_top_1_bmstack ~f:(fun x -> [ { x with cc_anl = inst.cc_anl } ]) ss
     |> running_ss_to_sr
   (* | MI_concat -> TODO *)
   (* | MI_slice -> TODO *)
   (* | MI_pack -> TODO *)
   (* | MI_unpack t -> TODO *)
   | MI_add -> (
     let add_gen_sr : mich_v * mich_f list -> se_result =
       fun (mv, csl) ->
       update_top_2_bmstack_and_constraint
         ~f:(fun _ -> ([ mv |> gen_custom_cc inst ], csl))
         ss
       |> running_ss_to_sr
     in
     (* let gen_vlfl : mich_v -> mich_v cc list * mich_f list =
          (fun mv -> ([ mv |> gen_custom_cc inst ], []))
        in *)
     let (h, h2) = get_bmstack_2 ss in
     match ((typ_of_val h).cc_v, (typ_of_val h2).cc_v) with
     | (MT_nat, MT_int)       -> add_gen_sr (MV_add_nii (h, h2), [])
     | (MT_int, MT_nat)       -> add_gen_sr (MV_add_ini (h, h2), [])
     | (MT_int, MT_int)       -> add_gen_sr (MV_add_iii (h, h2), [])
     | (MT_nat, MT_nat)       ->
       add_gen_sr
         ( MV_add_nnn (h, h2),
           [ MF_nat_bound (MV_add_nnn (h, h2) |> gen_custom_cc inst) ]
         )
     | (MT_timestamp, MT_int) -> add_gen_sr (MV_add_tit (h, h2), [])
     | (MT_int, MT_timestamp) -> add_gen_sr (MV_add_itt (h, h2), [])
     | (MT_mutez, MT_mutez)   ->
       let nv = MV_add_mmm (h, h2) |> gen_custom_cc inst in
       let qstate : sym_state =
          {
            ss with
            ss_block_mci =
              {
                mci_loc = inst.cc_loc;
                mci_cutcat = MCC_query Q_mutez_add_no_overflow;
              };
          }
       in
       let rstate : sym_state =
          update_top_2_bmstack_and_constraint
            ~f:(fun _ -> ([ nv ], [ MF_mutez_bound nv ]))
            ss
       in
       { (running_ss_to_sr rstate) with sr_queries = SSet.singleton qstate }
     | _                      -> failwith "run_inst_i : MI_add : unexpected"
   )
   | _ ->
     failwith
       ("run_inst_i : wildcard match triggered : "
       ^ (sexp_of_mich_i inst.cc_v |> string_of_sexp)
       )
(* function run_inst_i end *)

let run_inst_entry :
    Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc -> se_result =
  (fun (pt, st, c) -> run_inst c (run_inst_initial_se_result (pt, st, c)))
