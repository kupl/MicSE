open! Core
open Tz
open TzUtil

let trx_image_equality_fmla :
    mich_sym_ctxt -> mich_sym_ctxt -> trx_image -> trx_image -> mich_f =
  fun ctxt1 ctxt2 t1 t2 ->
  let eqf x y =
     MF_eq ({ ctx_i = ctxt1; ctx_v = x }, { ctx_i = ctxt2; ctx_v = y })
  in
  MF_and
    [
      eqf t1.ti_contract t2.ti_contract;
      eqf t1.ti_source t2.ti_source;
      eqf t1.ti_sender t2.ti_sender;
      eqf t1.ti_param t2.ti_param;
      eqf t1.ti_amount t2.ti_amount;
      eqf t1.ti_time t2.ti_time;
    ]

let stack_equality_fmlas :
    mich_sym_ctxt * mich_sym_ctxt ->
    mich_cut_category * mich_cut_category ->
    sym_image * sym_image ->
    mich_f list =
  fun (ctxt1, ctxt2) (mcc_1, mcc_2) (si1, si2) ->
  let eqf x y =
     MF_eq ({ ctx_i = ctxt1; ctx_v = x }, { ctx_i = ctxt2; ctx_v = y })
  in
  let eqf_cc c1 c2 x y =
     MF_eq ({ ctx_i = c1; ctx_v = x }, { ctx_i = c2; ctx_v = y })
  in
  (* let _ =
        Utils.Log.debug (fun m ->
            m "mcc-1 = %s , mcc-2 = %s"
              (Tz.sexp_of_mich_cut_category mcc_1 |> string_of_sexp)
              (Tz.sexp_of_mich_cut_category mcc_2 |> string_of_sexp)
        )
     in *)
  match (mcc_1, mcc_2) with
  (* TRX *)
  | (MCC_trx_exit, MCC_trx_entry) ->
    (* 1. michelson stack - storage equality *)
    (* NOTE : This CDR should be optimized, maybe? *)
    let ms_c =
       eqf
         (MV_cdr (List.hd_exn si1.si_mich) |> gen_dummy_cc)
         (MV_cdr (List.hd_exn si2.si_mich) |> gen_dummy_cc)
    (* 2. dip, map, iter stacks should be emptied - No Constraints *)
    (* 3. balance equality *)
    and b_c = eqf si1.si_balance si2.si_balance
    (* 4. bc_balance equality *)
    and bcb_c = eqf si1.si_bc_balance si2.si_bc_balance
    (* 5. time inequality *)
    and time_c =
       MF_time_leq
         ( { ctx_i = ctxt1; ctx_v = si1.si_param.ti_time },
           { ctx_i = ctxt2; ctx_v = si2.si_param.ti_time }
         )
    in
    (* 6. contract equality *)
    let ctrt_c = eqf si1.si_param.ti_contract si2.si_param.ti_contract in
    [ ms_c; b_c; bcb_c; time_c; ctrt_c ]
  (* LOOP *)
  | (MCC_ln_loop, MCC_ln_loop)
  | (MCC_ln_loop, MCC_lb_loop)
  | (MCC_lb_loop, MCC_ln_loop)
  | (MCC_lb_loop, MCC_lb_loop) ->
    (* NOTE: question here : is (AND []) valid formula ? *)
    let ms_c =
       MF_and (List.map2_exn (List.tl_exn si1.si_mich) si2.si_mich ~f:eqf)
    and ds_c = MF_and (List.map2_exn si1.si_dip si2.si_dip ~f:eqf)
    and maps_entry_c =
       MF_and (List.map2_exn si1.si_map_entry si2.si_map_entry ~f:eqf)
    and maps_exit_c =
       MF_and (List.map2_exn si1.si_map_exit si2.si_map_exit ~f:eqf)
    and is_c = MF_and (List.map2_exn si1.si_iter si2.si_iter ~f:eqf)
    and b_c = eqf si1.si_balance si2.si_balance
    and bcb_c = eqf si1.si_bc_balance si2.si_bc_balance
    and ti_c = trx_image_equality_fmla ctxt1 ctxt2 si1.si_param si2.si_param in
    [ ms_c; ds_c; maps_entry_c; maps_exit_c; is_c; b_c; bcb_c; ti_c ]
  (* LOOP-LEFT *)
  | (MCC_ln_loopleft, MCC_ln_loopleft)
  | (MCC_ln_loopleft, MCC_lb_loopleft)
  | (MCC_lb_loopleft, MCC_ln_loopleft)
  | (MCC_lb_loopleft, MCC_lb_loopleft) ->
    (* 1. common parts
       - mich-stack tail, dip-stack, map-entry/exit/mapkey-stack,
          iter-stack, balance, bc-balance, trx-images
    *)
    let ms_c_common =
       MF_and
         (List.map2_exn (List.tl_exn si1.si_mich) (List.tl_exn si2.si_mich)
            ~f:eqf
         )
    and ds_c = MF_and (List.map2_exn si1.si_dip si2.si_dip ~f:eqf)
    and map_entry_sc =
       MF_and (List.map2_exn si1.si_map_entry si2.si_map_entry ~f:eqf)
    and map_exit_sc =
       MF_and (List.map2_exn si1.si_map_exit si2.si_map_exit ~f:eqf)
    and map_mkey_sc =
       MF_and (List.map2_exn si1.si_map_mapkey si2.si_map_mapkey ~f:eqf)
    and is_c = MF_and (List.map2_exn si1.si_iter si2.si_iter ~f:eqf)
    and b_c = eqf si1.si_balance si2.si_balance
    and bcb_c = eqf si1.si_bc_balance si2.si_bc_balance
    and ti_c = trx_image_equality_fmla ctxt1 ctxt2 si1.si_param si2.si_param in
    (* 2. Left/Right specific constraints *)
    let lr_c =
       let (h1, h2) = (List.hd_exn si1.si_mich, List.hd_exn si2.si_mich) in
       let (left_typ, right_typ) =
          match (typ_of_val h1).cc_v with
          | MT_or (t1, t2) -> (t1, t2)
          | _              ->
            failwith
              "MState : stack_equality_fmlas : LOOP_LEFT : 1 : unexpected"
       in
       match mcc_2 with
       | MCC_ln_loopleft ->
         (* Right *)
         eqf h1 (MV_right (left_typ, h2) |> gen_dummy_cc)
       | MCC_lb_loopleft ->
         (* Left *) eqf h1 (MV_left (right_typ, h2) |> gen_dummy_cc)
       | _               ->
         failwith "MState : stack_equality_fmlas : LOOP_LEFT : 2 : unexpected"
    in
    [
      ms_c_common;
      ds_c;
      map_entry_sc;
      map_exit_sc;
      map_mkey_sc;
      is_c;
      b_c;
      bcb_c;
      ti_c;
      lr_c;
    ]
  (* ITER *)
  (*
    [ln-ln]
      mich1-tail = mich2
      iter1 = iter2
      mich1-head = empty-container
    [ln-lb]
      mich1-tail = mich2-tail
      iter1 = iter2-tail
      mich1-head = mich2-head + iter2-head
      (set,map : iter2-head has no mich2-head key)
    [lb-ln]
      mich1 = mich2
      iter1-tail = iter2
      iter1-head = empty-container
    [lb-lb]
      mich1 = mich2-tail
      iter1-tail = iter2-tail
      iter1-head = mich2-head + iter2-head
      (set,map : iter2-head has no mich2-head key)
  *)
  | (MCC_ln_iter, MCC_ln_iter)
  | (MCC_ln_iter, MCC_lb_iter)
  | (MCC_lb_iter, MCC_ln_iter)
  | (MCC_lb_iter, MCC_lb_iter) ->
    (* 1. common parts
       - dip-stack, map-entry/exit/mapkey-stack, balance, bc-balance, trx-images
    *)
    let ds_c = MF_and (List.map2_exn si1.si_dip si2.si_dip ~f:eqf)
    and map_entry_sc =
       MF_and (List.map2_exn si1.si_map_entry si2.si_map_entry ~f:eqf)
    and map_exit_sc =
       MF_and (List.map2_exn si1.si_map_exit si2.si_map_exit ~f:eqf)
    and map_mkey_sc =
       MF_and (List.map2_exn si1.si_map_mapkey si2.si_map_mapkey ~f:eqf)
    and b_c = eqf si1.si_balance si2.si_balance
    and bcb_c = eqf si1.si_bc_balance si2.si_bc_balance
    and ti_c = trx_image_equality_fmla ctxt1 ctxt2 si1.si_param si2.si_param in
    (* 2. non-common parts - mich-stack & iter-stack *)
    let (ms_c_common, is_c_common, iter_specific_c) : mich_f * mich_f * mich_f =
       match (mcc_1, mcc_2) with
       | (MCC_ln_iter, MCC_ln_iter) ->
         let (m1h, m1t, m2) =
            (List.hd_exn si1.si_mich, List.tl_exn si1.si_mich, si2.si_mich)
         in
         let empty_container =
            match (typ_of_val m1h).cc_v with
            | MT_list t       -> MV_nil t |> gen_dummy_cc
            | MT_set t        -> MV_empty_set t |> gen_dummy_cc
            | MT_map (t1, t2) -> MV_empty_map (t1, t2) |> gen_dummy_cc
            | _               ->
              failwith "MState : stack_equality_fmlas : ITER : 2 : unexpected"
         in
         ( MF_and (List.map2_exn m1t m2 ~f:eqf),
           MF_and (List.map2_exn si1.si_iter si2.si_iter ~f:eqf),
           eqf_cc ctxt1 ctxt1 m1h empty_container
         )
       | (MCC_ln_iter, MCC_lb_iter) -> (
         let (m1h, m1t, m2h, m2t) =
            ( List.hd_exn si1.si_mich,
              List.tl_exn si1.si_mich,
              List.hd_exn si2.si_mich,
              List.tl_exn si2.si_mich
            )
         in
         let (i1, i2h, i2t) =
            (si1.si_iter, List.hd_exn si2.si_iter, List.tl_exn si2.si_iter)
         in
         ( MF_and (List.map2_exn m1t m2t ~f:eqf),
           MF_and (List.map2_exn i1 i2t ~f:eqf),
           match (typ_of_val m1h).cc_v with
           | MT_list _ -> eqf m1h (MV_cons (m2h, i2h) |> gen_dummy_cc)
           | MT_set _  ->
             MF_and
               [
                 eqf m1h
                   (MV_update_xbss (m2h, MV_lit_bool true |> gen_dummy_cc, i2h)
                   |> gen_dummy_cc
                   );
                 MF_not
                   (MF_is_true
                      {
                        ctx_i = ctxt2;
                        ctx_v = MV_mem_xsb (m2h, i2h) |> gen_dummy_cc;
                      }
                   );
               ]
           | MT_map _  ->
             let (m2_kv, m2_ev) =
                (MV_car m2h |> gen_dummy_cc, MV_cdr m2h |> gen_dummy_cc)
             in
             MF_and
               [
                 eqf m1h
                   (MV_update_xomm (m2_kv, MV_some m2_ev |> gen_dummy_cc, i2h)
                   |> gen_dummy_cc
                   );
                 MF_not
                   (MF_is_true
                      {
                        ctx_i = ctxt2;
                        ctx_v = MV_mem_xmb (m2h, i2h) |> gen_dummy_cc;
                      }
                   );
               ]
           | _         ->
             failwith "MState : stack_equality_fmlas : ITER : 3 : unexpected"
         )
       )
       | (MCC_lb_iter, MCC_ln_iter) ->
         let (i1h, i1t, i2) =
            (List.hd_exn si1.si_iter, List.tl_exn si1.si_iter, si2.si_iter)
         in
         let empty_container =
            match (typ_of_val i1h).cc_v with
            | MT_list t       -> MV_nil t |> gen_dummy_cc
            | MT_set t        -> MV_empty_set t |> gen_dummy_cc
            | MT_map (t1, t2) -> MV_empty_map (t1, t2) |> gen_dummy_cc
            | _               ->
              failwith "MState : stack_equality_fmlas : ITER : 4 : unexpected"
         in
         ( MF_and (List.map2_exn si1.si_mich si2.si_mich ~f:eqf),
           MF_and (List.map2_exn i1t i2 ~f:eqf),
           eqf_cc ctxt1 ctxt1 i1h empty_container
         )
       | (MCC_lb_iter, MCC_lb_iter) -> (
         let (m1, m2h, m2t) =
            (si1.si_mich, List.hd_exn si2.si_mich, List.tl_exn si2.si_mich)
         in
         let (i1h, i1t, i2h, i2t) =
            ( List.hd_exn si1.si_iter,
              List.tl_exn si1.si_iter,
              List.hd_exn si2.si_iter,
              List.tl_exn si2.si_iter
            )
         in
         ( MF_and (List.map2_exn m1 m2t ~f:eqf),
           MF_and (List.map2_exn i1t i2t ~f:eqf),
           match (typ_of_val m2h).cc_v with
           | MT_list _ -> eqf i1h (MV_cons (m2h, i2h) |> gen_dummy_cc)
           | MT_set _  ->
             MF_and
               [
                 eqf i1h
                   (MV_update_xbss (m2h, MV_lit_bool true |> gen_dummy_cc, i2h)
                   |> gen_dummy_cc
                   );
                 MF_not
                   (MF_is_true
                      {
                        ctx_i = ctxt2;
                        ctx_v = MV_mem_xsb (m2h, i2h) |> gen_dummy_cc;
                      }
                   );
               ]
           | MT_map _  ->
             let (m2_kv, m2_ev) =
                (MV_car m2h |> gen_dummy_cc, MV_cdr m2h |> gen_dummy_cc)
             in
             MF_and
               [
                 eqf i1h
                   (MV_update_xomm (m2_kv, MV_some m2_ev |> gen_dummy_cc, i2h)
                   |> gen_dummy_cc
                   );
                 MF_not
                   (MF_is_true
                      {
                        ctx_i = ctxt2;
                        ctx_v = MV_mem_xmb (m2h, i2h) |> gen_dummy_cc;
                      }
                   );
               ]
           | _         ->
             failwith "MState : stack_equality_fmlas : ITER : 5 : unexpected"
         )
       )
       | _                          ->
         failwith "MState : stack_equality_fmlas : ITER : 1 : unexpected"
    in
    [
      ds_c;
      map_entry_sc;
      map_exit_sc;
      map_mkey_sc;
      b_c;
      bcb_c;
      ti_c;
      ms_c_common;
      is_c_common;
      iter_specific_c;
    ]
  (* MAP *)
  (* TODO : check or rewrite existing code using constriaints below *)
  (*
    [ln-ln]
      mich1-tail = mich2-tail
      entry1 = entry2
      exit1 = exit2
      key1 = key2
      mich1-head = empty-container (t1)
      mich2-head = empty-container (t2)
    [ln-lb]
      mich1-tail = mich2-tail
      entry1 = entry2-tail
      exit1 = exit2-tail
      (list : key1 = key2)
      (map : key1 = key2-tail)
      mich1-head = mich2-head + entry2-head
      (map : entry2-head has no mich2-head key)
      exit2-head = empty-container
      (map : key2-head = mich2-head key)
    [lb-ln]
      mich1-tail = mich2-tail
      entry1-tail = entry2
      exit1-tail = exit2
      (list : key1 = key2)
      (map : key1-tail = key2)
      (list : mich2-head = mich1-head + exit1-head)
      (map : mich2-head = (key1-head, mich1-head) + exit1-head)
      (map : entry1-head = empty-container (t1))
    [lb-lb]
      mich1-tail = mich2-tail
      entry1-tail = entry2-tail
      exit1-tail = exit2-tail
      (list : key1 = key2)
      (map : key1-tail = key2-tail)
      (list : exit2-head = mich1-head + exit1-head)
      (map : exit2-head = (key1-head, mich1-head) + exit1-head)
      (map : exit1-head has no key1-head)
      (list : entry1-head = mich2-head + entry2-head)
      (map : entry1-head = (key2-head, mich2-head) + entry2-head)
      (map : entry2-head has no key2-head)
  *)
  | (MCC_ln_map, MCC_ln_map)
  | (MCC_ln_map, MCC_lb_map)
  | (MCC_lb_map, MCC_ln_map)
  | (MCC_lb_map, MCC_lb_map) ->
    (* 1. common parts *)
    (* 1.1. mich-stack tail, dip-stack, iter-stack, balance, bc-balance, trx-image *)
    let ms_c_common =
       MF_and
         (List.map2_exn (List.tl_exn si1.si_mich) (List.tl_exn si2.si_mich)
            ~f:eqf
         )
    and ds_c = MF_and (List.map2_exn si1.si_dip si2.si_dip ~f:eqf)
    and is_c = MF_and (List.map2_exn si1.si_iter si2.si_iter ~f:eqf)
    and b_c = eqf si1.si_balance si2.si_balance
    and bcb_c = eqf si1.si_bc_balance si2.si_bc_balance
    and ti_c = trx_image_equality_fmla ctxt1 ctxt2 si1.si_param si2.si_param in
    (* 1.2. map-stack common parts *)
    (* 1.2.1. construct map-stack entry/exit tails *)
    let ((m1_entry_h, m1_entry_t), (m1_exit_h, m1_exit_t)) =
       match mcc_1 with
       | MCC_ln_map -> (([], si1.si_map_entry), ([], si1.si_map_exit))
       | MCC_lb_map ->
         (List.split_n si1.si_map_entry 1, List.split_n si1.si_map_exit 1)
       | _          ->
         failwith "MState : stack_equality_fmlas : MAP : 1 : unexpected"
    and ((m2_entry_h, m2_entry_t), (m2_exit_h, m2_exit_t)) =
       match mcc_2 with
       | MCC_ln_map -> (([], si2.si_map_entry), ([], si2.si_map_exit))
       | MCC_lb_map ->
         (List.split_n si2.si_map_entry 1, List.split_n si2.si_map_exit 1)
       | _          ->
         failwith "MState : stack_equality_fmlas : MAP : 2 : unexpected"
    in
    (* 1.2.2. map-stack tail constraints *)
    let maps_entry_c_common = MF_and (List.map2_exn m1_entry_t m2_entry_t ~f:eqf)
    and maps_exit_c_common =
       MF_and (List.map2_exn m1_exit_t m2_exit_t ~f:eqf)
    in
    (* 1.2.3. construct mapkey-stack tails *)
    let (s1_mshd, s2_mshd) =
       (List.hd_exn si1.si_mich, List.hd_exn si2.si_mich)
    in
    let ((mk1_key_h, mk1_key_t), (mk2_key_h, mk2_key_t)) =
       match
         (mcc_1, mcc_2, (typ_of_val s1_mshd).cc_v, (typ_of_val s2_mshd).cc_v)
       with
       | (MCC_ln_map, MCC_ln_map, MT_map _, _) ->
         (List.split_n si1.si_map_mapkey 0, List.split_n si2.si_map_mapkey 0)
       | (MCC_ln_map, MCC_lb_map, MT_map _, _) ->
         (List.split_n si1.si_map_mapkey 0, List.split_n si2.si_map_mapkey 1)
       | (MCC_lb_map, MCC_ln_map, _, MT_map _) ->
         (List.split_n si1.si_map_mapkey 1, List.split_n si2.si_map_mapkey 0)
       | (MCC_lb_map, MCC_lb_map, _, _) -> (
         (* WARNING : Tricky case *)
         match (typ_of_val (List.hd_exn m1_entry_h)).cc_v with
         | MT_map _ ->
           (List.split_n si1.si_map_mapkey 1, List.split_n si2.si_map_mapkey 1)
         | _        ->
           (List.split_n si1.si_map_mapkey 0, List.split_n si2.si_map_mapkey 0)
       )
       | _ ->
         (List.split_n si1.si_map_mapkey 0, List.split_n si2.si_map_mapkey 0)
    in
    (* 1.2.4. mapkey-stack tail constraints *)
    let mapkeys_c_common = MF_and (List.map2_exn mk1_key_t mk2_key_t ~f:eqf) in
    (* 2. MAP-specific constraints *)
    let (s1_mshd_t, s2_mshd_t) = (typ_of_val s1_mshd, typ_of_val s2_mshd) in
    let maps_specific_c =
       match (mcc_1, mcc_2) with
       | (MCC_ln_map, MCC_ln_map) -> (
         (* 1. does not enter MAP-loop
            - both entry-exit containers in mich-stack are empty container
            - there are no map-entry, map-exit container exists
         *)
         match (s1_mshd_t.cc_v, s2_mshd_t.cc_v) with
         | (MT_list t1, MT_list t2) ->
           MF_and
             [
               eqf_cc ctxt1 ctxt1 s1_mshd (MV_nil t1 |> gen_dummy_cc);
               eqf_cc ctxt2 ctxt2 s2_mshd (MV_nil t2 |> gen_dummy_cc);
             ]
         | (MT_map (t1k, t1e), MT_map (t2k, t2e)) ->
           MF_and
             [
               eqf_cc ctxt1 ctxt1 s1_mshd
                 (MV_empty_map (t1k, t1e) |> gen_dummy_cc);
               eqf_cc ctxt2 ctxt2 s2_mshd
                 (MV_empty_map (t2k, t2e) |> gen_dummy_cc);
             ]
         | _ -> failwith "MState : stack_equality_fmlas : MAP : 4 : unexpected"
       )
       | (MCC_ln_map, MCC_lb_map) -> (
         (* 2. first-enter the MAP-loop
            - (entry container in ss1-mich-stack) = (ss2-mich-head + ss2-map-entry-head)
            - map-exit stack in ss2 contains empty-container
            --- Following two constraints are encoded in Se module.
            - If container is map-type, then (ss2-map-key-head = CAR (ss2-mich-head))
            - ss2-map-key-head is not a key of ss2-map-entry-head
         *)
         let m2_entry_hd = List.hd_exn m2_entry_h
         and m2_exit_hd = List.hd_exn m2_exit_h in
         match (s1_mshd_t.cc_v, s2_mshd_t.cc_v) with
         | (MT_list t1, t2) when equal_mich_t t1.cc_v t2 ->
           MF_and
             [
               eqf s1_mshd (MV_cons (s2_mshd, m2_entry_hd) |> gen_dummy_cc);
               eqf_cc ctxt2 ctxt2 m2_exit_hd
                 (MV_nil (typ_of_val m2_exit_hd) |> gen_dummy_cc);
             ]
         | (MT_map (t1k, t1e), MT_pair (t2k, t2e))
           when equal_mich_t t1k.cc_v t2k.cc_v && equal_mich_t t1e.cc_v t2e.cc_v
           ->
           let (k, v) =
              match s2_mshd.cc_v with
              | MV_pair (v1, v2) -> (v1, v2)
              | _                ->
                failwith "MState : stack_equality_fmlas : MAP : 6 : unexpected"
           in
           let (exit_tk, exit_te) =
              match (typ_of_val m2_exit_hd).cc_v with
              | MT_map (t1, t2) -> (t1, t2)
              | _               ->
                failwith "MState : stack_equality_fmlas : MAP : 7 : unexpected"
           in
           MF_and
             [
               eqf s1_mshd
                 (MV_update_xomm (k, gen_dummy_cc (MV_some v), m2_entry_hd)
                 |> gen_dummy_cc
                 );
               eqf_cc ctxt2 ctxt2 m2_exit_hd
                 (MV_empty_map (exit_tk, exit_te) |> gen_dummy_cc);
             ]
         | _ -> failwith "MState : stack_equality_fmlas : MAP : 5 : unexpected"
       )
       | (MCC_lb_map, MCC_ln_map) -> (
         (* 3. last-exit the MAP-loop
            - (ss1-mich-head + ss1-map-exit-head) = (exit container in ss2-mich-stack)
            - map-entry stack in ss1 contains empty-container
         *)
         let m1_entry_hd = List.hd_exn m1_entry_h
         and m1_exit_hd = List.hd_exn m1_exit_h in
         match (s1_mshd_t.cc_v, s2_mshd_t.cc_v) with
         | (t1, MT_list t2) when equal_mich_t t1 t2.cc_v ->
           MF_and
             [
               eqf_cc ctxt2 ctxt1 s2_mshd
                 (MV_cons (s1_mshd, m1_exit_hd) |> gen_dummy_cc);
               eqf_cc ctxt1 ctxt1 m1_entry_hd
                 (MV_nil (typ_of_val m1_entry_hd) |> gen_dummy_cc);
             ]
         | (t1e, MT_map (_, t2e)) when equal_mich_t t1e t2e.cc_v ->
           let (entry_tk, entry_te) =
              match (typ_of_val m1_entry_hd).cc_v with
              | MT_map (t1, t2) -> (t1, t2)
              | _               ->
                failwith "MState : stack_equality_fmlas : MAP : 9 : unexpected"
           in
           MF_and
             [
               eqf
                 (MV_update_xomm
                    ( List.hd_exn mk1_key_h,
                      MV_some s1_mshd |> gen_dummy_cc,
                      m1_exit_hd
                    )
                 |> gen_dummy_cc
                 )
                 s2_mshd;
               eqf_cc ctxt1 ctxt1 m1_entry_hd
                 (MV_empty_map (entry_tk, entry_te) |> gen_dummy_cc);
             ]
         | _ -> failwith "MState : stack_equality_fmlas : MAP : 8 : unexpected"
       )
       | (MCC_lb_map, MCC_lb_map) -> (
         (* 4. MAP-loop to MAP-loop
            - (ss1-map-entry-head) = (ss2-mich-head + ss2-map-entry-head)
            - (ss1-mich-head + ss1-map-exit-head) = (ss2-map-exit-head)
         *)
         let m1_entry_hd = List.hd_exn m1_entry_h
         and m1_exit_hd = List.hd_exn m1_exit_h
         and m2_entry_hd = List.hd_exn m2_entry_h
         and m2_exit_hd = List.hd_exn m2_exit_h in
         match
           ((typ_of_val m1_entry_hd).cc_v, (typ_of_val m1_exit_hd).cc_v)
         with
         | (MT_list _, _) ->
           MF_and
             [
               eqf m1_entry_hd (MV_cons (s2_mshd, m2_entry_hd) |> gen_dummy_cc);
               eqf (MV_cons (s1_mshd, m1_exit_hd) |> gen_dummy_cc) m2_exit_hd;
             ]
         | (MT_map _, _)  ->
           let m1_kv = List.hd_exn mk1_key_h
           and m2_kv = List.hd_exn mk2_key_h
           and m2_vv = MV_cdr s2_mshd |> gen_dummy_cc in
           MF_and
             [
               eqf m1_entry_hd
                 (MV_update_xomm
                    (m2_kv, MV_some m2_vv |> gen_dummy_cc, m2_entry_hd)
                 |> gen_dummy_cc
                 );
               eqf
                 (MV_update_xomm
                    (m1_kv, MV_some s1_mshd |> gen_dummy_cc, m1_entry_hd)
                 |> gen_dummy_cc
                 )
                 m2_exit_hd;
             ]
         | _              ->
           failwith "MState : stack_equality_fmlas : MAP : 10 : unexpected"
       )
       | _                        ->
         failwith "MState : stack_equality_fmlas : MAP : 3 : unexpected"
    in
    [
      ms_c_common;
      ds_c;
      is_c;
      b_c;
      bcb_c;
      ti_c;
      maps_entry_c_common;
      maps_exit_c_common;
      mapkeys_c_common;
      maps_specific_c;
    ]
  | _ -> failwith "MState : stack_equality_fmlas : 1 : unexpected"
(* function stack_equality_fmlas end *)

type t = (sym_state * mich_f list) list [@@deriving sexp, compare, equal]

type summary = {
  sm_rmci : Tz.r_mich_cut_info;
  sm_s_id : Tz.sym_state_id;
}
[@@deriving sexp, compare, equal]

module SMY_cmp = struct
  type t = summary [@@deriving sexp, compare]
end

let init ss : t = [ (ss, []) ]

let cons ss ms : t =
   let fst_ms_ss = List.hd_exn ms |> fst in
   let new_ctxt = ss.ss_id @ fst_ms_ss.ss_id in
   (* let _ =
         Utils.Log.debug (fun m -> m "DBG - MState.cons - renamed_ss before")
      in *)
   let renamed_ss = sym_state_symbol_context_swap ~ctx:new_ctxt ss in
   (* let _ =
         Utils.Log.debug (fun m -> m "DBG - MState.cons - renamed_ss after")
      in *)
   (* let _ =
         Utils.Log.debug (fun m -> m "DBG - MState.cons - connection_fmla before")
      in *)
   let connection_fmla =
      stack_equality_fmlas
        (renamed_ss.ss_id, fst_ms_ss.ss_id)
        (renamed_ss.ss_block_mci.mci_cutcat, fst_ms_ss.ss_start_mci.mci_cutcat)
        (renamed_ss.ss_block_si, fst_ms_ss.ss_start_si)
   in
   (* let _ =
         Utils.Log.debug (fun m -> m "DBG - MState.cons - connection_fmla after")
      in *)
   (renamed_ss, connection_fmla) :: ms

let get_constraint : t -> mich_f list =
  fun ms ->
  List.concat (List.map ~f:(fun (ss, fl) -> ss.ss_constraints @ fl) ms)

let get_first_ss : t -> sym_state = (fun ms -> List.hd_exn ms |> fst)

let get_last_ss : t -> sym_state = (fun ms -> List.last_exn ms |> fst)

let get_tail_ms : t -> t = (fun ms -> List.tl_exn ms)

let cut_first_found_loop : t -> t option =
   let proper_mcc : mich_cut_category -> bool = function
   | MCC_trx_entry
   | MCC_lb_loop
   | MCC_lb_loopleft
   | MCC_lb_map
   | MCC_lb_iter ->
     true
   | _ -> false
   in
   fun ms ->
   let fss = get_first_ss ms in
   (* 1. check if the start-mcc is proper category *)
   if not (proper_mcc fss.ss_start_mci.mci_cutcat)
   then None
   else (
     let start_rmci = get_reduced_mci fss.ss_start_mci in
     (* 2. find corresponding block-mci *)
     List.find ms ~f:(fun (ss, _) ->
         equal_r_mich_cut_info (get_reduced_mci ss.ss_block_mci) start_rmci
     )
     |> Option.map ~f:(fun (ss, _) -> ss.ss_block_mci)
     (* 3. if exists, then cut *)
     |> Option.map ~f:(fun found_mci ->
            List.split_while ms ~f:(fun (ss, _) ->
                not (equal_mich_cut_info ss.ss_block_mci found_mci)
            )
            |> fun (hd, tl) ->
            match tl with
            | []           ->
              failwith "MState : cut_first_found_loop : unexpected"
            | (ss, _) :: _ -> hd @ [ (ss, []) ]
        )
   )

let cut_after_first_loop : t -> t =
  fun ms ->
  let (cms_opt : t option) = cut_first_found_loop ms in
  if Option.is_none cms_opt
  then ms
  else (
    let (cms : t) = Option.value_exn cms_opt in
    let (rms : t) = List.drop ms (List.length cms) in
    rms
  )
(* function cut_after_first_loop end *)

let get_length : t -> int = List.length

let get_summary : t -> summary =
  fun ms ->
  let fss = get_first_ss ms in
  { sm_rmci = TzUtil.get_reduced_mci fss.ss_start_mci; sm_s_id = fss.ss_id }
