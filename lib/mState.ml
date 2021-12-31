(* MState : State merge *)

open! Core

exception MStateError of string

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(* Map of Tz.r_mich_cut_info *)
module RMCIMap = Map.Make (Tz.RMichCutInfo_cmp)

(* Map of Tz.qid *)
module QIDMap = Map.Make (Tz.QId_cmp)

(* Set of Tz.sym_state *)
module SSet = Set.Make (Tz.SymState_cmp)

(* Set of Igdt.igdt *)
module ISet = Set.Make (Igdt.IGDT_cmp)

(******************************************************************************)
(******************************************************************************)
(* Constraint from Merging                                                    *)
(******************************************************************************)
(******************************************************************************)

let eq_fmla :
    Tz.mich_sym_ctxt * Tz.mich_v Tz.cc ->
    Tz.mich_sym_ctxt * Tz.mich_v Tz.cc ->
    Tz.mich_f list =
   let open Tz in
   let open TzUtil in
   let open Igdt in
   let collect_sigma_from_mich_v : Tz.mich_v Tz.cc -> ISet.t =
     fun value_cc ->
     collect_igdt_from_igdt (gen_custom_igdt value_cc)
     |> ISet.filter ~f:(fun igdt -> is_sigma igdt.ig_value)
     (* inner-function collect_sigma_from_mich_v end *)
   in
   fun (ctxt1, v1) (ctxt2, v2) ->
   (* 1. Exactly equality *)
   let (exact_eq : mich_f) =
      MF_eq (gen_mich_v_ctx ~ctx:ctxt1 v1, gen_mich_v_ctx ~ctx:ctxt2 v2)
   in
   (* 2. Additional equality of sigma which in value *)
   let (set_of_sigma : ISet.t) = collect_sigma_from_mich_v v2 in
   let (sigma_eq : mich_f list) =
      let (mapf : mich_v -> mich_v) =
        (fun value -> if equal_mich_v value v2.cc_v then v1.cc_v else value)
      in
      ISet.to_list set_of_sigma
      |> List.map
           ~f:(fun { ig_value = sigma_v2; ig_precond_lst = sigma_prec2_raw; _ }
              ->
             let (sigma_prec1 : mich_f list) =
                List.map sigma_prec2_raw
                  ~f:
                    (mvcc_subst_mf ~mapf:(fun { ctx_v; _ } ->
                         ctx_v
                         |> mvcc_map_innerfst ~mapf
                         |> gen_mich_v_ctx ~ctx:ctxt1
                     )
                    )
             in
             let (sigma_prec2 : mich_f list) =
                List.map sigma_prec2_raw
                  ~f:
                    (mvcc_subst_mf ~mapf:(fun { ctx_v; _ } ->
                         ctx_v |> gen_mich_v_ctx ~ctx:ctxt2
                     )
                    )
             in
             let (sigma_v1 : mich_v cc) = mvcc_map_innerfst ~mapf sigma_v2 in
             let (sigma_v1_ctx : mich_v_cc_ctx) =
                gen_mich_v_ctx ~ctx:ctxt1 sigma_v1
             in
             let (sigma_v2_ctx : mich_v_cc_ctx) =
                gen_mich_v_ctx ~ctx:ctxt2 sigma_v2
             in
             [
               MF_mutez_bound sigma_v1_ctx;
               MF_mutez_bound sigma_v2_ctx;
               MF_imply
                 ( MF_and (sigma_prec1 @ sigma_prec2),
                   MF_eq (sigma_v1_ctx, sigma_v2_ctx)
                 );
             ]
         )
      |> List.join
   in
   exact_eq :: sigma_eq
(* function eq_fmla end *)

let trx_image_equality_fmla :
    Tz.mich_sym_ctxt ->
    Tz.mich_sym_ctxt ->
    Tz.trx_image ->
    Tz.trx_image ->
    Tz.mich_f list =
   let open Tz in
   fun ctxt1 ctxt2 t1 t2 ->
   let eqf x y = eq_fmla (ctxt1, x) (ctxt2, y) in
   (* syntax sugar *)
   eqf t1.ti_contract t2.ti_contract
   @ eqf t1.ti_source t2.ti_source
   @ eqf t1.ti_sender t2.ti_sender
   @ eqf t1.ti_param t2.ti_param
   @ eqf t1.ti_amount t2.ti_amount
   @ eqf t1.ti_time t2.ti_time

let stack_equality_fmlas :
    Tz.mich_sym_ctxt * Tz.mich_sym_ctxt ->
    Tz.mich_cut_category * Tz.mich_cut_category ->
    Tz.sym_image * Tz.sym_image ->
    Tz.mich_f list =
   let open Tz in
   let open TzUtil in
   fun (ctxt1, ctxt2) (mcc_1, mcc_2) (si1, si2) ->
   let eqf x y = eq_fmla (ctxt1, x) (ctxt2, y) in
   let eqf_cc c1 c2 x y = eq_fmla (c1, x) (c2, y) in
   (* syntax sugar *)
   (* let _ =
         Utils.Log.debug (fun m ->
             m "mcc-1 = %s , mcc-2 = %s"
               (Tz.sexp_of_mich_cut_category mcc_1 |> string_of_sexp)
               (Tz.sexp_of_mich_cut_category mcc_2 |> string_of_sexp)
         )
      in *)
   try
     match (mcc_1, mcc_2) with
     (* TRX *)
     | (MCC_trx_exit, MCC_trx_entry) ->
       (* 1. michelson stack - storage equality *)
       (* NOTE : This CDR should be optimized, maybe? *)
       let (ms_c : mich_f list) =
          eqf
            (MV_cdr (List.hd_exn si1.si_mich) |> gen_dummy_cc)
            (MV_cdr (List.hd_exn si2.si_mich) |> gen_dummy_cc)
       (* 2. dip, map, iter stacks should be emptied - No Constraints *)
       (* 3. balance equality *)
       and (b_c : mich_f list) = eqf si1.si_balance si2.si_balance
       (* 4. bc_balance equality *)
       and (bcb_c : mich_f list) = eqf si1.si_bc_balance si2.si_bc_balance
       (* 5. time inequality *)
       and (time_c : mich_f) =
          MF_time_leq
            ( gen_mich_v_ctx ~ctx:ctxt1 si1.si_param.ti_time,
              gen_mich_v_ctx ~ctx:ctxt2 si2.si_param.ti_time
            )
       in
       (* 6. contract equality *)
       let (ctrt_c : mich_f list) =
          eqf si1.si_param.ti_contract si2.si_param.ti_contract
       in
       (time_c :: ms_c) @ b_c @ bcb_c @ ctrt_c
     (* LOOP *)
     | (MCC_ln_loop, MCC_ln_loop)
     | (MCC_ln_loop, MCC_lb_loop)
     | (MCC_lb_loop, MCC_ln_loop)
     | (MCC_lb_loop, MCC_lb_loop) ->
       (* NOTE: question here : is (AND []) valid formula ? *)
       let (ms_hd_c : mich_f list) =
          let bool_v =
             match mcc_2 with
             | MCC_ln_loop -> false
             | _           -> true
          in
          eqf_cc ctxt1 ctxt1 (List.hd_exn si1.si_mich)
            (MV_lit_bool bool_v |> gen_dummy_cc)
       in
       let (ms_c : mich_f list) =
          List.map2_exn (List.tl_exn si1.si_mich) si2.si_mich ~f:eqf
          |> List.join
       and (ds_c : mich_f list) =
          List.map2_exn si1.si_dip si2.si_dip ~f:eqf |> List.join
       and (maps_entry_c : mich_f list) =
          List.map2_exn si1.si_map_entry si2.si_map_entry ~f:eqf |> List.join
       and (maps_exit_c : mich_f list) =
          List.map2_exn si1.si_map_exit si2.si_map_exit ~f:eqf |> List.join
       and (is_c : mich_f list) =
          List.map2_exn si1.si_iter si2.si_iter ~f:eqf |> List.join
       and (b_c : mich_f list) = eqf si1.si_balance si2.si_balance
       and (bcb_c : mich_f list) = eqf si1.si_bc_balance si2.si_bc_balance
       and (ti_c : mich_f list) =
          trx_image_equality_fmla ctxt1 ctxt2 si1.si_param si2.si_param
       in
       ms_hd_c
       @ ms_c
       @ ds_c
       @ maps_entry_c
       @ maps_exit_c
       @ is_c
       @ b_c
       @ bcb_c
       @ ti_c
     (* LOOP-LEFT *)
     | (MCC_ln_loopleft, MCC_ln_loopleft)
     | (MCC_ln_loopleft, MCC_lb_loopleft)
     | (MCC_lb_loopleft, MCC_ln_loopleft)
     | (MCC_lb_loopleft, MCC_lb_loopleft) ->
       (* 1. common parts
          - mich-stack tail, dip-stack, map-entry/exit/mapkey-stack,
             iter-stack, balance, bc-balance, trx-images
       *)
       let (ms_c_common : mich_f list) =
          List.map2_exn (List.tl_exn si1.si_mich) (List.tl_exn si2.si_mich)
            ~f:eqf
          |> List.join
       and (ds_c : mich_f list) =
          List.map2_exn si1.si_dip si2.si_dip ~f:eqf |> List.join
       and (map_entry_sc : mich_f list) =
          List.map2_exn si1.si_map_entry si2.si_map_entry ~f:eqf |> List.join
       and (map_exit_sc : mich_f list) =
          List.map2_exn si1.si_map_exit si2.si_map_exit ~f:eqf |> List.join
       and (map_mkey_sc : mich_f list) =
          List.map2_exn si1.si_map_mapkey si2.si_map_mapkey ~f:eqf |> List.join
       and (is_c : mich_f list) =
          List.map2_exn si1.si_iter si2.si_iter ~f:eqf |> List.join
       and (b_c : mich_f list) = eqf si1.si_balance si2.si_balance
       and (bcb_c : mich_f list) = eqf si1.si_bc_balance si2.si_bc_balance
       and (ti_c : mich_f list) =
          trx_image_equality_fmla ctxt1 ctxt2 si1.si_param si2.si_param
       in
       (* 2. Left/Right specific constraints *)
       let (lr_c : mich_f list) =
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
            failwith
              "MState : stack_equality_fmlas : LOOP_LEFT : 2 : unexpected"
       in
       ms_c_common
       @ ds_c
       @ map_entry_sc
       @ map_exit_sc
       @ map_mkey_sc
       @ is_c
       @ b_c
       @ bcb_c
       @ ti_c
       @ lr_c
     (* ITER *)
     (*
    (* WARNING: Below Spec is DEPRECATED *)
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
       let (ds_c : mich_f list) =
          List.map2_exn si1.si_dip si2.si_dip ~f:eqf |> List.join
       and (map_entry_sc : mich_f list) =
          List.map2_exn si1.si_map_entry si2.si_map_entry ~f:eqf |> List.join
       and (map_exit_sc : mich_f list) =
          List.map2_exn si1.si_map_exit si2.si_map_exit ~f:eqf |> List.join
       and (map_mkey_sc : mich_f list) =
          List.map2_exn si1.si_map_mapkey si2.si_map_mapkey ~f:eqf |> List.join
       and (b_c : mich_f list) = eqf si1.si_balance si2.si_balance
       and (bcb_c : mich_f list) = eqf si1.si_bc_balance si2.si_bc_balance
       and (ti_c : mich_f list) =
          trx_image_equality_fmla ctxt1 ctxt2 si1.si_param si2.si_param
       in
       (* 2. non-common parts - mich-stack & iter-stack *)
       let (ms_c_common, is_c_common, iter_specific_c)
             : mich_f list * mich_f list * mich_f list =
          match (mcc_1, mcc_2) with
          | (MCC_ln_iter, MCC_ln_iter) ->
            let (m1h, m1t, m2) =
               (List.hd_exn si1.si_mich, List.tl_exn si1.si_mich, si2.si_mich)
            in
            let (empty_container : mich_v cc) =
               match (typ_of_val m1h).cc_v with
               | MT_list t       -> MV_nil t |> gen_dummy_cc
               | MT_set t        -> MV_empty_set t |> gen_dummy_cc
               | MT_map (t1, t2) -> MV_empty_map (t1, t2) |> gen_dummy_cc
               | _               ->
                 failwith
                   "MState : stack_equality_fmlas : ITER : 2 : unexpected"
            in
            ( List.map2_exn m1t m2 ~f:eqf |> List.join,
              List.map2_exn si1.si_iter si2.si_iter ~f:eqf |> List.join,
              eqf_cc ctxt1 ctxt1 m1h empty_container
            )
          | (MCC_ln_iter, MCC_lb_iter) ->
            let (m1h, m1t) =
               (List.hd_exn si1.si_mich, List.tl_exn si1.si_mich)
            in
            let (i1, i2h, i2t) =
               (si1.si_iter, List.hd_exn si2.si_iter, List.tl_exn si2.si_iter)
            in
            ( List.map2_exn m1t si2.si_mich ~f:eqf |> List.join,
              List.map2_exn i1 i2t ~f:eqf |> List.join,
              eqf m1h i2h
            )
          | (MCC_lb_iter, MCC_ln_iter) ->
            let (i1h, i1t, i2) =
               (List.hd_exn si1.si_iter, List.tl_exn si1.si_iter, si2.si_iter)
            in
            let (empty_container : mich_v cc) =
               match (typ_of_val i1h).cc_v with
               | MT_list t       -> MV_nil t |> gen_dummy_cc
               | MT_set t        -> MV_empty_set t |> gen_dummy_cc
               | MT_map (t1, t2) -> MV_empty_map (t1, t2) |> gen_dummy_cc
               | _               ->
                 failwith
                   "MState : stack_equality_fmlas : ITER : 4 : unexpected"
            in
            ( List.map2_exn si1.si_mich si2.si_mich ~f:eqf |> List.join,
              List.map2_exn i1t i2 ~f:eqf |> List.join,
              eqf_cc ctxt1 ctxt1 i1h empty_container
            )
          | (MCC_lb_iter, MCC_lb_iter) ->
            let (m1 : mich_v cc list) = si1.si_mich in
            ( List.map2_exn m1 si2.si_mich ~f:eqf |> List.join,
              List.map2_exn si1.si_iter si2.si_iter ~f:eqf |> List.join,
              []
            )
          | _                          ->
            failwith "MState : stack_equality_fmlas : ITER : 1 : unexpected"
       in
       ds_c
       @ map_entry_sc
       @ map_exit_sc
       @ map_mkey_sc
       @ b_c
       @ bcb_c
       @ ti_c
       @ ms_c_common
       @ is_c_common
       @ iter_specific_c
     (* MAP *)
     (*
    (* WARNING: Below Spec is DEPRECATED *)
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
       let (ms_c_common : mich_f list) =
          (match (mcc_1, mcc_2) with
          | (MCC_ln_map, MCC_ln_map) ->
            List.map2_exn (List.tl_exn si1.si_mich) (List.tl_exn si2.si_mich)
              ~f:eqf
          | (MCC_ln_map, MCC_lb_map) ->
            List.map2_exn (List.tl_exn si1.si_mich) si2.si_mich ~f:eqf
          | (MCC_lb_map, MCC_ln_map) ->
            List.map2_exn si1.si_mich (List.tl_exn si2.si_mich) ~f:eqf
          | (MCC_lb_map, MCC_lb_map) ->
            List.map2_exn si1.si_mich si2.si_mich ~f:eqf
          | _                        ->
            failwith "MState : stack_equality_fmlas : MAP : 4 : unexpected")
          |> List.join
       and (ds_c : mich_f list) =
          List.map2_exn si1.si_dip si2.si_dip ~f:eqf |> List.join
       and (is_c : mich_f list) =
          List.map2_exn si1.si_iter si2.si_iter ~f:eqf |> List.join
       and (b_c : mich_f list) = eqf si1.si_balance si2.si_balance
       and (bcb_c : mich_f list) = eqf si1.si_bc_balance si2.si_bc_balance
       and (ti_c : mich_f list) =
          trx_image_equality_fmla ctxt1 ctxt2 si1.si_param si2.si_param
       in
       let ( (maps_entry_c_common : mich_f list),
             (maps_exit_c_common : mich_f list),
             (mapkeys_c_common : mich_f list),
             (maps_specific_c : mich_f list)
           ) =
          let (container_t : mich_t cc) =
             match mcc_1 with
             | MCC_ln_map -> typ_of_val (List.hd_exn si1.si_mich)
             | MCC_lb_map -> typ_of_val (List.hd_exn si1.si_map_entry)
             | _          ->
               failwith "MState : stack_equality_fmlas : MAP : 2 : unexpected"
          in
          let empty_container mvcc =
             match (typ_of_val mvcc).cc_v with
             | MT_list t       -> MV_nil t |> gen_dummy_cc
             | MT_set t        -> MV_empty_set t |> gen_dummy_cc
             | MT_map (t1, t2) -> MV_empty_map (t1, t2) |> gen_dummy_cc
             | _               ->
               failwith "MState : stack_equality_fmlas : ITER : 2 : unexpected"
          in
          match (mcc_1, mcc_2, container_t.cc_v) with
          | (MCC_ln_map, MCC_ln_map, _) ->
            let (m1h, m2h) =
               (List.hd_exn si1.si_mich, List.hd_exn si2.si_mich)
            in
            ( List.map2_exn si1.si_map_entry si2.si_map_entry ~f:eqf |> List.join,
              List.map2_exn si1.si_map_exit si2.si_map_exit ~f:eqf |> List.join,
              List.map2_exn si1.si_map_mapkey si2.si_map_mapkey ~f:eqf
              |> List.join,
              eqf_cc ctxt1 ctxt1 m1h (empty_container m1h)
              @ eqf_cc ctxt2 ctxt2 m2h (empty_container m2h)
            )
          | (MCC_ln_map, MCC_lb_map, MT_map _) ->
            ( List.map2_exn si1.si_map_entry
                (List.tl_exn si2.si_map_entry)
                ~f:eqf
              |> List.join,
              List.map2_exn si1.si_map_exit (List.tl_exn si2.si_map_exit) ~f:eqf
              |> List.join,
              List.map2_exn si1.si_map_mapkey
                (List.tl_exn si2.si_map_mapkey)
                ~f:eqf
              |> List.join,
              eqf (List.hd_exn si1.si_mich) (List.hd_exn si2.si_map_entry)
              @ eqf (List.hd_exn si1.si_mich) (List.hd_exn si2.si_map_exit)
            )
          | (MCC_ln_map, MCC_lb_map, _) ->
            ( List.map2_exn si1.si_map_entry
                (List.tl_exn si2.si_map_entry)
                ~f:eqf
              |> List.join,
              List.map2_exn si1.si_map_exit (List.tl_exn si2.si_map_exit) ~f:eqf
              |> List.join,
              List.map2_exn si1.si_map_mapkey si2.si_map_mapkey ~f:eqf
              |> List.join,
              eqf (List.hd_exn si1.si_mich) (List.hd_exn si2.si_map_entry)
            )
          | (MCC_lb_map, MCC_ln_map, MT_map _) ->
            ( List.map2_exn
                (List.tl_exn si1.si_map_entry)
                si2.si_map_entry ~f:eqf
              |> List.join,
              List.map2_exn (List.tl_exn si1.si_map_exit) si2.si_map_exit ~f:eqf
              |> List.join,
              List.map2_exn
                (List.tl_exn si1.si_map_mapkey)
                si2.si_map_mapkey ~f:eqf
              |> List.join,
              eqf (List.hd_exn si1.si_map_exit) (List.hd_exn si2.si_mich)
            )
          | (MCC_lb_map, MCC_ln_map, _) ->
            ( List.map2_exn
                (List.tl_exn si1.si_map_entry)
                si2.si_map_entry ~f:eqf
              |> List.join,
              List.map2_exn (List.tl_exn si1.si_map_exit) si2.si_map_exit ~f:eqf
              |> List.join,
              List.map2_exn si1.si_map_mapkey si2.si_map_mapkey ~f:eqf
              |> List.join,
              eqf (List.hd_exn si1.si_map_exit) (List.hd_exn si2.si_mich)
            )
          | (MCC_lb_map, MCC_lb_map, MT_map _) ->
            ( List.map2_exn si1.si_map_entry si2.si_map_entry ~f:eqf |> List.join,
              List.map2_exn si1.si_map_exit si2.si_map_exit ~f:eqf |> List.join,
              List.map2_exn
                (List.tl_exn si1.si_map_mapkey)
                (List.tl_exn si2.si_map_mapkey)
                ~f:eqf
              |> List.join,
              []
            )
          | (MCC_lb_map, MCC_lb_map, _) ->
            ( List.map2_exn si1.si_map_entry si2.si_map_entry ~f:eqf |> List.join,
              List.map2_exn si1.si_map_exit si2.si_map_exit ~f:eqf |> List.join,
              List.map2_exn si1.si_map_mapkey si2.si_map_mapkey ~f:eqf
              |> List.join,
              []
            )
          | _ -> failwith "MState : stack_equality_fmlas : MAP : 3 : unexpected"
       in
       ms_c_common
       @ ds_c
       @ is_c
       @ b_c
       @ bcb_c
       @ ti_c
       @ maps_entry_c_common
       @ maps_exit_c_common
       @ mapkeys_c_common
       @ maps_specific_c
     | _ -> failwith "MState : stack_equality_fmlas : 1 : unexpected"
   with
   | exn ->
     Utils.Log.err (fun m ->
         m "Error occurred in MState\n%s\n" (Printexc.get_backtrace ())
     );
     MStateError
       (Printf.sprintf "MState : %s :  %s ++ %s" (Exn.to_string exn)
          (List.to_string ~f:string_of_int ctxt1)
          (List.to_string ~f:string_of_int ctxt2)
       )
     |> raise
(* function stack_equality_fmlas end *)

(******************************************************************************)
(******************************************************************************)
(* Merged State                                                               *)
(******************************************************************************)
(******************************************************************************)

type t = (Tz.sym_state * Tz.mich_f list) list [@@deriving sexp, compare, equal]

type summary = {
  sm_rmci : Tz.r_mich_cut_info;
  sm_s_id : Tz.sym_state_id;
}
[@@deriving sexp, compare, equal]

module SMY_cmp = struct
  type t = summary [@@deriving sexp, compare]
end

let get_constraint : t -> Tz.mich_f list =
  fun ms ->
  List.concat (List.map ~f:(fun (ss, fl) -> ss.ss_constraints @ fl) ms)
(* function get_constraint end *)

let get_first_ss : t -> Tz.sym_state = (fun ms -> List.hd_exn ms |> fst)
(* function get_first_ss end *)

let get_last_ss : t -> Tz.sym_state = (fun ms -> List.last_exn ms |> fst)
(* function get_last_ss end *)

let get_tail_ms : t -> t = (fun ms -> List.tl_exn ms)
(* function get_tail_ms end *)

let get_length : t -> int = (fun ms -> List.length ms)
(* function get_length end *)

let get_summary : t -> summary =
  fun ms ->
  let fss = get_first_ss ms in
  { sm_rmci = TzUtil.get_reduced_mci fss.ss_start_mci; sm_s_id = fss.ss_id }
(* function get_summary end *)

let init : Tz.sym_state -> t = (fun ss -> [ (ss, []) ])
(* function init end *)

let cons : Tz.sym_state -> t -> t =
   let open Tz in
   let open TzUtil in
   fun ss ms ->
   let fst_ms_ss = List.hd_exn ms |> fst in
   let new_ctxt = ss.ss_id @ fst_ms_ss.ss_id in
   let renamed_ss = sym_state_symbol_context_swap ~ctx:new_ctxt ss in
   let connection_fmla =
      stack_equality_fmlas
        (renamed_ss.ss_id, fst_ms_ss.ss_id)
        (renamed_ss.ss_block_mci.mci_cutcat, fst_ms_ss.ss_start_mci.mci_cutcat)
        (renamed_ss.ss_block_si, fst_ms_ss.ss_start_si)
   in
   (renamed_ss, connection_fmla) :: ms
(* function cons end *)

let cut_first_found_loop : t -> (t * t) option =
   let open Tz in
   let open TzUtil in
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
            | []                 ->
              failwith "MState : cut_first_found_loop : unexpected"
            | (ss, _) :: remains -> (hd @ [ (ss, []) ], remains)
        )
   )
(* function cut_first_found_loop end *)

let extract_trx_state : t -> Tz.sym_state list =
   let open TzUtil in
   fun ms ->
   List.fold_right ms
     ~f:(fun (ss, _) acc_ts ->
       if is_trx_entry_state ss then ss :: acc_ts else acc_ts)
     ~init:[]
(* function extract_trx_state end *)

let get_trx_length : t -> int = (fun ms -> extract_trx_state ms |> List.length)
(* function get_trx_length end *)

let get_last_trx_ms : t -> t option =
   let open Tz in
   let rec get_last_trx_ms_i : t -> t =
     fun ms ->
     if equal_mich_cut_category (get_first_ss ms).ss_start_mci.mci_cutcat
          MCC_trx_entry
     then ms
     else get_last_trx_ms_i (get_tail_ms ms)
     (* function get_last_trx_ms_i end *)
   in
   fun ms ->
   if equal_int (get_trx_length ms) 1
   then None
   else Some (get_last_trx_ms_i (get_tail_ms ms))
(* function get_last_trx_ms end *)

let is_trx_path : t -> bool =
   let open TzUtil in
   fun ms ->
   is_trx_entry_state (get_first_ss ms) && is_trx_exit_state (get_last_ss ms)
(* function is_trx_path end *)

let is_query_path : t -> bool =
   let open TzUtil in
   fun ms ->
   is_trx_entry_state (get_first_ss ms) && is_query_exit_state (get_last_ss ms)
(* function is_query_path end *)

let gen_trx_paths :
    is_path_sat:(t -> bool) ->
    SSet.t ->
    Se.SSGraph.mci_view ->
    t list * t list QIDMap.t =
   let open Tz in
   let open TzUtil in
   let rec gen_trx_paths_i :
       is_done:(t -> bool) -> Se.SSGraph.mci_view -> int -> t list -> t list =
      let open Se in
      fun ~is_done m_view len tpaths ->
      if equal_int len 0
      then List.filter tpaths ~f:is_done
      else (
        let expand : t list -> t -> t list =
          fun acc ms ->
          if is_trx_entry_state (get_first_ss ms)
          then ms :: acc
          else (
            let (first_ss : sym_state) = get_first_ss ms in
            let (pred : SSet.t) = SSGraph.ss_view_pred ~m_view first_ss in
            let (expanded : t list) =
               SSet.to_list pred |> List.map ~f:(fun ss -> cons ss ms)
            in
            expanded @ acc
          )
        in
        let (e_paths : t list) = List.fold tpaths ~init:[] ~f:expand in
        gen_trx_paths_i ~is_done m_view (len - 1) e_paths
      )
      (* inner-function gen_trx_paths_i end *)
   in
   fun ~is_path_sat qset m_view ->
   let (trx_exit_state : t list) =
      List.find_exn (RMCIMap.keys m_view) ~f:(fun rmci ->
          equal_r_mich_cut_category rmci.rmci_cutcat RMCC_trx
      )
      |> RMCIMap.find_exn m_view
      |> (fun psp -> psp.pred)
      |> SSet.to_list
      |> List.map ~f:init
   in
   let (query_state : t list QIDMap.t) =
      SSet.group_by qset ~equiv:(fun qs1 qs2 ->
          equal_mich_cut_info qs1.ss_block_mci qs2.ss_block_mci
      )
      |> List.map ~f:(fun qset ->
             ( (SSet.choose_exn qset).ss_block_mci |> qid_of_mci_exn,
               SSet.to_list qset |> List.map ~f:init
             )
         )
      |> QIDMap.of_alist_exn
   in
   let (trx_sat_unchecked_paths : t list) =
      gen_trx_paths_i ~is_done:is_trx_path m_view
        (!Utils.Argument.path_limit - 1)
        trx_exit_state
   in
   let (query_sat_unchecked_paths : t list QIDMap.t) =
      QIDMap.map query_state
        ~f:
          (gen_trx_paths_i ~is_done:is_query_path m_view
             (!Utils.Argument.path_limit - 1)
          )
   in
   ( List.filter trx_sat_unchecked_paths ~f:is_path_sat,
     QIDMap.map query_sat_unchecked_paths ~f:(List.filter ~f:is_path_sat)
   )
(* function gen_trx_paths end *)
