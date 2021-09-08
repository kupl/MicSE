open! Core
open Tz
open TzUtil

let trx_image_equality_fmla : trx_image -> trx_image -> mich_f =
   let eqf x y = MF_eq (x, y) in
   fun t1 t2 ->
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
    mich_cut_category * mich_cut_category ->
    sym_image * sym_image ->
    mich_f list =
   let eqf x y = MF_eq (x, y) in
   fun (mcc_1, mcc_2) (si1, si2) ->
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
        MF_eq
          ( MV_cdr (List.hd_exn si1.si_mich) |> gen_dummy_cc,
            MV_cdr (List.hd_exn si2.si_mich) |> gen_dummy_cc
          )
     (* 2. dip, map, iter stacks should be emptied - No Constraints *)
     (* 3. balance equality *)
     and b_c = MF_eq (si1.si_balance, si2.si_balance)
     (* 4. bc_balance equality *)
     and bcb_c = MF_eq (si1.si_bc_balance, si2.si_bc_balance)
     (* 5. time inequality *)
     and time_c =
        MF_is_true
          (MV_leq_ib (si1.si_param.ti_time, si2.si_param.ti_time)
          |> gen_dummy_cc
          )
     in
     [ ms_c; b_c; bcb_c; time_c ]
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
     and ti_c = trx_image_equality_fmla si1.si_param si2.si_param in
     [ ms_c; ds_c; maps_entry_c; maps_exit_c; is_c; b_c; bcb_c; ti_c ]
   (* TODO *)
   (*
          (* LOOP-LEFT *)
          | (MCC_ln_loopleft, MCC_ln_loopleft) -> () (* TODO *)
          | (MCC_ln_loopleft, MCC_lb_loopleft) -> () (* TODO *)
          | (MCC_lb_loopleft, MCC_ln_loopleft) -> () (* TODO *)
          | (MCC_lb_loopleft, MCC_lb_loopleft) -> () (* TODO *)
       *)
   (*
     (* ITER *)
     | (MCC_ln_iter, MCC_ln_iter) -> () (* TODO *)
     | (MCC_ln_iter, MCC_lb_iter) -> () (* TODO *)
     | (MCC_lb_iter, MCC_ln_iter) -> () (* TODO *)
     | (MCC_lb_iter, MCC_lb_iter) -> () (* TODO *)
     *)
   (* MAP *)
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
     and ti_c = trx_image_equality_fmla si1.si_param si2.si_param in
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
     let maps_entry_c_common =
        MF_and (List.map2_exn m1_entry_t m2_entry_t ~f:eqf)
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
                eqf s1_mshd (MV_nil t1 |> gen_dummy_cc);
                eqf s2_mshd (MV_nil t2 |> gen_dummy_cc);
              ]
          | (MT_map (t1k, t1e), MT_map (t2k, t2e)) ->
            MF_and
              [
                eqf s1_mshd (MV_empty_map (t1k, t1e) |> gen_dummy_cc);
                eqf s2_mshd (MV_empty_map (t2k, t2e) |> gen_dummy_cc);
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
                eqf m2_exit_hd (MV_nil (typ_of_val m2_exit_hd) |> gen_dummy_cc);
              ]
          | (MT_map (t1k, t1e), MT_pair (t2k, t2e))
            when equal_mich_t t1k.cc_v t2k.cc_v
                 && equal_mich_t t1e.cc_v t2e.cc_v ->
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
                eqf m2_exit_hd (MV_empty_map (exit_tk, exit_te) |> gen_dummy_cc);
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
                eqf s2_mshd (MV_cons (s1_mshd, m1_exit_hd) |> gen_dummy_cc);
                eqf m1_entry_hd (MV_nil (typ_of_val m1_entry_hd) |> gen_dummy_cc);
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
                eqf m1_entry_hd
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

let init ss : t = [ (ss, []) ]

let cons ss ms : t =
   let fst_ms_ss = List.hd_exn ms |> fst in
   let new_ctxt = ss.ss_id @ fst_ms_ss.ss_id in
   let renamed_ss = sym_state_symbol_context_swap new_ctxt ss in
   let connection_fmla =
      stack_equality_fmlas
        (renamed_ss.ss_block_mci.mci_cutcat, fst_ms_ss.ss_start_mci.mci_cutcat)
        (renamed_ss.ss_block_si, fst_ms_ss.ss_start_si)
   in
   (renamed_ss, connection_fmla) :: ms

let get_constraint : t -> mich_f list =
  fun ms ->
  List.concat (List.map ~f:(fun (ss, fl) -> ss.ss_constraints @ fl) ms)

let get_first_ss : t -> sym_state = (fun ms -> List.hd_exn ms |> fst)

let get_last_ss : t -> sym_state = (fun ms -> List.last_exn ms |> fst)

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
