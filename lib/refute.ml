open! Core
open Se
open MState
module MSSet = Core.Set.Make (MState)
module MCIMap = Map.Make (Tz.MichCutInfo_cmp)
module MPMap = Map.Make (Inv.MciPair_cmp)

let expand_ms : m_view:SSGraph.mci_view -> MState.t -> MSSet.t =
  fun ~m_view ms ->
  let open SSGraph in
  ss_view_pred ~m_view (get_first_ss ms)
  |> MSSet.map ~f:(fun ss ->
         (* let _ =
               Utils.Log.debug (fun m -> m "DBG - expand_ms - MSSet.map ~f entered")
            in *)
         cons ss ms
     )

let expand_ms_multiple : m_view:Se.SSGraph.mci_view -> MSSet.t -> MSSet.t =
  fun ~m_view msset ->
  MSSet.fold msset ~init:MSSet.empty ~f:(fun accs ms ->
      MSSet.union (expand_ms ~m_view ms) accs
  )

let naive_run_init_res : se_result -> Res.res =
   let open Res in
   fun sr ->
   let mci_queries : SSet.t MCIMap.t =
      SSet.fold sr.sr_queries ~init:MCIMap.empty ~f:(fun acc qs ->
          MCIMap.update acc qs.ss_block_mci ~f:(function
          | Some s -> SSet.add s qs
          | None   -> SSet.singleton qs
          )
      )
   in
   let qresl : qres list =
      MCIMap.mapi mci_queries ~f:(fun ~key ~data ->
          {
            qr_qid = key;
            qr_prv_flag = PF_u;
            qr_rft_flag = RF_u;
            qr_unk_qs = data;
            qr_exp_ppaths = PPSet.map data ~f:PPath.t_of_ss;
            qr_rft_ppath = None;
            qr_exp_cnt = SSet.length data;
          }
      )
      |> MCIMap.to_alist
      |> List.map ~f:snd
   in
   {
     r_qr_lst = qresl;
     r_inv = Inv.gen_true_inv_map sr;
     r_cand = RMCIMap.empty;
     r_failcp = MPMap.empty;
     r_comb_cnt = 0;
   }

let naive_run_qres_atomic_action : Res.config -> Res.res -> Res.qres -> Res.qres
    =
   let open Res in
   let expand_pp : m_view:SSGraph.mci_view -> PPath.t -> PPSet.t =
     fun ~m_view pp ->
     PPSet.map (expand_ms ~m_view pp.pp_mstate) ~f:(fun ms ->
         { pp_mstate = ms; pp_goalst = []; pp_score = 0 }
     )
   in
   fun { cfg_timer; cfg_istrg; cfg_m_view; cfg_smt_ctxt; cfg_smt_slvr; _ } res
       qr ->
   (* let _ =
         Utils.Log.debug (fun m -> m "DBG - naive_run_qres_qtomic_action entered")
      in *)
   let { qr_prv_flag; qr_rft_flag; qr_exp_ppaths; qr_exp_cnt; _ } = qr in
   (* 1. Check escape conditions *)
   (* 1.1. Escape when (Timeout || (R-flag <> RF_u) || (P-flag = PF_p)) *)
   if (not (equal_refuter_flag qr_rft_flag RF_u))
      || equal_prover_flag qr_prv_flag PF_p
      || Utils.Time.is_timeout cfg_timer
   then qr
   else if (* 1.2. If Size(exp-ppaths) == 0, set refuter-flag to "failed" *)
           PPSet.is_empty qr_exp_ppaths
   then { qr with qr_rft_flag = RF_f }
   else (
     (* 2. For every total paths, check if the path is refutable *)
     let (refuted_opt, new_ppset, new_count)
           : (PPath.t * Smt.Model.t) option * PPSet.t * int =
        let open PPath in
        let open Tz in
        PPSet.fold qr_exp_ppaths ~init:(None, PPSet.empty, qr_exp_cnt)
          ~f:(fun (r_opt, acc_new_ppset, acc_cnt) pp ->
            (* let _ =
                  Utils.Log.debug (fun m ->
                      m "DBG - naive_run_qres_qtomic_action - ppath-fold entered"
                  )
               in *)
            let { pp_mstate; pp_goalst = _; pp_score = _ } = pp in
            (* let _ =
                  Utils.Log.debug (fun m ->
                      m
                        "DBG - naive_run_qres_qtomic_action - ppath-fold - before expanded-pps"
                  )
               in *)
            let expanded_pps = expand_pp ~m_view:cfg_m_view pp in
            (* let _ =
                  Utils.Log.debug (fun m ->
                      m
                        "DBG - naive_run_qres_qtomic_action - ppath-fold - after expanded-pps"
                  )
               in *)
            let new_count_if_expanded = acc_cnt + PPSet.length expanded_pps in
            (* 2.1. if (timeout || any refuted results appeared before), then skip (add current pp to new-exp-ppath) *)
            if Utils.Time.is_timeout cfg_timer || Option.is_some r_opt
            then (r_opt, PPSet.add acc_new_ppset pp, acc_cnt)
            else if (* 2.2. if pp is not total path, expand and add it to new-exp-ppath *)
                    not
                      (equal_mich_cut_category
                         (get_first_ss pp_mstate).ss_start_mci.mci_cutcat
                         MCC_trx_entry
                      )
            then
              ( r_opt,
                PPSet.union expanded_pps acc_new_ppset,
                new_count_if_expanded
              )
            else (
              (* 2.3. pp is total path. so try to refute *)
              let fmla : mich_f =
                 Vc.gen_query_vc_from_ms_with_init_strg res.r_inv cfg_istrg
                   pp_mstate
                 |> TzUtil.opt_mf
              in
              match Vc.check_sat cfg_smt_ctxt cfg_smt_slvr fmla with
              | (SAT, Some mdl) ->
                ( (* 2.3.1. successfully refute *) Some (pp, mdl),
                  acc_new_ppset,
                  acc_cnt
                )
              | _               ->
                ( (* 2.3.2. failed to refute *) r_opt,
                  PPSet.union expanded_pps acc_new_ppset,
                  new_count_if_expanded
                )
            )
        )
     in
     (* 3. If refuted, then update refuter-flag and rft-ppath. Else, update exp-ppaths *)
     match refuted_opt with
     | Some _ ->
       {
         qr with
         qr_rft_flag = RF_r;
         qr_exp_ppaths = new_ppset;
         qr_rft_ppath = refuted_opt;
         qr_exp_cnt = new_count;
       }
     | None   -> { qr with qr_exp_ppaths = new_ppset; qr_exp_cnt = new_count }
   )

let naive_run_res_escape_condition : Res.config -> Res.res -> bool =
   let open Res in
   fun { cfg_timer; _ } { r_qr_lst; _ } ->
   (* 1. Timeout *)
   if Utils.Time.is_timeout cfg_timer
   then (
     let _ =
        (* Debugging info *)
        Utils.Log.debug (fun m ->
            m "Refute : naive_run_res_escape_condition : TIMEOUT!!!"
        )
     in
     true
   )
   else if (* 2. Every queries are PF_p or RF_r or RF_f *)
           List.for_all r_qr_lst ~f:(fun qres ->
               equal_prover_flag qres.qr_prv_flag PF_p
               || not (equal_refuter_flag qres.qr_rft_flag RF_u)
           )
   then (
     let _ =
        (* Debugging info *)
        Utils.Log.debug (fun m ->
            m "Refute : naive_run_res_escape_condition : ALL NON-UNKNOWN!!!"
        )
     in
     true
   )
   else false

let naive_run_res_atomic_action : Res.config -> Res.res -> Res.res =
   let open Res in
   fun cfg res ->
   {
     res with
     r_qr_lst =
       List.fold res.r_qr_lst ~init:[] ~f:(fun acc qres ->
           naive_run_qres_atomic_action cfg res qres :: acc
       );
   }

let rec naive_run : Res.config -> Res.res -> Res.res =
  fun cfg res ->
  let _ =
     (* DEBUGGING INFOs *)
     Utils.Log.debug (fun m ->
         m "%s" (Res.string_of_res_rough_in_refuter_perspective cfg res)
     )
  in
  if naive_run_res_escape_condition cfg res
  then res
  else naive_run cfg (naive_run_res_atomic_action cfg res)
