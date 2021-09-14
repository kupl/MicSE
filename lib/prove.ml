(* Prove : Naïve prover for verification *)

exception PrvError of string

open! Core

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(* Set of Tz.mich_f *)
module MFSet = Set.Make (Tz.MichF_cmp)

(* Map of Tz.mich_cut_info *)
module MCIMap = Map.Make (Tz.MichCutInfo_cmp)

(* Map of Tz.r_mich_cut_info *)
module RMCIMap = Map.Make (Tz.RMichCutInfo_cmp)

(* Set of Tz.sym_state *)
module SSet = Set.Make (Tz.SymState_cmp)

(* Set of Inv.inv_map *)
module InvSet = Set.Make (Inv.InvMap_cmp)

(******************************************************************************)
(******************************************************************************)
(* Prover                                                                     *)
(******************************************************************************)
(******************************************************************************)

let check_failed :
    SSet.t -> Inv.failed_cp -> Tz.r_mich_cut_info -> Inv.inv_map -> bool =
   let open Tz in
   let open TzUtil in
   let open Inv in
   fun bsset failed_cp rmci imap ->
   SSet.exists bsset ~f:(fun bs ->
       let (mp_start : r_mich_cut_info) = get_reduced_mci bs.ss_start_mci in
       let (mp_block : r_mich_cut_info) = get_reduced_mci bs.ss_block_mci in
       if (* only the target mci *)
          equal_r_mich_cut_info mp_start rmci
          || equal_r_mich_cut_info mp_block rmci
       then (
         let (cp_start : MFSet.t) = find_inv_by_rmci imap mp_start in
         let (cp_block : MFSet.t) = find_inv_by_rmci imap mp_block in
         is_already_failed_by_rmci failed_cp { mp_start; mp_block }
           { cp_start; cp_block }
       )
       else false
   )
(* function check_failed end *)

let check_inductiveness :
    Smt.Ctx.t ->
    Smt.Solver.t ->
    SSet.t ->
    Inv.inv_map ->
    (Inv.inv_map, Tz.sym_state) Result.t =
   let open Smt in
   let open Vc in
   fun ctx slvr bsset imap ->
   SSet.fold bsset ~init:(Result.return imap) ~f:(fun inductive bs ->
       if Result.is_error inductive
       then inductive
       else (
         let (vc : Tz.mich_f) = gen_inductiveness_vc imap bs in
         let ((vld : Solver.validity), _) = check_val ctx slvr vc in
         if Solver.is_val vld then inductive else Result.fail bs
       )
   )
(* function check_inductiveness end *)

let add_failed :
    Inv.failed_cp * Inv.cand_map * InvSet.t ->
    Tz.sym_state ->
    failed:Inv.inv_map ->
    Inv.failed_cp * Inv.cand_map * InvSet.t =
   let open Inv in
   fun (failed_cp, cmap, combs) state ~failed ->
   let (mcip : mci_pair) =
      cvt_mci_pair (state.ss_start_mci, state.ss_block_mci)
   in
   let (cand_start : MFSet.t) = find_inv_by_rmci failed mcip.mp_start in
   let (cand_block : MFSet.t) = find_inv_by_rmci failed mcip.mp_block in
   let (candp : cand_pair) = cvt_cand_pair (cand_start, cand_block) in
   let (new_failed_cp : failed_cp) =
      add_failed_cp failed_cp ~key:mcip ~value:candp
   in
   let (new_cmap : cand_map) =
      score_cand cmap ~key:mcip.mp_start ~value:cand_start ~point:(-1)
      |> score_cand ~key:mcip.mp_block ~value:cand_block ~point:(-1)
   in
   let (new_combs : InvSet.t) =
      InvSet.filter combs ~f:(fun comb ->
          not (check_contain_pair comb mcip candp)
      )
   in
   (new_failed_cp, new_cmap, new_combs)
(* function add_failed end *)

let rec combinate :
    SSet.t ->
    Inv.failed_cp ->
    Inv.inv_map ->
    Inv.cand_map ->
    InvSet.t ->
    Inv.inv_map ->
    InvSet.t =
   let open Inv in
   let open Res in
   let (top_k : int) = 20 in
   let (threshold : int) = 100 in
   fun bsset failed_cp cinv targets combs acc_imap ->
   let next_comb : Inv.cand_map -> InvSet.t -> Inv.inv_map -> InvSet.t =
      combinate bsset failed_cp cinv
   in
   (* syntax sugar *)
   if InvSet.length combs >= threshold
   then combs
   else if RMCIMap.is_empty targets
   then
     if equal_inv_map cinv acc_imap || InvSet.mem combs acc_imap
     then combs
     else InvSet.add combs acc_imap
   else (
     (* 1. Target MCI for combinate *)
     let (rmci : Tz.r_mich_cut_info) = List.hd_exn (RMCIMap.keys targets) in
     let (cands : MFSet.t list) =
        find_cand_by_rmci targets rmci
        |> CMap.filter ~f:(fun (f, _) -> f)
        |> CMap.to_alist
        |> List.sort ~compare:(fun (_, (_, s1)) (_, (_, s2)) ->
               compare_int s1 s2
           )
        |> List.map ~f:fst
        |> List.rev
     in
     let (remains : cand_map) = RMCIMap.remove targets rmci in
     (* 2. Get candidates from target *)
     let (target : inv_map list) =
        List.fold cands ~init:[] ~f:(fun acc fset ->
            if List.length acc >= top_k
            then acc
            else (
              let (updated_inv : inv_map) =
                 update_inv_map acc_imap ~key:rmci ~value:fset
              in
              if check_failed bsset failed_cp rmci updated_inv
              then acc (* remove already failed combination *)
              else updated_inv :: acc
            )
        )
     in
     (* 3. Combinate candidates *)
     let (new_combs : InvSet.t) =
        List.fold target ~init:combs ~f:(fun acc_comb imap ->
            next_comb remains acc_comb imap
        )
     in
     new_combs
   )
(* function combinate end *)

let prove : Smt.Ctx.t -> Smt.Solver.t -> Inv.inv_map -> SSet.t -> SSet.t =
   let open Res in
   let open Smt in
   let open Vc in
   fun ctx slvr imap unk_qs ->
   SSet.filter unk_qs ~f:(fun qs ->
       let (vc : Tz.mich_f) = gen_query_vc imap qs in
       let ((vld : Solver.validity), _) = check_val ctx slvr vc in
       not (Solver.is_val vld)
   )
(* function prove end *)

(******************************************************************************)
(******************************************************************************)
(* Naïve Run                                                                  *)
(******************************************************************************)
(******************************************************************************)

(* Query Result ***************************************************************)

let naive_run_qres_escape_condition : Res.config -> Res.qres -> bool =
   let open Res in
   fun { cfg_timer; cfg_memory; _ } { qr_prv_flag; qr_rft_flag; _ } ->
   if (* 1. Timeout *)
      Utils.Time.is_timeout cfg_timer
   then true
   else if (* 2. Memoryout *)
           Utils.Memory.is_memoryout cfg_memory
   then true
   else if (* 3. Query result is already judged *)
           equal_refuter_flag qr_rft_flag RF_r
           || not (equal_prover_flag qr_prv_flag PF_u)
   then true
   else false
(* function naive_run_res_escape_condition end *)

let naive_run_qres_atomic_action :
    Res.config -> Inv.inv_map -> Res.qres -> Res.qres =
   let open Res in
   fun cfg imap qres ->
   if naive_run_qres_escape_condition cfg qres
   then qres
   else (
     let (qr_unk_qs : SSet.t) =
        prove cfg.cfg_smt_ctxt cfg.cfg_smt_slvr imap qres.qr_unk_qs
     in
     let (qr_prv_flag : prover_flag) =
        if SSet.is_empty qr_unk_qs then PF_p else qres.qr_prv_flag
     in
     let (new_qres : qres) = { qres with qr_unk_qs; qr_prv_flag } in
     new_qres
   )
(* function naive_run_qres_atomic_action end *)

(* Worklist *******************************************************************)

let naive_run_wlst_escape_condition : Res.config -> Res.worklist -> bool =
   let open Res in
   fun { cfg_timer; cfg_memory; _ } { wl_combs; _ } ->
   if (* 1. Timeout *)
      Utils.Time.is_timeout cfg_timer
   then true
   else if (* 2. Memoryout *)
           Utils.Memory.is_memoryout cfg_memory
   then true
   else if (* 3. Empty combinations *)
           InvSet.is_empty wl_combs
   then true
   else false
(* function naive_run_wlst_escape_condition end *)

let rec naive_run_wlst_atomic_action :
    Res.config ->
    Inv.inv_map * Inv.cand_map * Res.worklist ->
    Inv.inv_map option * Inv.cand_map * Res.worklist =
   let open Inv in
   let open Res in
   fun cfg (imap, cands, wlst) ->
   if naive_run_wlst_escape_condition cfg wlst
   then (None, cands, wlst)
   else (
     (* 1. Choose one combination *)
     let (comb : inv_map) = InvSet.choose_exn wlst.wl_combs in
     let (remain_combs : InvSet.t) = InvSet.remove wlst.wl_combs comb in
     (* 2. Check inductiveness *)
     let (inductive : (inv_map, Tz.sym_state) Result.t) =
        check_inductiveness cfg.cfg_smt_ctxt cfg.cfg_smt_slvr
          cfg.cfg_se_res.sr_blocked comb
     in
     if Result.is_ok inductive
     then (
       (* 3-1-1. New invariant is found *)
       let (new_imap : inv_map) =
          Result.ok inductive |> Option.value ~default:imap
       in
       (* 3-1-2. Update candidates *)
       let (new_cands : cand_map) = strengthen_cand_map cands new_imap in
       (* 3-1-3. Update combinations *)
       let (new_combs : InvSet.t) =
          Inv.strengthen_inv_map remain_combs new_imap
       in
       ( Some new_imap,
         new_cands,
         { wlst with wl_combs = new_combs; wl_comb_cnt = wlst.wl_comb_cnt + 1 }
       )
     )
     else (
       (* 3-2-1. This combination is not inductive *)
       let (failed_state : Tz.sym_state) =
          Result.error inductive |> Option.value ~default:cfg.cfg_istate
       in
       (* 3-2-2. Update failed information *)
       let ( (new_failed_cp : failed_cp),
             (new_cands : cand_map),
             (new_combs : InvSet.t)
           ) =
          add_failed
            (wlst.wl_failcp, cands, wlst.wl_combs)
            failed_state ~failed:comb
       in
       let (new_wlst : worklist) =
          {
            wl_combs = new_combs;
            wl_failcp = new_failed_cp;
            wl_comb_cnt = wlst.wl_comb_cnt + 1;
          }
       in
       naive_run_wlst_atomic_action cfg (imap, new_cands, new_wlst)
     )
   )
(* function naive_run_wlst_atomic_action end *)

(* Result *********************************************************************)

let naive_run_res_atomic_action : Res.config -> Res.res -> Res.res =
   let open Res in
   fun cfg res ->
   (* 1. Generate combinations *)
   let (wl_combs : InvSet.t) =
      combinate cfg.cfg_se_res.sr_blocked res.r_wlst.wl_failcp res.r_inv
        res.r_cands res.r_wlst.wl_combs res.r_inv
   in
   Utils.Log.debug (fun m -> m "%d" (InvSet.length wl_combs));
   if InvSet.length wl_combs <= 0
   then
     {
       res with
       r_qr_lst =
         List.map res.r_qr_lst ~f:(fun qres -> { qres with qr_prv_flag = PF_f });
     }
   else (
     (* 2. Check inductiveness *)
     let ( (r_inv_opt : Inv.inv_map option),
           (r_cands : Inv.cand_map),
           (r_wlst : worklist)
         ) =
        naive_run_wlst_atomic_action cfg
          (res.r_inv, res.r_cands, { res.r_wlst with wl_combs })
     in
     if Option.is_some r_inv_opt
     then (
       (* 3-1. Prove queries when indutive invariant was found *)
       let (r_inv : Inv.inv_map) = Option.value r_inv_opt ~default:res.r_inv in
       let (r_qr_lst : qres list) =
          List.map res.r_qr_lst ~f:(fun qres ->
              naive_run_qres_atomic_action cfg r_inv qres
          )
       in
       { r_qr_lst; r_inv; r_cands; r_wlst }
     )
     else
       (* 3-2. Pass query proving when indutive invariant was not found *)
       { res with r_cands; r_wlst }
   )
(* function naive_run_res_atomic_action end *)

(* Entry Point ****************************************************************)

let naive_run_escape_condition : Res.config -> Res.res -> bool =
   let open Res in
   fun { cfg_timer; cfg_memory; _ } { r_qr_lst; _ } ->
   if (* 1. Timeout *)
      Utils.Time.is_timeout cfg_timer
   then (
     Utils.Log.debug (fun m ->
         m "Prove : naive_run_res_escape_condition : TIMEOUT!!!"
     );
     true
   )
   else if (* 2. Memoryout *)
           Utils.Memory.is_memoryout cfg_memory
   then (
     Utils.Log.debug (fun m ->
         m "Prove : naive_run_res_escape_condition : MEMORYOUT!!!"
     );
     true
   )
   else if (* 2. Every queries are PF_p or RF_f or RF_r *)
           List.for_all r_qr_lst ~f:(fun qres ->
               (not (equal_prover_flag qres.qr_prv_flag PF_u))
               || equal_refuter_flag qres.qr_rft_flag RF_r
           )
   then (
     Utils.Log.debug (fun m ->
         m "Prove : naive_run_res_escape_condition : ALL NON-UNKNOWN!!!"
     );
     true
   )
   else false
(* function naive_run_res_escape_condition end *)

let naive_run : Res.config -> Res.res -> Res.res =
   let rec naive_run_i : Res.config -> Res.res -> Res.res =
     fun cfg res ->
     let _ =
        (* DEBUGGING INFOs *)
        Utils.Log.debug (fun m -> m "%s" (Res.string_of_res_rough cfg res))
     in
     if naive_run_escape_condition cfg res
     then res
     else naive_run_i cfg (naive_run_res_atomic_action cfg res)
   in
   (* inner-function naive_run_i end *)
   fun cfg res ->
   let _ =
      (* DEBUGGING INFOs *)
      Utils.Log.debug (fun m -> m "%s" (Res.string_of_res_rough cfg res))
   in
   let (r_qr_lst : Res.qres list) =
      List.map res.r_qr_lst ~f:(fun qres ->
          naive_run_qres_atomic_action cfg res.r_inv qres
      )
   in
   naive_run_i cfg { res with r_qr_lst }
(* function naive_run end *)
