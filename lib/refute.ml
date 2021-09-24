(* Refute : Naïve refuter for bug finding *)

exception RftError of string

open! Core

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(* Set of MState.t *)
module MSSet = Set.Make (MState)

(* Map of Tz.mich_cut_info *)
module MCIMap = Map.Make (Tz.MichCutInfo_cmp)

(* Set of Tz.sym_state *)
module SSet = Set.Make (Tz.SymState_cmp)

(* Set of Res.PPath.t *)
module PPSet = Set.Make (Res.PPath)

(******************************************************************************)
(******************************************************************************)
(* Refuter                                                                    *)
(******************************************************************************)
(******************************************************************************)

let select_pp : top_k:int -> PPSet.t -> PPSet.t * PPSet.t =
   let open MState in
   fun ~top_k ppaths ->
   List.sort (PPSet.to_list ppaths) ~compare:(fun pp1 pp2 ->
       compare_int (get_length pp1.pp_mstate) (get_length pp2.pp_mstate)
   )
   |> (fun l -> List.split_n l top_k)
   |> (fun (l1, l2) -> (PPSet.of_list l1, PPSet.of_list l2))
(* function select_pp end *)

let expand_pp : m_view:Se.SSGraph.mci_view -> Res.PPath.t -> PPSet.t =
   let open Se.SSGraph in
   let open MState in
   fun ~m_view pp ->
   let (pred : SSet.t) = ss_view_pred ~m_view (get_first_ss pp.pp_mstate) in
   let (ems : MSSet.t) = MSSet.map pred ~f:(fun ss -> cons ss pp.pp_mstate) in
   PPSet.map ems ~f:(fun ems ->
       { pp_mstate = ems; pp_score = pp.pp_score - 1; pp_checked = false }
   )
(* function expand_pp end *)

let refute :
    Smt.Ctx.t ->
    Smt.Solver.t ->
    Tz.mich_v Tz.cc ->
    Res.PPath.t ->
    (Res.PPath.t * Smt.Model.t) option =
   let open Smt in
   let open Tz in
   let open MState in
   let open Vc in
   fun ctx slvr istrg ppath ->
   if not
        (equal_mich_cut_category
           (get_first_ss ppath.pp_mstate).ss_start_mci.mci_cutcat MCC_trx_entry
        )
   then None
   else (
     let (vc : mich_f) = gen_refute_vc istrg ppath.pp_mstate |> TzUtil.opt_mf in
     let ((sat : Solver.satisfiability), (m_opt : Smt.Model.t option)) =
        check_sat ctx slvr vc
     in
     if Solver.is_sat sat then Some (ppath, Option.value_exn m_opt) else None
   )
(* function refute end *)

(******************************************************************************)
(******************************************************************************)
(* Naïve Run                                                                  *)
(******************************************************************************)
(******************************************************************************)

(* Partial Path ***************************************************************)

let naive_run_ppath_escape_condition : Res.config -> Res.PPath.t -> bool =
  fun { cfg_timer; cfg_memory; _ } _ ->
  if (* 1. Timeout *)
     Utils.Time.is_timeout cfg_timer
  then true
  else if (* 2. Memoryout *)
          Utils.Memory.is_memoryout cfg_memory
  then true
  else false
(* function naive_run_ppath_escape_condition end *)

let naive_run_ppath_atomic_action :
    Res.config -> Res.PPath.t -> PPSet.t * (Res.PPath.t * Smt.Model.t) option =
   let open Res in
   fun cfg ppath ->
   if naive_run_ppath_escape_condition cfg ppath
   then (PPSet.singleton ppath, None)
   else (
     (* 1. Get expanded paths *)
     let (expanded_ppaths : PPSet.t) = expand_pp ~m_view:cfg.cfg_m_view ppath in
     (* 2. Try to refute them *)
     let (r_opt : (Res.PPath.t * Smt.Model.t) option) =
        PPSet.fold expanded_ppaths ~init:None ~f:(fun r_opt eppath ->
            if Option.is_some r_opt
            then r_opt
            else refute cfg.cfg_smt_ctxt cfg.cfg_smt_slvr cfg.cfg_istrg eppath
        )
     in
     (expanded_ppaths, r_opt)
   )
(* function naive_run_ppath_atomic_action end *)

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
           equal_prover_flag qr_prv_flag PF_p
           || not (equal_refuter_flag qr_rft_flag RF_u)
   then true
   else false
(* function naive_run_qres_escape_condition end *)

let naive_run_qres_atomic_action : Res.config -> Res.qres -> Res.qres =
   let open Res in
   fun cfg qres ->
   (* 1. Check escape conditions *)
   if (* 1.1. Escape when (Timeout || (R-flag <> RF_u) || (P-flag = PF_p)) *)
      naive_run_qres_escape_condition cfg qres
   then qres
   else if (* 1.2. If Size(exp-ppaths) == 0, set refuter-flag to "failed" *)
           PPSet.is_empty qres.qr_exp_ppaths
   then { qres with qr_rft_flag = RF_f }
   else (
     (* 2. Select some predetermined number of paths, perform logic for them only *)
     let ((selected_ppaths : PPSet.t), (unselected_ppaths : PPSet.t)) =
        select_pp ~top_k:cfg.cfg_ppath_k qres.qr_exp_ppaths
     in
     (* 3. Expand and try to refute each selected paths *)
     let ( (qr_rft_ppath : (PPath.t * Smt.Model.t) option),
           (qr_exp_ppaths : PPSet.t),
           (qr_exp_cnt : int)
         ) =
        PPSet.fold selected_ppaths
          ~init:(None, unselected_ppaths, qres.qr_exp_cnt)
          ~f:(fun (r_opt, acc_ppaths, acc_cnt) ppath ->
            if Option.is_some r_opt
            then (r_opt, PPSet.add acc_ppaths ppath, acc_cnt)
            else (
              let ( (expanded_ppaths : PPSet.t),
                    (r_opt : (PPath.t * Smt.Model.t) option)
                  ) =
                 naive_run_ppath_atomic_action cfg ppath
              in
              let (new_ppath_cnt : int) = PPSet.length expanded_ppaths in
              ( r_opt,
                PPSet.union acc_ppaths expanded_ppaths,
                acc_cnt + new_ppath_cnt
              )
            )
        )
     in
     if Option.is_some qr_rft_ppath
     then
       { qres with qr_rft_flag = RF_r; qr_rft_ppath; qr_exp_ppaths; qr_exp_cnt }
     else { qres with qr_exp_ppaths; qr_exp_cnt }
   )
(* function naive_run_qres_atomic_action end *)

(* Result *********************************************************************)

let naive_run_res_atomic_action : Res.config -> Res.res -> Res.res =
   let open Res in
   fun cfg res ->
   {
     res with
     r_qr_lst =
       List.fold_right res.r_qr_lst ~f:(fun qres acc ->
           naive_run_qres_atomic_action cfg qres :: acc
       ) ~init:[];
   }
(* function naive_run_res_atomic_action end *)

(* Entry Point ****************************************************************)

let naive_run_escape_condition : Res.config -> Res.res -> bool =
   let open Res in
   fun { cfg_timer; cfg_memory; _ } { r_qr_lst; _ } ->
   if (* 1. Timeout *)
      Utils.Time.is_timeout cfg_timer
   then (
     let _ =
        (* Debugging info *)
        Utils.Log.debug (fun m ->
            m "Refute : naive_run_escape_condition : TIMEOUT!!!"
        )
     in
     true
   )
   else if (* 2. Memoryout *)
           Utils.Memory.is_memoryout cfg_memory
   then (
     Utils.Log.debug (fun m ->
         m "Refute : naive_run_escape_condition : MEMORYOUT!!!"
     );
     true
   )
   else if (* 3. Every queries are PF_p or RF_r or RF_f *)
           List.for_all r_qr_lst ~f:(fun qres ->
               equal_prover_flag qres.qr_prv_flag PF_p
               || not (equal_refuter_flag qres.qr_rft_flag RF_u)
           )
   then (
     let _ =
        (* Debugging info *)
        Utils.Log.debug (fun m ->
            m "Refute : naive_run_escape_condition : ALL NON-UNKNOWN!!!"
        )
     in
     true
   )
   else false
(* function naive_run_escape_condition end *)

let rec naive_run : Res.config -> Res.res -> Res.res =
  fun cfg res ->
  let _ =
     (* DEBUGGING INFOs *)
     Utils.Log.debug (fun m -> m "%s" (Res.string_of_res_rough cfg res))
  in
  if naive_run_escape_condition cfg res
  then res
  else naive_run cfg (naive_run_res_atomic_action cfg res)
(* function naive_run end *)
