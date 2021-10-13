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

(* Map of Tz.r_mich_cut_info *)
module RMCIMap = Map.Make (Tz.RMichCutInfo_cmp)

(* Set of Tz.sym_state *)
module SSet = Set.Make (Tz.SymState_cmp)

(* Set of Res.PPath.t *)
module PPSet = Set.Make (Res.PPath)

(******************************************************************************)
(******************************************************************************)
(* Refuter                                                                    *)
(******************************************************************************)
(******************************************************************************)

let separate_ppset_in_length_increasing_order :
    Res.PPSet.t -> Res.PPath.t list RMCIMap.t =
   let open Res in
   let open Inv in
   (* Design note : it recieves rmciset to fill empty-list even if there are no ppath start with it.
      So the caller can use "RMCIMap.find_exn" freely without worry about exceptions *)
   fun ppset ->
   let accumulated_map =
      PPSet.fold ppset ~init:RMCIMap.empty ~f:(fun accmap ppath ->
          let start_rmci =
             (MState.get_first_ss ppath.pp_mstate).ss_start_mci
             |> TzUtil.get_reduced_mci
          in
          RMCIMap.update accmap start_rmci ~f:(function
          | None   -> [ ppath ]
          | Some l -> ppath :: l
          )
      )
   in
   let sorted_map =
      let compare : PPath.t -> PPath.t -> int =
        fun pp1 pp2 ->
        compare_int
          (MState.get_length pp1.pp_mstate)
          (MState.get_length pp2.pp_mstate)
      in
      RMCIMap.map accumulated_map ~f:(List.sort ~compare)
   in
   sorted_map
(* function separate_ppset_in_length_increasing_order end *)

let select_pp : top_k:int -> PPSet.t -> PPSet.t * PPSet.t =
   let open Res in
   fun ~top_k ppaths ->
   let (rmcipmap : PPath.t list RMCIMap.t) =
      separate_ppset_in_length_increasing_order ppaths
   in
   let (selected_ppaths : PPSet.t) =
      RMCIMap.fold rmcipmap ~init:PPSet.empty ~f:(fun ~key:_ ~data pset ->
          let (lst : PPath.t list) = List.take data top_k in
          List.fold lst ~init:pset ~f:PPSet.add
      )
   in
   (selected_ppaths, PPSet.diff ppaths selected_ppaths)
(* function select_pp end *)

let expand_pp : m_view:Se.SSGraph.mci_view -> Res.PPath.t -> PPSet.t =
   let open Se.SSGraph in
   let open MState in
   fun ~m_view pp ->
   let (pred : SSet.t) = ss_view_pred ~m_view (get_first_ss pp.pp_mstate) in
   let (ems : MSSet.t) = MSSet.map pred ~f:(fun ss -> cons ss pp.pp_mstate) in
   PPSet.map ems ~f:(fun ems ->
       {
         pp_mstate = ems;
         pp_satisfiability = None;
         pp_score = pp.pp_score;
         pp_score_fixed = false;
       }
   )
(* function expand_pp end *)

let filter_sat_ppaths : Smt.Ctx.t -> Smt.Solver.t -> PPSet.t -> PPSet.t =
  fun ctx slvr ppaths ->
  PPSet.fold ppaths ~init:PPSet.empty ~f:(fun acc ppath ->
      let (sat_filled_pp, _) =
         Res.PPath.satisfiability_fill (ctx, slvr) ppath
      in
      if equal_option Smt.Solver.equal_satisfiability
           sat_filled_pp.pp_satisfiability (Some Smt.Solver.SAT)
      then PPSet.add acc sat_filled_pp
      else acc
  )

(* function check_sat end *)

let refute :
    Smt.Ctx.t ->
    Smt.Solver.t ->
    Tz.mich_v Tz.cc ->
    Res.PPath.t ->
    (Res.PPath.t * Smt.Solver.satisfiability) option * Smt.Model.t option =
   let open Smt in
   let open Tz in
   let open MState in
   let open Vc in
   fun ctx slvr istrg ppath ->
   if not
        (equal_mich_cut_category
           (get_first_ss ppath.pp_mstate).ss_start_mci.mci_cutcat MCC_trx_entry
        )
   then (None, None)
   else (
     let (vc : mich_f) = gen_refute_vc istrg ppath.pp_mstate |> TzUtil.opt_mf in
     let ((sat : Solver.satisfiability), (m_opt : Smt.Model.t option)) =
        check_sat ctx slvr vc
     in
     ( Some (ppath, sat),
       if Solver.is_sat sat then Some (Option.value_exn m_opt) else None
     )
   )
(* function refute end *)

(******************************************************************************)
(******************************************************************************)
(* Path-Pick Functions                                                        *)
(******************************************************************************)
(******************************************************************************)

module PickFun = struct
  (* NOTE : pick_func return (picked-ppaths, unpicked-ppaths) pair.
           Dead paths (unsatisfiable paths) SHOULD NOT BE put in picked-ppaths.
           (neither in unpicked-ppaths, but unknown paths whether dead or not
           can be put in unpicked-ppaths)
  *)
  type t = Smt.Ctx.t * Smt.Solver.t -> PPSet.t -> PPSet.t * PPSet.t
end
(* module PickFun end *)

let pick_short_k_for_each_mci : top_k:int -> PickFun.t =
   let open Res in
   fun ~top_k (ctx, slvr) ppaths ->
   let sat_filtered_ppset = filter_sat_ppaths ctx slvr ppaths in
   let (rmcipmap : PPath.t list RMCIMap.t) =
      separate_ppset_in_length_increasing_order sat_filtered_ppset
   in
   let (selected_ppaths : PPSet.t) =
      RMCIMap.fold rmcipmap ~init:PPSet.empty ~f:(fun ~key:_ ~data pset ->
          let (lst : PPath.t list) = List.take data top_k in
          List.fold lst ~init:pset ~f:PPSet.add
      )
   in
   (selected_ppaths, PPSet.diff sat_filtered_ppset selected_ppaths)

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
    Res.config ->
    Res.PPath.t ->
    (Res.PPath.t * Smt.Solver.satisfiability) list
    * PPSet.t
    * (Res.PPath.t * Smt.Model.t) option =
   let open Res in
   fun cfg ppath ->
   if naive_run_ppath_escape_condition cfg ppath
   then ([], PPSet.singleton ppath, None)
   else (
     (* 1. Get expanded paths *)
     let (expanded_ppaths : PPSet.t) = expand_pp ~m_view:cfg.cfg_m_view ppath in
     (* 2. Filter unsatisfiable paths *)
     let (sat_ppaths : PPSet.t) =
        filter_sat_ppaths cfg.cfg_smt_ctxt cfg.cfg_smt_slvr expanded_ppaths
     in
     (* 3. Try to refute them *)
     let ( (total_paths : (PPath.t * Smt.Solver.satisfiability) list),
           (r_opt : (Res.PPath.t * Smt.Model.t) option)
         ) =
        PPSet.fold sat_ppaths ~init:([], None)
          ~f:(fun (t_paths, r_opt) eppath ->
            if Option.is_some r_opt
            then (t_paths, r_opt)
            else (
              let ( (total_path_opt :
                      (PPath.t * Smt.Solver.satisfiability) option
                      ),
                    (model_opt : Smt.Model.t option)
                  ) =
                 refute cfg.cfg_smt_ctxt cfg.cfg_smt_slvr cfg.cfg_istrg eppath
              in
              if Option.is_none total_path_opt
              then (t_paths, r_opt)
              else (
                let (total_path : PPath.t * Smt.Solver.satisfiability) =
                   Option.value_exn total_path_opt
                in
                ( total_path :: t_paths,
                  Option.map model_opt ~f:(fun model -> (fst total_path, model))
                )
              )
            )
        )
     in
     (total_paths, sat_ppaths, r_opt)
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
     let ( (qr_total_ppaths : (Res.PPath.t * Smt.Solver.satisfiability) list),
           (qr_exp_ppaths : PPSet.t),
           (qr_rft_ppath : (PPath.t * Smt.Model.t) option),
           (qr_exp_cnt : int)
         ) =
        PPSet.fold selected_ppaths
          ~init:(qres.qr_total_ppaths, unselected_ppaths, None, qres.qr_exp_cnt)
          ~f:(fun (t_paths, acc_ppaths, r_opt, acc_cnt) ppath ->
            if Option.is_some r_opt
            then (t_paths, PPSet.add acc_ppaths ppath, r_opt, acc_cnt)
            else (
              let ( (new_t_paths :
                      (Res.PPath.t * Smt.Solver.satisfiability) list
                      ),
                    (expanded_ppaths : PPSet.t),
                    (r_opt : (PPath.t * Smt.Model.t) option)
                  ) =
                 naive_run_ppath_atomic_action cfg ppath
              in
              let (new_ppath_cnt : int) = PPSet.length expanded_ppaths in
              ( new_t_paths @ t_paths,
                PPSet.union acc_ppaths expanded_ppaths,
                r_opt,
                acc_cnt + new_ppath_cnt
              )
            )
        )
     in
     if Option.is_some qr_rft_ppath
     then
       {
         qres with
         qr_rft_flag = RF_r;
         qr_total_ppaths;
         qr_last_picked_paths = selected_ppaths;
         qr_exp_ppaths;
         qr_rft_ppath;
         qr_exp_cnt;
       }
     else { qres with qr_total_ppaths; qr_exp_ppaths; qr_exp_cnt }
   )
(* function naive_run_qres_atomic_action end *)

(* Result *********************************************************************)

let naive_run_res_atomic_action : Res.config -> Res.res -> Res.res =
   let open Res in
   fun cfg res ->
   {
     res with
     r_qr_lst =
       List.fold_right res.r_qr_lst
         ~f:(fun qres acc -> naive_run_qres_atomic_action cfg qres :: acc)
         ~init:[];
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

(******************************************************************************)
(******************************************************************************)
(* Refuting with Parametric Path Selection Enabled                            *)
(******************************************************************************)
(******************************************************************************)

let guided_run_qres_escape_condition = naive_run_qres_escape_condition

let guided_run_qres : Res.config -> pick_f:PickFun.t -> Res.qres -> Res.qres =
  fun cfg ~pick_f qres ->
  (* 1. Escape Conditions *)
  if (* 1.1. Escape when (Timeout || (R-flag <> RF_u) || (P-flag = PF_p)) *)
     guided_run_qres_escape_condition cfg qres
  then qres
  else if (* 1.2. If Size(exp-ppaths) == 0, set refuter-flag to "failed" *)
          PPSet.is_empty qres.qr_exp_ppaths
  then { qres with qr_rft_flag = RF_f }
  else (
    (* 2. Pick paths to expand *)
    let _ = Utils.Log.debug (fun m -> m "  Pick-Path Start") in
    let (picked_paths, unpicked_paths) : PPSet.t * PPSet.t =
       pick_f (cfg.cfg_smt_ctxt, cfg.cfg_smt_slvr) qres.qr_exp_ppaths
    in
    let _ =
       Utils.Log.debug (fun m ->
           m "  Pick-Path End >> #Picked / #Unpicked = %d / %d"
             (PPSet.length picked_paths)
             (PPSet.length unpicked_paths)
       )
    in
    let _ =
       if Option.is_none !Utils.Argument.status_interval
       then ()
       else (
         let _ = Utils.Log.debug (fun m -> m "  List of Picked Paths") in
         let _ =
            PPSet.iter picked_paths ~f:(fun pp ->
                Utils.Log.debug (fun m ->
                    m "\nPathSummary : %s\n      Score : %s"
                      (MState.get_summary pp.pp_mstate
                      |> MState.sexp_of_summary
                      |> Sexp.to_string
                      )
                      (pp.pp_score
                      |> List.sexp_of_t sexp_of_int
                      |> Sexp.to_string
                      )
                )
            )
         in
         ()
       )
    in
    (* 3. Expand picked paths *)
    let expanded_paths : PPSet.t =
       PPSet.fold picked_paths ~init:PPSet.empty ~f:(fun acc pp ->
           PPSet.union (expand_pp ~m_view:cfg.cfg_m_view pp) acc
       )
    in
    (* 4. For each expanded paths, check refutability *)
    let (total_ppaths, rft_ppath_opt)
          : (Res.PPath.t * Smt.Solver.satisfiability) list
            * (Res.PPath.t * Smt.Model.t) option =
       let f (tpl_acc, rftopt_acc) pp =
          (* 4.f.1. Check escape condition
                  - Check if already refuted path found
                  - No timeout check
          *)
          if Option.is_some rftopt_acc
          then (tpl_acc, rftopt_acc)
          else (
            (* 4.f.2. check refutability *)
            match refute cfg.cfg_smt_ctxt cfg.cfg_smt_slvr cfg.cfg_istrg pp with
            | (Some tp, Some mdl) -> (tp :: tpl_acc, Some (pp, mdl))
            | (Some tp, None)     -> (tp :: tpl_acc, None)
            | (None, _)           ->
              (tpl_acc, rftopt_acc) (* (None, None) case exists only *)
          )
       in
       PPSet.fold expanded_paths ~init:([], None) ~f
    in
    (* Last. return value construction *)
    let qr_total_ppaths = total_ppaths @ qres.qr_total_ppaths
    and qr_last_picked_paths = picked_paths
    and qr_exp_ppaths = PPSet.union expanded_paths unpicked_paths
    and qr_rft_ppath = rft_ppath_opt
    and qr_exp_cnt : int = PPSet.length expanded_paths + qres.qr_exp_cnt
    and qr_rft_flag : Res.refuter_flag =
       match rft_ppath_opt with
       | None   -> RF_u
       | Some _ -> RF_r
    in
    {
      qres with
      qr_rft_flag;
      qr_total_ppaths;
      qr_last_picked_paths;
      qr_exp_ppaths;
      qr_rft_ppath;
      qr_exp_cnt;
    }
  )

let guided_run_escape_condition = naive_run_escape_condition

let guided_run :
    Res.config ->
    pick_f_gen:(Res.res -> Tz.qid -> PickFun.t) ->
    Res.res ->
    Res.res =
  fun cfg ~pick_f_gen res ->
  let _ = Utils.Log.debug (fun m -> m "%s" (Res.string_of_res_rough cfg res)) in
  if guided_run_escape_condition cfg res
  then res
  else (
    let new_res : Res.res =
       {
         res with
         r_qr_lst =
           List.map res.r_qr_lst ~f:(fun qres ->
               guided_run_qres cfg ~pick_f:(pick_f_gen res qres.qr_qid) qres
           );
       }
    in
    new_res (* guided_run cfg ~pick_f_gen new_res *)
  )
