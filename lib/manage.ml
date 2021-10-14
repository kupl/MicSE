(* Manage : Manager for MicSE *)

exception ManageError of string

open! Core

(******************************************************************************)
(******************************************************************************)
(* Initial Procedure                                                          *)
(******************************************************************************)
(******************************************************************************)

let initial_prove_run_res_atomic_action : Res.config -> Res.res -> Res.res =
   let open Res in
   fun cfg res ->
   let (r_qr_lst : Res.qres list) =
      List.map res.r_qr_lst ~f:(fun qres ->
          Prove.naive_run_qres_atomic_action cfg res.r_inv qres
      )
   in
   { res with r_qr_lst }
(* function initial_prove_run_res_atomic_action end *)

let initial_refute_run_res_atomic_action : Res.config -> Res.res -> Res.res =
   let open Res in
   fun cfg res ->
   let (r_qr_lst : Res.qres list) =
      List.map res.r_qr_lst ~f:(fun qres ->
          let (qr_exp_ppaths : PPSet.t) =
             Refute.filter_sat_ppaths cfg.cfg_smt_ctxt cfg.cfg_smt_slvr
               qres.qr_exp_ppaths
          in
          let ( (qr_total_ppaths :
                  (Res.PPath.t * Smt.Solver.satisfiability) list
                  ),
                (qr_rft_ppath : (Res.PPath.t * Smt.Model.t) option)
              ) =
             PPSet.fold qr_exp_ppaths ~init:(qres.qr_total_ppaths, None)
               ~f:(fun (t_paths, r_opt) eppath ->
                 if Option.is_some r_opt
                 then (t_paths, r_opt)
                 else (
                   let ( (total_path_opt :
                           (PPath.t * Smt.Solver.satisfiability) option
                           ),
                         (model_opt : Smt.Model.t option)
                       ) =
                      Refute.refute cfg.cfg_smt_ctxt cfg.cfg_smt_slvr
                        cfg.cfg_istrg eppath
                   in
                   if Option.is_none total_path_opt
                   then (t_paths, r_opt)
                   else (
                     let (total_path : PPath.t * Smt.Solver.satisfiability) =
                        Option.value_exn total_path_opt
                     in
                     ( total_path :: t_paths,
                       Option.map model_opt ~f:(fun model ->
                           (fst total_path, model)
                       )
                     )
                   )
                 )
             )
          in
          if Option.is_some qr_rft_ppath
          then
            {
              qres with
              qr_rft_flag = RF_r;
              qr_exp_ppaths;
              qr_total_ppaths;
              qr_rft_ppath;
            }
          else qres
      )
   in
   { res with r_qr_lst }
(* function initial_refute_run_res_atomic_action end *)

(******************************************************************************)
(******************************************************************************)
(* Entry Point                                                                *)
(******************************************************************************)
(******************************************************************************)

let naive_run_escape_condition : Res.config -> Res.res -> bool =
   let open Res in
   fun { cfg_timer; cfg_memory; _ } { r_qr_lst; _ } ->
   if (* 1. Timeout *)
      Utils.Time.is_timeout cfg_timer
   then (
     Utils.Log.debug (fun m -> m "naive_run_escape_condition : TIMEOUT!!!");
     true
   )
   else if (* 2. Memoryout *)
           Utils.Memory.is_memoryout cfg_memory
   then (
     Utils.Log.debug (fun m -> m "naive_run_escape_condition : MEMORYOUT!!!");
     true
   )
   else if (* 2. Every queries are PF_p or PF_f or RF_r or RF_f *)
           List.for_all r_qr_lst ~f:(fun { qr_prv_flag; qr_rft_flag; _ } ->
               equal_prover_flag qr_prv_flag PF_p
               || equal_refuter_flag qr_rft_flag RF_r
               || equal_prover_flag qr_prv_flag PF_f
                  && equal_refuter_flag qr_rft_flag RF_f
           )
   then (
     Utils.Log.debug (fun m ->
         m "naive_run_escape_condition : ALL NON-UNKNOWN!!!"
     );
     true
   )
   else false
(* function naive_run_escape_condition end *)

let naive_run : Res.config -> Res.res -> Res.res =
   let log_report : Res.config -> Res.res -> unit =
     fun cfg res ->
     Utils.Log.info (fun m -> m "> Report: %s" (Res.string_of_res_rough cfg res))
     (* inner-function log_report end *)
   in
   let rec naive_run_i : Res.config -> Res.res -> Res.res =
     fun cfg res ->
     if naive_run_escape_condition cfg res
     then res
     else (
      let _ = log_report cfg res in
       let _ = Utils.Log.info (fun m -> m "> Prover Turn Start") in
       let (p_res : Res.res) =
          if Prove.naive_run_escape_condition cfg res
          then res
          else Prove.naive_run_res_atomic_action cfg res
       in
       let _ = Utils.Log.info (fun m -> m "> Prover Turn End") in
       let _ = log_report cfg p_res in
       let _ = Utils.Log.info (fun m -> m "> Refuter Turn Start") in
       let (r_res : Res.res) =
          if Refute.naive_run_escape_condition cfg p_res
          then p_res
          else Refute.naive_run_res_atomic_action cfg p_res
       in
       let _ = Utils.Log.info (fun m -> m "> Refuter Turn End") in
       naive_run_i cfg r_res
     )
   in
   (* inner-function naive_run_i end *)
   fun cfg res ->
      let _ = log_report cfg res in
   let _ = Utils.Log.info (fun m -> m "> Prover Turn Start") in
   let (p_res : Res.res) = initial_prove_run_res_atomic_action cfg res in
   let _ = Utils.Log.info (fun m -> m "> Prover Turn End") in
   let _ = log_report cfg p_res in
   let _ = Utils.Log.info (fun m -> m "> Refuter Turn Start") in
   let (r_res : Res.res) = initial_refute_run_res_atomic_action cfg p_res in
   let _ = Utils.Log.info (fun m -> m "> Refuter Turn End") in
   let (n_res : Res.res) = naive_run_i cfg r_res in
   let _ = log_report cfg n_res in
   n_res
(* function naive_run end *)
