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
          let (r_opt : (Res.PPath.t * Smt.Model.t) option) =
             PPSet.fold qres.qr_exp_ppaths ~init:None ~f:(fun r_opt eppath ->
                 if Option.is_some r_opt
                 then r_opt
                 else
                   Refute.refute cfg.cfg_smt_ctxt cfg.cfg_smt_slvr cfg.cfg_istrg
                     eppath
             )
          in
          if Option.is_some r_opt
          then { qres with qr_rft_flag = RF_r; qr_rft_ppath = r_opt }
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
   let rec naive_run_i : Res.config -> Res.res -> Res.res =
     fun cfg res ->
     if naive_run_escape_condition cfg res
     then res
     else (
       let _ =
          Utils.Log.info (fun m ->
              m "> Mid-Report: %s" (Res.string_of_res_rough cfg res)
          )
       in
       let _ = Utils.Log.info (fun m -> m "> Prover Turn Start") in
       let (p_res : Res.res) =
          if Prove.naive_run_escape_condition cfg res
          then res
          else Prove.naive_run_res_atomic_action cfg res
       in
       let _ = Utils.Log.info (fun m -> m "> Prover Turn End") in
       let _ =
          Utils.Log.info (fun m ->
              m "> Mid-Report: %s" (Res.string_of_res_rough cfg p_res)
          )
       in
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
   let _ =
      Utils.Log.info (fun m ->
          m "> Init-Report: %s" (Res.string_of_res_rough cfg res)
      )
   in
   let _ = Utils.Log.info (fun m -> m "> Prover Turn Start") in
   let (p_res : Res.res) = initial_prove_run_res_atomic_action cfg res in
   let _ = Utils.Log.info (fun m -> m "> Prover Turn End") in
   let _ =
      Utils.Log.info (fun m ->
          m "> Mid-Report: %s" (Res.string_of_res_rough cfg p_res)
      )
   in
   let _ = Utils.Log.info (fun m -> m "> Refuter Turn Start") in
   let (r_res : Res.res) = initial_refute_run_res_atomic_action cfg p_res in
   let _ = Utils.Log.info (fun m -> m "> Refuter Turn End") in
   naive_run_i cfg r_res
(* function naive_run end *)
