(* Prove : Naïve prover for verification *)

exception PrvError of string

open! Core

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(* Set of Tz.mich_v Tz.cc *)
module MVSet = Set.Make (Tz.MichVCC_cmp)

(* Set of Tz.mich_f *)
module MFSet = Set.Make (Tz.MichF_cmp)

(* Map of Tz.mich_cut_info *)
module MCIMap = Map.Make (Tz.MichCutInfo_cmp)

(* Map of Tz.r_mich_cut_info *)
module RMCIMap = Map.Make (Tz.RMichCutInfo_cmp)

(* Set of Tz.sym_state *)
module SSet = Set.Make (Tz.SymState_cmp)

(* Set of Inv.mci_pair *)
module MPMap = Map.Make (Inv.MciPair_cmp)

(* Set of Inv.cand_pair *)
module CPSet = Set.Make (Inv.CandPair_cmp)

(******************************************************************************)
(******************************************************************************)
(* Utility                                                                    *)
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

(******************************************************************************)
(******************************************************************************)
(* Prover                                                                     *)
(******************************************************************************)
(******************************************************************************)

let rec combinate :
    SSet.t ->
    Inv.failed_cp ->
    Inv.inv_map ->
    Inv.cand_map ->
    Inv.inv_map list ->
    Inv.inv_map ->
    Inv.inv_map list =
   let open Inv in
   let open Res in
   let (top_k : int) = 20 in
   let (threshold : int) = 100 in
   fun bsset failed_cp cinv targets combs acc_imap ->
   let next_comb :
       Inv.cand_map -> Inv.inv_map list -> Inv.inv_map -> Inv.inv_map list =
      combinate bsset failed_cp cinv
   in
   if List.length combs >= threshold
   then combs
   else if RMCIMap.is_empty targets
   then if equal_inv_map cinv acc_imap then combs else acc_imap :: combs
   else (
     (* 1. Target MCI for combinate *)
     let (rmci : Tz.r_mich_cut_info) = List.hd_exn (RMCIMap.keys targets) in
     let (remains : cand_map) = RMCIMap.remove targets rmci in
     (* 2. Get candidates from target *)
     let (cands : MFSet.t list) =
        find_cand_by_rmci targets rmci
        |> CMap.to_alist
        |> List.sort ~compare:(fun (_, s1) (_, s2) -> compare_int s1 s2)
        |> List.map ~f:fst
        |> List.rev
     in
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
     let (new_combs : Inv.inv_map list) =
        List.fold target ~init:combs ~f:(fun acc_comb imap ->
            next_comb remains acc_comb imap
        )
     in
     new_combs
   )
(* function combinate end *)

(******************************************************************************)
(******************************************************************************)
(* Naïve Run                                                                  *)
(******************************************************************************)
(******************************************************************************)

(* Qurey Result ***************************************************************)

let naive_run_qres_atomic_action : Res.config -> Res.qres -> Res.res -> Res.res
    =
   let open Res in
   fun _ qres res ->
   let (new_qres : qres) = qres in
   { res with r_qr_lst = new_qres :: res.r_qr_lst }
(* function naive_run_qres_atomic_action end *)

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

(* Result *********************************************************************)

let naive_run_res_atomic_action : Res.config -> Res.res -> Res.res =
   let open Res in
   fun cfg res ->
   List.fold_right res.r_qr_lst
     ~f:(fun qres res ->
       if naive_run_qres_escape_condition cfg qres
       then { res with r_qr_lst = qres :: res.r_qr_lst }
       else naive_run_qres_atomic_action cfg qres res)
     ~init:{ res with r_qr_lst = [] }
(* function naive_run_res_atomic_action end *)

let naive_run_res_escape_condition : Res.config -> Res.res -> bool =
   let open Res in
   fun { cfg_timer; _ } { r_qr_lst; _ } ->
   if (* 1. Timeout *)
      Utils.Time.is_timeout cfg_timer
   then (
     Utils.Log.debug (fun m ->
         m "Prove : naive_run_res_escape_condition : TIMEOUT!!!"
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

(* Entry Point ****************************************************************)

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
(* function naive_run end *)
