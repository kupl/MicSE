(* Prove : Naïve prover for verification *)

exception PrvError of string

open! Core

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(* Set of Tz.r_mich_cut_info *)
module RMCISet = Set.Make (Tz.RMichCutInfo_cmp)

(* Map of Tz.r_mich_cut_info *)
module RMCIMap = Map.Make (Tz.RMichCutInfo_cmp)

(* Set of Tz.sym_state *)
module SSet = Set.Make (Tz.SymState_cmp)

(******************************************************************************)
(******************************************************************************)
(* Prover                                                                     *)
(******************************************************************************)
(******************************************************************************)

let check_failed :
    SSet.t ->
    Inv.inductive_info ->
    RMCISet.t ->
    Tz.r_mich_cut_info ->
    Inv.inv_map ->
    bool =
   let open Tz in
   let open TzUtil in
   let open Inv in
   fun bsset idts picked target imap ->
   let is_target : r_mich_cut_info * r_mich_cut_info -> bool =
     fun (rmci1, rmci2) ->
     equal_r_mich_cut_info target rmci1 && RMCISet.mem picked rmci2
   in
   SSet.exists bsset ~f:(fun bs ->
       let (r_start : r_mich_cut_info) = get_reduced_mci bs.ss_start_mci in
       let (r_block : r_mich_cut_info) = get_reduced_mci bs.ss_block_mci in
       if (* only the target mci *)
          is_target (r_start, r_block) || is_target (r_block, r_start)
       then (
         let (cp_start : cand) = find_inv_by_rmci imap r_start in
         let (cp_block : cand) = find_inv_by_rmci imap r_block in
         is_already_failed idts bs { cp_start; cp_block }
       )
       else false
   )
(* function check_failed end *)

let rec combinate :
    Tz.qid ->
    SSet.t ->
    Inv.inductive_info ->
    Inv.inv_map ->
    Inv.cand_map ->
    Inv.inv_map ->
    Inv.inv_map option =
   let open Inv in
   fun qid bsset idts cinv targets acc_imap ->
   let next_comb : Inv.cand_map -> Inv.inv_map -> Inv.inv_map option =
      combinate qid bsset idts cinv
   in
   if RMCIMap.is_empty targets
   then if equal_inv_map cinv acc_imap then None else Some acc_imap
   else (
     (* 1. Target MCI for combinate *)
     let (rmci : Tz.r_mich_cut_info) =
        RMCIMap.keys targets |> List.permute |> List.hd_exn
     in
     let (c_inv_f : cand) = find_inv_by_rmci cinv rmci in
     let (cands : cand list) =
        c_inv_f
        :: find_ordered_cand_by_rmci ~remove_unflaged:true
             ~remove_not_precond:true targets rmci qid
     in
     let (remains : cand_map) = RMCIMap.remove targets rmci in
     let (picked : RMCISet.t) =
        RMCISet.diff
          (RMCIMap.keys cinv |> RMCISet.of_list)
          (RMCIMap.keys remains |> RMCISet.of_list)
     in
     (* 2. Combinate candidates *)
     let (new_imap_opt : Inv.inv_map option) =
        List.fold cands ~init:None ~f:(fun new_imap_opt fset ->
            if Option.is_some new_imap_opt
            then new_imap_opt
            else (
              let (updated_inv : inv_map) =
                 update_inv_map acc_imap ~key:rmci ~value:fset
              in
              if check_failed bsset idts picked rmci updated_inv
              then new_imap_opt
              else next_comb remains updated_inv
            )
        )
     in
     new_imap_opt
   )
(* function combinate end *)

let check_inductiveness_of_ss :
    Smt.Ctx.t ->
    Smt.Solver.t ->
    Tz.sym_state ->
    Inv.inv_map ->
    Tz.sym_state * bool =
   let open Smt in
   let open Tz in
   let open Vc in
   fun ctx slvr bs imap ->
   let (vc : mich_f) = gen_inductiveness_vc imap bs |> TzUtil.opt_mf in
   let ((vld : Solver.validity), _) = check_val ctx slvr vc in
   if not (Solver.is_val vld) then (bs, false) else (bs, true)
(* function check_inductiveness_of_ss end *)

let check_inductiveness :
    Smt.Ctx.t ->
    Smt.Solver.t ->
    SSet.t ->
    Inv.inductive_info ->
    Inv.inv_map ->
    Inv.inv_map option * (Tz.sym_state * bool) list =
   let open Tz in
   let open Inv in
   fun ctx slvr bsset idtmap imap ->
   let (threshold : int) = SSet.length bsset in
   SSet.to_list bsset
   |> List.permute
   |> List.fold
        ~init:((Some imap, []), 0)
        ~f:(fun ((imap_opt, idts), cnt) bs ->
          if cnt > threshold && Option.is_none imap_opt
          then ((imap_opt, idts), cnt)
          else (
            let (cand_start : cand) = find_inv imap bs.ss_start_mci in
            let (cand_block : cand) = find_inv imap bs.ss_block_mci in
            let (cp : cand_pair) = cvt_cand_pair (cand_start, cand_block) in
            if is_already_succeeded idtmap bs cp
            then ((imap_opt, idts), cnt)
            else (
              let (idt_pair : Tz.sym_state * bool) =
                 check_inductiveness_of_ss ctx slvr bs imap
              in
              if snd idt_pair
              then ((imap_opt, idt_pair :: idts), cnt + 1)
              else ((None, idt_pair :: idts), cnt + 1)
            )
          ))
   |> fst
(* function check_inductiveness end *)

let check_number_of_cands : Tz.qid -> Inv.cand_map -> bool =
   let open Inv in
   fun qid cmap ->
   RMCIMap.existsi cmap ~f:(fun ~key ~data:_ ->
       let (cands : cand list) =
          find_ordered_cand_by_rmci ~remove_unflaged:true
            ~remove_not_precond:true cmap key qid
       in
       List.length cands > 0
   )
(* function check_number_of_cands end *)

let add_inductive_info :
    Inv.inductive_info ->
    (Tz.sym_state * bool) list ->
    Inv.inv_map ->
    Inv.inductive_info =
   let open Inv in
   fun idtmap si_lst comb ->
   List.fold si_lst ~init:idtmap ~f:(fun idtmap (ss, inductiveness) ->
       let (cand_start : cand) = find_inv comb ss.ss_start_mci in
       let (cand_block : cand) = find_inv comb ss.ss_block_mci in
       let (cp : cand_pair) = cvt_cand_pair (cand_start, cand_block) in
       let (new_idts : inductive_info) =
          add_inductiveness idtmap (ss, cp, inductiveness)
       in
       new_idts
   )
(* function add_inductiveness end *)

let print_invariant : Inv.inv_map -> unit =
   let open Inv in
   fun imap ->
   let _ =
      Utils.Log.info (fun m ->
          m "> Invariant is updated:\n%s\n"
            (List.map (RMCIMap.keys imap) ~f:(fun rmci ->
                 let (inv : cand) = find_inv_by_rmci imap rmci in
                 Printf.sprintf "\tRMCI: %s\n\t\t%s"
                   (rmci |> Tz.sexp_of_r_mich_cut_info |> SexpUtil.to_string)
                   (inv
                   |> fmla_of_cand_pre
                   |> TzUtil.opt_mf
                   |> Tz.sexp_of_mich_f
                   |> SexpUtil.to_string
                   )
             )
            |> String.concat ~sep:"\n"
            )
      )
   in
   ()
(* function print_invariant end *)

let prove : Smt.Ctx.t -> Smt.Solver.t -> Inv.inv_map -> SSet.t -> SSet.t =
   let open Res in
   let open Smt in
   let open Vc in
   fun ctx slvr imap unk_qs ->
   SSet.filter unk_qs ~f:(fun qs ->
       let (vc : Tz.mich_f) = gen_query_vc imap qs |> TzUtil.opt_mf in
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

(* Result *********************************************************************)

let naive_run_find_inv_escape_condition :
    Res.config -> Res.qres * Inv.inv_map option list -> bool =
   let open Res in
   fun { cfg_timer; cfg_memory; _ } ({ qr_prv_flag; qr_rft_flag; _ }, io_lst) ->
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
   else if (* 4. Invariant is already found *)
           List.exists io_lst ~f:(fun io -> Option.is_some io)
   then true
   else false
(* function naive_run_res_escape_condition end *)

let naive_run_res_atomic_action : Res.config -> Res.res -> Res.res =
   let open Res in
   let open Inv in
   fun cfg res ->
   (* 1. Find invariant *)
   let rec find_inv :
       inductive_info * int ->
       (inv_map option, unit) result * inductive_info * int =
     fun (idts, iter) ->
     (* 1-1-1. Make combination and check inductiveness in each query *)
     let ((io_lst : inv_map option list), (new_idts : inductive_info)) =
        List.permute res.r_qr_lst
        |> List.fold ~init:([], idts) ~f:(fun (io_lst, new_idts) qres ->
               if (* 1-1-1-1. If new invariant is already found, skip other procedure *)
                  naive_run_find_inv_escape_condition cfg (qres, io_lst)
               then (io_lst, new_idts)
               else (
                 (* 1-1-1-2. Make combination from precondition of query *)
                 let (comb_opt : inv_map option) =
                    if check_number_of_cands qres.qr_qid res.r_cands
                    then
                      combinate qres.qr_qid cfg.cfg_se_res.sr_blocked new_idts
                        res.r_inv res.r_cands res.r_inv
                    else None
                 in
                 if (* 1-1-1-3. If combination cannot be generated, record its failure *)
                    Option.is_none comb_opt
                 then (io_lst, new_idts)
                 else (
                   (* 1-1-1-4. Else, check combination's inductiveness *)
                   let (comb : inv_map) = Option.value_exn comb_opt in
                   let ( (imap_opt : inv_map option),
                         (si_lst : (Tz.sym_state * bool) list)
                       ) =
                      check_inductiveness cfg.cfg_smt_ctxt cfg.cfg_smt_slvr
                        cfg.cfg_se_res.sr_blocked new_idts comb
                   in
                   let (updated_idt : inductive_info) =
                      add_inductive_info new_idts si_lst comb
                   in
                   (imap_opt :: io_lst, updated_idt)
                 )
               )
           )
     in
     (* 1-1-2. Reduce result of combinating and inductiveness checking *)
     let ((ior : (inv_map option, unit) result), (new_iter : int)) =
        List.fold io_lst ~init:(Error (), iter)
          ~f:(fun (ior, new_iter) new_io ->
            let (io : inv_map option) =
               Option.value (Result.ok ior) ~default:None
            in
            match (new_io, io) with
            | (Some _, Some _) ->
              PrvError
                "naive_run_res_atomic_action : find_inv : ior, new_iter : wrong process"
              |> raise
            | _                -> (Ok new_io, new_iter + 1)
        )
     in
     match ior with
     | Ok None when new_iter < cfg.cfg_comb_k ->
       (* 1-1-3-1. If iteration is not enough and there is no new invariant, then repeat again *)
       find_inv (new_idts, new_iter)
     | _ ->
       (* 1-1-3-1. Else, escape the loop *)
       (ior, new_idts, new_iter)
   in
   (* 1-1. Make combinations with invariant candidates and check its inductiveness *)
   let ( (ior : (inv_map option, unit) result),
         (r_idts : inductive_info),
         (new_iter : int)
       ) =
      find_inv (res.r_idts, 0)
   in
   (* 1-2. Add the number of iteration *)
   let (r_comb_cnt : int) = res.r_comb_cnt + new_iter in
   (* 2. Check the result of invariant search *)
   if (* 2-1. There is no combination can make *)
      Result.is_error ior
   then (
     (* 2-1-1. Check all that prove is failed *)
     let (r_qr_lst : qres list) =
        List.map res.r_qr_lst ~f:(fun qres ->
            if Res.equal_prover_flag qres.qr_prv_flag PF_u
            then { qres with qr_prv_flag = PF_f }
            else qres
        )
     in
     { res with r_qr_lst; r_idts; r_comb_cnt }
   )
   else (
     let (imap_opt : inv_map option) = Option.value_exn (Result.ok ior) in
     if (* 2-2. There is combinations, but there is no invariant *)
        Option.is_none imap_opt
     then { res with r_idts; r_comb_cnt }
     else (
       (* 2-2. There is combinations, and there is invariant *)
       (* 2-2-1. Get invariant *)
       let (r_inv : inv_map) = Option.value_exn imap_opt in
       let _ = print_invariant r_inv in
       (* 2-2-2. Update candidates *)
       let (r_cands : cand_map) =
          strengthen_cand_map res.r_cands r_inv
            ~is_cand_sat:(Vc.is_cand_sat cfg.cfg_smt_ctxt cfg.cfg_smt_slvr)
            ~do_cand_sat_istrg:
              (Vc.do_cand_sat_istrg cfg.cfg_smt_ctxt cfg.cfg_smt_slvr
                 cfg.cfg_istrg cfg.cfg_istate
              )
       in
       (* 2-2-3. Prove all with new invariant *)
       let (r_qr_lst : qres list) =
          List.map res.r_qr_lst ~f:(fun qres ->
              naive_run_qres_atomic_action cfg r_inv qres
          )
       in
       { r_qr_lst; r_inv; r_cands; r_idts; r_comb_cnt }
     )
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
         m "Prove : naive_run_escape_condition : TIMEOUT!!!"
     );
     true
   )
   else if (* 2. Memoryout *)
           Utils.Memory.is_memoryout cfg_memory
   then (
     Utils.Log.debug (fun m ->
         m "Prove : naive_run_escape_condition : MEMORYOUT!!!"
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
         m "Prove : naive_run_escape_condition : ALL NON-UNKNOWN!!!"
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
       let (p_res : Res.res) = naive_run_res_atomic_action cfg res in
       let _ = Utils.Log.info (fun m -> m "> Prover Turn End") in
       naive_run_i cfg p_res
     )
   in
   (* inner-function naive_run_i end *)
   fun cfg res ->
   let _ = log_report cfg res in
   let (p_res : Res.res) =
      let (r_qr_lst : Res.qres list) =
         List.map res.r_qr_lst ~f:(fun qres ->
             naive_run_qres_atomic_action cfg res.r_inv qres
         )
      in
      { res with r_qr_lst }
   in
   naive_run_i cfg p_res
(* function naive_run end *)
