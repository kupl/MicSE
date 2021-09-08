(* Result of MicSE *)

open! Core

(* Set of Tz.mich_f *)
module MFSet = Set.Make (Tz.MichF_cmp)

(* Set of set of Tz.mich_f *)
module MFSSet = Set.Make (MFSet)

(* Map of Tz.r_mich_cut_info *)
module RMCIMap = Map.Make (Tz.RMichCutInfo_cmp)

(* Set of Tz.sym_state *)
module SSet = Set.Make (Tz.SymState_cmp)

type prover_flag =
  | PF_p (* proved  *)
  | PF_u (* unknown *)
  | PF_f
(* failed - no invariant candidate combination left *)
[@@deriving sexp, compare, equal]

type refuter_flag =
  | RF_r (* refuted *)
  | RF_u (* unknown *)
  | RF_f
(* failed - no ppath left to check *)
[@@deriving sexp, compare, equal]

module PPath = struct
  type t = {
    pp_mstate : MState.t;
    pp_goalst : MFSSet.t RMCIMap.t list;
    pp_score : int;
  }
  [@@deriving sexp, compare, equal]

  let t_of_ss : Tz.sym_state -> t =
    (fun ss -> { pp_mstate = MState.init ss; pp_goalst = []; pp_score = 0 })
end

module PPSet = Set.Make (PPath)

type qres = {
  qr_qid : Tz.mich_cut_info;
  (* Overall Status *)
  qr_prv_flag : prover_flag;
  qr_rft_flag : refuter_flag;
  (* Query States *)
  qr_unk_qs : SSet.t;
  (* Partial Paths and Invariant Candidates *)
  qr_exp_ppaths : PPSet.t;
  qr_rft_ppath : (PPath.t * Smt.Model.t) option;
  (* Count expanding_ppaths *)
  qr_exp_cnt : int;
}
[@@deriving sexp, compare, equal]

type res = {
  r_qr_lst : qres list;
  r_inv : Inv.inv_map;
  r_cand : Inv.cand_map;
  r_failcp : Inv.failed_cp;
  r_comb_cnt : int;
}
[@@deriving sexp, compare, equal]

type config = {
  (* Execution configuration *)
  cfg_timer : Utils.Time.t;
  cfg_memory : Utils.Memory.t;
  (* Information from symbolic execution *)
  cfg_istate : Tz.sym_state;
  cfg_istrg : Tz.mich_v Tz.cc;
  cfg_se_res : Se.se_result;
  cfg_m_view : Se.SSGraph.mci_view;
  (* Ingrdients for invariant synthesis *)
  cfg_imap : Igdt.igdts_map;
  (* Environment for SMT solver *)
  cfg_smt_ctxt : Smt.Ctx.t;
  cfg_smt_slvr : Smt.Solver.t;
}
[@@deriving sexp, compare, equal]

let string_of_res_rough_in_refuter_perspective : config -> res -> string =
   let soi = string_of_int in
   fun cfg res ->
   let t = Utils.Time.string_of_elapsed_time cfg.cfg_timer in
   let (p, r, u, f, c, e) =
      List.fold res.r_qr_lst ~init:(0, 0, 0, 0, 0, 0)
        ~f:(fun (p, r, u, f, c, e) qr ->
          let c = c + PPSet.length qr.qr_exp_ppaths in
          let e = e + qr.qr_exp_cnt in
          match (qr.qr_prv_flag, qr.qr_rft_flag) with
          (* WARNING : Be aware of match-with order *)
          | (PF_p, _) -> (p + 1, r, u, f, c, e)
          | (_, RF_r) -> (p, r + 1, u, f, c, e)
          | (PF_f, RF_f) -> (p, r, u, f + 1, c, e)
          | _ -> (p, r, u + 1, f, c, e)
      )
   in
   let (p, r, u, f, c, e) = (soi p, soi r, soi u, soi f, soi c, soi e) in
   let tstr = "Time = " ^ t in
   let prufstr = "P/R/U/F = " ^ String.concat ~sep:" / " [ p; r; u; f ] in
   let cstr = "expanding-ppath = " ^ c in
   let estr = "expanding-ppath-acc = " ^ e in
   String.concat ~sep:" , " [ tstr; prufstr; cstr; estr ]
