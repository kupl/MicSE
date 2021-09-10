(* Result of MicSE *)

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

(* Set of set of Tz.mich_f *)
module MFSSet = Set.Make (MFSet)

(* Map of Tz.mich_cut_info *)
module MCIMap = Map.Make (Tz.MichCutInfo_cmp)

(* Map of Tz.r_mich_cut_info *)
module RMCIMap = Map.Make (Tz.RMichCutInfo_cmp)

(* Set of Tz.sym_state *)
module SSet = Set.Make (Tz.SymState_cmp)

(* Set of Inv.inv_map *)
module ISet = Set.Make (Inv.InvMap_cmp)

(******************************************************************************)
(******************************************************************************)
(* Types                                                                      *)
(******************************************************************************)
(******************************************************************************)

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

type worklist = {
  wl_combs : Inv.inv_map list;
  wl_failcp : Inv.failed_cp;
  wl_comb_cnt : int;
}
[@@deriving sexp, compare, equal]

type res = {
  r_qr_lst : qres list;
  r_inv : Inv.inv_map;
  r_cands : Inv.cand_map;
  r_wlst : worklist;
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

(******************************************************************************)
(******************************************************************************)
(* Initialization                                                             *)
(******************************************************************************)
(******************************************************************************)

let init_qres : Tz.mich_cut_info -> SSet.t -> qres =
  fun qr_qid qr_unk_qs ->
  {
    qr_qid;
    qr_prv_flag = PF_u;
    qr_rft_flag = RF_u;
    qr_unk_qs;
    qr_exp_ppaths = PPSet.map qr_unk_qs ~f:PPath.t_of_ss;
    qr_rft_ppath = None;
    qr_exp_cnt = SSet.length qr_unk_qs;
  }
(* function init_qres end *)

let init_worklist : unit -> worklist =
  fun () ->
  { wl_combs = []; wl_failcp = Inv.gen_initial_failed_cp (); wl_comb_cnt = 0 }
(* function init_worklist end *)

let init_res : config -> res =
   let open Se in
   fun { cfg_se_res; cfg_istrg; _ } ->
   let (mci_queries : SSet.t MCIMap.t) =
      SSet.fold cfg_se_res.sr_queries ~init:MCIMap.empty ~f:(fun acc qs ->
          MCIMap.update acc qs.ss_block_mci ~f:(function
          | Some s -> SSet.add s qs
          | None   -> SSet.singleton qs
          )
      )
   in
   let (qresl : qres list) =
      MCIMap.mapi mci_queries ~f:(fun ~key ~data -> init_qres key data)
      |> MCIMap.to_alist
      |> List.map ~f:snd
   in
   {
     r_qr_lst = qresl;
     r_inv = Inv.gen_true_inv_map cfg_se_res;
     r_cands = Inv.gen_initial_cand_map cfg_se_res cfg_istrg MVSet.empty;
     r_wlst = init_worklist ();
   }
(* function init_res end *)

let init_config :
    Tz.mich_v Tz.cc option -> Se.se_result -> Tz.sym_state -> config =
  fun cfg_istrg_opt cfg_se_res cfg_istate ->
  let (cfg_istrg : Tz.mich_v Tz.cc) =
     match cfg_istrg_opt with
     | Some v -> v
     | None   -> failwith "ExecFlow : config_base : cfg_istrg = None"
  in
  let (cfg_smt_ctxt : Smt.Ctx.t) = Vc.gen_ctx () in
  {
    cfg_timer = Utils.Time.create ~budget:!Utils.Argument.total_timeout ();
    cfg_memory = Utils.Memory.create ~budget:!Utils.Argument.memory_bound ();
    cfg_istate;
    cfg_istrg;
    cfg_se_res;
    cfg_m_view =
      Se.SSGraph.construct_mci_view ~basic_blocks:cfg_se_res.sr_blocked;
    cfg_imap =
      Igdt.get_igdts_map cfg_se_res.sr_blocked cfg_istrg Igdt.MVSet.empty;
    cfg_smt_ctxt;
    cfg_smt_slvr = Vc.gen_solver cfg_smt_ctxt;
  }
(* function init_config end *)

(******************************************************************************)
(******************************************************************************)
(* Utility                                                                    *)
(******************************************************************************)
(******************************************************************************)

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
