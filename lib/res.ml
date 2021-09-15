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
module MFSSet = Set.Make (Tz.MFSet)

(* Map of Tz.mich_cut_info *)
module MCIMap = Map.Make (Tz.MichCutInfo_cmp)

(* Set of Tz.sym_state *)
module SSet = Set.Make (Tz.SymState_cmp)

(* Set of Inv.inv_map *)
module InvSet = Set.Make (Inv.InvMap_cmp)

(* Map of MState.summary *)
module SMYMap = Map.Make (MState.SMY_cmp)

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
    pp_score : int;
    pp_checked : bool;
  }
  [@@deriving sexp, compare, equal]

  let t_of_ss : Tz.sym_state -> t =
    (fun ss -> { pp_mstate = MState.init ss; pp_score = 0; pp_checked = false })
  (* function t_of_ss end *)

  let t_of_ms : MState.t -> t =
    (fun ms -> { pp_mstate = ms; pp_score = 0; pp_checked = false })
  (* function t_of_ms end *)
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
  qr_prec_map : MFSSet.t SMYMap.t;
  qr_rft_ppath : (PPath.t * Smt.Model.t) option;
  (* Count expanding_ppaths *)
  qr_exp_cnt : int;
}
[@@deriving sexp, compare, equal]

module QRes_cmp = struct
  type t = qres [@@deriving sexp, compare]
end

type worklist = {
  wl_combs : InvSet.t;
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
    qr_prec_map = SMYMap.empty;
    qr_rft_ppath = None;
    qr_exp_cnt = SSet.length qr_unk_qs;
  }
(* function init_qres end *)

let init_worklist : unit -> worklist =
  fun () ->
  {
    wl_combs = InvSet.empty;
    wl_failcp = Inv.gen_initial_failed_cp ();
    wl_comb_cnt = 0;
  }
(* function init_worklist end *)

let init_res : config -> res =
   let open Se in
   fun { cfg_se_res; cfg_imap; _ } ->
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
     r_cands = Inv.gen_initial_cand_map cfg_imap;
     r_wlst = init_worklist ();
   }
(* function init_res end *)

let init_config :
    Tz.mich_i Tz.cc ->
    Tz.mich_v Tz.cc option ->
    Se.se_result ->
    Tz.sym_state ->
    config =
  fun cfg_code cfg_istrg_opt cfg_se_res cfg_istate ->
  let (mv_literal_set : MVSet.t) = TzUtil.scrap_code_literals cfg_code in
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
    cfg_imap = Igdt.get_igdts_map cfg_se_res.sr_blocked cfg_istrg mv_literal_set;
    cfg_smt_ctxt;
    cfg_smt_slvr = Vc.gen_solver cfg_smt_ctxt;
  }
(* function init_config end *)

(******************************************************************************)
(******************************************************************************)
(* Utility                                                                    *)
(******************************************************************************)
(******************************************************************************)

module QRSet = Set.Make (QRes_cmp)

type qres_classified = {
  (* proved           *)
  qrc_p : QRSet.t;
  (* refuted          *)
  qrc_r : QRSet.t;
  (* error case       *)
  qrc_err : QRSet.t;
  (* unknown-unknown  *)
  qrc_uu : QRSet.t;
  (* unknown-failed   *)
  qrc_uf : QRSet.t;
  (* failed-unknown   *)
  qrc_fu : QRSet.t;
  (* failed-failed    *)
  qrc_ff : QRSet.t;
}

let string_of_res_rough : config -> res -> string =
   let soi = string_of_int in
   fun cfg res ->
   let t = Utils.Time.string_of_elapsed_time cfg.cfg_timer in
   let m = Utils.Memory.string_of_used_memory cfg.cfg_memory in
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
   let b = soi res.r_wlst.wl_comb_cnt in
   let tstr = "Time = " ^ t in
   let mstr = "Memory = " ^ m in
   let prufstr = "P/R/U/F = " ^ String.concat ~sep:" / " [ p; r; u; f ] in
   let cstr = "expanding-ppath = " ^ c in
   let estr = "expanding-ppath-acc = " ^ e in
   let bstr = "combinations = " ^ b in
   String.concat ~sep:" , " [ tstr; mstr; prufstr; cstr; estr; bstr ]
(* function string_of_res_rough end *)

let string_of_res : config -> res -> string =
   let open Tz in
   let qres_str : qres -> string =
     fun qres ->
     match qres.qr_qid.mci_cutcat with
     | MCC_query qcat ->
       Printf.sprintf "> Location:%s\n  Category:%s\n"
         (qres.qr_qid.mci_loc |> sexp_of_ccp_loc |> Sexp.to_string)
         (qcat |> sexp_of_query_category |> Sexp.to_string)
     | _              -> Failure "Wrong query result" |> raise
   in
   fun cfg res ->
   let (cres : qres_classified) =
      Core.List.fold res.r_qr_lst
        ~init:
          {
            qrc_p = QRSet.empty;
            qrc_r = QRSet.empty;
            qrc_err = QRSet.empty;
            qrc_uu = QRSet.empty;
            qrc_uf = QRSet.empty;
            qrc_fu = QRSet.empty;
            qrc_ff = QRSet.empty;
          } ~f:(fun acc qres ->
          match (qres.qr_prv_flag, qres.qr_rft_flag) with
          | (PF_p, RF_r) -> { acc with qrc_err = QRSet.add acc.qrc_err qres }
          | (PF_p, _)    -> { acc with qrc_p = QRSet.add acc.qrc_p qres }
          | (_, RF_r)    -> { acc with qrc_r = QRSet.add acc.qrc_r qres }
          | (PF_u, RF_u) -> { acc with qrc_uu = QRSet.add acc.qrc_uu qres }
          | (PF_u, RF_f) -> { acc with qrc_uf = QRSet.add acc.qrc_uf qres }
          | (PF_f, RF_u) -> { acc with qrc_fu = QRSet.add acc.qrc_fu qres }
          | (PF_f, RF_f) -> { acc with qrc_ff = QRSet.add acc.qrc_ff qres }
      )
   in
   let (tot_c : int) = Core.List.length res.r_qr_lst
   and (p_c : int) = QRSet.length cres.qrc_p
   and (r_c : int) = QRSet.length cres.qrc_r
   and (err_c : int) = QRSet.length cres.qrc_err
   and (uu_c : int) = QRSet.length cres.qrc_uu
   and (uf_c : int) = QRSet.length cres.qrc_uf
   and (fu_c : int) = QRSet.length cres.qrc_fu
   and (ff_c : int) = QRSet.length cres.qrc_ff in
   let (failed_c : int) = err_c + uu_c + uf_c + fu_c + ff_c in
   let (failed : QRSet.t) =
      List.fold ~f:QRSet.union ~init:cres.qrc_err
        [ cres.qrc_uu; cres.qrc_uf; cres.qrc_fu; cres.qrc_ff ]
   in
   let (head : string) = "=== Final Result ===" in
   let (conf : string) =
      Printf.sprintf "Time: %s\t\tMemory: %s"
        (Utils.Time.string_of_elapsed_time cfg.cfg_timer)
        (Utils.Memory.string_of_used_memory cfg.cfg_memory)
   in
   let (summ : string) =
      Printf.sprintf "#Total: %d\t\t#Proved: %d\t\t#Refuted: %d\t\t#Failed: %d"
        tot_c p_c r_c failed_c
   in
   let (finf : string) =
      Printf.sprintf "#Err: %d\t#UU: %d\t#UF: %d\t#FU: %d\t#FF: %d" err_c uu_c
        uf_c fu_c ff_c
   in
   let (prvd : string) =
      Printf.sprintf "<< Proved >>\n%s"
        (QRSet.to_list cres.qrc_p
        |> List.map ~f:qres_str
        |> String.concat ~sep:"\n"
        )
   in
   let (rftd : string) =
      Printf.sprintf "<< Refuted >>\n%s"
        (QRSet.to_list cres.qrc_r
        |> List.map ~f:qres_str
        |> String.concat ~sep:"\n"
        )
   in
   let (fail : string) =
      Printf.sprintf "<< Failed >>\n%s"
        (QRSet.to_list failed |> List.map ~f:qres_str |> String.concat ~sep:"\n")
   in
   String.concat ~sep:"\n" [ ""; head; conf; summ; finf; prvd; rftd; fail ]
(* function string_of_res end *)
