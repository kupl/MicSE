(* Result of MicSE *)

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(* Set of Tz.mich_v Tz.cc *)
module MVSet : module type of Core.Set.Make (Tz.MichVCC_cmp)

(* Set of Tz.mich_f *)
module MFSet : module type of Core.Set.Make (Tz.MichF_cmp)

(* Set of set of Tz.mich_f *)
module MFSSet : module type of Core.Set.Make (MFSet)

(* Map of Tz.mich_cut_info *)
module MCIMap : module type of Core.Map.Make (Tz.MichCutInfo_cmp)

(* Map of Tz.r_mich_cut_info *)
module RMCIMap : module type of Core.Map.Make (Tz.RMichCutInfo_cmp)

(* Set of Tz.sym_state *)
module SSet : module type of Core.Set.Make (Tz.SymState_cmp)

(* Set of Inv.inv_map *)
module ISet : module type of Core.Set.Make (Inv.InvMap_cmp)

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

module PPath : sig
  type t = {
    pp_mstate : MState.t;
    pp_goalst : MFSSet.t RMCIMap.t list;
    pp_score : int;
  }
  [@@deriving sexp, compare, equal]

  val t_of_ss : Tz.sym_state -> t
end

module PPSet : module type of Core.Set.Make (PPath)

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
  wl_invs : ISet.t;
  wl_cands : Inv.cand_map;
  wl_failcp : Inv.failed_cp;
  wl_comb_cnt : int;
}
[@@deriving sexp, compare, equal]

type res = {
  r_qr_lst : qres list;
  r_inv : Inv.inv_map;
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

val init_qres : Tz.mich_cut_info -> SSet.t -> qres

val init_worklist : config -> worklist

val init_res : config -> res

val init_config :
  Tz.mich_v Tz.cc option -> Se.se_result -> Tz.sym_state -> config

(******************************************************************************)
(******************************************************************************)
(* Utility                                                                    *)
(******************************************************************************)
(******************************************************************************)

val string_of_res_rough_in_refuter_perspective : config -> res -> string
