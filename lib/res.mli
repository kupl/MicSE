(* Result of MicSE *)

open! Core

(* Set of Tz.mich_f *)
module MFSet : module type of Set.Make (Tz.MichF_cmp)

(* Set of set of Tz.mich_f *)
module MFSSet : module type of Set.Make (MFSet)

(* Map of Tz.r_mich_cut_info *)
module RMCIMap : module type of Map.Make (Tz.RMichCutInfo_cmp)

(* Set of Tz.sym_state *)
module SSet : module type of Set.Make (Tz.SymState_cmp)

(* Set of (MFSet.t * MFSet.t) *)
module MFSP_cmp : sig
  type t = MFSet.t * MFSet.t [@@deriving compare, sexp]
end

module MFSPSet : module type of Set.Make (MFSP_cmp)

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
end

module PPSet : module type of Set.Make (PPath)

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
  r_failcp : MFSPSet.t RMCIMap.t;
  r_comb_cnt : int;
}
[@@deriving sexp, compare, equal]
