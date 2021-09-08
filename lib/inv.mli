(* Inv : Invariant manager for verification *)

exception InvError of string

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(* Map of Tz.mich_t Tz.cc *)
module MTMap : module type of Core.Map.Make (Tz.MichTCC_cmp)

(* Set of Tz.mich_v Tz.cc *)
module MVSet : module type of Core.Set.Make (Tz.MichVCC_cmp)

(* Set of Tz.mich_f *)
module MFSet : module type of Core.Set.Make (Tz.MichF_cmp)

(* Map of Tz.mich_cut_info *)
module MCIMap : module type of Core.Map.Make (Tz.MichCutInfo_cmp)

(* Set of Tz.r_mich_cut_info *)
module RMCISet : module type of Core.Set.Make (Tz.RMichCutInfo_cmp)

(* Map of Tz.r_mich_cut_info *)
module RMCIMap : module type of Core.Map.Make (Tz.RMichCutInfo_cmp)

(* Set of Tz.sym_state *)
module SSet : module type of Core.Set.Make (Tz.SymState_cmp)

(* Set of Igdt.igdt *)
module ISet : module type of Core.Set.Make (Igdt.IGDT_cmp)

(******************************************************************************)
(******************************************************************************)
(* Invariants                                                                 *)
(******************************************************************************)
(******************************************************************************)

type inv_map = MFSet.t RMCIMap.t [@@deriving sexp, compare, equal]

module InvMap_cmp : sig
  type t = inv_map [@@deriving sexp, compare]
end

(******************************************************************************)
(******************************************************************************)
(* Invariant Candidates                                                       *)
(******************************************************************************)
(******************************************************************************)

type cand = {
  cd_fmla : MFSet.t;
  cd_score : int; [@ignore]
}
[@@deriving sexp, compare, equal]

module Cand_cmp : sig
  type t = cand [@@deriving sexp, compare]
end

module CSet : module type of Core.Set.Make (Cand_cmp)

type cand_map = CSet.t RMCIMap.t [@@deriving sexp, compare, equal]

(******************************************************************************)
(******************************************************************************)
(* Failed Candidate Pair                                                      *)
(******************************************************************************)
(******************************************************************************)

type mci_pair = {
  mp_start : Tz.r_mich_cut_info;
  mp_block : Tz.r_mich_cut_info;
}
[@@deriving sexp, compare, equal]

type cand_pair = {
  cp_start : MFSet.t;
  cp_block : MFSet.t;
}
[@@deriving sexp, compare, equal]

module MciPair_cmp : sig
  type t = mci_pair [@@deriving compare, sexp]
end

module CandPair_cmp : sig
  type t = cand_pair [@@deriving compare, sexp]
end

module MPMap : module type of Core.Map.Make (MciPair_cmp)

module CPSet : module type of Core.Set.Make (CandPair_cmp)

type failed_cp = CPSet.t MPMap.t [@@deriving sexp, compare, equal]

(******************************************************************************)
(******************************************************************************)
(* Utility functions                                                          *)
(******************************************************************************)
(******************************************************************************)

(******************************************************************************)
(* Helper Functions                                                           *)
(******************************************************************************)

module IGDTL_cmp : sig
  type t = Igdt.igdt list [@@deriving sexp, compare]
end

module ILSet : module type of Core.Set.Make (IGDTL_cmp)

(* combination [{1; 2;}; {a; b;}; ...] === {[1; a; ...]; [1; b; ...]; [2; a; ...]; [2; b; ...];} *)
val combination : ISet.t list -> ILSet.t

(* combination_self {a; b;} 2 === {[a; a;]; [a; b;]; [b; a;]; [b; b;];} *)
val combination_self : ISet.t -> size:int -> ILSet.t

module MFOPT_cmp : sig
  type t = Tz.mich_f option [@@deriving sexp, compare]
end

module MFOSet : module type of Core.Set.Make (MFOPT_cmp)

(* filter_symmetry {[a; a;]; [a; b;]; [a; c;] [b; a;]; [b; b;]; [b; c;]; [c; a;]; [c; b;]; [c; c;];} === {[a; b;]; [a; c;]; [b; c;];}  *)
(* This function is only working at ingredient list size 2 *)
val filter_symmetry : ILSet.t -> ILSet.t

(* filter_equal {[a; a;]; [a; b;]; [a; c;] [b; a;]; [b; b;]; [b; c;]; [c; a;]; [c; b;]; [c; c;];} === {[a; b;]; [a; c;] [b; a;]; [b; c;]; [c; a;]; [c; b;];}  *)
val filter_equal : ILSet.t -> ILSet.t

(******************************************************************************)
(* Invariant Candidate Templates                                              *)
(******************************************************************************)

val gen_template :
  ?except_lit_only:bool ->
  ?target_mode:[ `Normal | `Asymm | `Asymm_rfl ] ->
  f:((Tz.mich_t * Tz.mich_v Tz.cc) list -> Tz.mich_f option) ->
  Igdt.igdt_sets ->
  Tz.mich_t Tz.cc list list ->
  MFSet.t

val tmp_eq : Igdt.igdt_sets -> MFSet.t

val tmp_ge : Igdt.igdt_sets -> MFSet.t

val tmp_gt : Igdt.igdt_sets -> MFSet.t

val tmp_add_2_eq : Igdt.igdt_sets -> MFSet.t

val tmp_add_3_eq : Igdt.igdt_sets -> MFSet.t

(******************************************************************************)
(* Invariants & Invariant Candidates                                          *)
(******************************************************************************)

val cvt_mci_pair : Tz.mich_cut_info * Tz.mich_cut_info -> mci_pair

val cvt_cand_pair : MFSet.t * MFSet.t -> cand_pair

val gen_true_inv_map : Se.se_result -> inv_map

val gen_initial_inv_map : Se.se_result -> inv_map

val gen_initial_cand_map :
  Se.se_result -> Tz.mich_v Tz.cc -> MVSet.t -> cand_map

val gen_initial_failed_cp : unit -> failed_cp

val find_inv_map_by_rmci : inv_map -> Tz.r_mich_cut_info -> MFSet.t

val find_inv_map : inv_map -> Tz.mich_cut_info -> MFSet.t

val find_cand_map_by_rmci : cand_map -> Tz.r_mich_cut_info -> CSet.t

val find_cand_map : cand_map -> Tz.mich_cut_info -> CSet.t

val find_failed_cp_by_rmci : failed_cp -> mci_pair -> CPSet.t

val find_failed_cp : failed_cp -> Tz.mich_cut_info * Tz.mich_cut_info -> CPSet.t

val is_already_failed_by_rmci : failed_cp -> mci_pair -> cand_pair -> bool

val is_already_failed :
  failed_cp -> Tz.mich_cut_info * Tz.mich_cut_info -> MFSet.t * MFSet.t -> bool

val update_inv_map :
  inv_map -> key:Tz.r_mich_cut_info -> value:MFSet.t -> inv_map

val merge_inv_map : inv_map -> inv_map -> inv_map

val strengthen_inv_map : inv_map list -> inv_map

val strengthen_cand_map : cand_map -> inv_map -> cand_map
