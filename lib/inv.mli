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

(* Map of set of Tz.mich_f *)
module MFSMap : module type of Core.Map.Make (Tz.MFSet)

(* Map of Tz.mich_cut_info *)
module MCIMap : module type of Core.Map.Make (Tz.MichCutInfo_cmp)

(* Set of Tz.r_mich_cut_info *)
module RMCISet : module type of Core.Set.Make (Tz.RMichCutInfo_cmp)

(* Map of Tz.r_mich_cut_info *)
module RMCIMap : module type of Core.Map.Make (Tz.RMichCutInfo_cmp)

(* Set of Tz.qid *)
module QIDSet : module type of Core.Set.Make (Tz.QId_cmp)

(* Map of Tz.qid *)
module QIDMap : module type of Core.Map.Make (Tz.QId_cmp)

(* Set of Tz.sym_state *)
module SSet : module type of Core.Set.Make (Tz.SymState_cmp)

(* Map of Tz.sym_state_id *)
module SIDMap : module type of Core.Map.Make (Tz.SymStateID_cmp)

(* Set of Igdt.igdt *)
module ISet : module type of Core.Set.Make (Igdt.IGDT_cmp)

(******************************************************************************)
(******************************************************************************)
(* Invariant Candidates                                                       *)
(******************************************************************************)
(******************************************************************************)

type cand = {
  c_fmla : MFSet.t;
  c_cond : MFSet.t;
  c_igdt : ISet.t;
}
[@@deriving sexp, compare, equal]

module Cand_cmp : sig
  type t = cand [@@deriving sexp, compare]
end

(* Set of cand *)
module CSet : module type of Core.Set.Make (Cand_cmp)

(* Map of cand *)
module CMap : module type of Core.Map.Make (Cand_cmp)

(******************************************************************************)
(******************************************************************************)
(* Invariants                                                                 *)
(******************************************************************************)
(******************************************************************************)

type inv_map = cand RMCIMap.t [@@deriving sexp, compare, equal]

module InvMap_cmp : sig
  type t = inv_map [@@deriving sexp, compare]
end

(* Set of inv_map *)
module InvSet : module type of Core.Set.Make (InvMap_cmp)

(******************************************************************************)
(******************************************************************************)
(* Map of Invariant Candidates                                                *)
(******************************************************************************)
(******************************************************************************)

type cands = (bool * (int * int) QIDMap.t) CMap.t
[@@deriving sexp, compare, equal]

type cand_map = cands RMCIMap.t [@@deriving sexp, compare, equal]

(******************************************************************************)
(******************************************************************************)
(* Failed Candidate Pair                                                      *)
(******************************************************************************)
(******************************************************************************)

type cand_pair = {
  cp_start : cand;
  cp_block : cand;
}
[@@deriving sexp, compare, equal]

module CandPair_cmp : sig
  type t = cand_pair [@@deriving compare, sexp]
end

module CPSet : module type of Core.Set.Make (CandPair_cmp)

module CPMap : module type of Core.Map.Make (CandPair_cmp)

type cp_inductiveness = {
  ir_valid : CPSet.t;
  ir_invalid : CPSet.t;
}
[@@deriving compare, sexp, equal]

type inductive_info = cp_inductiveness SIDMap.t
[@@deriving compare, sexp, equal]

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

(* shuffle [a; b; c;] === {[a; b; c;]; [a; c; b;]; [b; a; c;]; [b; c; a;]; [c; a; b;]; [c; b; a;];} *)
val shuffle : Igdt.igdt list -> ILSet.t

(* igdt_combination [{1; 2;}; {a; b;}; ...] === {[1; a; ...]; [1; b; ...]; [2; a; ...]; [2; b; ...];} *)
val igdt_combination : ISet.t list -> ILSet.t

(* igdt_combination_self {a; b;} 2 === {[a; a;]; [a; b;]; [b; a;]; [b; b;];} *)
val igdt_combination_self : ISet.t -> size:int -> ILSet.t

(* filter_symmetry {[a; a;]; [a; b;]; [a; c;] [b; a;]; [b; b;]; [b; c;]; [c; a;]; [c; b;]; [c; c;];} === {[a; b;]; [a; c;]; [b; c;];}  *)
(* This function is only working at ingredient list size 2 *)
val filter_symmetry : int -> ILSet.t -> ILSet.t

(* filter_equal {[a; a;]; [a; b;]; [a; c;] [b; a;]; [b; b;]; [b; c;]; [c; a;]; [c; b;]; [c; c;];} === {[a; b;]; [a; c;] [b; a;]; [b; c;]; [c; a;]; [c; b;];}  *)
val filter_equal : ILSet.t -> ILSet.t

val gen_cand_by_fmla : ?cond:MFSet.t -> ?igdt:ISet.t -> Tz.mich_f -> cand

val join_cands : cand -> cand -> cand

val is_subcand : cand -> of_:cand -> bool

val fmla_of_cand_pre : cand -> Tz.mich_f

val fmla_of_cand_post : cand -> Tz.mich_f

(******************************************************************************)
(* Invariant Candidate Templates                                              *)
(******************************************************************************)

val gen_template :
  ?except_lit_only:bool ->
  ?target_mode:[ `Normal | `Asymm     of int | `Asymm_rfl ] ->
  f:((Tz.mich_t * Tz.mich_v Tz.cc) list -> (Tz.mich_f * MFSet.t) option) ->
  Igdt.igdt_sets ->
  Tz.mich_t Tz.cc list list ->
  CSet.t

val tmp_eq : Igdt.igdt_sets -> CSet.t

val tmp_ge : Igdt.igdt_sets -> CSet.t

val tmp_gt : Igdt.igdt_sets -> CSet.t

val tmp_add_2_eq : Igdt.igdt_sets -> CSet.t

val tmp_add_3_eq : Igdt.igdt_sets -> CSet.t

val tmp_add_2_ge : Igdt.igdt_sets -> CSet.t

val tmp_all_elem_eq : Igdt.igdt_sets -> CSet.t

val tmp_eq_and_all_elem_eq : Igdt.igdt_sets -> CSet.t

(******************************************************************************)
(* Invariants & Invariant Candidates                                          *)
(******************************************************************************)

val cvt_cand_pair : cand * cand -> cand_pair

(* Invariants *****************************************************************)

val gen_true_inv_map : Se.se_result -> inv_map

val gen_initial_inv_map : Se.se_result -> inv_map

val find_inv_by_rmci : inv_map -> Tz.r_mich_cut_info -> cand

val find_inv : inv_map -> Tz.mich_cut_info -> cand

val update_inv_map : inv_map -> key:Tz.r_mich_cut_info -> value:cand -> inv_map

val merge_inv_map : inv_map -> inv_map -> inv_map

val strengthen_inv_map : InvSet.t -> inv_map -> InvSet.t

val check_contain_pair : inv_map -> Tz.sym_state -> cand_pair -> bool

(* Invariant Candidates *******************************************************)

val gen_initial_cand_map :
  is_cand_sat:(cand -> bool) ->
  do_cand_sat_istrg:(Tz.r_mich_cut_info -> cand -> bool) ->
  QIDSet.t ->
  Igdt.igdts_map ->
  cand_map

val find_cand_by_rmci : cand_map -> Tz.r_mich_cut_info -> cands

val find_cand : cand_map -> Tz.mich_cut_info -> cands

val mem_by_rmci : cand_map -> key:Tz.r_mich_cut_info -> value:cand -> bool

val mem : cand_map -> key:Tz.mich_cut_info -> value:cand -> bool

val get_score_by_rmci :
  cand_map -> key:Tz.r_mich_cut_info -> value:cand -> qid:Tz.qid -> int

val get_score :
  cand_map -> key:Tz.mich_cut_info -> value:cand -> qid:Tz.qid -> int

val get_count_by_rmci :
  cand_map -> key:Tz.r_mich_cut_info -> value:cand -> qid:Tz.qid -> int

val find_ordered_cand_by_rmci :
  ?order:[ `Increasing | `Decreasing ] ->
  ?remove_unflaged:bool ->
  ?remove_not_precond:bool ->
  cand_map ->
  Tz.r_mich_cut_info ->
  Tz.qid ->
  cand list

val find_ordered_cand :
  ?order:[ `Increasing | `Decreasing ] ->
  ?remove_unflaged:bool ->
  ?remove_not_precond:bool ->
  cand_map ->
  Tz.mich_cut_info ->
  Tz.qid ->
  cand list

val find_cand_top_k_by_rmci :
  ?order:[ `Increasing | `Decreasing ] ->
  ?remove_unflaged:bool ->
  ?remove_not_precond:bool ->
  top_k:int ->
  cand_map ->
  Tz.r_mich_cut_info ->
  Tz.qid ->
  cand list

val find_cand_top_k :
  ?order:[ `Increasing | `Decreasing ] ->
  ?remove_unflaged:bool ->
  ?remove_not_precond:bool ->
  top_k:int ->
  cand_map ->
  Tz.mich_cut_info ->
  Tz.qid ->
  cand list

val strengthen_cand_map :
  is_cand_sat:(cand -> bool) ->
  do_cand_sat_istrg:(Tz.r_mich_cut_info -> cand -> bool) ->
  cand_map ->
  inv_map ->
  cand_map

val update_score_by_rmci :
  cand_map ->
  key:Tz.r_mich_cut_info ->
  value:cand ->
  qid:Tz.qid ->
  point:int * int ->
  cand_map

val unflag_cand : cand_map -> key:Tz.r_mich_cut_info -> value:cand -> cand_map

(* Failed Candidate Pair ******************************************************)

val gen_initial_inductive_info_map : SSet.t -> inductive_info

val get_inductiveness_from_bs :
  ?ss_id_normalize:bool -> inductive_info -> Tz.sym_state -> cp_inductiveness

val count_each_cands :
  inductive_info -> Tz.sym_state -> CSet.t * CSet.t -> int * int

val add_inductiveness :
  inductive_info -> Tz.sym_state * cand_pair * bool -> inductive_info

val is_already_succeeded : inductive_info -> Tz.sym_state -> cand_pair -> bool

val is_already_failed : inductive_info -> Tz.sym_state -> cand_pair -> bool
