(* Refute : Naïve refuter for bug finding *)

exception RftError of string

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(* Set of MState.t *)
module MSSet : module type of Core.Set.Make (MState)

(* Map of Tz.mich_cut_info *)
module MCIMap : module type of Core.Map.Make (Tz.MichCutInfo_cmp)

(* Map of Tz.r_mich_cut_info *)
module RMCIMap : module type of Core.Map.Make (Tz.RMichCutInfo_cmp)

(* Set of Tz.sym_state *)
module SSet : module type of Core.Set.Make (Tz.SymState_cmp)

(* Set of Res.PPath.t *)
module PPSet : module type of Core.Set.Make (Res.PPath)

(******************************************************************************)
(******************************************************************************)
(* Refuter                                                                    *)
(******************************************************************************)
(******************************************************************************)

val separate_ppset_in_length_increasing_order :
  Res.PPSet.t -> Res.PPath.t list RMCIMap.t

val select_pp : top_k:int -> PPSet.t -> PPSet.t * PPSet.t

val expand_pp : m_view:Se.SSGraph.mci_view -> Res.PPath.t -> PPSet.t

val filter_sat_ppaths : Smt.Ctx.t -> Smt.Solver.t -> PPSet.t -> PPSet.t

val refute :
  Smt.Ctx.t ->
  Smt.Solver.t ->
  Tz.mich_v Tz.cc ->
  Res.PPath.t ->
  (Res.PPath.t * Smt.Solver.satisfiability) option * Smt.Model.t option

val refute_lst :
  Smt.Ctx.t ->
  Smt.Solver.t ->
  Tz.mich_v Tz.cc ->
  Res.PPath.t ->
  (Res.PPath.t * Smt.Solver.satisfiability) option * Smt.Model.t option

(******************************************************************************)
(******************************************************************************)
(* Path-Pick Functions                                                        *)
(******************************************************************************)
(******************************************************************************)

module PickFun : sig
  (* NOTE : pick_func return (picked-ppaths, unpicked-ppaths) pair.
           Dead paths (unsatisfiable paths) SHOULD NOT BE put in picked-ppaths.
           (neither in unpicked-ppaths, but unknown paths whether dead or not
           can be put in unpicked-ppaths)
  *)
  type t = Smt.Ctx.t * Smt.Solver.t -> PPSet.t -> PPSet.t * PPSet.t
end
(* module PickFun end *)

val pick_short_k_for_each_mci : top_k:int -> PickFun.t

(******************************************************************************)
(******************************************************************************)
(* Naïve Run                                                                  *)
(******************************************************************************)
(******************************************************************************)

(* Partial Path ***************************************************************)

val naive_run_ppath_escape_condition : Res.config -> Res.PPath.t -> bool

val naive_run_ppath_atomic_action :
  Res.config ->
  Res.PPath.t ->
  (Res.PPath.t * Smt.Solver.satisfiability) list
  * PPSet.t
  * (Res.PPath.t * Smt.Model.t) option

(* Query Result ***************************************************************)

val naive_run_qres_escape_condition : Res.config -> Res.qres -> bool

val naive_run_qres_atomic_action : Res.config -> Res.qres -> Res.qres

(* Result *********************************************************************)

val naive_run_res_atomic_action : Res.config -> Res.res -> Res.res

(* Entry Point ****************************************************************)

val naive_run_escape_condition : Res.config -> Res.res -> bool

val naive_run : Res.config -> Res.res -> Res.res

(******************************************************************************)
(******************************************************************************)
(* Refuting with Parametric Path Selection Enabled                            *)
(******************************************************************************)
(******************************************************************************)

val guided_run_qres : Res.config -> pick_f:PickFun.t -> Res.qres -> Res.qres

val guided_run :
  Res.config ->
  pick_f_gen:(Res.res -> Tz.qid -> PickFun.t) ->
  Res.res ->
  Res.res

(******************************************************************************)
(******************************************************************************)
(* Refuting with Parametric Path Selection Enabled, but using TrxPaths        *)
(******************************************************************************)
(******************************************************************************)

val shortest_first_score_f_gen :
  Res.config -> Res.res -> Tz.qid -> MState.t -> float

val featurediff_first_score_f_gen :
  Res.config -> Res.res -> Tz.qid -> MState.t -> float

val trxpath_guided_run_qres :
  Res.config -> score_f:(Tz.qid -> MState.t -> float) -> Res.qres -> Res.qres

val trxpath_guided_run_res_atomic_action :
  Res.config -> score_f:(Tz.qid -> MState.t -> float) -> Res.res -> Res.res

val trxpath_guided_run :
  Res.config -> score_f:(Tz.qid -> MState.t -> float) -> Res.res -> Res.res

(******************************************************************************)
(******************************************************************************)
(* Refuting with Parametric Path Selection Enabled & score saved on PPath,
   but using TrxPaths *********************************************************)
(******************************************************************************)
(******************************************************************************)

val trxpath_score_saved_guided_run_qres :
  pick_f:(Res.PPath.t -> float) ->
  score_f:(Res.PPath.t -> int list) ->
  Res.config ->
  Res.qres ->
  Res.qres

val trxpath_score_saved_guided_run_res_atomic_action :
  pick_f_gen:(Res.config -> Res.res -> Res.qres -> Res.PPath.t -> float) ->
  score_f_gen:(Res.config -> Res.res -> Res.qres -> Res.PPath.t -> int list) ->
  Res.config ->
  Res.res ->
  Res.res
