(* Prove : Naïve prover for verification *)

exception PrvError of string

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(* Set of Tz.r_mich_cut_info *)
module RMCISet : module type of Core.Set.Make (Tz.RMichCutInfo_cmp)

(* Map of Tz.r_mich_cut_info *)
module RMCIMap : module type of Core.Map.Make (Tz.RMichCutInfo_cmp)

(* Set of Tz.sym_state *)
module SSet : module type of Core.Set.Make (Tz.SymState_cmp)

(******************************************************************************)
(******************************************************************************)
(* Prover                                                                     *)
(******************************************************************************)
(******************************************************************************)

val check_failed :
  SSet.t ->
  Inv.inductive_info ->
  RMCISet.t ->
  Tz.r_mich_cut_info ->
  Inv.inv_map ->
  bool

val combinate :
  Tz.qid ->
  SSet.t ->
  Inv.inductive_info ->
  Inv.inv_map ->
  Inv.cand_map ->
  Inv.inv_map ->
  Inv.inv_map option

val check_inductiveness :
  Smt.Ctx.t ->
  Smt.Solver.t ->
  SSet.t ->
  Inv.inductive_info ->
  Inv.inv_map ->
  Inv.inv_map option * (Tz.sym_state * bool) list

val check_number_of_cands : Tz.qid -> Inv.cand_map -> bool

val add_inductive_info :
  Inv.inductive_info ->
  (Tz.sym_state * bool) list ->
  Inv.inv_map ->
  Inv.inductive_info

val prove : Smt.Ctx.t -> Smt.Solver.t -> Inv.inv_map -> SSet.t -> SSet.t

(******************************************************************************)
(******************************************************************************)
(* Naïve Run                                                                  *)
(******************************************************************************)
(******************************************************************************)

(* Qurey Result ***************************************************************)

val naive_run_qres_escape_condition : Res.config -> Res.qres -> bool

val naive_run_qres_atomic_action :
  Res.config -> Inv.inv_map -> Res.qres -> Res.qres

(* Result *********************************************************************)

val naive_run_res_atomic_action : Res.config -> Res.res -> Res.res

(* Entry Point ****************************************************************)

val naive_run_escape_condition : Res.config -> Res.res -> bool

val naive_run : Res.config -> Res.res -> Res.res
