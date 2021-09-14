(* Prove : Naïve prover for verification *)

exception PrvError of string

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(* Set of Tz.mich_f *)
module MFSet : module type of Core.Set.Make (Tz.MichF_cmp)

(* Map of Tz.mich_cut_info *)
module MCIMap : module type of Core.Map.Make (Tz.MichCutInfo_cmp)

(* Map of Tz.r_mich_cut_info *)
module RMCIMap : module type of Core.Map.Make (Tz.RMichCutInfo_cmp)

(* Set of Tz.sym_state *)
module SSet : module type of Core.Set.Make (Tz.SymState_cmp)

(* Set of Inv.inv_map *)
module InvSet : module type of Core.Set.Make (Inv.InvMap_cmp)

(******************************************************************************)
(******************************************************************************)
(* Prover                                                                     *)
(******************************************************************************)
(******************************************************************************)

val check_failed :
  SSet.t -> Inv.failed_cp -> Tz.r_mich_cut_info -> Inv.inv_map -> bool

val check_inductiveness :
  Smt.Ctx.t ->
  Smt.Solver.t ->
  SSet.t ->
  Inv.inv_map ->
  (Inv.inv_map, Tz.sym_state) Result.t

val add_failed :
  Inv.failed_cp * Inv.cand_map * InvSet.t ->
  Tz.sym_state ->
  failed:Inv.inv_map ->
  Inv.failed_cp * Inv.cand_map * InvSet.t

val combinate :
  SSet.t ->
  Inv.failed_cp ->
  Inv.inv_map ->
  Inv.cand_map ->
  InvSet.t ->
  Inv.inv_map ->
  InvSet.t

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

(* Worklist *******************************************************************)

val naive_run_wlst_escape_condition : Res.config -> Res.worklist -> bool

val naive_run_wlst_atomic_action :
  Res.config ->
  Inv.inv_map * Inv.cand_map * Res.worklist ->
  Inv.inv_map option * Inv.cand_map * Res.worklist

(* Result *********************************************************************)

val naive_run_res_atomic_action : Res.config -> Res.res -> Res.res

(* Entry Point ****************************************************************)

val naive_run_escape_condition : Res.config -> Res.res -> bool

val naive_run : Res.config -> Res.res -> Res.res
