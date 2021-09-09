(* Prove : Naïve prover for verification *)

exception PrvError of string

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(* Set of Tz.sym_state & Set of Tz.mich_cut_info *)
module SSet : module type of Core.Set.Make (Tz.SymState_cmp)

(******************************************************************************)
(******************************************************************************)
(* Utility                                                                    *)
(******************************************************************************)
(******************************************************************************)

val init_res : Res.config -> Res.res

val check_failed :
  SSet.t -> Inv.failed_cp -> Tz.r_mich_cut_info -> Inv.inv_map -> bool

(******************************************************************************)
(******************************************************************************)
(* Prover                                                                     *)
(******************************************************************************)
(******************************************************************************)

val combinate :
cfg:Res.config ->
res:Res.res ->
Inv.cand_map ->
Inv.inv_map list ->
Inv.inv_map ->
Inv.inv_map list

(******************************************************************************)
(******************************************************************************)
(* Naïve Run                                                                  *)
(******************************************************************************)
(******************************************************************************)

(* Qurey Result ***************************************************************)

val naive_run_qres_atomic_action : Res.config -> Res.qres -> Res.res -> Res.res

val naive_run_qres_escape_condition : Res.config -> Res.qres -> bool

(* Result *********************************************************************)

val naive_run_res_escape_condition : Res.config -> Res.res -> bool

val naive_run_res_atomic_action : Res.config -> Res.res -> Res.res

(* Entry Point ****************************************************************)

val naive_run : Res.config -> Res.res -> Res.res
