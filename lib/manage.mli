(* Manage : Manager for MicSE *)

exception ManageError of string

(******************************************************************************)
(******************************************************************************)
(* Initial Procedure                                                          *)
(******************************************************************************)
(******************************************************************************)

val initial_prove_run_res_atomic_action : Res.config -> Res.res -> Res.res

val initial_refute_run_res_atomic_action : Res.config -> Res.res -> Res.res

(******************************************************************************)
(******************************************************************************)
(* Entry Point                                                                *)
(******************************************************************************)
(******************************************************************************)

val naive_run_escape_condition : Res.config -> Res.res -> bool

val naive_run : Res.config -> Res.res -> Res.res

val adv_run :
  Res.config -> score_f:(Tz.qid -> MState.t -> float) -> Res.res -> Res.res
