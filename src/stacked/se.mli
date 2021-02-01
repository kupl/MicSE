(* Symbolic Executer *)

exception Error of string

type query_category =
  | Q_mutez_overflow
  | Q_mutez_underflow
  | Q_shiftleft_prohibited
  | Q_shiftright_prohibited
  | Q_assertion

type state_set = {
  running : Tz.sym_state Tz.PSet.t;
  queries : (Tz.sym_state * query_category) Tz.PSet.t;
  terminated : Tz.sym_state Tz.PSet.t;
}


(*****************************************************************************)
(*****************************************************************************)
(* Run Instruction                                                           *)
(*****************************************************************************)
(*****************************************************************************)

val run_inst : state_set -> (Tz.mich_i Tz.cc) -> state_set
val run_inst_i : Tz.sym_state -> (Tz.mich_i Tz.cc) -> state_set


(*****************************************************************************)
(*****************************************************************************)
(* Run Operation                                                             *)
(*****************************************************************************)
(*****************************************************************************)

val run_operation : state_set -> Tz.operation -> state_set
val run_operation_i : Tz.sym_state -> Tz.operation -> state_set


(*****************************************************************************)
(*****************************************************************************)
(* Main                                                                      *)
(*****************************************************************************)
(*****************************************************************************)

val main : Tz.blockchain -> string -> Tz.operation list -> state_set
