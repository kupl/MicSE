(* Symbolic Executer - Extract One Symbolic State per Basic Block (Execution Path Sequence) *)

exception Error of string


(*****************************************************************************)
(*****************************************************************************)
(* Run Instruction                                                           *)
(*****************************************************************************)
(*****************************************************************************)

val run_inst : (Tz.mich_i Tz.cc) -> Se.state_set -> Se.state_set
val run_inst_i : (Tz.mich_i Tz.cc) -> Tz.sym_state -> Se.state_set

(* PLACEHOLDER *) val merge_ss : Tz.sym_state Tz.PSet.t -> (Tz.mich_v Tz.cc list * Tz.mich_f)
(* PLACEHOLDER *) val merge_stack : Tz.sym_state Tz.PSet.t -> (Tz.mich_v Tz.cc list * Tz.mich_f)
