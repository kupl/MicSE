(* Symbolic Executer *)

exception Error of string

type query_category =
  (* Each of them are indicator of "State -> Formula" function *)
  | Q_mutez_add_overflow
  | Q_mutez_mul_overflow
  | Q_mutez_sub_underflow
  | Q_shiftleft_prohibited
  | Q_shiftright_prohibited
  | Q_assertion

type state_set = {
  running : Tz.sym_state Tz.PSet.t;
  blocked : Tz.sym_state Tz.PSet.t;
  queries : (Tz.sym_state * query_category) Tz.PSet.t;
  terminated : Tz.sym_state Tz.PSet.t;
}

type cache = {
  ch_entered_loop : Tz.mich_cut_info Tz.PSet.t;
  ch_entered_lmbd : Tz.mich_cut_info Tz.PSet.t;
}


(*****************************************************************************)
(*****************************************************************************)
(* Utilities - Cache                                                         *)
(*****************************************************************************)
(*****************************************************************************)

val init_cache : unit -> cache ref

val add_entered_loop : cache ref -> Tz.mich_cut_info -> unit
val is_entered_loop : cache ref -> Tz.mich_cut_info -> bool

val add_entered_lmbd : cache ref -> Tz.mich_cut_info -> unit
val is_entered_lmbd : cache ref -> Tz.mich_cut_info -> bool


(*****************************************************************************)
(*****************************************************************************)
(* Utilities - State Set Mapping                                             *)
(*****************************************************************************)
(*****************************************************************************)

val map_ss_running : (Tz.sym_state -> Tz.sym_state) -> state_set -> state_set


(*****************************************************************************)
(*****************************************************************************)
(* Run Instruction                                                           *)
(*****************************************************************************)
(*****************************************************************************)

val run_inst : cache ref -> (Tz.mich_i Tz.cc) -> state_set -> state_set
val run_inst_i : cache ref -> (Tz.mich_i Tz.cc) -> Tz.sym_state -> state_set

(*
(*****************************************************************************)
(*****************************************************************************)
(* Run Operation                                                             *)
(*****************************************************************************)
(*****************************************************************************)

val run_operation : Tz.operation -> state_set -> state_set
val run_operation_i : Tz.operation -> Tz.sym_state -> state_set


(*****************************************************************************)
(*****************************************************************************)
(* Main                                                                      *)
(*****************************************************************************)
(*****************************************************************************)

val main : Tz.blockchain -> string -> Tz.operation list -> state_set
*)