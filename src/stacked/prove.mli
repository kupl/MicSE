(* Prover *)

exception Error of string


(*****************************************************************************)
(*****************************************************************************)
(* Types                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

type 'a set = 'a Tz.PSet.t
type ('a, 'b) map = ('a, 'b) Tz.PMap.t

type query_state = Tz.sym_state * Se.query_category
type query_id = Tz.mich_cut_info * Se.query_category

type solved_query_state = (query_state * Tz.mich_f * Utils.Timer.time) 
type failed_query_state = (query_state * (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option) * Tz.mich_f * Utils.Timer.time)

type ret = {
  solved_map: (query_id, solved_query_state set) map;
  failed_set: failed_query_state set;
  untouched_set: query_state set;
}


(*****************************************************************************)
(*****************************************************************************)
(* Utilities                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

val gen_sset : PreLib.Adt.t -> (PreLib.Adt.data option) -> ((Tz.mich_v Tz.cc option) * Tz.sym_state * (Se.cache ref) * Se.state_set)

val check_validity : Tz.mich_f -> ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option

val check_inv_inductiveness : 
  Utils.Timer.t ref 
  -> (Tz.mich_v Tz.cc * Tz.sym_state) option
  -> Tz.sym_state set
  -> Se.invmap 
  -> (bool * (Tz.sym_state * (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option)) option * Utils.Timer.time)

val solve_queries : 
  Utils.Timer.t ref 
  -> query_state set
  -> Se.invmap 
  -> ret

val remove_solved_queries :
  query_state set
  -> (query_id, solved_query_state set) map
  -> query_state set


(*****************************************************************************)
(*****************************************************************************)
(* Small functionalities, returns unit value only                            *)
(*****************************************************************************)
(*****************************************************************************)

val f_count_sset : Se.state_set -> unit
val f_print_blocked_paths_pretty : Se.state_set -> unit
val f_print_queries_pretty : Se.state_set -> unit
val f_print_query_solved_result_simple_pretty : ret -> unit


(*****************************************************************************)
(*****************************************************************************)
(* Main function                                                             *)
(*****************************************************************************)
(*****************************************************************************)

val main : 
  (Tz.mich_v Tz.cc option) * Tz.sym_state
  -> Se.state_set
  -> ret
