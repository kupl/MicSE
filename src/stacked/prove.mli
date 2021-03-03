(* Prover *)

exception Error of string


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
  -> Tz.sym_state Tz.PSet.t 
  -> Se.invmap 
  -> (bool * (Tz.sym_state * (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option)) option * Utils.Timer.time)

val solve_queries : 
  Utils.Timer.t ref 
  -> (Tz.sym_state * Se.query_category) Tz.PSet.t 
  -> Se.invmap 
  -> ((Tz.mich_cut_info * Se.query_category), ((Tz.sym_state * Se.query_category) * Tz.mich_f * Utils.Timer.time) Tz.PSet.t) Tz.PMap.t
      * ((Tz.sym_state * Se.query_category) * (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option) * Tz.mich_f * Utils.Timer.time) Tz.PSet.t
      * (Tz.sym_state * Se.query_category) Tz.PSet.t

val remove_solved_queries :
  (Tz.sym_state * Se.query_category) Tz.PSet.t
  -> ((Tz.mich_cut_info * Se.query_category), ((Tz.sym_state * Se.query_category) * Tz.mich_f * Utils.Timer.time) Tz.PSet.t) Tz.PMap.t
  -> (Tz.sym_state * Se.query_category) Tz.PSet.t


(*****************************************************************************)
(*****************************************************************************)
(* Small functionalities, returns unit value only                            *)
(*****************************************************************************)
(*****************************************************************************)

val f_count_sset : Se.state_set -> unit
val f_print_blocked_paths_pretty : Se.state_set -> unit
val f_print_queries_pretty : Se.state_set -> unit
val f_print_query_solved_result_simple_pretty :
  ((Tz.mich_cut_info * Se.query_category), ((Tz.sym_state * Se.query_category) * Tz.mich_f * Utils.Timer.time) Tz.PSet.t) Tz.PMap.t
    * ((Tz.sym_state * Se.query_category) * (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option) * Tz.mich_f * Utils.Timer.time) Tz.PSet.t
    * (Tz.sym_state * Se.query_category) Tz.PSet.t
  -> unit
