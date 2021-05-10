(* Refuter *)

val check_ppath_validity : Utils.Timer.t ref -> Se.invmap -> Merge.ms -> (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option * Utils.Timer.time)



(*****************************************************************************)
(* Set Structured Variable Names to sym_state and                            *)
(*****************************************************************************)

(* "set_stvn_ss" modifies variables following "Refute Target Properties",
  - ("Tz.ss_entry_mci")
  - "Tz.ss_entry_symstack"
  - ("Tz.ss_block_mci")
  - "Tz.ss_symstack"
  - "Tz.ss_constraints"
*)

(* val set_stvn_ss : (int option * int option) -> Tz.sym_state -> Tz.sym_state *)


(*

(******************************************************************************)
(******************************************************************************)
(* Types                                                                      *)
(******************************************************************************)
(******************************************************************************)

(* typew with prefix "rqu" :
  these types exists to describe "f_refute_queries_naive_unroll"'s return type.
*)

(* rqu : query result summary - summary for a query-id *)
type rqu_s = 
  | Rqus_proved
  | Rqus_refuted of Prove.query_state * Merge.ms * Tz.mich_f * ProverLib.Smt.ZModel.t * Utils.Timer.time
  | Rqus_unknown
  | Rqus_untouched

(* rqu : smt-result for rqu_d *)
type rqu_smtr = 
  | Rquds_proved of Utils.Timer.time
  | Rquds_refuted of Tz.mich_f * ProverLib.Smt.ZModel.t * Utils.Timer.time
  | Rquds_unknown of Utils.Timer.time
  | Rquds_untouched

(* rqu : query result details - depicts every results of  *)
type rqu_d = (Prove.query_state * (Merge.ms * rqu_smtr) list) list

(* rqu : return type *)
type rqu_ret = (Prove.query_id, rqu_s * (rqu_d option)) Tz.PMap.t


(******************************************************************************)
(******************************************************************************)
(* Small functionalities, returns unit value only                             *)
(******************************************************************************)
(******************************************************************************)

type f_refute_queries_naive_unroll_param = {
  rqu_timer : Utils.Timer.t ref;
  rqu_sset : Se.state_set;
  rqu_pret : Prove.ret;
}

type f_refute_queries_naive_unroll_output = {
  rquo_v : rqu_ret;
}

val f_refute_queries_naive_unroll : f_refute_queries_naive_unroll_param -> f_refute_queries_naive_unroll_output


*)
