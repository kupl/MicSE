(* Merge Two States *)


(*****************************************************************************)
(*****************************************************************************)
(* Merge States in Intra-Transaction Situation                               *)
(*****************************************************************************)
(*****************************************************************************)

type ms_iter_info = {
  (* iteration information for the function "intratrx-merge-state" *)
  mii_iter_iv : (Tz.mich_cut_info, Tz.mich_v Tz.cc list) Tz.PMap.t;  (* input-var-info for ITER instruction // MCC_lb_iter *)
  (* "mii_map_iov" : (single-ivar holder) * (single-ovar holder) * io-var-info for MAP instruction // MCC_lb_map *)
  mii_map_iov : (Tz.mich_cut_info, (Tz.mich_v Tz.cc option * Tz.mich_v Tz.cc option * ((Tz.mich_v Tz.cc * Tz.mich_v Tz.cc) list))) Tz.PMap.t; 
  mii_map_accv : (Tz.mich_cut_info, Tz.mich_v Tz.cc) Tz.PMap.t; (* result-var-info for MAP instruction // MCC_ln_map *)
  mii_iter_ef : Tz.mich_cut_info Tz.PSet.t; (* ending-included-flag for ITER instruction // MCC_ln_iter // If exists, then true, else false *)
  mii_map_ef : Tz.mich_cut_info Tz.PSet.t; (* ending-included-flag for MAP instruction // MCC_ln_map // If exists, then true, else false *)
}

val empty_ms_iter_info : ms_iter_info

val intratrx_merge_state : Tz.sym_state -> (Tz.sym_state * ms_iter_info) -> (Tz.sym_state * ms_iter_info)


(*****************************************************************************)
(*****************************************************************************)
(* Merge States in Inter-Transaction Situation                               *)
(*****************************************************************************)
(*****************************************************************************)

(* "ms" : Merged State Type 
  ms_state    : merged state
  ms_te_count : count transaction-entered
  ms_le_count : count loop-entered (initialized when the state leaves current transaction)
  ms_le_stack : count loop-entered using stack. It is useful to restrict the number of loop unrolling.
  ms_iinfo    : iteration information for intratrx-merge.
  ms_querycat : query-category if exists
  ms_cf_l_st  : control-flow last state. Mainly it is the first query-state.
  ms_history  : (entry-mci * exit-mci) list which shows merged states' cut-infos.
*)
type ms = {
  ms_state    : Tz.sym_state;
  ms_te_count : int;
  ms_le_count : (Tz.mich_cut_info, int) Tz.PMap.t;
  ms_le_stack : (Tz.mich_cut_info * int) list;
  ms_iinfo    : ms_iter_info;
  ms_querycat : Se.query_category option;
  ms_cf_l_st  : Tz.sym_state;
  ms_history  : (Tz.mich_cut_info * Tz.mich_cut_info) list;
}

val is_trxentry_path : ms -> bool

val intertrx_merge_state : Tz.sym_state -> Tz.sym_state -> Tz.sym_state


(*****************************************************************************)
(*****************************************************************************)
(* Expand states (merging / unrolling restriction and renaming considered)   *)
(*****************************************************************************)
(*****************************************************************************)

type expand_param = {
  ep_bss : (Tz.mich_cut_info, Tz.sym_state Tz.PSet.t) Tz.PMap.t;  (* blocked-states. key-mci should be symstate's blocked-mci *)
  ep_uloop_lim : int; (* loop-unrolling-numbers in [1, ep_uloop_lim] are allowed. Negative value for no-limit *)
  ep_utrx_lim : int;  (* trx-unrolling-numbers in [1, ep_utrx_lim] are allowed. Negative value for no-limit *)
}


(*****************************************************************************)
(* Set Structured Variable Names to sym_state                                *)
(*****************************************************************************)

val set_stvn_ss : (int option * int option) -> Tz.sym_state -> Tz.sym_state


(*****************************************************************************)
(* Expand                                                                    *)
(*****************************************************************************)

(* val expand_ii : Tz.sym_state -> ms -> ms *)
val expand_i : expand_param -> (Tz.sym_state Tz.PSet.t) -> ms -> (ms Tz.PSet.t)
val expand : expand_param -> (ms Tz.PSet.t) -> (ms Tz.PSet.t)
