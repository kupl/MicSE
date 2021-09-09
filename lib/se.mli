(* Se is a symbolic execution module based on Tz.sym_state definition *)

(* Set of Tz.sym_state & Set of Tz.mich_cut_info *)
module SSet : module type of Core.Set.Make (Tz.SymState_cmp)

module MciSet : module type of Core.Set.Make (Tz.MichCutInfo_cmp)

type se_result = {
  (* symbolic states *)
  sr_running : SSet.t;
  sr_blocked : SSet.t;
  sr_queries : SSet.t;
  sr_terminated : SSet.t;
  (* caches - accumulates which loop/lambdas passed *)
  sr_entered_loops : MciSet.t;
  sr_entered_lmbds : MciSet.t;
  (* caches - count integer to assign sym_state_id *)
  sr_sid_counter : int;
}
[@@deriving sexp, compare, equal]

val se_result_empty : se_result

val se_result_pointwise_union : se_result -> se_result -> se_result

(******************************************************************************)
(* SymState as Graph                                                          *)
(******************************************************************************)

module SidMap : module type of Core.Map.Make (Core.Int)

val construct_sid_checkmap : SSet.t -> Tz.sym_state SidMap.t

module SSGraph : sig
  module RMCIMap : module type of Core.Map.Make (Tz.RMichCutInfo_cmp)

  type 'a ps_pair = {
    pred : 'a;
    succ : 'a;
  }
  [@@deriving sexp, compare, equal]

  type mci_view = SSet.t ps_pair RMCIMap.t [@@deriving sexp, compare, equal]

  val construct_mci_view : basic_blocks:SSet.t -> mci_view

  val ss_view_pred : m_view:mci_view -> Tz.sym_state -> SSet.t

  val ss_view_succ : m_view:mci_view -> Tz.sym_state -> SSet.t
end

(******************************************************************************)
(* Utilities : Constraint                                                     *)
(******************************************************************************)

val add_constraints : c:Tz.mich_f list -> Tz.sym_state -> Tz.sym_state

val mtz_constriant_if_it_is_or_true :
  ctx:Tz.mich_sym_ctxt -> tv:Tz.mich_t Tz.cc * Tz.mich_v Tz.cc -> Tz.mich_f

val add_mtz_constraint_if_it_is :
  ctx:Tz.mich_sym_ctxt ->
  tv:Tz.mich_t Tz.cc * Tz.mich_v Tz.cc ->
  Tz.sym_state ->
  Tz.sym_state

val nat_constriant_if_it_is_or_true :
  ctx:Tz.mich_sym_ctxt -> tv:Tz.mich_t Tz.cc * Tz.mich_v Tz.cc -> Tz.mich_f

val add_nat_constraint_if_it_is :
  ctx:Tz.mich_sym_ctxt ->
  tv:Tz.mich_t Tz.cc * Tz.mich_v Tz.cc ->
  Tz.sym_state ->
  Tz.sym_state

val michv_maybe_mtznat_constraints :
  ctx:Tz.mich_sym_ctxt -> v:Tz.mich_v Tz.cc -> Tz.mich_f list

val amount_balance_mutez_constraints :
  ctx:Tz.mich_sym_ctxt ->
  amount_v:Tz.mich_v Tz.cc ->
  balance_v:Tz.mich_v Tz.cc ->
  bc_balance_v:Tz.mich_v Tz.cc ->
  Tz.mich_f list

val mtz_comes_from_constraint :
  ctx:Tz.mich_sym_ctxt ->
  mtz_v:Tz.mich_v Tz.cc ->
  from_v:Tz.mich_v Tz.cc ->
  Tz.mich_f

val lt_2_63_constraint : ctx:Tz.mich_sym_ctxt -> Tz.mich_v Tz.cc -> Tz.mich_f

(* val ge_balance_amount_in_non_trx_entry_constraint :
   amount_v:Tz.mich_v Tz.cc -> balance_v:Tz.mich_v Tz.cc -> Tz.mich_f *)

(******************************************************************************)
(* Symbolic Run Instruction                                                   *)
(******************************************************************************)

val run_inst_initial_se_result :
  Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc ->
  se_result * Tz.sym_state

val run_inst : Tz.mich_i Tz.cc -> se_result -> se_result

val run_inst_i : Tz.mich_i Tz.cc -> se_result * Tz.sym_state -> se_result

val run_inst_entry :
  Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc ->
  se_result * Tz.sym_state
