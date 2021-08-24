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

val run_inst_initial_se_result :
  Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc -> se_result

val se_result_pointwise_union : se_result -> se_result -> se_result

val run_inst : Tz.mich_i Tz.cc -> se_result -> se_result

val run_inst_i : Tz.mich_i Tz.cc -> Tz.sym_state -> se_result

val run_inst_entry :
  Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc -> se_result
