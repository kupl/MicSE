module BpGen = BpGen
module InvGen = InvGen
module Results = Results
module Validator = Validator
module VcGen = VcGen
module Verifier = Verifier
module VlGen = VlGen


type run_ret = {
  all_qs : (ProverLib.Bp.query_category * PreLib.Cfg.vertex) Core.Set.Poly.t;
  proved_qs : (ProverLib.Inv.t * (ProverLib.Bp.query_category * PreLib.Cfg.vertex)) Core.Set.Poly.t;
  unproved_qs : (ProverLib.Bp.query_category * PreLib.Cfg.vertex) Core.Set.Poly.t;
}

type run_env = {
  worklist : ProverLib.Inv.t Core.Set.Poly.t;
  invs_collected : ProverLib.Inv.t Core.Set.Poly.t;
  timer : Utils.Timer.t;
  igi : ProverLib.Inv.invgen_info;  (* information for invariant generation process *)
  vcl : (ProverLib.Inv.t -> VcGen.v_cond) list; (* the list of verification condition *)
  isc : ProverLib.Inv.t -> ProverLib.Vlang.t; (* initial-storage condition *)
  istg_exists : bool; (* is initial storage exists? if exists, true, else, false. *)
  ret_opt : run_ret option;
}

type prover_ret = run_ret

val collect_really_proved_queries_with_inv : ProverLib.Inv.t -> Validator.validate_result -> run_ret
val update_runret_opt : (run_ret option * run_ret option) -> run_ret option

val run : run_env -> run_ret option

val main : PreLib.Cfg.t -> PreLib.Adt.data option -> prover_ret option
