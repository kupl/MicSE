(* Verification Condition Generator (from BasicPath) *)
(* IT CONSIDERS VARIABLE RENAMING *)

(* SUGAR *)
module CPSet = Core.Set.Poly

type v_cond = {
  path_vc : ProverLib.Vlang.t;
  query_vcs : (ProverLib.Vlang.t * ProverLib.Bp.query_category * PreLib.Cfg.vertex) CPSet.t;
}

type v_cond_ingr = ProverLib.Inv.t -> v_cond

(* renaming process performed here *)
val construct_verifier_vc : PreLib.Cfg.t -> ProverLib.Bp.t -> v_cond_ingr

