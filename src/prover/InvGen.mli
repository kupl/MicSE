
(* sugar *)
open PreLib
open ProverLib
module CPSet = Core.Set.Poly

val refine_T : Inv.t * Bp.t * Cfg.vertex * Inv.invgen_info * bool -> (Inv.t CPSet.t)
val refine_L : Inv.t * Bp.t * Cfg.vertex * Inv.invgen_info -> (Inv.t CPSet.t)

val generate : (Validator.validate_result * ProverLib.Inv.invgen_info * ProverLib.Inv.t * bool * ProverLib.Inv.t CPSet.t) -> ProverLib.Inv.t CPSet.t
