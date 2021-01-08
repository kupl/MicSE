(* BasicPath Generator (from Cfg) *)
(* This does not consider variable renaming issue too. *)

module CPSet = Core.Set.Poly

val collect_bp_vtx : PreLib.Cfg.t -> PreLib.Cfg.vertex -> (PreLib.Cfg.vertex list) CPSet.t

val bp_of_vtxlst : ProverLib.GlVar.Env.t ref -> PreLib.Cfg.t -> (PreLib.Cfg.vertex list) -> ProverLib.Bp.t

