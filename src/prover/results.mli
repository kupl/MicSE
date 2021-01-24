(* module Results stores prover running result value. 
  Refuter will read values in this module, to decide which query to refute.
*)

val is_prover_used : bool ref

val unproved_queries : (ProverLib.Bp.query_category * PreLib.Mich.loc) Core.Set.Poly.t ref
