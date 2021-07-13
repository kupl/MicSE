(* sugar *)
module CPSet = Core.Set.Poly

type validate_result = {
  inductive : bool;
  p : VcGen.query_vc CPSet.t; (* proved queri set *)
  u : VcGen.query_vc CPSet.t; (* unproved queri set *)
  allq : (ProverLib.Bp.query_category * PreLib.Cfg.vertex) CPSet.t; (* all queries *)
}

(* Query-Comparator and Query-Set 
  It will be used to remove duplicated queries.
  This module will be used in the function "validate"
*)
module QueryOT : sig
  type t = VcGen.query_vc
  val compare : t -> t -> int
end

module QuerySet : Set.S with type elt=QueryOT.t


val validate : (Utils.Timer.t * ProverLib.Inv.t * (ProverLib.Inv.t -> VcGen.v_cond) list * (ProverLib.Inv.t -> ProverLib.Vlang.t) * (ProverLib.Bp.query_category * PreLib.Cfg.vertex -> bool)) -> validate_result
