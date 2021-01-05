(* Invariant *)

(* Sugar *)
module CPSet = Core.Set.Poly
module CPMap = Core.Map.Poly


type t = {
    trx_inv : Vlang.t;  (* Transaction invariant *)
    loop_inv : (int, Vlang.t) CPMap.t;  (* Loop invariant : loop-vertex -> Vlang formula *)
}


(* invgen_info should contain information about which variables can be used when generating invariants. 
    In current implementation, we will remain loop-invariant's component empty. 
    Available variables are enough to generate invariant.
*)
type invgen_info = {
    igi_trx : Vlang.Component.t; (* available vlang-expr (component) set *)
    (* "igi_loop" : loop-vertex -> available vlang-expr set * (component-set (empty in current implementation)) *)
    igi_loop : (int, (Vlang.Ty.t * Vlang.Expr.t) CPSet.t * Vlang.Component.t CPSet.t) CPMap.t;
    igi_entryvtx : PreLib.Cfg.vertex; (* cfg entry vertex *)
    igi_exitvtx : PreLib.Cfg.vertex;  (* cfg exit vertex *)
}

val inv_true_gen : invgen_info -> t

(* In our blueprint, Invariant is used for a verifier, not refuter. *)
val gen_invgen_info_for_single_contract_verification : Pre.Lib.Cfg.t -> invgen_info

val strengthen_worklist : (t * t CPSet.t) -> t CPSet.t
