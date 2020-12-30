(* Invariant *)

(* Sugar *)
module CPSet = Core.Set.Poly
module CPMap = Core.Map.Poly


type t = {
    trx_inv : Vlang.t;  (* Transaction invariant *)
    loop_inv : (int, Vlang.t) CPMap.t;  (* Loop invariant : loop-vertex -> Vlang formula *)
}

(* invgen_info should contain information about which variables can be used when generating invariants. *)
type invgen_info = {
    igi_trx : string CPSet.t; (* available variable set *)
    igi_loop : (int, string CPSet.t) CPMap.t; (* loop-vertex -> available variable set *)
}

(* In our blueprint, Invariant is used for a verifier, not refuter. *)
val gen_invgen_info_for_single_contract_verification : Pre.Lib.Cfg.t -> invgen_info
