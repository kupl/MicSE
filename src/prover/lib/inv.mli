(* Invariant *)

(* Sugar *)
module CPSet = Core.Set.Poly
module CPMap = Core.Map.Poly


type t = {
    trx_inv : Vlang.t CPSet.t;  (* Transaction invariant *)
    loop_inv : (int, Vlang.t CPSet.t) CPMap.t;  (* Loop invariant : loop-vertex -> Vlang formula *)
}


(* invgen_info should contain information about which variables can be used when generating invariants. 
    Query-Verification condition will convey a set of variables which appeared in that basic-path,
    so this datatype does not need to carry a set of variables appeared in each basicpaths.
*)
type invgen_info = {
    igi_stgcomp : Vlang.Component.t; (* storage's available vlang-expr (component) set *)
    igi_glvar_comp : Vlang.Component.t;  (* global variables. they are constnat in only one transaction execution (without storage) *)
    igi_loopv_set : PreLib.Cfg.vertex CPSet.t;  (* a vertex set contains every loop vertices *)
    igi_entryvtx : PreLib.Cfg.vertex; (* cfg entry vertex *)
    igi_exitvtx : PreLib.Cfg.vertex;  (* cfg exit vertex *)
}

val inv_true_gen : invgen_info -> t

(* In our blueprint, Invariant is used for a verifier, not refuter. *)
val gen_invgen_info_for_single_contract_verification : Pre.Lib.Cfg.t -> invgen_info

val strengthen_worklist : (t * t CPSet.t) -> t CPSet.t

val inv_to_formula : Vlang.t CPSet.t -> Vlang.t
