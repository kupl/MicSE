
(* sugar *)
module CPSet = Core.Set.Poly

open PreLib
open ProverLib


let refine_T : Inv.t * Bp.t * Cfg.vertex * Inv.invgen_info -> (Inv.t CPSet.t)
= (* currnet-invariant, basic-path, trx-vertex (main-entry or main-exit), invariant-generation-info *)
  fun (cur_inv, _, vtx, igi) -> begin
  (* for current design choice and implementation, igi is enough. constants in basicpath are not used for now. *)
  let _ = ignore cur_inv, ignore vtx, ignore igi in
  CPSet.empty (* TODO *)
end (* function refine_T end *)

let refine_L : Inv.t * Bp.t * Cfg.vertex * Inv.invgen_info -> (Inv.t CPSet.t)
= (* currnet-invariant, basic-path, vertex (loop-vtx), invariant-generation-info *)
  fun (cur_inv, _, vtx, igi) -> begin
  (* for current design choice and implementation, igi is enough. constants in basicpath are not used for now. *)
  let _ = ignore cur_inv, ignore vtx, ignore igi in
  CPSet.empty (* TODO *)
end (* function refine_L end *)


let generate : (Validator.validate_result * ProverLib.Inv.invgen_info * ProverLib.Inv.t) -> ProverLib.Inv.t CPSet.t
= (* current-worklist, validation-result, invariant-generation-information, current-invariant *)
  fun (val_res, igi, cur_inv) -> begin
  (* collect refine targets *)
  let refine_targets : (Cfg.vertex * Bp.t) CPSet.t =
    CPSet.fold
      val_res.u
      ~init:CPSet.empty
      ~f:(
        fun accset uq -> 
        (fun s x y -> CPSet.add (CPSet.add s x) y)
          accset
          (uq.qvc_bp.entry_vtx, uq.qvc_bp)
          (uq.qvc_bp.exit_vtx, uq.qvc_bp)
      )
  in
  (* generate invariants modified from current-invariant *)
  let newly_generated_inv : Inv.t CPSet.t = 
    CPSet.fold
      refine_targets
      ~init:CPSet.empty
      ~f:(
        fun accset (vtx, bp) ->
        (* if the refine target is entry or exit vertex *)
        if (vtx = igi.igi_entryvtx || vtx = igi.igi_exitvtx)
        then (CPSet.union (refine_T (cur_inv, bp, vtx, igi)) accset)
        (* else (refine target is loop-vertex) *)
        else (CPSet.union (refine_L (cur_inv, bp, vtx, igi)) accset)
      )
  in
  (* return invariant set. Set union processing with existing inv-set will be performed at the module "Prover" *)
  newly_generated_inv
end (* function generate end *)
