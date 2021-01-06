
(* sugar *)
module CPSet = Core.Set.Poly
module CPMap = Core.Map.Poly

open PreLib
open ProverLib


let collect_set : ('a CPSet.t) list -> ('a CPSet.t)
=fun setlist -> begin
  match setlist with
  | [] -> CPSet.empty
  | h :: [] -> h
  | h :: t -> List.fold_left (fun accs set -> CPSet.fold set ~init:accs ~f:CPSet.add) h t
end (* function collect_set end *)


let refine_T : Inv.t * Bp.t * Cfg.vertex * Inv.invgen_info -> (Inv.t CPSet.t)
= let open Vlang in
  (* currnet-invariant, basic-path, trx-vertex (main-entry or main-exit), invariant-generation-info *)
  fun (cur_inv, bp, _, igi) -> begin
  (* update function : it combines former one and the given one with VF_and. *)
  let update : Vlang.t -> Inv.t = fun fmla -> {cur_inv with trx_inv=(VF_and [cur_inv.trx_inv; fmla]);} in
  (* 0. collect all components *)
  let all_components : Component.t = 
    collect_set [
      igi.igi_stgcomp;
      igi.igi_glvar_comp;
      CPSet.map bp.appeared_vars ~f:Component.comp_of_vexpr;
    ]
  in
  (* 1. mutez-equal formula *)
  let mutez_comps : Component.t = Component.filter_typ (fun x -> x = Ty.T_mutez) all_components in
  let mutez_equal_fmlas : Formula.t CPSet.t = ProverLib.InvRecipe.mutez_equal mutez_comps in
  (* 2. mtzmap-partial-sum formula *)
  let mtzmap_comps : (Ty.t, Component.t) CPMap.t = Component.filter_types (function | Ty.T_map (_, Ty.T_mutez) -> true | _ -> false) all_components in
  let mtzmap_partial_sum_equal_fmlas : Formula.t CPSet.t = 
    CPMap.fold
      mtzmap_comps
      ~init:CPSet.empty
      ~f:(
        fun ~key ~data fmla_accset ->
        let key_comps : Component.t = Component.filter_typ (fun x -> x = key) all_components in
        (* we already collect mutez components at the above procedure *)
        let mm_ps_fmlas : Vlang.Formula.t CPSet.t = ProverLib.InvRecipe.mtzmap_partial_sum data key_comps mutez_comps in
        CPSet.union mm_ps_fmlas fmla_accset
      )
  in
  (* "+oo". collect all formulas and return *)
  let fmlas : Formula.t CPSet.t list = 
    [ mutez_equal_fmlas;
      mtzmap_partial_sum_equal_fmlas;
    ]
  in
  collect_set (List.map (fun x -> CPSet.map x ~f:update) fmlas)
end (* function refine_T end *)

let refine_L : Inv.t * Bp.t * Cfg.vertex * Inv.invgen_info -> (Inv.t CPSet.t)
= (* currnet-invariant, basic-path, vertex (loop-vtx), invariant-generation-info *)
  fun (cur_inv, _, vtx, igi) -> begin
  (* update function : it combines former one and the given one with VF_and. *)
  let update : Vlang.t -> Inv.t
  =fun fmla -> begin
    let new_linv : (int, Vlang.t) CPMap.t = 
      CPMap.update cur_inv.loop_inv vtx ~f:(function | None -> fmla | Some li -> VF_and [li; fmla])
    in
    {cur_inv with loop_inv=new_linv}
  end in
  (* for current design choice and implementation, igi is enough. constants in basicpath are not used for now. *)
  let _ = ignore cur_inv, ignore vtx, ignore igi, ignore update in
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
