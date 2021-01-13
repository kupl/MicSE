
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


let refine_T : Inv.t * Bp.t * Cfg.vertex * Inv.invgen_info * bool -> (Inv.t CPSet.t)
= let open Vlang in
  (* currnet-invariant, basic-path, trx-vertex (main-entry or main-exit), invariant-generation-info, is-initial-storage-exists *)
  fun (cur_inv, bp, _, igi, istg_exists) -> begin
  (* -1. If there are no initial storage condition given, the transaction invariant should be always "True".
      No more invariants will be generated.
  *)
  if Stdlib.not istg_exists then CPSet.empty else
  (* update function : it combines former one and the given one with VF_and. *)
  let update : Vlang.t -> Inv.t = fun fmla -> {cur_inv with trx_inv=(CPSet.add cur_inv.trx_inv fmla);} in
  (* 0. collect all components *)
  let bp_components : Component.t = BpUtil.collect_components (BpUtil.bp_substitution bp) in
  let strg_components : Component.t = igi.igi_stgcomp in
  let all_components : Component.t = 
    collect_set [
      bp_components;
      strg_components;
      (* igi.igi_stgcomp; *)
      (* igi.igi_glvar_comp; *)
      (* CPSet.map bp.appeared_vars ~f:Component.comp_of_vexpr;  *)
    ]
  in
  (* 1. mutez-equal formula *)
  let strg_mutez_comps : Component.t = Component.filter_typ (fun x -> x = Ty.T_mutez) strg_components in
  let mutez_equal_fmlas : Formula.t CPSet.t = ProverLib.InvRecipe.mutez_equal strg_mutez_comps in
  (* 2. mtzmap-partial-sum formula *)
  let mtzmap_comps : (Ty.t, Component.t) CPMap.t = Component.filter_types (function | Ty.T_map (_, Ty.T_mutez) -> true | _ -> false) strg_components in
  let mtzmap_partial_sum_equal_fmlas : Formula.t CPSet.t = 
    CPMap.fold
      mtzmap_comps
      ~init:CPSet.empty
      ~f:(
        fun ~key ~data fmla_accset ->
        let key_comps : Component.t = Component.filter_typ (fun x -> Ty.T_map (x, Ty.T_mutez) = key) all_components in
        (* we already collect mutez components at the above procedure *)
        let mm_ps_fmlas : Vlang.Formula.t CPSet.t = ProverLib.InvRecipe.mtzmap_partial_sum data key_comps strg_mutez_comps in
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
= let open Vlang in
  (* currnet-invariant, basic-path, vertex (loop-vtx), invariant-generation-info *)
  fun (cur_inv, bp, vtx, _) -> begin
  (* update function : it combines former one and the given one with VF_and. *)
  let update : Vlang.t -> Inv.t
  =fun fmla -> begin
    let new_linv : (int, Vlang.t CPSet.t) CPMap.t = 
      CPMap.update cur_inv.loop_inv vtx ~f:(function | None -> CPSet.singleton fmla | Some li -> CPSet.add li fmla)
    in
    {cur_inv with loop_inv=new_linv}
  end in
  (* 0. collect all components *)
  let all_components : Component.t =
    collect_set [
      (* igi.igi_stgcomp; *)
      (* igi.igi_glvar_comp; *)
      (* 
        (* At the first implementation, we think that the appeared_vars is enough. 
          However, appeared_vars has too many duplicated values, which results to too many
          invariant candidates to check.
          Now we use expressions from "substituted-basicpath".
          This approach removes some duplicated vlang-expressions.
        *)
        CPSet.map bp.appeared_vars ~f:Component.comp_of_vexpr; 
      *)
      BpUtil.collect_components (BpUtil.bp_substitution bp);
    ]
  in
  (* 1. mutez-equal formula *)
  let mutez_comps : Component.t = Component.filter_typ (fun x -> x = Ty.T_mutez) all_components in
  let mutez_equal_fmlas : Formula.t CPSet.t = ProverLib.InvRecipe.mutez_equal mutez_comps in
  (* . mtzmap-partial-sum formula *)
  let mtzmap_comps : (Ty.t, Component.t) CPMap.t = Component.filter_types (function | Ty.T_map (_, Ty.T_mutez) -> true | _ -> false) all_components in
  let mtzmap_partial_sum_equal_fmlas : Formula.t CPSet.t = 
    CPMap.fold
      mtzmap_comps
      ~init:CPSet.empty
      ~f:(
        fun ~key ~data fmla_accset ->
          let key_comps : Component.t = Component.filter_typ (fun x -> Ty.T_map (x, Ty.T_mutez) = key) all_components in
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
end (* function refine_L end *)


let generate : (Validator.validate_result * ProverLib.Inv.invgen_info * ProverLib.Inv.t * bool * ProverLib.Inv.t CPSet.t) -> ProverLib.Inv.t CPSet.t
= (* current-worklist, validation-result, invariant-generation-information, current-invariant, is-initial-stroage-exists, invariants-already-used *)
  fun (val_res, igi, cur_inv, istg_exists, invs_collected) -> begin
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
        then (CPSet.union (refine_T (cur_inv, bp, vtx, igi, istg_exists)) accset)
        (* else (refine target is loop-vertex) *)
        else (CPSet.union (refine_L (cur_inv, bp, vtx, igi)) accset)
      )
  in
  (* return invariant set. Set union processing with existing inv-set will be performed at the module "Prover" *)
  (* remove already-used invariants with "CPSet.diff" *)
  CPSet.diff newly_generated_inv invs_collected
end (* function generate end *)
