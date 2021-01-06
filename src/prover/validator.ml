(* sugar *)
module CPSet = Core.Set.Poly

type validate_result = {
  inductive : bool;
  p : VcGen.query_vc CPSet.t; (* proved query set *)
  u : VcGen.query_vc CPSet.t; (* unproved query set *)
}

let validate : (ProverLib.Inv.t * (ProverLib.Inv.t -> VcGen.v_cond) list * (ProverLib.Inv.t -> ProverLib.Vlang.t)) -> validate_result
= let open ProverLib in
  let is_valid : Smt.ZSolver.validity -> bool = (function | Smt.ZSolver.VAL -> true | _ -> false) in
  fun (inv_candidate, vc_fl, isc_f) -> begin
  (* vcl : verification condition list *)
  (* isc : initial storage condition *)
  (* indt_vc : inductiveness condition *)
  let vcl : VcGen.v_cond list = List.map (fun x -> x inv_candidate) vc_fl in
  let isc : Vlang.t = isc_f inv_candidate in
  let indt_vc : Vlang.t = Vlang.Formula.VF_and (isc :: (List.map (fun x -> x.VcGen.path_vc) vcl)) in
  (* Validate Inductiveness (initial-storage-cond reflected in "indt_vc") *)
  let indt_validity, _ = Verifier.verify indt_vc in (* Verifier.verify runs validity check. *)
  if Stdlib.not (is_valid indt_validity) then ({inductive=false; p=CPSet.empty; u=CPSet.empty}) else
  (* Validate Query *)
  let qset : VcGen.query_vc list =
    List.fold_left 
      (* Design choice: CPSet.union has worse time complexity than Fold+Add, but not tested which one is better. *)
      (fun accll vc -> (CPSet.to_list vc.VcGen.query_vcs) :: accll) 
      []
      vcl 
    |> List.flatten
  in
  let (pset, uset) : VcGen.query_vc CPSet.t * VcGen.query_vc CPSet.t = 
    let open VcGen in
    List.fold_left
      (fun (p_acc, u_acc) q -> 
        let q_validity, _ = Verifier.verify q.qvc_fml in
        if is_valid q_validity then (CPSet.add p_acc q, u_acc) else (p_acc, CPSet.add u_acc q)
      )
      (CPSet.empty, CPSet.empty)
      qset
  in
  {inductive=true; p=pset; u=uset}
end
