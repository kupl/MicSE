(* sugar *)
module CPSet = Core.Set.Poly

type validate_result = {
  inductive : bool;
  p : VcGen.query_vc CPSet.t; (* proved query set *)
  u : VcGen.query_vc CPSet.t; (* unproved query set *)
}

let validate : (Utils.Timer.t ref * ProverLib.Inv.t * (ProverLib.Inv.t -> VcGen.v_cond) list * (ProverLib.Inv.t -> ProverLib.Vlang.t)) -> validate_result
= let open ProverLib in
  let is_valid : Smt.ZSolver.validity -> bool = (function | Smt.ZSolver.VAL -> true | _ -> false) in
  fun (timer, inv_candidate, vc_fl, isc_f) -> begin
  (* vcl : verification condition list *)
  (* isc : initial storage condition *)
  (* indt_vc : inductiveness condition *)
  let vcl : VcGen.v_cond list = List.map (fun x -> x inv_candidate) vc_fl in
  let isc : Vlang.t = isc_f inv_candidate in
  (* Validate Inductiveness (initial-storage-cond reflected in "indt_vc") *)
  let indt_vc_l : Vlang.t list = List.map (fun x -> Vlang.Formula.VF_and [isc; x.VcGen.path_vc]) vcl in
  let indt_validity, _ =
    let rec foldf : (bool * Vlang.t list) -> (bool * Vlang.t list)
    =fun (acc_validity_b, remain_indt_vc_l) -> begin
      (* check if timeover *)
      if Utils.Timer.is_timeout timer then false, remain_indt_vc_l else
      (* this fold-function escapes earlier if any of indt_vc in list fails *)
      if Stdlib.not acc_validity_b then acc_validity_b, remain_indt_vc_l else
      match remain_indt_vc_l with
      | [] -> acc_validity_b, []
      | h :: t -> foldf (Verifier.verify h |> Stdlib.fst |> is_valid, t) (* Verifier.verify runs validity check. *)
    end in
    foldf (true, indt_vc_l)
  in
  (* (* this expression is deprecated, since big and-expression would affect performance. *) 
    let indt_vc : Vlang.t = Vlang.Formula.VF_and (isc :: (List.map (fun x -> x.VcGen.path_vc) vcl)) in 
  *)
  if Stdlib.not indt_validity then ({inductive=false; p=CPSet.empty; u=CPSet.empty}) else
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
    let rec foldf : (VcGen.query_vc CPSet.t * VcGen.query_vc CPSet.t) -> (VcGen.query_vc list) -> (VcGen.query_vc CPSet.t * VcGen.query_vc CPSet.t)
    =fun (p_acc, u_acc) qlist -> begin
      (* check if timeover *)
      if Utils.Timer.is_timeout timer then (p_acc, List.fold_left CPSet.add u_acc qlist) else
      (* if the list empty then escape. *)
      if qlist = [] then (p_acc, u_acc) else
      let (q, rqlst) = (List.hd qlist, List.tl qlist) in
      (* this fold-function separates validated-queries and invalidated/unproven-queries *)
      let q_validity, _ = Verifier.verify q.qvc_fml in
      let (npacc, nuacc) = if is_valid q_validity then (CPSet.add p_acc q, u_acc) else (p_acc, CPSet.add u_acc q) in
      foldf (npacc, nuacc) rqlst
    end in
    foldf (CPSet.empty, CPSet.empty) qset
  in
  {inductive=true; p=pset; u=uset}
end
