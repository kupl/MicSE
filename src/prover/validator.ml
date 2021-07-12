(* sugar *)
module CPSet = Core.Set.Poly

type validate_result = {
  inductive : bool;
  p : VcGen.query_vc CPSet.t; (* proved query set *)
  u : VcGen.query_vc CPSet.t; (* unproved query set *)
  allq : (ProverLib.Bp.query_category * PreLib.Cfg.vertex) CPSet.t; (* all queries *)
}

(* Query-Comparator and Query-Set 
  It will be used to remove duplicated queries.
  This module will be used in the function "validate"
*)
module QueryOT = struct
  type t = VcGen.query_vc
  let compare : t -> t -> int =
    let open VcGen in
    fun q1 q2 -> Stdlib.compare (q1.qvc_fml, q1.qvc_cat, q1.qvc_vtx) (q2.qvc_fml, q2.qvc_cat, q2.qvc_vtx)
end
module QuerySet = Set.Make(QueryOT)


let validate : (Utils.Timer.t ref * ProverLib.Inv.t * (ProverLib.Inv.t -> VcGen.v_cond) list * (ProverLib.Inv.t -> ProverLib.Vlang.t) * (ProverLib.Bp.query_category * PreLib.Cfg.vertex -> bool)) -> validate_result
= let open ProverLib in
  let is_valid : Smt_deprecated.ZSolver.validity -> bool = (function | Smt_deprecated.ZSolver.VAL -> true | _ -> false) in
  fun (timer, inv_candidate, vc_fl, isc_f, is_up_query) -> begin
  (* vcl : verification condition list *)
  (* isc : initial storage condition. deprecated *)
  (* indt_vc : inductiveness condition *)
  (* is_up_query : is_unproved_query? *)
  let vcl : VcGen.v_cond list = List.map (fun x -> x inv_candidate) vc_fl in
  let _ : Vlang.t = isc_f inv_candidate in (* "isc" : deprecated *)
  (* Validate Inductiveness (initial-storage-cond reflected in "indt_vc") *)
  
  (* (* this expression is deprecated, since we cannot prove "this procedure" has the semantic equals to the "the semantic of validity-checking of whole big-formula of path-inductiveness". *)
  (* Naive Optimization used here *)
  (* let indt_vc_l : Vlang.t list = List.map (fun x -> Vlang.Formula.VF_and [isc; x.VcGen.path_vc] |> VlangUtil.NaiveOpt.run) vcl in (* deprecated *) *)
  let indt_vc_l : Vlang.t list = List.map (fun x -> VlangUtil.NaiveOpt.run x.VcGen.path_vc) vcl in
  let indt_validity, _ =
    let rec foldf : (bool * Vlang.t list) -> (bool * Vlang.t list)
    =fun (acc_validity_b, remain_indt_vc_l) -> begin
      (* check if timeover *)
      if Utils.Timer.is_timeout timer then false, remain_indt_vc_l else
      (* this fold-function escapes earlier if any of indt_vc in list fails *)
      if Stdlib.not acc_validity_b then acc_validity_b, remain_indt_vc_l else
      match remain_indt_vc_l with
      | [] -> acc_validity_b, []
      | h :: t -> 
        let (validity, _) : Smt_deprecated.ZSolver.validity * Smt_deprecated.ZModel.t option = 
          (* if the formula trivial, do not pass it to z3 *)
          (match h with
          | Vlang.Formula.VF_true -> (VAL, None)
          | Vlang.Formula.VF_false -> (INVAL, None)
          | _ -> (Verifier.verify h)
          )
        in 
        foldf (Smt_deprecated.ZSolver.is_valid validity, t)
    end in
    foldf (true, indt_vc_l)
  in
  *)
  (* (* deprecated, since "isc" deprecated above*) let indt_vc : Vlang.t = Vlang.Formula.VF_and (isc :: (List.map (fun x -> x.VcGen.path_vc) vcl)) in *)
  let indt_vc : Vlang.t = Vlang.Formula.VF_and (List.map (fun x -> x.VcGen.path_vc) vcl) in
  let (indt_validity_i, _) = Verifier.verify indt_vc in
  let indt_validity = indt_validity_i |> is_valid in
  if Stdlib.not indt_validity then ({inductive=false; p=CPSet.empty; u=CPSet.empty; allq=CPSet.empty}) else
  (* Validate Query *)
  (* debug *) let _ = print_endline "DEBUG" in
  (* debug *) let _ = List.iter (fun x -> x.VcGen.path_vc |> Vlang.string_of_formula |> print_endline) vcl in
  let qset : VcGen.query_vc list =
    List.fold_left 
      (* Design choice: CPSet.union has worse time complexity than Fold+Add, but not tested which one is better. *)
      (fun accll vc -> (CPSet.to_list vc.VcGen.query_vcs) :: accll) 
      []
      vcl 
    |> List.flatten
    (* REMOVE DUPLICATED QUERIES 
        In fact, it is inefficient to remove duplicated queries in validate-process
        than remove queries while constructing basic-path.
        However, in this implementation, it is hard to distinguish
        which queries are duplicated for (Inv.t -> VcGen.v_cond) datatype.
    *)
    |> QuerySet.of_list
    |> QuerySet.elements
    (* REMOVE PROVED QUERIES
    *)
    |> List.filter (fun q -> is_up_query (q.VcGen.qvc_cat, q.VcGen.qvc_vtx))
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
      let q_validity, mopt = Verifier.verify q.qvc_fml in
      let _ = mopt in
(*       
      (* debug *) let _ = 
        (match mopt with
        | Some m -> print_endline "FORMULA : "; print_endline (Vlang.Formula.to_string q.qvc_fml); print_endline "ZMODEL : "; Smt_deprecated.ZModel.to_string m |> print_endline 
          | _ -> ()

        )
      in *)

      let (npacc, nuacc) = if is_valid q_validity then (CPSet.add p_acc q, u_acc) else (p_acc, CPSet.add u_acc q) in
      foldf (npacc, nuacc) rqlst
    end in
    foldf (CPSet.empty, CPSet.empty) qset
  in
  let allq_val : (ProverLib.Bp.query_category * PreLib.Cfg.vertex) CPSet.t =
    let open VcGen in
    List.fold_left (fun accset q -> CPSet.add accset (q.qvc_cat, q.qvc_vtx)) CPSet.empty qset
  in
  {inductive=true; p=pset; u=uset; allq=allq_val}
end
