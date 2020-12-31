(* BasicPath Generator (from Cfg) *)

(* sugar *)
module CPSet = Core.Set.Poly

(* for now, main-entry will be the only strat_vtx in "collect_bp_vtx". 
  TODO: if there are any needs to generate basic path at the lambda value,
    some code should call "collect_bp_vtx" function with lambda-entry vertex too.
*)

let collect_bp_vtx : PreLib.Cfg.t -> PreLib.Cfg.vertex -> (PreLib.Cfg.vertex list) CPSet.t
= let open PreLib.Cfg in
  fun cfg start_vtx -> begin
  let rec foldf : vertex CPSet.t -> vertex -> (vertex list * (vertex list) CPSet.t) -> (vertex list) CPSet.t
  =fun visited_loopvtx_set cur_vtx (accl, acclset) -> begin
    let common_bp_cons = cur_vtx :: accl in
    (* check if current search point is main-exit vertex *)
    if cur_vtx = cfg.main_exit then (CPSet.add acclset (List.rev common_bp_cons)) else (
      (* serach next-search candidates first *)
      let outedg : G.edge list = 
        List.filter 
        (fun (_, el, _) -> match el with | Normal | If_true | If_false -> true | _ -> false) 
        (G.succ_e cfg.flow cur_vtx) 
      in
      (* dfs by recursively call foldf *)
      let stmt = t_map_find ~errtrace:("BpGen.collect_bp_vtx : foldf : " ^ (Stdlib.string_of_int cur_vtx)) cfg.vertex_info cur_vtx in
      match stmt with
      | Cfg_loop _ | Cfg_loop_left _ | Cfg_map _ | Cfg_iter _ -> (
          let new_acclset = CPSet.add acclset (List.rev common_bp_cons) in
          (* if we enter loop-vtx again, do not search further. *)
          if CPSet.mem visited_loopvtx_set cur_vtx then (CPSet.add acclset (List.rev common_bp_cons)) else
          (* else, *)
          let new_visited_set = CPSet.add visited_loopvtx_set cur_vtx in
          List.fold_left (fun acc (_, _, ov) -> foldf new_visited_set ov ([cur_vtx], acc)) new_acclset outedg
        )
      | Cfg_failwith _ -> CPSet.add acclset (List.rev common_bp_cons)
      | _ -> List.fold_left (fun acc (_, _, ov) -> foldf visited_loopvtx_set ov (common_bp_cons, acc)) acclset outedg (* acclset will be accumulated with List.fold_left / for every normal execution paths. *)
    )
  end in
  foldf CPSet.empty start_vtx ([], CPSet.empty)
end (* function collect_bp_vtx end *)



let bp_of_vtxlst : ProverLib.GlVar.Env.t ref -> PreLib.Cfg.t -> (PreLib.Cfg.vertex list) -> ProverLib.Bp.t
= let open PreLib in
  let open ProverLib.Bp in
  let open ProverLib.Vlang in
  fun glenv_ref cfg vtxlst -> begin
    (* Fold Function :
        PRECONDITION: (* IMPORTANT *)
        - There are NO TWO EDGES whose source vertex is same AND sink vertex is same, for any two vertices in CFG.
        Parameter:
        - Assume-Cond : (vertex * formula) option (* previous-vertex & previous stmt's condition *)
        - In-edge Label : edge_label
        - current-vertex : vertex
        - remain-vertice : vertex list
        - accum-bp : Bp.t
        Return: Bp.t
      1. If Assume-cond is (Some _), it means that the previous vertex requires to put assumption (BI_assume)
        - The reason to insert assume-condition "after" conditional vertex appears (not the current vertex is conditional vertex)
          - If Basicpath-vertexlist ends with loop-vertex, there are no reason to put assume-inst in basicpath.
          - If the previous vertex is loop-vertex (basicpath-starts with loop-vertex) or if-vertex,
            it is okay to put BI_assume in front of current vertex's basicpath instruction.
          - It is important to check in-edge's label after conditional vertex (If_true and If_false)
      2. After assume-cond for previous vertex encoded, "foldf" inspects current-vertex's stmt.
        - Plain Assignment
          - Basic Safety Properties
            - BI_assign
            - BI_assert (if user wants to check basic safety property)
            - Next Assume-Condition : Some _
          - Others
            - BI_assign
            - Next Assume-Condition : None
        - MICSE-CHECK
          - BI_assert
          - Next Assume-Condition : None
        - LOOP, IF, and other conditional stmt
          - BI_skip
          - Next Assume-Condition : Some _
        - Fail
          - BI_assume (false)
          - Next Assume-Condition : None 
            (* There are no next vertex for this case (refer to "collect_bp_vtx" implementation), 
              so we put the assume-stmt earlier.
            *)
        - Others (e.g. stack operation statements)
          - BI_skip
          - Next Assume-Condition : None
    *)
    let rec foldf : (Cfg.vertex * ProverLib.Vlang.t) option -> Cfg.edge_label -> Cfg.vertex -> Cfg.vertex list -> ProverLib.Bp.t -> ProverLib.Bp.t
    =fun assume_cond_opt in_edg_lbl cur_vtx remain_vtc acc_bp -> begin
      (* 1 *)
      let assume_instlst : ProverLib.Bp.t = begin
        match assume_cond_opt, in_edg_lbl with
        | None, _ -> []
        | Some (prev_vtx, prev_cond), If_true -> [{
            glenv_ref=glenv_ref;
            cfgvtx=prev_vtx;
            inst=(BI_assume prev_cond);
          }]
        | Some (prev_vtx, prev_cond), If_false -> [{
            glenv_ref=glenv_ref;
            cfgvtx=prev_vtx;
            inst=(BI_assume (Formula.VF_not prev_cond));
          }]
        | _ -> Stdlib.failwith ("BpGen.bp_of_vtxlst : foldf : assume_instlst : match failed in cur-vtx : " ^ (Stdlib.string_of_int cur_vtx))
      end in
      (* 2 *)
      let cur_stmt = Cfg.t_map_find ~errtrace:("BpGen.bp_of_vtxlst : foldf : cur_stmt : " ^ (Stdlib.string_of_int cur_vtx)) cfg.vertex_info cur_vtx in
      let (cur_instlst, next_assume_cond) : (ProverLib.Bp.t * (Cfg.vertex * ProverLib.Vlang.t) option) = begin
        match cur_stmt with
        (* ASSIGNMENT CASE *)
        | Cfg_assign (_, _) -> [], None (* TODO *)
        (* MICSE-CHECK CASE *)
        | Cfg_micse_check_value _ -> [], None (* TODO *)
        (* CONDITIONAL CASES *)
        | Cfg_if _ -> [], None (* TODO *)
        | Cfg_if_none _ -> [], None (* TODO *)
        | Cfg_if_left _ -> [], None (* TODO *)
        | Cfg_if_cons _ -> [], None (* TODO *)
        | Cfg_loop _ -> [], None (* TODO *)
        | Cfg_loop_left _ -> [], None (* TODO *)
        | Cfg_map _ -> [], None (* TODO *)
        | Cfg_iter _ -> [], None (* TODO *)
        (* FAIL CASE *)
        | Cfg_failwith _ -> [], None (* TODO *)
        (* OTHER CASES *)
        | Cfg_skip
        | Cfg_drop _
        | Cfg_swap
        | Cfg_dig
        | Cfg_dug
        | Cfg_micse_check_entry -> ([{glenv_ref=glenv_ref; cfgvtx=cur_vtx; inst=BI_skip}], None)
      end in
      let new_bp = (cur_instlst @ assume_instlst) @ acc_bp in
      (* escape cond *)
      if remain_vtc = [] then List.rev new_bp else
      (* recursively call fold function - remain_vtc is not empty list, WARNING: PRECONDITION *)
      let next_vtx = List.hd remain_vtc in
      let next_edg_lbl = Cfg.G.find_edge cfg.flow cur_vtx next_vtx |> (fun (_, lbl, _) -> lbl) in
      foldf next_assume_cond next_edg_lbl next_vtx (List.tl remain_vtc) new_bp 
    end in
    foldf None Normal (List.hd vtxlst) (List.tl vtxlst) []
  end (* function bp_of_vtxlst end *)



(* 
let rec instlst_of_cfgstmt : ProverLib.GlVar.Env.t ref -> PreLib.Cfg.t -> PreLib.Cfg.edge_label -> PreLib.Cfg.vertex -> ProverLib.Bp.t
=fun glenv_ref cfg inedg vtx -> begin
  let cfgstmt : PreLib.Cfg.stmt = 
  
  match cfgstmt with
  | Cfg_assign (v, e) -> [] (* TODO. PLACEHOLDER *)
  | Cfg_skip -> [BI_skip]
  | Cfg_drop _ -> [BI_skip]
  | Cfg_swap _ -> [BI_skip]
  | Cfg_dig -> [BI_skip]
  | Cfg_dug -> [BI_skip]
  | Cfg_if v -> []
  | 
  (* escape condition *)
end (* function instlst_of_cfgstmt end *)

let rec bpset_start_from_vtx : ProverLib.GlVar.Env.t ref -> PreLib.Cfg.t -> PreLib.Cfg.vertex -> ProverLib.Bp.t CPSet.t
=fun glenv_ref cfg vtx -> begin
  (* DFS from the given vertex, construct a worklist if  *)
end (* function bpset_start_from_vtx *)
 *)

(*
let bp_of_vtxlst : ProverLib.GlVar.Env.t ref -> PreLib.Cfg.t -> (PreLib.Cfg.vertex list) -> ProverLib.Bp.t
= let open PreLib.Cfg in
  let open ProverLib in
  let open ProverLib.Vlang.Expr in
  let open ProverLib.Vlang.Formula in
  fun glenv_ref cfg vtxlst -> begin
  let eoce = VlGen.expr_of_cfgexpr glenv_ref cfg in (* "eoce" : syntax sugar *)
  let rtcv = VlGen.read_type_cfgvar cfg in (* "rtcv" : syntax sugar *)
  let rec foldf : stmt option -> edge_label -> vertex -> vertex list -> Bp.t -> Bp.t
  =fun cond_opt el curv vl bp -> begin
    let curv_cond_opt : Vlang.t option = (
      match t_map_find cfg.vertex_info curv with
      | Cfg_if v -> VF_mich_if (eoce v)
      | Cfg_if_none v -> VF_mich_if_none (eoce v)
      | Cfg_if_left v -> VF_mich_if_left (eoce v)
      | Cfg_if_cons v -> VF_mich_if_cons (eoce v)
      | Cfg_loop v -> VF_mich_loop (eoce v)
      | Cfg_loop_left v -> VF_mich_loop_left (eoce v)
      | Cfg_map v -> (
        match 
      )
      | Cfg_iter v ->
      | _ -> None
    ) in
    let front_assume : Bp.t = (
      match el with
      | If_true ->  (* TODO *)
      | If_false -> []  (* TODO *)
      | Failed -> [BI_assume Vlang.Formula.VF_false]
      | _ ->  (* TODO *)
    ) in
    let accbp = bp in (* TODO!!!!!!!!!!!!!!!!! *)
    match vl with
    | [] -> accbp
    | nv :: vtl ->
      let next_el = G.find_edge curv nv |> (fun (_, el, _) -> el) in
      foldf next_el nv vtl accbp
  end in
  foldf Normal (List.hd vtxlst) (List.tl vtxlst, [])
  |> Stdlib.snd
  |> List.rev

end (* function bp_of_vtxlst end *)
*)