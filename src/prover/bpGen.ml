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
      - accum-bp : Bp.basic_node list
      Return: Bp.basic_node list  (* reversed list. It should be reversed after to get normal order. *)
    1. If Assume-cond is (Some _), it means that the previous vertex requires to put assumption (BI_assume)
      - The reason to insert assume-condition "after" conditional vertex appears (not the current vertex is conditional vertex)
        - If Basicpath-vertexlist ends with loop-vertex, there are no reason to put assume-inst in basicpath.
        - If the previous vertex is loop-vertex (basicpath-starts with loop-vertex) or if-vertex,
          it is okay to put BI_assume in front of current vertex's basicpath instruction.
        - It is important to check in-edge's label after conditional vertex (If_true and If_false)
      - Normal edge-label can appear after BI_assume. In this case, the BI_assume instruction is inserted to force
        basic safety property. So the normal edge is considered as true-branch.
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
  let rec foldf : (Cfg.vertex * ProverLib.Vlang.t) option -> Cfg.edge_label -> Cfg.vertex -> Cfg.vertex list -> ProverLib.Bp.basic_node list -> ProverLib.Bp.basic_node list
  =fun assume_cond_opt in_edg_lbl cur_vtx remain_vtc acc_bnl -> begin
    (* 1 *)
    let assume_instlst : ProverLib.Bp.basic_node list = begin
      match assume_cond_opt, in_edg_lbl with
      | None, _ -> []
      | Some (prev_vtx, prev_cond), Normal -> [{
          glenv_ref=glenv_ref;
          cfgvtx=prev_vtx;
          inst=(BI_assume prev_cond);
        }]
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
    let (cur_instlst, next_assume_cond) : (ProverLib.Bp.basic_node list * (Cfg.vertex * ProverLib.Vlang.t) option) = begin
      let open ProverLib.Vlang.Formula in
      let cstr_bn : ProverLib.Bp.inst -> ProverLib.Bp.basic_node = fun i -> {glenv_ref=glenv_ref; cfgvtx=cur_vtx; inst=i} in  (* sugar *)
      let rtcv = VlGen.read_type_cfgvar cfg in (* "rtcv" : syntax sugar *)
      let cvoc = VlGen.create_var_of_cfgvar glenv_ref cfg in  (* "cvoc" : syntax sugar *)
      let eoce = VlGen.expr_of_cfgexpr glenv_ref cfg in (* "eoce" : syntax sugar *)
      match cur_stmt with
      (* ASSIGNMENT CASE *)
      | Cfg_assign (v, e) ->
        let v_expr = eoce e in
        let assign_bi = BI_assign (rtcv v, v, v_expr) in
        (* check if RHS expression requires basic safety checks *)
        (match v_expr with
        (* mutez overflow underflow *)
        | V_add_mmm (e1, e2) -> 
          let no_overflow = VF_add_mmm_no_overflow (e1, e2) in
          let assert_bi = BI_assert (no_overflow, Q_mutez_arith_safety) in 
          ([cstr_bn assert_bi; cstr_bn assign_bi], Some (cur_vtx, no_overflow)) (* If you want to remove assumption, modify this "Some _" to None *)
        | V_sub_mmm (e1, e2) -> 
          let no_underflow = VF_sub_mmm_no_underflow (e1, e2) in
          let assert_bi = BI_assert (no_underflow, Q_mutez_arith_safety) in
          ([cstr_bn assert_bi; cstr_bn assign_bi], Some (cur_vtx, no_underflow))
        | V_mul_mnm (e1, e2) ->
          let no_overflow = VF_mul_mnm_no_overflow (e1, e2) in
          let assert_bi = BI_assert (no_overflow, Q_mutez_arith_safety) in
          ([cstr_bn assert_bi; cstr_bn assign_bi], Some (cur_vtx, no_overflow))
        | V_mul_nmm (e1, e2) ->
          let no_overflow = VF_mul_mnm_no_overflow (e1, e2) in
          let assert_bi = BI_assert (no_overflow, Q_mutez_arith_safety) in
          ([cstr_bn assert_bi; cstr_bn assign_bi], Some (cur_vtx, no_overflow))
        (* lsl, lsr runtime failure *)
        | V_shiftL_nnn (e1, e2) ->
          let rhs_in_256 = VF_shiftL_nnn_rhs_in_256 (e1, e2) in
          let assert_bi = BI_assert (rhs_in_256, Q_int_nat_shift_safety) in
          ([cstr_bn assert_bi; cstr_bn assign_bi], Some (cur_vtx, rhs_in_256))
        | V_shiftR_nnn (e1, e2) -> 
          let rhs_in_256 = VF_shiftR_nnn_rhs_in_256 (e1, e2) in
          let assert_bi = BI_assert (rhs_in_256, Q_int_nat_shift_safety) in
          ([cstr_bn assert_bi; cstr_bn assign_bi], Some (cur_vtx, rhs_in_256))
        (* others *)
        | _ ->  ([cstr_bn assign_bi], None)
        )
      (* MICSE-CHECK CASE *)
      | Cfg_micse_check_value v -> ([cstr_bn (BI_assert (VF_mich_micse_check_value (cvoc v), Q_assertion))], None)(*([cstr_bn (BI_assert (VF_eq (cvoc v, ProverLib.Vlang.Expr.V_lit_bool true), Q_assertion))], None)*)
      (* CONDITIONAL CASES *)
      | Cfg_if v -> [cstr_bn BI_skip], Some (cur_vtx, VF_mich_if (cvoc v))
      | Cfg_if_none v -> [cstr_bn BI_skip], Some (cur_vtx, VF_mich_if_none (cvoc v))
      | Cfg_if_left v -> [cstr_bn BI_skip], Some (cur_vtx, VF_mich_if_left (cvoc v))
      | Cfg_if_cons v -> [cstr_bn BI_skip], Some (cur_vtx, VF_mich_if_cons (cvoc v))
      | Cfg_loop v -> [cstr_bn BI_skip], Some (cur_vtx, VF_mich_loop (cvoc v))
      | Cfg_loop_left v -> [cstr_bn BI_skip], Some (cur_vtx, VF_mich_loop_left (cvoc v))
      | Cfg_map v -> begin
          match rtcv v with
          | T_list _ -> [cstr_bn BI_skip], Some (cur_vtx, VF_mich_map_l (cvoc v))
          | T_map _ -> [cstr_bn BI_skip], Some (cur_vtx, VF_mich_map_m (cvoc v))
          | _ -> Stdlib.failwith "BpGen.bp_of_vtxlst : foldf : cur_instlst : cur_stmt : Cfg_map : match failed"
        end
      | Cfg_iter v -> begin
          match rtcv v with
          | T_list _ -> [cstr_bn BI_skip], Some (cur_vtx, VF_mich_iter_l (cvoc v))
          | T_set _ -> [cstr_bn BI_skip], Some (cur_vtx, VF_mich_iter_s (cvoc v))
          | T_map _ -> [cstr_bn BI_skip], Some (cur_vtx, VF_mich_iter_m (cvoc v))
          | _ -> Stdlib.failwith "BpGen.bp_of_vtxlst : foldf : cur_instlst : cur_stmt : Cfg_iter : match failed"
        end
      (* FAIL CASE *)
      | Cfg_failwith _ -> ([cstr_bn (BI_assume VF_false)], None)
      (* OTHER CASES *)
      | Cfg_skip
      | Cfg_drop _
      | Cfg_swap
      | Cfg_dig
      | Cfg_dug
      | Cfg_micse_check_entry -> ([cstr_bn BI_skip], None)
    end in
    let new_bp = (cur_instlst @ assume_instlst) @ acc_bnl in
    (* escape cond *)
    if remain_vtc = [] then new_bp else
    (* recursively call fold function - remain_vtc is not empty list, WARNING: PRECONDITION *)
    let next_vtx = List.hd remain_vtc in
    let next_edg_lbl = Cfg.G.find_edge cfg.flow cur_vtx next_vtx |> (fun (_, lbl, _) -> lbl) in
    foldf next_assume_cond next_edg_lbl next_vtx (List.tl remain_vtc) new_bp 
  end in
  let rev_order_bnl : ProverLib.Bp.basic_node list = foldf None Normal (List.hd vtxlst) (List.tl vtxlst) [] in
  let exit_vtx : PreLib.Cfg.vertex = try let q = List.hd rev_order_bnl in q.cfgvtx with | _ -> Stdlib.failwith "BpGen : bp_of_vtxlst : exit_vtx" in
  let normal_order_bnl : ProverLib.Bp.basic_node list = List.rev rev_order_bnl in
  let entry_vtx : PreLib.Cfg.vertex = try let q = List.hd normal_order_bnl in q.cfgvtx with | _ -> Stdlib.failwith "BpGen : bp_of_vtxlst : entry_vtx" in
  { entry_vtx = entry_vtx;
    exit_vtx = exit_vtx;
    content = normal_order_bnl;
  }
end (* function bp_of_vtxlst end *)
