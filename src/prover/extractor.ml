open ProverLib

(************************************************)
(************************************************)

let loop_inv_vtx = ref []

let rec extract : Cfg.t -> Bp.raw_t_list
=fun cfg -> begin
  let entry_bp = Bp.create_new_bp cfg.main_entry cfg.main_exit in
  let result = translate entry_bp cfg.main_entry cfg in
  { bps=result; trx_inv_vtx=[cfg.main_entry; cfg.main_exit]; loop_inv_vtx=(!loop_inv_vtx) }
end

and translate : Bp.t -> Cfg.vertex -> Cfg.t -> Bp.t list
=fun cur_bp cur_vtx cfg -> begin
  let stmt = Cfg.read_stmt_from_vtx cfg cur_vtx in
  let succ = Cfg.read_succ_from_vtx cfg cur_vtx in
  let make_branch_bp f_if = begin
    let f_else = Bp.create_cond_not f_if in
    let inst_if = Bp.create_inst_assume f_if in
    let inst_else = Bp.create_inst_assume f_else in
    let new_bp_if = Bp.update_body cur_bp inst_if in
    let new_bp_else = Bp.update_body cur_bp inst_else in
    (new_bp_if, new_bp_else)
  end in
  let make_loop_bp loop = begin
    let (terminated_bp, new_bp) = Bp.create_cut_bp cur_bp loop in
    let loop_bp = Bp.create_new_bp loop loop in
    (terminated_bp, loop_bp, new_bp)
  end in
  let normal_search new_bp (edge, vtx) result = begin
    if Cfg.is_edge_normal edge then result@(translate new_bp vtx cfg)
    else result (*raise (Failure "Extractor.translate: Wrong edge label")*)
  end in
  let branch_search (new_bp_if, new_bp_else) (edge, vtx) result = begin
    if Cfg.is_edge_true edge then result@(translate new_bp_if vtx cfg)
    else if Cfg.is_edge_false edge then result@(translate new_bp_else vtx cfg)
    else begin
      let result_set = Core.Set.Poly.of_list (result@(translate new_bp_if vtx cfg)@(translate new_bp_else vtx cfg)) in
      let result = Core.Set.Poly.to_list result_set in
      result
    end (*raise (Failure "Extractor.translate: Wrong edge label")*)
  end in
  if Cfg.is_main_exit cfg cur_vtx then [cur_bp]
  else begin
    match stmt with
    | Cfg_assign (id, e) -> begin
        let inst = Bp.create_inst_assign (id, e) in
        let new_bp = Bp.update_body cur_bp inst in
        let search = normal_search new_bp in
        let result = Core.List.fold_right succ ~f:search ~init:[] in
        result
      end
    | Cfg_skip | Cfg_drop _ | Cfg_swap | Cfg_dig | Cfg_dug -> begin
        let inst = Bp.create_inst_skip () in
        let new_bp = Bp.update_body cur_bp inst in
        let search = normal_search new_bp in
        let result = Core.List.fold_right succ ~f:search ~init:[] in
        result
      end
    | Cfg_if id -> begin
        let f_if = Bp.create_cond_is_true id in
        let bps = make_branch_bp f_if in
        let search = branch_search bps in
        let result = Core.List.fold_right succ ~f:search ~init:[] in
        result
      end
    | Cfg_if_none id -> begin
        let f_if = Bp.create_cond_is_none id in
        let bps = make_branch_bp f_if in
        let search = branch_search bps in
        let result = Core.List.fold_right succ ~f:search ~init:[] in
        result
      end
    | Cfg_if_left id -> begin
        let f_if = Bp.create_cond_is_left id in
        let bps = make_branch_bp f_if in
        let search = branch_search bps in
        let result = Core.List.fold_right succ ~f:search ~init:[] in
        result
      end
    | Cfg_if_cons id -> begin
        let f_if = Bp.create_cond_is_cons id in
        let bps = make_branch_bp f_if in
        let search = branch_search bps in
        let result = Core.List.fold_right succ ~f:search ~init:[] in
        result
      end
    | Cfg_loop _ | Cfg_loop_left _ | Cfg_map _ | Cfg_iter _ -> begin
        let _ = loop_inv_vtx := cur_vtx::!loop_inv_vtx in
        let (terminated_bp, loop_bp, new_bp) = make_loop_bp cur_vtx in
        let search = normal_search new_bp in
        let result = terminated_bp::[] in
        let result = loop_bp::result in
        let result = Core.List.fold_right succ ~f:search ~init:result in
        result
      end
    | Cfg_failwith _ -> begin
        let inst = Bp.create_inst_skip () in
        let new_bp = Bp.update_body cur_bp inst in
        [new_bp]
      end
    | _ -> raise (Failure "Extractor.translate: Not Implemented.")
  end
end