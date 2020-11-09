open ProverLib

(************************************************)
(************************************************)

let loop_inv_vtx = ref []

let exit_var = ref None

let rec extract : Pre.Lib.Cfg.t -> Bp.lst
=fun cfg -> begin
  let entry_bp = Bp.create_new_bp cfg.main_entry cfg.main_exit in
  let result = translate entry_bp cfg.main_entry cfg in
  Bp.create_bp_list
    ~bp_list:result
    ~entry:(Bp.create_inv_point ~vtx:cfg.main_entry ~var_opt:(Some (Pre.Lib.Cfg.param_storage_name)))
    ~exit:(Bp.create_inv_point ~vtx:cfg.main_exit ~var_opt:(Some (Option.get !exit_var)))
    ~loop:(Core.List.map !loop_inv_vtx ~f:(fun vtx -> Bp.create_inv_point ~vtx:vtx ~var_opt:None))
end

and translate : Bp.t -> Bp.vertex -> Pre.Lib.Cfg.t -> Bp.t list
=fun cur_bp cur_vtx cfg -> begin
  let stmt = Pre.Lib.Cfg.read_stmt_from_vtx cfg cur_vtx in
  let succ = Pre.Lib.Cfg.read_succ_from_vtx cfg cur_vtx in
  if Pre.Lib.Cfg.is_main_exit cfg cur_vtx then begin
    match stmt with
    | Cfg_assign (id, e) -> begin
        let assert_inst = create_basic_safety_property cur_vtx e (Pre.Lib.Cfg.CPMap.find_exn cfg.type_info id) in
        let new_bp = update_current_bp cur_bp assert_inst in
        let _ = if (Option.is_none !exit_var)
                then exit_var := Some (id)
                else if (Option.get !exit_var) <> id then raise (Failure "Extractor.translator: main-exit var conflict") in
        let inst = Bp.create_inst_assign id e in
        let new_bp' = update_current_bp new_bp (Some (cur_vtx, inst)) in
        [new_bp']
      end
    | _ -> raise (Failure "Extractor.translate: main-exit vertex error")
  end else begin
    match stmt with
    | Cfg_assign (id, e) -> begin
        let assert_inst = create_basic_safety_property cur_vtx e (Pre.Lib.Cfg.CPMap.find_exn cfg.type_info id) in
        let new_bp = update_current_bp cur_bp assert_inst in
        let inst = Bp.create_inst_assign id e in
        let new_bp' = update_current_bp new_bp (Some (cur_vtx, inst)) in
        let search = translate_search_normal cfg new_bp' in
        let result = Core.List.fold_right succ ~f:search ~init:[] in
        result
      end
    | Cfg_skip | Cfg_drop _ | Cfg_swap | Cfg_dig | Cfg_dug -> begin
        let inst = Bp.create_inst_skip in
        let new_bp = update_current_bp cur_bp (Some (cur_vtx, inst)) in
        let search = translate_search_normal cfg new_bp in
        let result = Core.List.fold_right succ ~f:search ~init:[] in
        result
      end
    | Cfg_if id -> begin
        let f_if = Bp.create_cond_is_true id in
        let bps = create_bp_of_branch cur_bp cur_vtx f_if in
        let search = translate_search_branch cfg bps in
        let result = Core.List.fold_right succ ~f:search ~init:[] in
        result
      end
    | Cfg_if_none id -> begin
        let f_if = Bp.create_cond_is_none id in
        let bps = create_bp_of_branch cur_bp cur_vtx f_if in
        let search = translate_search_branch cfg bps in
        let result = Core.List.fold_right succ ~f:search ~init:[] in
        result
      end
    | Cfg_if_left id -> begin
        let f_if = Bp.create_cond_is_left id in
        let bps = create_bp_of_branch cur_bp cur_vtx f_if in
        let search = translate_search_branch cfg bps in
        let result = Core.List.fold_right succ ~f:search ~init:[] in
        result
      end
    | Cfg_if_cons id -> begin
        let f_if = Bp.create_cond_is_cons id in
        let bps = create_bp_of_branch cur_bp cur_vtx f_if in
        let search = translate_search_branch cfg bps in
        let result = Core.List.fold_right succ ~f:search ~init:[] in
        result
      end
    | Cfg_loop _ | Cfg_loop_left _ | Cfg_map _ | Cfg_iter _ -> begin
        let _ = loop_inv_vtx := cur_vtx::!loop_inv_vtx in
        let (terminated_bp, loop_bp, new_bp) = create_bp_of_loop cur_bp cur_vtx in
        let search = translate_search_loop cfg (loop_bp, new_bp) in
        let result = terminated_bp::[] in
        let result = loop_bp::result in
        let result = Core.List.fold_right succ ~f:search ~init:result in
        result
      end
    | Cfg_failwith _ -> begin
        let inst = Bp.create_inst_skip in
        let new_bp = update_current_bp cur_bp (Some (cur_vtx, inst)) in
        [new_bp]
      end
    | Cfg_micse_check_entry -> begin
        let inst = Bp.create_inst_skip in
        let new_bp = update_current_bp cur_bp (Some (cur_vtx, inst)) in
        let search = translate_search_normal cfg new_bp in
        let result = Core.List.fold_right succ ~f:search ~init:[] in
        result
      end
    | Cfg_micse_check_value id -> begin
        let f_assert = Bp.create_cond_is_true id in
        let inst = Bp.create_inst_assert f_assert (read_loc_of_check cfg cur_vtx) (Bp.create_category_assertion) in
        let new_bp = update_current_bp cur_bp (Some (cur_vtx, inst)) in
        let search = translate_search_normal cfg new_bp in
        let result = Core.List.fold_right succ ~f:search ~init:[] in
        result
      end
    (*| _ -> raise (Failure "Extractor.translate: Not Implemented.")*)
  end
end

and translate_search_normal : Pre.Lib.Cfg.t -> Bp.t -> (Bp.edge * Bp.vertex) -> Bp.t list -> Bp.t list
=fun cfg new_bp (edge, vtx) result -> begin
  match edge with
  | Normal -> result@(translate new_bp vtx cfg)
  | If_true | If_false -> raise (Failure "Extractor.translate_search_normal: Wrong edge label")
  | Check_skip | Failed -> result
end

and translate_search_branch : Pre.Lib.Cfg.t -> (Bp.t * Bp.t) -> (Bp.edge * Bp.vertex) -> Bp.t list -> Bp.t list
=fun cfg (new_bp_if, new_bp_else) (edge, vtx) result -> begin
  match edge with
  | If_true -> result@(translate new_bp_if vtx cfg)
  | If_false -> result@(translate new_bp_else vtx cfg)
  | Normal | Failed -> raise (Failure "Extractor.translate_search_branch: Wrong edge label")
  | Check_skip -> result
end

and translate_search_loop : Pre.Lib.Cfg.t -> (Bp.t * Bp.t) -> (Bp.edge * Bp.vertex) -> Bp.t list -> Bp.t list
=fun cfg (_, new_bp_exit) (edge, vtx) result -> begin
  match edge with
  | If_true -> result (* Not implemented *)
  | If_false -> result@(translate new_bp_exit vtx cfg)
  | Normal | Failed -> raise (Failure "Extractor.translate_search_loop: Wrong edge label")
  | Check_skip -> result
end

and create_bp_of_branch : Bp.t -> Bp.vertex -> Bp.cond -> (Bp.t * Bp.t)
=fun cur_bp cur_vtx f_if -> begin
  let f_else = Bp.create_cond_not f_if in
  let inst_if = Bp.create_inst_assume f_if in
  let inst_else = Bp.create_inst_assume f_else in
  let new_bp_if = update_current_bp cur_bp (Some (cur_vtx, inst_if)) in
  let new_bp_else = update_current_bp cur_bp (Some (cur_vtx, inst_else)) in
  (new_bp_if, new_bp_else)
end

and create_bp_of_loop : Bp.t -> Bp.vertex -> (Bp.t * Bp.t * Bp.t)
=fun cur_bp loop -> begin
  let (terminated_bp, new_bp) = Bp.create_cut_bp cur_bp loop in
  let loop_bp = Bp.create_new_bp loop loop in
  (terminated_bp, loop_bp, new_bp)
end

and create_basic_safety_property : Bp.vertex -> Bp.exp -> Bp.typ -> (Bp.vertex * Bp.inst) option
=fun vtx e t -> begin
  let loc = Bp.create_loc vtx vtx in
  match (e, t.d) with
  | E_add _, T_mutez -> Some (vtx, (Bp.create_inst_assert (Bp.create_cond_no_overflow e t) loc (Bp.create_category_mutez_overflow)))
  | E_sub _, T_mutez -> Some (vtx, (Bp.create_inst_assert (Bp.create_cond_no_underflow e t) loc (Bp.create_category_mutez_overflow)))
  | E_mul _, T_mutez -> Some (vtx, (Bp.create_inst_assert (Bp.create_cond_no_overflow e t) loc (Bp.create_category_mutez_overflow)))
  | _, _ -> None
end

and read_loc_of_check : Pre.Lib.Cfg.t -> Bp.vertex -> Bp.loc 
=fun cfg cur_vtx -> begin
  let pred = Pre.Lib.Cfg.read_pred_from_vtx cfg cur_vtx in
  let etr_vtxs = Core.List.filter pred ~f:(fun (edge, _) -> Pre.Lib.Cfg.is_edge_check_skip edge) in
  if Core.List.length etr_vtxs = 1
  then begin
    let (_, etr_vtx) = Core.List.hd_exn etr_vtxs in
    Bp.create_loc etr_vtx cur_vtx
  end else Bp.create_loc cur_vtx cur_vtx
end

and update_current_bp : Bp.t -> (Bp.vertex * Bp.inst) option -> Bp.t
=fun cur_bp vtx_inst_opt -> begin
  match vtx_inst_opt with
  | None -> cur_bp
  | Some vtx_inst -> Bp.update_body cur_bp vtx_inst
end
