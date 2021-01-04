open ProverLib

exception Error of string
exception InvalidExtraction of (Pre.Lib.Cfg.stmt * string)

module CPSet = Core.Set.Poly

(************************************************)
(************************************************)

module CFGUtils = struct
  exception Error of string

  let read_var_type : Pre.Lib.Cfg.t -> Pre.Lib.Cfg.ident -> Pre.Lib.Mich.typ Pre.Lib.Mich.t
  = fun cfg var -> begin
      match var |> Core.Map.find cfg.type_info with
      | Some typ -> typ
      | None -> Error ("read_var_type: type of " ^ var ^ " is invalid") |> raise
  end

  let read_exit_var : Pre.Lib.Cfg.t -> Pre.Lib.Cfg.ident
  = let open Pre.Lib in
    fun cfg -> begin
      let stmt = Cfg.read_stmt_from_vtx
      cfg cfg.main_exit in
      match stmt with
      | Cfg_assign (id, _) -> id
      | _ -> Error "read_exit_var: main-exit vertex error" |> raise
  end
end

(************************************************)

module BPUtils = struct
  type foldingType = (Pre.Lib.Cfg.vertex * Bp.inst) option

  exception Error of string

  let create_loop_bp : Bp.t -> Pre.Lib.Cfg.vertex -> (Bp.t * Bp.t * Bp.t)
  = fun bp loop_vtx -> begin
    let loop_inv = Inv.T.create ~vtx:loop_vtx in
    let terminated_bp : Bp.t = { pre=bp.pre; body=bp.body; post=loop_inv } in
    let loop_bp : Bp.t = { pre=loop_inv; body=[]; post=loop_inv } in
    let new_bp : Bp.t = { pre=loop_inv; body=[]; post=bp.post } in
    (terminated_bp, loop_bp, new_bp )
  end

  let read_loc_of_check : Pre.Lib.Cfg.t -> Pre.Lib.Cfg.vertex -> Bp.loc
  = let open Pre.Lib in
    fun cfg cur_vtx -> begin
    let pred = cur_vtx |> Cfg.read_pred_from_vtx cfg in
    let entry_vtx_list = pred |> Core.List.filter ~f:(fun (edge, _) -> edge = Cfg.Check_skip) in
    if Core.List.length entry_vtx_list = 1 then begin
      let (_, entry_vtx) = entry_vtx_list |> Core.List.hd |> (function | Some vv -> vv | None -> Error "read_loc_of_check: Invalid access to head" |> raise) in
      { entry=entry_vtx; exit=cur_vtx }
    end else begin
      { entry=cur_vtx; exit=cur_vtx }
    end
  end

  let update_current_bp : Bp.t -> foldingType -> Bp.t
  = fun cur_bp vtx_inst_opt -> begin
      match vtx_inst_opt with
      | None -> cur_bp
      | Some vtx_inst -> { cur_bp with body=cur_bp.body@[vtx_inst] }
  end
end

(************************************************)

module InstUtils = struct
  let create_inst_assume : Pre.Lib.Cfg.vertex -> Bp.cond -> BPUtils.foldingType
  = fun vtx c -> Some (vtx, BI_assume c)

  let create_inst_assert : Pre.Lib.Cfg.vertex -> Bp.cond -> Bp.loc -> Bp.category -> BPUtils.foldingType
  = fun vtx c loc category -> Some (vtx, BI_assert (c, loc, category))

  let create_inst_assert_basic_prop : Pre.Lib.Cfg.vertex -> Pre.Lib.Cfg.expr -> Pre.Lib.Mich.typ Pre.Lib.Mich.t -> BPUtils.foldingType
  = fun vtx e t -> begin
      let loc = Bp.create_loc vtx vtx in
      match (e, t.d) with
      | E_add _, T_mutez -> create_inst_assert vtx (BC_no_overflow e) loc Q_mutez_arith_safety
      | E_sub _, T_mutez -> create_inst_assert vtx (BC_no_underflow e) loc Q_mutez_arith_safety
      | E_mul _, T_mutez -> create_inst_assert vtx (BC_no_overflow e) loc Q_mutez_arith_safety
      | _, _ -> None
  end

  let create_inst_assign : Pre.Lib.Cfg.vertex -> Pre.Lib.Cfg.ident -> Pre.Lib.Cfg.expr -> BPUtils.foldingType
  = fun vtx v e -> Some (vtx, BI_assign (v, e))

  let create_inst_skip : Pre.Lib.Cfg.vertex -> BPUtils.foldingType
  = fun vtx -> Some (vtx, BI_skip)
end

(************************************************)

let rec translate : Pre.Lib.Cfg.t -> Pre.Lib.Cfg.vertex -> Bp.t -> (Bp.t list * Pre.Lib.Cfg.vertex CPSet.t)
= let open Pre.Lib in
  fun cfg cur_vtx bp -> begin
    (*********************************)
    let search_normal (normal_bp: Bp.t) (succ_edge, succ_vtx: Cfg.edge_label * Cfg.vertex) (acc_bp, acc_loop_vtx: Bp.t list * Cfg.vertex CPSet.t) = begin
      match succ_edge with
      | Normal -> begin
          let (new_bp, new_loop_vtx) = translate cfg succ_vtx normal_bp in
          let loop_vtx_set = CPSet.union acc_loop_vtx new_loop_vtx in
          (acc_bp@new_bp, loop_vtx_set)
        end
      | If_true | If_false -> Error "translate.search_normal: Invalid edge label" |> raise
      | Check_skip | Failed -> (acc_bp, acc_loop_vtx)
    end in
    let search_branch (true_bp, false_bp: Bp.t * Bp.t) (succ_edge, succ_vtx: Cfg.edge_label * Cfg.vertex) (acc_bp, acc_loop_vtx: Bp.t list * Cfg.vertex CPSet.t) = begin
      match succ_edge with
      | If_true -> begin
          let (new_bp, new_loop_vtx) = translate cfg succ_vtx true_bp in
          let loop_vtx_set = CPSet.union acc_loop_vtx new_loop_vtx in
          (acc_bp@new_bp, loop_vtx_set)
        end
      | If_false -> begin
          let (new_bp, new_loop_vtx) = translate cfg succ_vtx false_bp in
          let loop_vtx_set = CPSet.union acc_loop_vtx new_loop_vtx in
          (acc_bp@new_bp, loop_vtx_set)
        end
      | Normal | Failed -> Error "translate.search_branch: Invalid edge label" |> raise
      | Check_skip -> (acc_bp, acc_loop_vtx)
    end in
    (*********************************)
    let stmt = cur_vtx |> Cfg.read_stmt_from_vtx cfg in
    let succ = cur_vtx |> Cfg.read_succ_from_vtx cfg in
    if cur_vtx = bp.post.id then begin (* Current basic path is ended. *)
      match stmt with
      | Cfg_assign (id, e) -> begin
          let assert_inst = InstUtils.create_inst_assert_basic_prop cur_vtx e (id |> CFGUtils.read_var_type cfg) in
          let bp' = BPUtils.update_current_bp bp assert_inst in
          let assign_inst = InstUtils.create_inst_assign cur_vtx id e in
          let bp'' = BPUtils.update_current_bp bp' assign_inst in
          ([bp''], CPSet.empty)
        end
      | Cfg_loop _ | Cfg_loop_left _ | Cfg_map _ | Cfg_iter _ -> ([bp], (cur_vtx |> CPSet.singleton))
      | _ -> Error "translate: main-exit vertex error" |> raise
    end else begin (* Being process *)
      match stmt with
      | Cfg_assign (id, e) -> begin
          let assert_inst = InstUtils.create_inst_assert_basic_prop cur_vtx e (id |> CFGUtils.read_var_type cfg) in
          let normal_bp' = BPUtils.update_current_bp bp assert_inst in
          let assign_inst = InstUtils.create_inst_assign cur_vtx id e in
          let normal_bp'' = BPUtils.update_current_bp normal_bp' assign_inst in
          succ |> Core.List.fold_right ~f:(normal_bp'' |> search_normal) ~init:([], CPSet.empty)
        end
      | Cfg_skip | Cfg_drop _ | Cfg_swap | Cfg_dig | Cfg_dug -> begin
          let skip_inst = InstUtils.create_inst_skip cur_vtx in
          let normal_bp' = BPUtils.update_current_bp bp skip_inst in
          succ |> Core.List.fold_right ~f:(normal_bp' |> search_normal) ~init:([], CPSet.empty)
        end
      | Cfg_if id -> begin
          let true_assume_inst = InstUtils.create_inst_assume cur_vtx (BC_is_true id) in
          let true_bp' = BPUtils.update_current_bp bp true_assume_inst in
          let false_assume_inst = InstUtils.create_inst_assume cur_vtx (BC_not (BC_is_true id)) in
          let false_bp' = BPUtils.update_current_bp bp false_assume_inst in
          succ |> Core.List.fold_right ~f:((true_bp', false_bp') |> search_branch) ~init:([], CPSet.empty)
        end
      | Cfg_if_left id -> begin
          let true_assume_inst = InstUtils.create_inst_assume cur_vtx (BC_is_left id) in
          let true_bp' = BPUtils.update_current_bp bp true_assume_inst in
          let false_assume_inst = InstUtils.create_inst_assume cur_vtx (BC_not (BC_is_left id)) in
          let false_bp' = BPUtils.update_current_bp bp false_assume_inst in
          succ |> Core.List.fold_right ~f:((true_bp', false_bp') |> search_branch) ~init:([], CPSet.empty)
        end
      | Cfg_if_cons id -> begin
          let true_assume_inst = InstUtils.create_inst_assume cur_vtx (BC_is_cons id) in
          let true_bp' = BPUtils.update_current_bp bp true_assume_inst in
          let false_assume_inst = InstUtils.create_inst_assume cur_vtx (BC_not (BC_is_cons id)) in
          let false_bp' = BPUtils.update_current_bp bp false_assume_inst in
          succ |> Core.List.fold_right ~f:((true_bp', false_bp') |> search_branch) ~init:([], CPSet.empty)
        end
      | Cfg_if_none id -> begin
          let true_assume_inst = InstUtils.create_inst_assume cur_vtx (BC_is_none id) in
          let true_bp' = BPUtils.update_current_bp bp true_assume_inst in
          let false_assume_inst = InstUtils.create_inst_assume cur_vtx (BC_not (BC_is_none id)) in
          let false_bp' = BPUtils.update_current_bp bp false_assume_inst in
          succ |> Core.List.fold_right ~f:((true_bp', false_bp') |> search_branch) ~init:([], CPSet.empty)
        end
      | Cfg_loop id -> begin
          let (terminated_bp, loop_bp, new_bp) = BPUtils.create_loop_bp bp cur_vtx in
          let true_assume_inst = InstUtils.create_inst_assume cur_vtx (BC_is_true id) in
          let loop_bp' = BPUtils.update_current_bp loop_bp true_assume_inst in
          let false_assume_inst = InstUtils.create_inst_assume cur_vtx (BC_not (BC_is_true id)) in
          let new_bp' = BPUtils.update_current_bp new_bp false_assume_inst in
          succ |> Core.List.fold_right ~f:((loop_bp', new_bp') |> search_branch) ~init:([terminated_bp], CPSet.empty)
        end
      | Cfg_loop_left id -> begin
          let (terminated_bp, loop_bp, new_bp) = BPUtils.create_loop_bp bp cur_vtx in
          let true_assume_inst = InstUtils.create_inst_assume cur_vtx (BC_is_left id) in
          let loop_bp' = BPUtils.update_current_bp loop_bp true_assume_inst in
          let false_assume_inst = InstUtils.create_inst_assume cur_vtx (BC_not (BC_is_left id)) in
          let new_bp' = BPUtils.update_current_bp new_bp false_assume_inst in
          succ |> Core.List.fold_right ~f:((loop_bp', new_bp') |> search_branch) ~init:([terminated_bp], CPSet.empty)
        end
      | Cfg_map id -> begin
          let (terminated_bp, loop_bp, new_bp) = BPUtils.create_loop_bp bp cur_vtx in
          let true_assume_inst = InstUtils.create_inst_assume cur_vtx (BC_is_cons id) in
          let loop_bp' = BPUtils.update_current_bp loop_bp true_assume_inst in
          let false_assume_inst = InstUtils.create_inst_assume cur_vtx (BC_not (BC_is_cons id)) in
          let new_bp' = BPUtils.update_current_bp new_bp false_assume_inst in
          succ |> Core.List.fold_right ~f:((loop_bp', new_bp') |> search_branch) ~init:([terminated_bp], CPSet.empty)
        end
      | Cfg_iter id -> begin
          let (terminated_bp, loop_bp, new_bp) = BPUtils.create_loop_bp bp cur_vtx in
          let true_assume_inst = InstUtils.create_inst_assume cur_vtx (BC_is_cons id) in
          let loop_bp' = BPUtils.update_current_bp loop_bp true_assume_inst in
          let false_assume_inst = InstUtils.create_inst_assume cur_vtx (BC_not (BC_is_cons id)) in
          let new_bp' = BPUtils.update_current_bp new_bp false_assume_inst in
          succ |> Core.List.fold_right ~f:((loop_bp', new_bp') |> search_branch) ~init:([terminated_bp], CPSet.empty)
        end
      | Cfg_failwith _ -> ([], CPSet.empty)
      | Cfg_micse_check_entry -> begin
          let skip_inst = InstUtils.create_inst_skip cur_vtx in
          let normal_bp' = BPUtils.update_current_bp bp skip_inst in
          succ |> Core.List.fold_right ~f:(normal_bp' |> search_normal) ~init:([], CPSet.empty)
        end
      | Cfg_micse_check_value id -> begin
          let assert_inst = InstUtils.create_inst_assert cur_vtx (BC_is_true id) (cur_vtx |> BPUtils.read_loc_of_check cfg) (Q_assertion) in
          let normal_bp' = BPUtils.update_current_bp bp assert_inst in
          succ |> Core.List.fold_right ~f:(normal_bp' |> search_normal) ~init:([], CPSet.empty)
        end
      (* | _ -> Error "translate: Not implemented" |> raise *)
    end
end

let extract : Pre.Lib.Cfg.t -> Bp.lst
= let open Pre.Lib in
  fun cfg -> begin
  try
    let entry_bp = Bp.create_new_bp cfg.main_entry cfg.main_exit in
    let result, loop_vertices = translate cfg cfg.main_entry entry_bp in
    Bp.create_bp_list
      ~bps: result
      ~entry:(Bp.create_inv_point ~vtx:cfg.main_entry ~var_opt:(Some (Cfg.param_storage_name)))
      ~exit:(Bp.create_inv_point ~vtx:cfg.main_exit ~var_opt:(Some (cfg |> CFGUtils.read_exit_var)))
      ~loop:(loop_vertices |> CPSet.to_list |> Core.List.map ~f:(fun vtx -> Bp.create_inv_point ~vtx:vtx ~var_opt:None))
  with
  | InvalidExtraction (stmt, msg) -> Error ("invalid extraction of statement [" ^ (stmt |> Cfg.stmt_to_str) ^ "]: " ^ msg) |> raise
  | e -> e |> raise
end