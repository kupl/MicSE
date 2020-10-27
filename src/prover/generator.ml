open ProverLib

(************************************************)
(************************************************)

let apply : Inv.Map.t -> Bp.t list -> Bp.t list
=fun inv_map raw_bps -> begin
  let bps = Core.List.map raw_bps ~f:(fun raw_bp -> (
    Bp.update_inv raw_bp ~pre:(Inv.Map.find inv_map raw_bp.pre.id) ~post:(Inv.Map.find inv_map raw_bp.post.id)
  )) in
  bps
end

let rec generate : Bp.raw_t_list -> Pre.Lib.Cfg.t -> Inv.Map.t list
=fun raw_bp_list cfg -> begin
  let param_storage = read_param_storage cfg in
  let _ = Comps.read_components param_storage Comps.empty in
  let _, _ = raw_bp_list, cfg in 
  []
end

and read_param_storage : Pre.Lib.Cfg.t -> Vlang.v_obj
=fun cfg -> begin
  let param_storage = "param_storage" in
  let param_storage_typ = Pre.Lib.Cfg.CPMap.find_exn cfg.type_info param_storage in
  let param_storage_var = Vlang.create_exp_var param_storage in
  Vlang.create_obj_of_exp ~exp:param_storage_var ~typ:param_storage_typ
end

let create_initial_worklist : Inv.vertex list -> Inv.vertex list -> Inv.WorkList.t
=fun trx_inv_vtx loop_inv_vtx -> begin
  let vtxs = trx_inv_vtx@loop_inv_vtx in
  let initial_inv_worklist = Inv.WorkList.empty in
  let initial_inv_map = Core.List.fold_right vtxs ~f:(fun vtx m -> (
    Inv.Map.add m ~key:vtx ~data:(Inv.create_inv vtx (Vlang.create_formula_true))
  )) ~init:(Inv.Map.empty) in
  Inv.WorkList.push initial_inv_worklist initial_inv_map
end