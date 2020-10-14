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
  let param_storage_var, param_storage_typ = read_param_storage cfg in
  let _ = Comps.read_components param_storage_var param_storage_typ Comps.empty in
  let _, _ = raw_bp_list, cfg in 
  []
end

and read_param_storage : Pre.Lib.Cfg.t -> Vlang.v_exp * Vlang.typ
=fun cfg -> begin
  let param_storage = "param_storage" in
  let param_storage_typ = Pre.Lib.Cfg.CPMap.find_exn cfg.type_info param_storage in
  let param_storage_var = Vlang.create_exp_var param_storage param_storage_typ in
  (param_storage_var, param_storage_typ)
end

let rec create_initial_worklist : Pre.Lib.Cfg.t -> Inv.vertex list -> Inv.vertex list -> Inv.WorkList.t
=fun cfg trx_inv_vtx loop_inv_vtx -> begin
  let vtxs = trx_inv_vtx@loop_inv_vtx in
  let initial_inv_worklist = Inv.WorkList.empty in
  let initial_inv_map = Core.List.fold_right vtxs ~f:(fun vtx m -> (
    Inv.Map.add m ~key:vtx ~data:(Inv.create_inv vtx (create_formula_from_param_storage cfg))
  )) ~init:(Inv.Map.empty) in
  Inv.WorkList.push initial_inv_worklist initial_inv_map
end

and create_formula_from_param_storage : Pre.Lib.Cfg.t -> Vlang.v_formula
=fun cfg -> begin
  let ps_var, ps_typ = read_param_storage cfg in
  let ps_comp = Comps.read_components ps_var ps_typ (Comps.empty) in
  let mutez_formulae = Core.List.map ps_comp.mutez ~f:(fun (e, _, fl) -> (
    Core.List.fold_right fl ~f:(fun func formula -> (
      func formula
    )) ~init:(Vlang.create_formula_mutez_bound e)
  )) in
  let mutez_map_formulae = Core.List.map ps_comp.mutez_map ~f:(fun (e, ty, fl) -> (
    let bound = "_bnd_mutez_map" in
    let ty1', ty2' = match ty.d with T_map (ty1', ty2') -> (ty1', ty2') | _ -> raise (Failure "") in
    let map_entry = Vlang.create_exp_bin_op_get (Vlang.create_exp_var bound ty1') e ty2' in
    Vlang.create_formula_forall [(bound, ty1')] (
      Core.List.fold_right fl ~f:(fun func formula -> (
        func formula
      )) ~init:(Vlang.create_formula_imply (Vlang.create_formula_is_some map_entry) (Vlang.create_formula_mutez_bound (Vlang.create_exp_uni_op_un_opt map_entry ty2')))
    )
  )) in
  Vlang.create_formula_and (
    mutez_formulae@
    mutez_map_formulae
  )
end
