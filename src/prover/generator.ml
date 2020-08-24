open ProverLib

(************************************************)
(************************************************)

let initial_inv_worklist : Inv.vertex list -> Inv.vertex list -> Inv.WorkList.t
=fun trx_inv_vtx loop_inv_vtx -> begin
  let vtxs = trx_inv_vtx@loop_inv_vtx in
  let initial_inv_worklist = Inv.WorkList.empty in
  let initial_inv_map = Core.List.fold_right vtxs ~f:(fun vtx m -> (
    Inv.Map.add m ~key:vtx ~data:(Inv.create_inv vtx (Vlang.create_formula_true))
  )) ~init:(Inv.Map.empty) in
  Inv.WorkList.push initial_inv_worklist initial_inv_map
end

let apply : Inv.Map.t -> Bp.t list -> Bp.t list
=fun inv_map raw_bps -> begin
  let bps = Core.List.map raw_bps ~f:(fun raw_bp -> (
    Bp.update_inv raw_bp ~pre:(Inv.Map.find inv_map raw_bp.pre.id) ~post:(Inv.Map.find inv_map raw_bp.post.id)
  )) in
  bps
end

let generate : Bp.raw_t_list -> Pre.Lib.Cfg.t -> Bp.t list
=fun {bps=raw_bps; trx_inv_vtx; loop_inv_vtx} cfg -> begin
  let _ = cfg in
  let _ = (raw_bps, trx_inv_vtx, loop_inv_vtx) in
  Core.List.fold_left raw_bps ~init:[] ~f:(fun bps bp -> (
    let pre_inv = Inv.create_inv bp.pre.id Vlang.create_formula_true in
    let post_inv = Inv.create_inv bp.post.id Vlang.create_formula_true in
    let bp : Bp.t = { pre=pre_inv; body=bp.body; post=post_inv } in
    bp::bps
  ))
end

let update_bp : Inv.Map.t -> Bp.t -> Bp.t
=fun inv_map bp -> begin
  let _ = inv_map in
  let _ = bp in
  bp (* TODO *)
end