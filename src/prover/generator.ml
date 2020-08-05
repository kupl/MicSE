open ProverLib

(************************************************)
(************************************************)

let generate : Bp.raw_t_list -> Cfg.t -> Bp.t list
=fun {bps=raw_bps; trx_inv_vtx; loop_inv_vtx} cfg -> begin
  let _ = cfg in
  let _ = (raw_bps, trx_inv_vtx, loop_inv_vtx) in
  Core.List.fold_left raw_bps ~init:[] ~f:(fun bps bp -> (
    let pre_inv = Bp.create_inv bp.pre.id Vlang.create_formula_true in
    let post_inv = Bp.create_inv bp.post.id Vlang.create_formula_true in
    let bp : Bp.t = { pre=pre_inv; body=bp.body; post=post_inv } in
    bp::bps
  ))
end

let update_bp : Bp.inv_map -> Bp.t -> Bp.t
=fun inv_map bp -> begin
  let _ = inv_map in
  let _ = bp in
  bp (* TODO *)
end