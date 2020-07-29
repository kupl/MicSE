open ProverLib

(************************************************)
(************************************************)

let upper_bound = Z.of_string "9223372036854775808"

let generate : Bp.raw_t_list -> Cfg.t -> Bp.t list
=fun {bps=raw_bps; trx_inv_vtx; loop_inv_vtx} cfg -> begin
  let _ = (raw_bps, trx_inv_vtx, loop_inv_vtx) in
  Core.List.fold_left raw_bps ~init:[] ~f:(fun bps bp -> (
    let qs = Core.List.fold_left bp.body ~init:[] ~f:(fun qs inst -> (
      match inst with
      | BI_assign (v, e) -> begin
        let t = Cfg.CPMap.find_exn cfg.type_info v in
        match e, t.d with
        | E_add (_, _), T_mutez -> (v, t)::qs
        | _, _ -> qs
        end
      | _ -> qs
    )) in
    let inv = Core.List.fold_left qs ~init:Vlang.create_formula_true ~f:(fun inv (v, t) -> (
      let ub : Vlang.v_exp = VE_int upper_bound in
      let var : Vlang.v_exp = VE_var (v, t) in
      Vlang.create_formula_and [inv; (Vlang.create_formula_le var ub)]
    )) in
    let pre_inv = Bp.create_inv bp.post.id inv in
    let post_inv = Bp.create_inv bp.post.id inv in
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