open ProverLib

(*****************************************************************************)
(*****************************************************************************)

let apply : Inv.Map.t -> Bp.t list -> Bp.t list
=fun inv_map raw_bps -> begin
  let bps = Core.List.map raw_bps ~f:(fun raw_bp -> begin
    Bp.update_inv raw_bp ~pre:(Inv.Map.find_empty inv_map (raw_bp.pre.id)) ~post:(Inv.Map.find_empty inv_map (raw_bp.post.id))
  end) in
  bps
end


(*****************************************************************************)
(*****************************************************************************)
(* Invariant Generator                                                       *)
(*****************************************************************************)
(*****************************************************************************)

module Stg = struct
  type t = Vlang.Expr.t
  type data = Pre.Lib.Mich.data Pre.Lib.Mich.t
  type cfg = Pre.Lib.Cfg.t

  let read_typt : cfg:cfg -> Vlang.Ty.t
  =fun ~cfg -> begin
    let param_stg_typt = Pre.Lib.Cfg.param_storage_name |> Pre.Lib.Cfg.CPMap.find_exn (cfg.type_info) |> Vlang.TypeUtil.ty_of_mty in
    match param_stg_typt with
    | T_pair (_, stg_typt) -> stg_typt
    | _ -> raise (Failure "Generator.Stg.read_typt: Type conflict on storage")
  end

  let create : cfg:cfg -> t
  =fun ~cfg -> Vlang.Expr.V_var ((read_typt ~cfg:cfg), "stg")

  let create_comp : cfg:cfg -> Comps.t
  =fun ~cfg -> Comps.read_components (create ~cfg:cfg) (Comps.empty)

  let equal_to_expr : expr:t -> cfg:cfg -> Vlang.t
  =fun ~expr ~cfg -> Vlang.Formula.VF_eq (expr, (create ~cfg:cfg))

  let equal_to_data : data:data -> cfg:cfg -> Vlang.t
  =fun ~data ~cfg -> equal_to_expr ~expr:(Converter.create_expr_of_michdata data (read_typt ~cfg:cfg)) ~cfg:cfg

  let equal_to_ps_or_os : ps_os:Vlang.var -> cfg:cfg -> Vlang.t
  =fun ~ps_os ~cfg -> equal_to_expr ~expr:(Vlang.Expr.V_car (Vlang.Expr.V_var ((read_typt ~cfg:cfg), ps_os))) ~cfg:cfg
end

module TrxInv = struct
  type m = Inv.Map.t
  type formula = Vlang.t
  type cfg = Pre.Lib.Cfg.t

  let formula_mutez_equal : comp:Comps.t -> formula list
  =fun ~comp -> begin
    let fs, _ = Core.List.fold_left comp.mutez ~f:(fun (fs, comps) (a_expr, a_app) -> begin
      let comps' = Core.List.tl_exn comps in
      let fs' = Core.List.map comps' ~f:(fun (b_expr, b_app) -> begin
        let app = a_app@b_app in
        let equal = Vlang.Formula.VF_eq (a_expr, b_expr) in
        Core.List.fold_right app ~f:(fun app f -> app f) ~init:equal
      end) in
      (fs'@fs, comps')
    end) ~init:([], comp.mutez) in
    fs
  end

  let wrap_formula : bp_list:Bp.lst -> cfg:cfg -> (f:(comp:Comps.t -> formula list) -> (formula * formula) list)
  =fun ~bp_list ~cfg -> begin
    let comp = Stg.create_comp ~cfg:cfg in
    let entry_equal = Stg.equal_to_ps_or_os ~ps_os:(Option.get bp_list.entry.var) ~cfg:cfg in
    let exit_equal = Stg.equal_to_ps_or_os ~ps_os:(Option.get bp_list.exit.var) ~cfg:cfg in
    let wrapper : f:(comp:Comps.t -> formula list) -> (formula * formula) list
    =fun ~f -> begin
      Core.List.map (f ~comp:comp) ~f:(fun f -> begin
        let entry_f = Vlang.Renaming.var_in_expr_formula "stg" "pre_stg" (Vlang.Formula.VF_and [entry_equal; f]) in
        let exit_f = Vlang.Renaming.var_in_expr_formula "stg" "post_stg" (Vlang.Formula.VF_imply (exit_equal, f)) in
        (entry_f, exit_f)
      end)
    end in
    wrapper
  end

  let create : bp_list:Bp.lst -> cfg:cfg -> m list
  =fun ~bp_list ~cfg -> begin
    let wrapper = wrap_formula ~bp_list:bp_list ~cfg:cfg in
    let fs = (wrapper ~f:formula_mutez_equal) in
    Core.List.map fs ~f:(fun (entry_f, exit_f) -> begin 
      let entry_inv = Inv.T.create_with_formulae ~vtx:(bp_list.entry.vtx) ~fl:[entry_f] in
      let exit_inv = Inv.T.create_with_formulae ~vtx:(bp_list.exit.vtx) ~fl:[exit_f] in
      let empty_map = Inv.Map.empty in
      let entry_map = Inv.Map.add empty_map ~key:(bp_list.entry.vtx) ~data:entry_inv in
      let exit_map = Inv.Map.add entry_map ~key:(bp_list.exit.vtx) ~data:exit_inv in
      exit_map
    end)
  end
end

(*
module LoopInv = struct
end
*)


(*****************************************************************************)
(*****************************************************************************)
(* Worklist Management                                                       *)
(*****************************************************************************)
(*****************************************************************************)

module W = struct
  type t = Inv.WorkList.t
  type m = Inv.Map.t
  type cfg = Pre.Lib.Cfg.t

  let create : bp_list:Bp.lst -> t
  =fun ~bp_list -> begin
    let empty_t = Inv.WorkList.empty in
    let vtxs = (bp_list.entry.vtx)::(bp_list.exit.vtx)::(Core.List.map bp_list.loop ~f:(fun pt -> pt.vtx)) in
    let init_m = Core.List.fold_right vtxs ~f:(fun vtx map -> begin
      Inv.Map.add map ~key:vtx ~data:(Inv.T.create ~vtx:vtx)
    end) ~init:(Inv.Map.empty) in
    let init_t = Inv.WorkList.push empty_t init_m in
    init_t
  end

  let update : bp_list:Bp.lst -> cfg:cfg -> init_stg:Stg.data option -> wlst:t -> t
  =fun ~bp_list ~cfg ~init_stg ~wlst -> begin
    let _ = init_stg in
    let trx_maps = TrxInv.create ~bp_list:bp_list ~cfg:cfg in
    let next_wlst = Inv.WorkList.push_list wlst trx_maps in
    next_wlst
  end

  let join : inv:m -> wlst:t -> t
  =fun ~inv ~wlst -> begin
    let next_wlst' = Inv.WorkList.update_current wlst ~new_:inv in
    let next_wlst = Inv.WorkList.map next_wlst' ~f:(Inv.Map.join inv) in
    next_wlst
  end

  let last_worklist : wlst:t -> t
  =fun ~wlst -> Inv.WorkList.push_force wlst (wlst.current)
end