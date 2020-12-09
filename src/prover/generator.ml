open ProverLib

(*****************************************************************************)
(*****************************************************************************)

let apply : Inv.Map.t -> Bp.t list -> Bp.t list
=fun inv_map raw_bps -> begin
  try
    let bps = Core.List.map raw_bps ~f:(fun raw_bp -> begin
      Bp.update_inv raw_bp ~pre:(Inv.Map.find_empty inv_map (raw_bp.pre.id)) ~post:(Inv.Map.find_empty inv_map (raw_bp.post.id))
    end) in
    bps
  with
  | e -> e |> raise
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

  exception Error of string

  let pre : string
  ="pre_stg"

  let post : string
  ="post_stg"

  let read_typt : cfg:cfg -> Vlang.Ty.t
  =fun ~cfg -> begin
    try
      let param_stg_typt = Pre.Lib.Cfg.param_storage_name |> Pre.Lib.Cfg.CPMap.find_exn (cfg.type_info) |> Vlang.TypeUtil.ty_of_mty in
      match param_stg_typt with
      | T_pair (_, stg_typt) -> stg_typt
      | _ -> Error "read_typt: Type conflict on storage" |> raise
    with
    | Core.Not_found_s _
    | Not_found -> Error "read_typt: Type of param_storage is not found" |> raise
    | e -> e |> raise
  end

  let create : cfg:cfg -> t
  =fun ~cfg -> Vlang.Expr.V_var ((read_typt ~cfg:cfg), "stg")

  let create_comp : cfg:cfg -> Comps.t
  =fun ~cfg -> Comps.read_components (create ~cfg:cfg) (Comps.empty)
end

module TrxInv = struct
  type m = Inv.Map.t
  type formula = Vlang.t
  type cfg = Pre.Lib.Cfg.t

  exception Error of string

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

  let formula_sigma_equal : comp:Comps.t -> formula list
  =fun ~comp -> begin
    let mutez_fs = Core.List.fold_left comp.mutez_map ~f:(fun fs (a_expr, a_app) -> begin
      let fs' = Core.List.map comp.mutez ~f:(fun (b_expr, b_app) -> begin
        let app = a_app@b_app in
        let sigma_equal = Vlang.Formula.VF_sigma_equal (a_expr, b_expr) in
        Core.List.fold_right app ~f:(fun app f -> app f) ~init:sigma_equal
      end) in
      (fs'@fs)
    end) ~init:[] in
    mutez_fs
  end

  let wrap_formula : cfg:cfg -> (f:(comp:Comps.t -> formula list) -> (formula * formula) list)
  =fun ~cfg -> begin
    let comp = Stg.create_comp ~cfg:cfg in
    let wrapper : f:(comp:Comps.t -> formula list) -> (formula * formula) list
    =fun ~f -> begin
      Core.List.map (f ~comp:comp) ~f:(fun f -> begin
        let entry_f = Vlang.Renaming.var_in_expr_formula "stg" Stg.pre (f) in
        let exit_f = Vlang.Renaming.var_in_expr_formula "stg" Stg.post (f) in
        (entry_f, exit_f)
      end)
    end in
    wrapper
  end

  let create : bp_list:Bp.lst -> cfg:cfg -> m list
  =fun ~bp_list ~cfg -> begin
    let wrapper = wrap_formula ~cfg:cfg in
    let fs1 = (wrapper ~f:formula_mutez_equal) in
    let fs2 = (wrapper ~f:formula_sigma_equal) in
    Core.List.map (fs1@fs2) ~f:(fun (entry_f, exit_f) -> begin 
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

  exception Error of string

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