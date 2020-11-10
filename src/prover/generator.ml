open ProverLib

(*****************************************************************************)
(*****************************************************************************)

let apply : Inv.Map.t -> Bp.t list -> Bp.t list
=fun inv_map raw_bps -> begin
  let bps = Core.List.map raw_bps ~f:(fun raw_bp -> begin
    Bp.update_inv raw_bp ~pre:(Inv.Map.find inv_map raw_bp.pre.id) ~post:(Inv.Map.find inv_map raw_bp.post.id)
  end) in
  bps
end


(*****************************************************************************)
(*****************************************************************************)
(* Worklist Management                                                       *)
(*****************************************************************************)
(*****************************************************************************)

module Stg = struct
  type t = Vlang.v_obj
  type data = Pre.Lib.Mich.data Pre.Lib.Mich.t
  type cfg = Pre.Lib.Cfg.t

  let read_typt : cfg:cfg -> Vlang.typ
  =fun ~cfg -> begin
    let param_stg_typt = Pre.Lib.Cfg.CPMap.find_exn (cfg.type_info) (Pre.Lib.Cfg.param_storage_name) in
    match param_stg_typt.d with
    | T_pair (_, stg_typt) -> stg_typt
    | _ -> raise (Failure "Generator.Stg.read_typt: Type conflict on storage")
  end

  let create : cfg:cfg -> t
  =fun ~cfg -> Vlang.create_obj_of_exp ~exp:(Vlang.create_exp_var "stg") ~typ:(read_typt ~cfg:cfg)

  let equal_to_obj : obj:t -> cfg:cfg -> Vlang.t
  =fun ~obj ~cfg -> begin
    let obj_t = create ~cfg:cfg in
    let f = Vlang.create_formula_eq obj obj_t in
    f
end

  let equal_to_data : data:data -> cfg:cfg -> Vlang.t
  =fun ~data ~cfg -> equal_to_obj ~obj:(Converter.create_convert_data data (read_typt ~cfg:cfg)) ~cfg:cfg

  let equal_to_ps_or_os : ps_os:Vlang.var -> cfg:cfg -> Vlang.t
  =fun ~ps_os ~cfg -> begin
    let typt_ps_os = Pre.Lib.Cfg.CPMap.find_exn (cfg.type_info) ps_os in
    let obj_ps_os = Vlang.create_obj_of_exp ~exp:(Vlang.create_exp_var ps_os) ~typ:typt_ps_os in
    let obj_stg = Vlang.create_obj_of_exp ~exp:(Vlang.create_exp_uni_op_cdr obj_ps_os) ~typ:(read_typt ~cfg:cfg) in
    let f = equal_to_obj ~obj:obj_stg ~cfg:cfg in
    f
  end
end

module W = struct
  type t = Inv.WorkList.t
  type m = Inv.Map.t
  type cfg = Pre.Lib.Cfg.t

  let create : bp_list:Bp.lst -> t
  =fun ~bp_list -> begin
    let empty_t = Inv.WorkList.empty in
  let vtxs = (bp_list.entry.vtx)::(bp_list.exit.vtx)::(Core.List.map bp_list.loop ~f:(fun pt -> pt.vtx)) in
    let init_m = Core.List.fold_right vtxs ~f:(fun vtx map -> begin
      Inv.Map.add map ~key:vtx ~data:(Inv.create ~vtx:vtx)
    end) ~init:(Inv.Map.empty) in
    let init_t = Inv.WorkList.push empty_t init_m in
    init_t
  end

  let update : bp_list:Bp.lst -> cfg:cfg -> init_stg:Stg.data option -> cur_wlst:t -> t
  =fun ~bp_list ~cfg ~init_stg ~cur_wlst -> begin
    let _, _, _, _ = bp_list, cfg, init_stg, cur_wlst in
    Inv.WorkList.empty
  end
end