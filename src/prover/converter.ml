open ProverLib

exception Error of string

exception InvalidConversion_Expr of PreLib.Cfg.expr
exception InvalidConversion_Cond of Bp.cond

module VarComparable = struct
  module Key = struct
    type t = ProverLib.Bp.var
    let compare : t -> t -> int
    =Core.String.compare
    let sexp_of_t : t -> Core.Sexp.t
    =Core.String.sexp_of_t
    let t_of_sexp : Core.Sexp.t -> t
    =Core.String.t_of_sexp
  end

  include Key
  include Core.Comparable.Make (Key)
end


module CvUtils = struct
  let to_vtyp : PreLib.Cfg.typ -> Vlang.typ
  =Vlang.TypeUtil.ty_of_mty

  let read_type : Vlang.Expr.t -> Vlang.typ
  =Vlang.TypeUtil.ty_of_expr
end

module Env = struct
  exception Error of string

  module VarMap = VarComparable.Map   (* Core.Map *)
  type body = {
    cfg : PreLib.Cfg.t;
    varname : Bp.var VarMap.t;        (* Variable-name Map          : Original Variable-name  -> Latest Variable-name *)
    varexpr : Vlang.Expr.t VarMap.t;  (* Variable to Expression Map : Variable-name           -> Expression of Verification Language *)
  }
  type t = body ref
  
  let newvar_prefix : string
  ="#"
  let gen_nv : Bp.var -> Bp.var
  =fun v -> newvar_prefix ^ v
  let get_ov : Bp.var -> Bp.var (* "get_ov" just removes continuous "newvar_prefix"es in front of the given string *)
  =fun v -> begin (* get_original_varname *)
    let idx : int ref = ref 0 in
    let flag : bool ref = ref true in
    let _ : unit = String.iter (fun c -> if !flag && (String.make 1 c = newvar_prefix) then Stdlib.incr idx else flag := false) v in
    String.sub v !idx (String.length v - !idx)
  end

  let create : Pre.Lib.Cfg.t -> t
  =fun cfg -> ref { cfg=cfg; varname=VarMap.empty; varexpr=VarMap.empty; }

  let read_vartype : PreLib.Cfg.ident -> env:t -> Vlang.typ
  =fun v ~env -> begin (* read_type_cfgvar *)
    let m = !env.cfg.type_info in (* cfg type info map *)
    match Pre.Lib.Cfg.CPMap.find m v with (* find v in cfg type info map *)
    | None -> Error "read_vartype: Cannot find the variable" |> raise
    | Some x -> CvUtils.to_vtyp x
  end

  let read_varname : Bp.var -> env:t -> Bp.var
  =fun v ~env -> begin (* get_cur_varname *)
    let m = !env.varname in (* varname map *)
    match VarMap.find m v with (* find v in varname map *)
    | None -> begin
        let nm = VarMap.add m ~key:v ~data:v |> (function | `Ok mm -> mm | `Duplicate -> Error "read_varname: Duplicate variable name" |> raise) in (* make new map with adding the current variable *)
        let _ = env := { !env with varname=nm } in
        v
      end
    | Some vv -> vv
  end
  let update_varname : Bp.var -> env:t -> Bp.var (* WARNING: "update_varname" will change the given env data *)
  =fun v ~env -> begin (* get_new_varname *)
    let m = !env.varname in (* varname map *)
    match VarMap.find m v with (* find v in varname map *)
    | None -> begin
        let nv = gen_nv v in
        let nm = VarMap.add m ~key:v ~data:nv |> (function | `Ok mm -> mm | `Duplicate -> Error "read_varname: Duplicate variable name" |> raise) in (* make new map with adding the current variable *)
        let _ = env := { !env with varname=nm } in
        nv
      end
    | Some cv -> begin
        let nv = gen_nv cv in
        let nm = VarMap.add m ~key:v ~data:nv |> (function | `Ok mm -> mm | `Duplicate -> Error "read_varname: Duplicate variable name" |> raise) in (* make new map with adding the current variable *)
        let _ = env := { !env with varname=nm } in
        nv
      end
  end

  let read_expr_of_cfgvar : Pre.Lib.Cfg.ident -> env:t -> Vlang.Expr.t
  =fun v ~env -> begin (* create_var_of_cfgvar *)
    let m = !env.varexpr in (* variable to expression map *)
    let cv = v |> read_varname ~env:env in (* current variable name of input variable *)
    if v = Pre.Lib.Cfg.param_storage_name
    then Vlang.Expr.V_var ((v |> read_vartype ~env:env), v)
    else match VarMap.find m cv with
    | None -> Error ("read_expr_of_cfgvar: Variable " ^ v ^ " is not defined with any expression") |> raise
    | Some ce -> ce
  end
  let update_expr_of_cfgvar : Pre.Lib.Cfg.ident -> Vlang.Expr.t -> env:t -> unit
  =fun v e ~env -> begin
    let m = !env.varexpr in (* variable to expression map *)
    let cv = v |> read_varname ~env:env in
    let nm = VarMap.add m ~key:cv ~data:e |> (function | `Ok mm -> mm | `Duplicate -> Error "update_expr_of_cfgvar: Duplicate variable name" |> raise) in (* make new map with adding the expression to current variable *)
    let _ = env := { !env with varexpr=nm } in
    ()
  end
  let string_of_var_expr_map : t -> string
  =fun env -> begin
    let m = !env.varexpr in (* variable to expression map *)
    "{\n" ^ 
    VarMap.fold m ~init:"" ~f:(fun ~key ~data str -> begin
      str ^
      "\t" ^ key ^ " |-> " ^ (Vlang.Expr.to_string data) ^ "\n"
    end) ^
    "}"
  end
end


let rec create_expr_of_michdata_i : PreLib.Mich.data -> Vlang.typ -> Vlang.Expr.t
= let open PreLib.Mich in
  let open Vlang.Ty in
  let open Vlang.Expr in
  let cem = create_expr_of_michdata in (* syntax sugar *)
  let set_func ~elt acc x = Core.Set.Poly.add acc (cem x elt) in
  let map_func ~kt ~vt acc x = begin
    match (Pre.Lib.Mich.get_d x) with
    | D_elt (k, v) -> acc |> Core.Map.Poly.add ~key:(cem k kt) ~data:(cem v vt) |> (function | `Ok m -> m | `Duplicate -> acc)
    | _ -> Error "create_expr_of_michdata_i: Invalid data in elt list" |> raise
  end in
  fun michdata vtyp -> begin
    match (vtyp, michdata) with 
    | T_int, D_int zn                   -> V_lit_int zn
    | T_nat, D_int zn                   -> V_lit_int zn
    | T_mutez, D_int zn                 -> V_lit_mutez zn
    | T_timestamp, D_int zn             -> V_lit_timestamp_sec zn
    | T_string, D_string s              -> V_lit_string s
    | T_key_hash, D_string s            -> V_lit_key_hash s
    | T_timestamp, D_string s           -> V_lit_timestamp_str s
    | T_address, D_string s             -> V_lit_address (V_lit_key_hash s)
    | T_key, D_string s                 -> V_lit_key s
    | T_bytes, D_bytes s                -> V_lit_bytes s
    | T_chain_id, D_bytes s             -> V_lit_chain_id s
    | T_unit, D_unit                    -> V_unit
    | T_bool, D_bool b                  -> V_lit_bool b
    | T_pair (t1, t2), D_pair (d1, d2)  -> V_pair (cem d1 t1, cem d2 t2)
    | T_or (t1, _), D_left d            -> V_left (vtyp, cem d t1)
    | T_or (_, t2), D_right d           -> V_right (vtyp, cem d t2)
    | T_option t, D_some d              -> V_some (cem d t)
    | T_option t, D_none                -> V_none t
    | T_list elt, D_list dlist          -> V_lit_list (elt, List.map (fun x -> cem x elt) dlist)
    | T_set elt, D_list dlist           -> V_lit_set (elt, dlist |> 
                                                           Core.List.fold_left
                                                              ~init:Core.Set.Poly.empty
                                                              ~f:(set_func ~elt:elt))
    | T_map (kt, vt), D_list dlist      -> V_lit_map (kt, vt, dlist |>
                                                              Core.List.fold_left 
                                                                ~init:Core.Map.Poly.empty
                                                                ~f:(map_func ~kt:kt ~vt:vt))
    | T_big_map (kt, vt), D_list dlist  -> V_lit_big_map (kt, vt, dlist |>
                                                                  Core.List.fold_left
                                                                    ~init:Core.Map.Poly.empty
                                                                    ~f:(map_func ~kt:kt ~vt:vt))
    | T_lambda (t1, t2), D_lambda it    -> V_lit_lambda (t1, t2, it)
    | _, D_elt _                        -> Error "create_expr_of_michdata_i : Invalid data D_elt" |> raise
    | _                                 -> Error "create_expr_of_michdata_i : Invalid match" |> raise
end
and create_expr_of_michdata : PreLib.Mich.data PreLib.Mich.t -> Vlang.typ -> Vlang.Expr.t
= fun michdata_t vtyp -> begin (* Wrapping function for create_expr_of_michdata_i *)
  create_expr_of_michdata_i (PreLib.Mich.get_d michdata_t) vtyp
end

(* TODO (plan) : make this function transaction-specific / amount, balance, source, sender, ... *)
let create_expr_of_cfgexpr : Env.t -> PreLib.Cfg.expr -> Vlang.Expr.t
= let open PreLib.Cfg in
  let open Vlang.Expr in
  fun cenv cfgexpr -> begin
  let cvt : PreLib.Cfg.ident -> Vlang.typ = Env.read_vartype ~env:cenv in (* syntax sugar *)
  let cvf : PreLib.Cfg.ident -> Vlang.Expr.t = fun v -> v |> Env.read_varname ~env:cenv |> Env.read_expr_of_cfgvar ~env:cenv in (* syntax sugar *)
  let err (): 'a = InvalidConversion_Expr cfgexpr |> raise in
  match cfgexpr with 
  | E_push (d, t) -> create_expr_of_michdata d (CvUtils.to_vtyp t)
  | E_car v -> (match cvt v with | T_pair _ -> V_car (cvf v) | _ -> err ())
  | E_cdr v -> (match cvt v with | T_pair _ -> V_cdr (cvf v) | _ -> err ())
  | E_abs v -> (match cvt v with | T_int -> V_abs_in (cvf v) | _ -> err ())
  | E_neg v -> (match cvt v with | T_nat -> V_neg_ni (cvf v) | T_int -> V_neg_ii (cvf v) | _ -> err ())
  | E_not v -> (match cvt v with | T_nat -> V_not_ni (cvf v) | T_int -> V_not_ii (cvf v) | T_bool -> V_not_bb (cvf v) | _ -> err ())
  | E_add (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in
      match cvt v1, cvt v2 with 
      | T_nat, T_int -> V_add_nii (vv1, vv2)
      | T_int, T_nat -> V_add_ini (vv1, vv2)
      | T_int, T_int -> V_add_iii (vv1, vv2)
      | T_nat, T_nat -> V_add_nnn (vv1, vv2)
      | T_mutez, T_mutez -> V_add_mmm (vv1, vv2)
      | T_timestamp, T_int -> V_add_tit (vv1, vv2)
      | T_int, T_timestamp -> V_add_itt (vv1, vv2)
      | _ -> err ()
    )
  | E_sub (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in
      match cvt v1, cvt v2 with
      | T_nat, T_nat -> V_sub_nni (vv1, vv2)
      | T_nat, T_int -> V_sub_nii (vv1, vv2)
      | T_int, T_nat -> V_sub_ini (vv1, vv2)
      | T_int, T_int -> V_sub_iii (vv1, vv2)
      | T_timestamp, T_timestamp -> V_sub_tti (vv1, vv2)
      | T_mutez, T_mutez -> V_sub_mmm (vv1, vv2)
      | T_timestamp, T_int -> V_sub_tit (vv1, vv2)
      | _ -> err ()
    )
  | E_mul (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in
      match cvt v1, cvt v2 with
      | T_nat, T_int -> V_mul_nii (vv1, vv2)
      | T_int, T_nat -> V_mul_ini (vv1, vv2)
      | T_int, T_int -> V_mul_iii (vv1, vv2)
      | T_nat, T_nat -> V_mul_nnn (vv1, vv2)
      | T_mutez, T_nat -> V_mul_mnm (vv1, vv2)
      | T_nat, T_mutez -> V_mul_nmm (vv1, vv2)
      | _ -> err ()
    )
  | E_ediv (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in
      match cvt v1, cvt v2 with
      | T_nat, T_nat -> V_ediv_nnnn (vv1, vv2)
      | T_nat, T_int -> V_ediv_niin (vv1, vv2)
      | T_int, T_nat -> V_ediv_inin (vv1, vv2)
      | T_int, T_int -> V_ediv_iiin (vv1, vv2)
      | T_mutez, T_nat -> V_ediv_mnmm (vv1, vv2)
      | T_mutez, T_mutez -> V_ediv_mmnm (vv1, vv2)
      | _ -> err ()
    )
  | E_shiftL (v1, v2) -> (match cvt v1, cvt v2 with | T_nat, T_nat -> V_shiftL_nnn (cvf v1, cvf v2) | _ -> err ())
  | E_shiftR (v1, v2) -> (match cvt v1, cvt v2 with | T_nat, T_nat -> V_shiftR_nnn (cvf v1, cvf v2) | _ -> err ())
  | E_and (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in
      match cvt v1, cvt v2 with
      | T_nat, T_nat -> V_and_nnn (vv1, vv2)
      | T_int, T_nat -> V_and_inn (vv1, vv2)
      | T_bool, T_bool -> V_and_bbb (vv1, vv2)
      | _ -> err ()
    )
  | E_or (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in
      match cvt v1, cvt v2 with
      | T_nat, T_nat -> V_or_nnn (vv1, vv2)
      | T_bool, T_bool -> V_or_bbb (vv1, vv2)
      | _ -> err ()
    )
  | E_xor (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in
      match cvt v1, cvt v2 with
      | T_nat, T_nat -> V_xor_nnn (vv1, vv2)
      | T_bool, T_bool -> V_xor_bbb (vv1, vv2)
      | _ -> err ()
    )
  | E_eq v -> (match cvt v with | T_int -> V_eq_ib (cvf v) | _ -> err ())
  | E_neq v -> (match cvt v with | T_int -> V_neq_ib (cvf v) | _ -> err ())
  | E_lt v -> (match cvt v with | T_int -> V_lt_ib (cvf v) | _ -> err ())
  | E_gt v -> (match cvt v with | T_int -> V_gt_ib (cvf v) | _ -> err ())
  | E_leq v -> (match cvt v with | T_int -> V_leq_ib (cvf v) | _ -> err ())
  | E_geq v -> (match cvt v with | T_int -> V_geq_ib (cvf v) | _ -> err ())
  | E_compare (v1, v2) -> (if cvt v1 = cvt v2 then V_compare (cvf v1, cvf v2) else err ())
  | E_cons (v1, v2) -> (match cvt v1, cvt v2 with | elt1, T_list elt2 when elt1 = elt2 -> V_cons (cvf v1, cvf v2) | _ -> err ())
  | E_operation op -> (
      (* TODO : check variable types! *)
      match op with
      | O_create_contract (p, v1, v2, v3) -> V_create_contract ((CvUtils.to_vtyp p.PreLib.Mich.param), (CvUtils.to_vtyp p.PreLib.Mich.storage), V_lit_program (p), cvf v1, cvf v2, cvf v3)
      | O_transfer_tokens (v1, v2, v3) -> V_transfer_tokens (cvf v1, cvf v2, cvf v3)
      | O_set_delegate v -> V_set_delegate (cvf v)
      | O_create_account _ -> err () (* deprecated operation *)
    )
  | E_unit -> V_unit
  | E_pair (v1, v2) -> V_pair (cvf v1, cvf v2)
  | E_left (v, t) -> let tt = CvUtils.to_vtyp t in (match tt, cvt v with | T_or (lt1, _), lt2 when lt1 = lt2 -> V_left (tt, cvf v) | _ -> err ())
  | E_right (v, t) -> let tt = CvUtils.to_vtyp t in (match tt, cvt v with | T_or (_, lt1), lt2 when lt1 = lt2 -> V_right (tt, cvf v) | _ -> err ())
  | E_some v -> V_some (cvf v)
  | E_none t -> V_none (CvUtils.to_vtyp t)
  | E_mem (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in
      match cvt v1, cvt v2 with
      | elt1, T_set elt2 when elt1 = elt2 -> V_mem_xsb (vv1, vv2)
      | kt1, T_map (kt2, _) when kt1 = kt2 -> V_mem_xmb (vv1, vv2)
      | kt1, T_big_map (kt2, _) when kt1 = kt2 -> V_mem_xbmb (vv1, vv2)
      | _ -> err ()
    )
  | E_get (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in
      match cvt v1, cvt v2 with
      | kt1, T_map (kt2, _) when kt1 = kt2 -> V_get_xmoy (vv1, vv2)
      | kt1, T_map (kt2, _) when kt1 = kt2 -> V_get_xbmo (vv1, vv2)
      | _ -> err ()
    )
  | E_update (v1, v2, v3) -> (
      let vv1, vv2, vv3 = cvf v1, cvf v2, cvf v3 in
      match cvt v1, cvt v2, cvt v3 with
      | t1, T_bool, T_set t2 when t1 = t2 -> V_update_xbss (vv1, vv2, vv3)
      | kt1, T_option vt1, T_map (kt2, vt2) when kt1 = kt2 && vt1 = vt2 -> V_update_xomm (vv1, vv2, vv3)
      | kt1, T_option vt1, T_big_map (kt2, vt2) when kt1 = kt2 && vt1 = vt2 -> V_update_xobmbm (vv1, vv2, vv3)
      | _ -> err ()
    )
  
  | E_concat (v1, v2) -> (
      let vv1, vv2 = cvf v1, cvf v2 in 
      match cvt v1, cvt v2 with 
      | T_string, T_string -> V_concat_sss (vv1, vv2)
      | T_bytes, T_bytes -> V_concat_bbb (vv1, vv2)
      | _ -> err ()
    )
  | E_concat_list v -> (match cvt v with | T_list T_string -> V_concat_list_s (cvf v) | T_list T_bytes -> V_concat_list_b (cvf v) | _ -> err ())
  | E_slice (v1, v2, v3) -> (
      let vv1, vv2, vv3 = cvf v1, cvf v2, cvf v3 in
      match cvt v1, cvt v2, cvt v3 with
      | T_nat, T_nat, T_string -> V_slice_nnso (vv1, vv2, vv3)
      | T_nat, T_nat, T_bytes -> V_slice_nnbo (vv1, vv2, vv3)
      | _ -> err ()
    )
  | E_pack v ->  V_pack (cvf v)
  | E_unpack (t, v) -> V_unpack (CvUtils.to_vtyp t, cvf v)
  | E_self -> (
      let paramstoragetyp = PreLib.Cfg.t_map_find ~errtrace:"Prover.Converter.create_expr_of_cfgexpr : E_self" !cenv.cfg.type_info PreLib.Cfg.param_storage_name in
      match CvUtils.to_vtyp paramstoragetyp with | T_pair (pt, _) -> V_self pt | _ -> err ()
    )
  | E_contract_of_address (t, v) -> (match cvt v with | T_address -> V_contract_of_address (CvUtils.to_vtyp t, cvf v) | _ -> err ())
  | E_implicit_account v -> (match cvt v with | T_key_hash -> V_implicit_account (cvf v) | _ -> err ())
  | E_now -> V_now
  | E_amount -> V_amount
  | E_balance -> V_balance
  | E_check_signature (v1, v2, v3) -> (match cvt v1, cvt v2, cvt v3 with | T_key, T_signature, T_bytes -> V_check_signature (cvf v1, cvf v2, cvf v3) | _ -> err ())
  | E_blake2b v -> (match cvt v with | T_bytes -> V_blake2b (cvf v) | _ -> err ())
  | E_sha256 v -> (match cvt v with | T_bytes -> V_sha256 (cvf v) | _ -> err ())
  | E_sha512 v -> (match cvt v with | T_bytes -> V_sha512 (cvf v) | _ -> err ())
  | E_hash_key v -> (match cvt v with | T_key -> V_hash_key (cvf v) | _ -> err ())
  | E_source -> V_source
  | E_sender -> V_sender
  | E_address_of_contract v -> (match cvt v with | T_contract _ -> V_address_of_contract (cvf v) | _ -> err ())
  | E_unlift_option v -> (match cvt v with | T_option _ -> V_unlift_option (cvf v) | _ -> err ())
  | E_unlift_left v -> (match cvt v with | T_or _ -> V_unlift_left (cvf v) | _ -> err ())
  | E_unlift_right v -> (match cvt v with | T_or _ -> V_unlift_right (cvf v) | _ -> err ())
  | E_hd v -> (
      let vv = cvf v in
      match cvt v with
      | T_list _ -> V_hd_l vv
      | T_set _ -> V_hd_s vv
      | T_map _ -> V_hd_m vv
      | T_big_map _ -> V_hd_bm vv
      | _ -> err ()
    )
  | E_tl v -> (
      let vv = cvf v in
      match cvt v with
      | T_list _ -> V_tl_l vv
      | T_set _ -> V_tl_s vv
      | T_map _ -> V_tl_m vv
      | T_big_map _ -> V_tl_bm vv
      | _ -> err ()
    )
  | E_hdtl v -> (
      let vv = cvf v in
      match cvt v with
      | T_list _ -> V_hdtl_l vv
      | T_set _ -> V_hdtl_s vv
      | T_map _ -> V_hdtl_m vv
      | _ -> err ()
    )
  | E_size v -> (
      let vv = cvf v in
      match cvt v with 
      | T_set _ -> V_size_s vv
      | T_map _ -> V_size_m vv
      | T_list _ -> V_size_l vv
      | T_string -> V_size_str vv
      | T_bytes -> V_size_b vv
      | _ -> err ()
    )
  | E_isnat v -> (match cvt v with | T_int -> V_isnat (cvf v) | _ -> err ())
  | E_int_of_nat v -> (match cvt v with | T_nat -> V_int_of_nat (cvf v) | _ -> err ())
  | E_chain_id -> V_chain_id
  | E_lambda_id n -> (
      let (_, _, pt, rett) = (t_map_find ~errtrace:"Prover.Converter.create_expr_of_cfgexpr" !cenv.cfg.lambda_id_map n) in
      V_lambda_id (CvUtils.to_vtyp pt, CvUtils.to_vtyp rett, n)
    )
  | E_exec (v1, v2) -> (
      match cvt v1, cvt v2 with
      | pt1, T_lambda (pt2, _) when pt1 = pt2 -> V_exec (cvf v1, cvf v2)
      | _ -> err ()
    )
  | E_dup v -> V_dup (cvf v)
  | E_nil t -> V_nil (CvUtils.to_vtyp t)
  | E_empty_set t -> V_empty_set (CvUtils.to_vtyp t)
  | E_empty_map (t1, t2) -> V_empty_map (CvUtils.to_vtyp t1, CvUtils.to_vtyp t2)
  | E_empty_big_map (t1, t2) -> V_empty_big_map (CvUtils.to_vtyp t1, CvUtils.to_vtyp t2)
  | E_itself v -> V_itself (cvf v)
  | E_append (v1, v2) -> (
      match cvt v1, cvt v2 with
      | elt1, T_list elt2 when elt1 = elt2 -> V_append_l (cvf v1, cvf v2)
      | _ -> err ()
    )

  (* TODOs *)
  | E_cast _ -> err () (* TODO : vlang-unimplemented *)
  | E_steps_to_quota -> err () (* deprecated instruction *)
  
  (* Deprecated from Cfg *)
  | E_div _ -> err ()
  | E_mod _ -> err ()
  | E_create_contract_address _ -> err ()
  | E_create_account_address _ -> err ()
  | E_lambda _ -> err ()
  | E_special_nil_list -> err ()
  | E_phi _ -> err ()
  | E_unlift_or _ -> err ()
end (* function create_expr_of_cfgexpr end *)

let rec create_formula_of_cond : Env.t -> Bp.cond -> Vlang.v_formula
= let open Vlang.Formula in
  let open Bp in
  fun cenv c -> begin
  let cvt : PreLib.Cfg.ident -> Vlang.typ = Env.read_vartype ~env:cenv in (* syntax sugar *)
  let cvf : PreLib.Cfg.ident -> Vlang.Expr.t = fun v -> v |> Env.read_varname ~env:cenv |> Env.read_expr_of_cfgvar ~env:cenv in (* syntax sugar *)
  let err (): 'a = Stdlib.raise (InvalidConversion_Cond c) in
  match c with
    | BC_is_true v -> VF_mich_if (v |> Env.read_expr_of_cfgvar ~env:cenv)
    | BC_is_none v -> VF_mich_if_none (v |> Env.read_expr_of_cfgvar ~env:cenv)
    | BC_is_left v -> VF_mich_if_left (v |> Env.read_expr_of_cfgvar ~env:cenv)
    | BC_is_cons v -> VF_mich_if_cons (v |> Env.read_expr_of_cfgvar ~env:cenv)
    | BC_no_overflow e -> (
        match e with
        | E_add (v1, v2) -> (
            let vv1, vv2 = cvf v1, cvf v2 in
            match cvt v1, cvt v2 with 
            | T_mutez, T_mutez -> VF_add_mmm_no_overflow (vv1, vv2)
            | _ -> err ()
          )
        | E_mul (v1, v2) -> (
            let vv1, vv2 = cvf v1, cvf v2 in
            match cvt v1, cvt v2 with 
            | T_mutez, T_nat -> VF_mul_mnm_no_overflow (vv1, vv2)
            | T_nat, T_mutez -> VF_mul_nmm_no_overflow (vv1, vv2)
            | _ -> err ()
          )
        | _ -> err () 
      )
    | BC_no_underflow e -> (
        match e with
        | E_sub (v1, v2) -> (
            let vv1, vv2 = cvf v1, cvf v2 in
            match cvt v1, cvt v2 with 
            | T_mutez, T_mutez -> VF_sub_mmm_no_underflow (vv1, vv2)
            | _ -> err ()
          )
        | _ -> err () 
      )
    | BC_not c -> VF_not (create_formula_of_cond cenv c)
end

let rename_formula : Vlang.t -> cenv:Env.t -> Vlang.t
=fun fmla ~cenv -> begin
  let is_var : Vlang.Expr.t -> bool = (function | Vlang.Expr.V_var _ -> true | _ -> false) in
  let rename_var : Vlang.Expr.t -> Vlang.Expr.t = fun e -> (
    match e with
    | Vlang.Expr.V_var (t, s) -> Vlang.Expr.V_var (t, (s |> Env.read_varname ~env:cenv))
    | _ -> e
  ) in
  Vlang.RecursiveMappingExprTemplate.map_formula_outer is_var rename_var fmla
end


let sp : Env.t -> (Vlang.t * Query.t list) -> (Bp.vertex * Bp.inst) -> (Vlang.t * Query.t list)
=fun cenv (f, qs) (_, s) -> begin
  match s with
  | BI_assume c -> 
      let f' = Vlang.Formula.VF_and [(create_formula_of_cond cenv c); f] in
      (f', qs)
  | BI_assert (c, loc, ctg) ->
      let formula = Vlang.Formula.VF_imply (f, (create_formula_of_cond cenv c)) in
      let query = Query.create_new_query formula ~loc:loc ~category:ctg in
      (f, (query::qs))
  | BI_assign (v, e) ->
      let e_f = create_expr_of_cfgexpr cenv e in
      let _ = Env.update_expr_of_cfgvar v e_f ~env:cenv in
      (f, qs)
  | BI_skip -> (f, qs)
end

let convert : Bp.t -> PreLib.Cfg.t -> (Vlang.t * Query.t list)
=fun bp cfg -> begin
  let cv_env : Env.t = cfg |> Env.create in
  let (f, g) = ((bp.pre |> Inv.T.read_formula), (bp.post |> Inv.T.read_formula)) in
  let (f', qs) = Core.List.fold_left bp.body ~init:(f, []) ~f:(sp cv_env) in
  let inductive = Vlang.Formula.VF_imply (f', (g |> rename_formula ~cenv:cv_env)) in
  (inductive, qs)
end
