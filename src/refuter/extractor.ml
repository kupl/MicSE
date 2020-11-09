(* get_unrolled_cfg : filepath -> unroll-num -> (cfg * cfg-counter) *)
(* Any cfg-printing options will be ignored. *)
let get_unrolled_cfg : string -> int -> (PreLib.Cfg.t * PreLib.Cfg.cfgcon_ctr)
= let open Pre in
  let open PreLib in
  fun filename unroll_num -> begin
  (* parsing and construct original cfg *)
  let adt : Adt.t = Adt.parse filename |> Mich.subst_standard_macro_all_pgm |> Mich.optm_all_pgm |> Mich.fill_position_all_pgm ~update_loc:false in
  let (cfg_first, ctr1) : Cfg.t * Cfg.cfgcon_ctr = Translator.adt_to_cfg_counter_included (adt, None) in
  let cfg_rssov_optimized = (if (!Utils.Options.flag_cfgopt_rssov) then (CfgUtil.remove_simple_stack_operation_vertices cfg_first) else (cfg_first)) in
  let cfg_rsv_optimized = (if (!Utils.Options.flag_cfgopt_rsv) then (CfgUtil.remove_meaningless_skip_vertices_fixpoint cfg_rssov_optimized) else (cfg_rssov_optimized)) in
  let cfg_rfv_optimized = (if (!Utils.Options.flag_cfgopt_rfv) then (CfgUtil.remove_meaningless_fail_vertices_fixpoint cfg_rsv_optimized) else (cfg_rsv_optimized)) in
  let cfg_optimized = cfg_rfv_optimized in
  (* unrolling cfg *)
  let (cfg_unrolled, ctr2) : Cfg.t * Cfg.cfgcon_ctr = CfgUtil.LoopUnrolling.run (cfg_optimized, ctr1, unroll_num) in
  let cfg_ur_rsv_optimized = (if (!Utils.Options.flag_cfgopt_rsv) then (CfgUtil.remove_meaningless_skip_vertices_fixpoint cfg_unrolled) else (cfg_unrolled)) in
  let cfg_ur_optimized = cfg_ur_rsv_optimized in
  let cfg = cfg_ur_optimized in
  (cfg, ctr2)
end

(* sugar/renaming *)
let extract_basicpaths : Pre.Lib.Cfg.t -> Prover.Lib.Bp.lst = Prover.Extractor.extract

(* Since there are no loops in unrolled-cfg, every basicpaths are Normal-execution or Failed-execution. *)
(* get_regular_basicpaths filters out any Failed-execution basicpaths in the input. *)
let get_regular_basicpaths : Pre.Lib.Cfg.t -> Prover.Lib.Bp.t list -> Prover.Lib.Bp.t list
=fun cfg tlist -> begin
  (* filter_func : Is body's last vertex Main-Exit? *)
  let filter_func : Prover.Lib.Bp.t -> bool = 
    fun {body=blist; _} -> 
    List.nth_opt blist (List.length blist - 1) 
    |> (function | None -> false | Some (v, _) -> v = cfg.main_exit) 
  in
  List.filter filter_func tlist
end

(* create length-N basicpaths (only when "acc" is empty list) *)
(* After recursion, it will generate every length-N transactions *)
let rec basicpath_sequences_N : (int * (Prover.Lib.Bp.t list) list) -> Prover.Lib.Bp.t list -> (int * (Prover.Lib.Bp.t list) list)
=fun (n, accparam) tlist -> begin
  if n <= 1 then (n, accparam) else
  let combination : (Prover.Lib.Bp.t list) list = 
    List.fold_left (fun acc bp -> (List.map (fun trxseq -> bp :: trxseq) accparam) @ acc) [] tlist
  in
  basicpath_sequences_N ((n-1), combination) tlist
end



(* parameter name for each transactions in transaction sequence. magic-string generator *)
let trx_seq_param_name : int -> string = fun i -> ("trxParam-" ^ (Stdlib.string_of_int i))
(* storage name for each transactions in transaction sequence. magic-string generator *)
let trx_seq_storage_name : int -> string = fun i -> ("trxStorage-" ^ (Stdlib.string_of_int i))
(* operation-list name for each transactions in transaction sequence. magic-string generator *)
let trx_seq_operation_name : int -> string = fun i -> ("trxOper-" ^ (Stdlib.string_of_int i))

(* add_trxseq_var_types adds transaction-related variables' type information *)
(* It heavily depends on the implementation of "initial_storage_typ" and some "trx_seq_" ... strings *)
let add_trxseq_var_types : PreLib.Cfg.t -> int -> PreLib.Cfg.t
=fun cfg n -> begin
  (* Collect newly-added variable names (generated from "initial_storage_typ" *)
  let rec vargen_template : (int -> string) -> int -> int -> string list -> string list
  =fun vargen i_start i_ubound acc_l -> begin
    if i_start >= i_ubound then acc_l else
    vargen_template vargen (i_start+1) i_ubound ((vargen i_start) :: acc_l)
  end in
  let trx_seq_param_names : string list = vargen_template trx_seq_param_name 0 n [] in
  let trx_seq_storage_names : string list = vargen_template trx_seq_storage_name 0 (n+1) [] in
  let trx_seq_operation_names : string list = vargen_template trx_seq_operation_name 1 (n+1) [] in
  (* Get the Michelson types of paramter, storage, and operation-list. *)
  let open PreLib in
  let gen_t = Mich.gen_t in
  let ripoff_pairtyp : Mich.typ Mich.t -> (Mich.typ Mich.t * Mich.typ Mich.t)
  =fun t -> begin
    match Mich.get_d t with
    | Mich.T_pair (t1, t2) -> (t1, t2)
    | _ -> Stdlib.failwith "Refuter : extractor.ml : add_trxseq_var_types : ripoff_pairtyp"
  end in
  let ps_typ : Mich.typ Mich.t = Cfg.t_map_find ~errtrace:("Refuter : extractor.ml : add_trxseq_var_types : ps_typ") cfg.type_info Cfg.param_storage_name in
  let (param_typ, storage_typ) = ripoff_pairtyp ps_typ in
  let operation_list_typ : Mich.typ Mich.t = gen_t (Mich.T_list (gen_t Mich.T_operation)) in
  (* Update Type-Info *)
  let update_cfg_tmpl : (Mich.typ Mich.t) -> string list -> Cfg.t -> Cfg.t
  =fun ty varlist cfg -> begin
    let new_type_info = 
      List.fold_left
        (fun typeinfo var -> Cfg.t_map_add ~errtrace:("Refuter : extractor.ml : add_trxseq_var_types : update_cfg_tmpl") typeinfo var ty)
        cfg.type_info
        varlist
    in
    {cfg with type_info=new_type_info}
  end in
  let updated_cfg : Cfg.t = begin
    cfg
    |> update_cfg_tmpl param_typ trx_seq_param_names
    |> update_cfg_tmpl storage_typ trx_seq_storage_names
    |> update_cfg_tmpl operation_list_typ trx_seq_operation_names
  end in
  (* Return type-information udpated cfg *)
  updated_cfg
end

type initial_storage_typ = (PreLib.Mich.data PreLib.Mich.t * PreLib.Mich.typ PreLib.Mich.t) option
(* concat N-basicpaths into one basicpath. *)
(* Example.
    (*  bp1.body = [(v1,i1);(v2,var-2 := E_itself _)];
        bp2.body = [(v3,i3);(v4,var-4 := E_itself _)];
        bp3.body = [(v5,i5);(v6,var-6 := E_itself _)];
    *)
  let result = concat_basicpath [bp1; bp2; bp3] counter

  (* Consider that the initial storage given *)
  <result.body>
  = [ (v-1,(trxStorage-0 := E_push (initial Storage Value)))
      (v-2,(param_storage := E_pair trxParam-0, trxStorage-0));
      (v1,i1); (v2,var-2 := E_itself _);
      (v-3,(trxStorage-1 := CDR var-2));
      (v9,(param_storage := E_pair trxParam-1, trxStorage-1));
      (v3,i3); (v4,var-4 := E_itself _);
      (v10,(param_storage := E_pair trxParam-2, var-4));
      (v5,i5); (v6,var-6 := E_itself _);
    ]
  (* v7,v8,v9,v10 are newly-created vertices. In implementation, they will be all negative-integer, and their uniqueness is not guaranteed *)
*)
let concat_basicpath : initial_storage_typ -> Pre.Lib.Cfg.t -> Prover.Lib.Bp.t list -> Prover.Lib.Bp.t
= let open Prover.Lib in
  let open PreLib in
  fun initStgOpt cfg bplist ->
  let exit_vtx = cfg.main_exit in
  let exit_vtx_stmt = Cfg.t_map_find ~errtrace:("refuter/extractor.ml : concat_basicpath : exit_vtx_stmt") cfg.vertex_info exit_vtx in
  let exit_vtx_var = (
    match exit_vtx_stmt with
    | Cfg.Cfg_assign (evv, E_itself _) -> evv
    | _ -> Stdlib.failwith "refuter/extractor.ml : concat_basicpath : exit_vtx_var"
  ) in
  let vtx_counter : int ref = ref 0 in
  let paramvar_counter : int ref = ref (-1) in
  let stgvar_counter : int ref = ref (-1) in
  let opvar_counter : int ref = ref 0 in
  let new_vertex () : Bp.vertex = vtx_counter := !vtx_counter + 1; (-(!vtx_counter)) in (* start with -1 *)
  let new_param_var () : string = paramvar_counter := !paramvar_counter + 1; trx_seq_param_name !paramvar_counter in (* start with 0, ends with N *)
  let new_stg_var () : string = stgvar_counter := !stgvar_counter + 1; trx_seq_storage_name !stgvar_counter in (* start with 0, ends with N+1 *)
  let new_oper_var () : string = opvar_counter := !opvar_counter + 1; trx_seq_operation_name !opvar_counter in (* start with 1, ends with N+1 *)
  let first_stg_var : string = new_stg_var () in
  let firstInst : (Bp.vertex * Bp.inst) list = (
    match initStgOpt with
    | None -> [new_vertex (), BI_assign (Cfg.param_storage_name, E_pair (new_param_var (), first_stg_var));]
    | Some (stgdata, stgtyp) -> [ new_vertex (), BI_assign (first_stg_var, E_push (stgdata, stgtyp));
                                  new_vertex (), BI_assign (Cfg.param_storage_name, E_pair (new_param_var (), first_stg_var));
                                ]
  ) in
  (* sandwich_inst VAR = Generate storage-variable-setting instruction with the storage variable VAR *)
  (* it has side-effect of three counters, vtx_counter, paramvar_counter, stgvar_counter *)
  let sandwich_inst : string -> (Bp.vertex * Bp.inst) list =
    fun v ->
      let opvar : string = new_oper_var () in
      let stgvar : string = new_stg_var () in
      let (psvtx, stgvtx, opvtx) = (new_vertex(), new_vertex(), new_vertex()) in
      [ opvtx, BI_assign (opvar, E_car v);
        stgvtx, BI_assign (stgvar, E_cdr v);
        psvtx, BI_assign (Cfg.param_storage_name, E_pair (new_param_var (), stgvar));
      ]
  in
  (* assertion : bplist's length must larger than 1. *)
  let (bplist_hd, bplist_tl) : (Bp.t * Bp.t list) = try (Core.List.hd_exn bplist, Core.List.tl_exn bplist) with _ -> Stdlib.failwith "Refuter : extractor.ml : concat_basicpath : bplist hd-tl failed" in 
  let bodylist : ((Bp.vertex * Bp.inst) list) list = 
    let bodylist_i : ((Bp.vertex * Bp.inst) list) list = (List.fold_left (fun acc bp -> bp.Bp.body :: (sandwich_inst exit_vtx_var) :: acc) [bplist_hd.Bp.body; firstInst] bplist_tl) in
    let lastOp_bp : (Bp.vertex * Bp.inst) = (new_vertex (), BI_assign (new_oper_var (), E_car exit_vtx_var)) in
    let lastStg_bp : (Bp.vertex * Bp.inst) = (new_vertex (), BI_assign (new_stg_var (), E_cdr exit_vtx_var)) in
    ([lastOp_bp; lastStg_bp] :: bodylist_i) |> List.rev
  in
  let bodylist_ft : (Bp.vertex * Bp.inst) list = List.flatten bodylist in
  let dummy_true_inv : Inv.t = {id=exit_vtx; formula=Some(Prover.Lib.Vlang.create_formula_true)} in
  {pre=dummy_true_inv; body=bodylist_ft; post=dummy_true_inv}

(* get_concatenated_basicpaths
input : initial starge, unrolled cfg, maximum length of transaction sequence
output : transaction sequences (from length N)
*)
let get_concatenated_basicpaths : initial_storage_typ -> Pre.Lib.Cfg.t -> int -> (Prover.Lib.Bp.t list * PreLib.Cfg.t)
= let open Prover.Lib in
  fun initStgOpt unrolled_cfg n -> begin
  let basicpaths : Bp.lst = extract_basicpaths unrolled_cfg in
  (*(* print basicpaths *) let _ : unit = List.iter (fun l -> (List.iter (fun (v, _) -> print_int v; print_string " ") l.Bp.body); print_newline ()) basicpaths.bps in*)
  let filtered_basicpaths : Bp.t list = get_regular_basicpaths unrolled_cfg basicpaths.bp_list in
  (* print filtered basicpaths' list-length *) let _ : unit = Stdlib.print_string "# of filtered_basicpaths : "; List.length filtered_basicpaths |> Stdlib.string_of_int |> Stdlib.print_endline in
  let wrapped_filtered_basicpaths : Bp.t list list = List.fold_left (fun acc x -> [x] :: acc) [] filtered_basicpaths |> List.rev in
  let (_, bpll) : int * (Bp.t list list) = basicpath_sequences_N (n, wrapped_filtered_basicpaths) filtered_basicpaths in
  let _ : unit = Stdlib.print_string "# of basicpath_sequences : "; List.length bpll |> Stdlib.print_int; Stdlib.print_newline () in
  let typeinfo_updated_cfg : PreLib.Cfg.t = add_trxseq_var_types unrolled_cfg n in
  ((List.map (fun bpl -> concat_basicpath initStgOpt unrolled_cfg bpl) bpll), typeinfo_updated_cfg)
end
