(* get_unrolled_cfg : filepath -> unroll-num -> (cfg * cfg-counter) *)
(* Any cfg-printing options will be ignored. *)
let get_unrolled_cfg : string -> int -> (PreLib.Cfg.t * PreLib.Cfg.cfgcon_ctr)
=fun filename unroll_num -> begin
  let open Pre in
  let open PreLib in
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
let extract_basicpaths : Pre.Lib.Cfg.t -> Prover.Lib.Bp.raw_t_list = Prover.Extractor.extract

(* Since there are no loops in unrolled-cfg, every basicpaths are Normal-execution or Failed-execution. *)
(* get_regular_basicpaths filters out any Failed-execution basicpaths in the input. *)
let get_regular_basicpaths : Pre.Lib.Cfg.t -> Prover.Lib.Bp.t list -> Prover.Lib.Bp.t list
=fun cfg tlist -> begin
  (* filter_func : Is body's last vertex Main-Exit? *)
  let filter_func : Prover.Lib.Bp.t -> bool = fun {body=blist; _} -> List.nth_opt blist (List.length blist - 1) |> (function | None -> false | Some (v, _) -> v = cfg.main_exit) in
  List.filter filter_func tlist
end

let rec basicpath_sequences : (int * (Prover.Lib.Bp.t list) list) -> Prover.Lib.Bp.t list -> (int * (Prover.Lib.Bp.t list) list)
=fun (n, acc) tlist -> begin
  (* bp in tlist, seq in acc, make a new list ( = list of (bp :: seq)) *)
  if n <= 0 then (n, acc) else
  let combination : (Prover.Lib.Bp.t list) list = List.fold_left (fun newlist_acc seq -> List.fold_left (fun newlist_acc_i bp -> (bp :: seq) :: newlist_acc_i) newlist_acc tlist) [] acc in
  basicpath_sequences ((n-1), combination) tlist
end

(* parameter name for each transactions in transaction sequence. magic-string generator *)
let trx_seq_param_name : int -> string = fun i -> ("tsParam-" ^ (Stdlib.string_of_int i))
(* variable name which contains initial storage. magic-storage generator *)
let trx_seq_storage_name : string = "tsStorage"

(* concat N-basicpaths into one basicpath. *)
(* Example.
    (*  bp1.body = [(v1,i1);(v2,var-2 := E_itself _)];
        bp2.body = [(v3,i3);(v4,var-4 := E_itself _)];
        bp3.body = [(v5,i5);(v6,var-6 := E_itself _)];
    *)
  let result = concat_basicpath [bp1; bp2; bp3] counter

  (* Consider that the initial storage given *)
  <result.body>
  = [ (v7,(tsStorage := E_push (initial Storage Value)))
      (v8,(param_storage := E_pair tsParam-0, initialStorage));
      (v1,i1); (v2,var-2 := E_itself _);
      (v9,(param_storage := E_pair tsParam-1, var-2));
      (v3,i3); (v4,var-4 := E_itself _);
      (v10,(param_storage := E_pair tsParam-2, var-4));
      (v5,i5); (v6,var-6 := E_itself _);
    ]
  (* v7,v8,v9,v10 are newly-created vertices. In implementation, they will be all negative-integer, and their uniqueness is not guaranteed *)
*)
(*
let concat_basicpath : (PreLib.Mich.data PreLib.Mich.t * PreLib.Mich.typ PreLib.Mich.t) option -> Pre.Lib.Cfg.t -> Prover.Lib.Bp.t list -> Prover.Lib.Bp.t
=fun initStgOpt cfg bplist ->
  let open Prover.Lib in
  let open PreLib in
  let exit_vtx = cfg.main_exit in
  let exit_vtx_stmt = Cfg.t_map_find ~errtrace:("refuter/extractor.ml : concat_basicpath : exit_vtx_stmt") cfg.vertex_info exit_vtx in
  let exit_vtx_var = (
    match exit_vtx_stmt with
    | Cfg.Cfg_assign (evv, E_itself _) -> evv
    | _ -> Stdlib.failwith "refuter/extractor.ml : concat_basicpath : exit_vtx_var"
  ) in
  let vtx_counter : int ref = ref 0 in
  let var_counter : int ref = ref (-1) in
  let new_vertex () : Bp.vertex = vtx_counter := !vtx_counter + 1; (-(!vtx_counter + 1)) in (* start with 1 *)
  let new_param_var () : string = var_counter := !var_counter + 1; trx_seq_param_name !var_counter in (* start with 0 *)
  let firstInst : (Bp.vertex * Bp.inst) list = (
    match initStgOpt with
    | None -> [new_vertex (), BI_assign (Cfg.param_storage_name, E_pair (new_param_var (), trx_seq_storage_name));]
    | Some (stgdata, stgtyp) -> [ new_vertex (), BI_assign (trx_seq_storage_name, E_push (stgdata, stgtyp));
                                  new_vertex (), BI_assign (Cfg.param_storage_name, E_pair (new_param_var (), trx_seq_storage_name));
                                ]
  ) in
  (* sandwich_inst N VAR = Generate storage-variable-setting instruction for (N-th transaction ends and the storage variable is VAR) *)
  (* N is omitted, var_counter and carefully called new_param_var will do the same job. *)
  let sandwich_inst : string -> (Bp.vertex * Bp.inst) list =
    fun v -> [new_vertex (), BI_assign (Cfg.param_storage_name, E_pair (new_param_var (), v))]
  in
  let bodylist : ((Bp.vertex * Bp.inst) list) list = (List.fold_left (fun acc bp -> bp.Bp.body :: (sandwich_inst exit_vtx_var) :: acc) [firstInst] bplist) |> List.rev in
  let bodylist_ft : (Bp.vertex * Bp.inst) list = List.flatten bodylist in
  {pre=Vlang.create_formula_true; body=bodylist_ft; }
*)
(* TODO *)
