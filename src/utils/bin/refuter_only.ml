(* It only takes refuter functions for the given Michelson code.
    It can take same options as MicSE, but the options unrelated with (Preprocessing & Refuter) will be ignored.
    Process returns 0 if normally executed.
*)

(* Example Usages
  * If you want to make sure it's working
      dune exec -- micse.utils.refuter_only -input [PROJECT-ROOT]/benchmarks/toy/add1.tz
*)

let unroll_NUM = 2
let transaction_seq_NUM = 2

let _ =
  let _ = Utils.Options.create_options () in
  let _ = Printexc.record_backtrace false in
  let open Pre in
  let open PreLib in
  let adt : Adt.t = Adt.parse !Utils.Options.input_file |> Mich.subst_standard_macro_all_pgm |> Mich.optm_all_pgm |> Mich.fill_position_all_pgm ~update_loc:false in
  let (cfg_first, ctr) : Cfg.t * Cfg.cfgcon_ctr = Translator.adt_to_cfg_counter_included (adt, None) in
  let cfg_rssov_optimized = (if (!Utils.Options.flag_cfgopt_rssov) then (CfgUtil.remove_simple_stack_operation_vertices cfg_first) else (cfg_first)) in
  let cfg_rsv_optimized = (if (!Utils.Options.flag_cfgopt_rsv) then (CfgUtil.remove_meaningless_skip_vertices_fixpoint cfg_rssov_optimized) else (cfg_rssov_optimized)) in
  let cfg_rfv_optimized = (if (!Utils.Options.flag_cfgopt_rfv) then (CfgUtil.remove_meaningless_fail_vertices_fixpoint cfg_rsv_optimized) else (cfg_rsv_optimized)) in
  let cfg_optimized = cfg_rfv_optimized in
  let _ : unit = (if (!Utils.Options.flag_cfg_print_dot) then print_endline (CfgUtil.cfg_to_dotformat cfg_optimized) else ()) in

  let (cfg_unrolled, _) : Cfg.t * Cfg.cfgcon_ctr = CfgUtil.LoopUnrolling.run (cfg_optimized, ctr, unroll_NUM) in
  let cfg_ur_rsv_optimized = (if (!Utils.Options.flag_cfgopt_rsv) then (CfgUtil.remove_meaningless_skip_vertices_fixpoint cfg_unrolled) else (cfg_unrolled)) in
  let cfg_ur_optimized = cfg_ur_rsv_optimized in
  let _ : unit = if (!Utils.Options.flag_cfg_print_dot) then (print_endline (CfgUtil.cfg_to_dotformat cfg_ur_optimized)) else () in
  let cfg = cfg_ur_optimized in

  
  (*
  let brt_lst = Refuter.Extractor.extract_basicpaths cfg in
  (*let _ = List.iter (fun x -> Prover.Lib.Bp.to_string x |> Stdlib.print_endline; Stdlib.print_newline ()) brt_lst.bps  in*)
  let _ = List.length brt_lst.bps |> Stdlib.string_of_int |> Stdlib.(^) "# of basic paths : " |> Stdlib.print_endline in
  let _ = List.length brt_lst.trx_inv_vtx |> Stdlib.string_of_int |> Stdlib.(^) "# of transaction invariant vertices : " |> Stdlib.print_endline in
  let _ = List.length brt_lst.loop_inv_vtx |> Stdlib.string_of_int |> Stdlib.(^) "# of loop invariant vertices : " |> Stdlib.print_endline in
  *)

  let (bplist, bpupdated_cfg) : (Prover.Lib.Bp.t list * PreLib.Cfg.t) = Refuter.Extractor.get_concatenated_basicpaths None cfg transaction_seq_NUM in
  (*let _ : unit = List.length bplist |> Stdlib.print_int in*)
  (*let _ : unit = List.iter (fun x -> (Prover.Lib.Bp.to_string x |> Stdlib.print_endline); Stdlib.print_newline ()) bplist in*)

  let querylist : Prover.Lib.Query.t list = Refuter.Runner.collect_queries bpupdated_cfg bplist (fun x -> x) in
  let _ : unit = print_endline ("# of queries : " ^ (List.length querylist |> string_of_int)) in
  
  (*
  let refute_results : (Z3.Solver.status * ((string * ProverLib.Smt.z_expr) list) option) list
    = List.map (fun q -> Refuter.Runner.refute_unit bpupdated_cfg q) querylist
  in
  let _ : unit = List.iter (fun q -> ProverLib.Vlang.string_of_formula q.ProverLib.Query.query |> Stdlib.print_endline; Stdlib.print_newline ()) querylist in
  let _ : unit = List.iter (fun r -> Refuter.Runner.string_of_refute_result r |> Stdlib.print_endline; Stdlib.print_newline ()) refute_results in
  *)

  (* Enhanced Readability *)
  let _ : unit = 
    let count = ref 0 in
    List.iter (
      fun q ->
        print_string ("Current processor time : " ^ (Sys.time () |> string_of_float));
        print_newline ();
        print_newline ();
        print_string "Query #";
        print_int !count;
        incr count;
        print_newline ();
        ProverLib.Vlang.string_of_formula q.ProverLib.Query.query |> print_endline;
        print_newline ();
        let result = Refuter.Runner.refute_unit bpupdated_cfg q in
        Refuter.Runner.string_of_refute_result result |> print_endline;
        print_newline ();
        print_newline ();
    )
    querylist
  in


  
  let _ : unit = (querylist, bpupdated_cfg) |> Stdlib.ignore in

  Stdlib.exit 0
