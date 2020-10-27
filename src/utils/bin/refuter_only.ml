(* It only takes refuter functions for the given Michelson code.
    It can take same options as MicSE, but the options unrelated with (Preprocessing & Refuter) will be ignored.
    Process returns 0 if normally executed.
*)

(* Example Usages
  * If you want to make sure it's working
      dune exec -- micse.utils.refuter_only -input [PROJECT-ROOT]/benchmarks/toy/add1.tz
*)

let unroll_NUM = 2

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
  let _ : unit = print_endline (CfgUtil.cfg_to_dotformat cfg_ur_optimized) in
  let cfg = cfg_ur_optimized in


  let brt_lst = Refuter.Extractor.extract_basicpaths cfg in
  let _ = List.iter (fun x -> Prover.Lib.Bp.to_string x |> Stdlib.print_endline; Stdlib.print_newline ()) brt_lst.bps  in
  (*let _ = List.length brt_lst.bps |> Stdlib.string_of_int |> Stdlib.print_endline in*)
  let _ = List.length brt_lst.trx_inv_vtx |> Stdlib.string_of_int |> Stdlib.print_endline in
  let _ = List.length brt_lst.loop_inv_vtx |> Stdlib.string_of_int |> Stdlib.print_endline in
  Stdlib.exit 0
