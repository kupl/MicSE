(* It only performs Loop Unrolling (3 times) with preprocessed Michelson code and PRINT UNROLLED CFG.
    It can take same options as MicSE, but the options unrelated with Preprocessing will be ignored.
    Process returns 0 if normally executed.
*)

(* Example Usages
  * Consider using micse.utils.construct_cfg for plain cfg.
  * print unrolled cfg
      dune exec -- micse.utils.print_unrolled_cfg -input [https://gitlab.com/nomadic-labs/smart-contracts]/tezos-contracts/attic/infinite_loop.tz
  * print original cfg too
      dune exec -- micse.utils.print_unrolled_cfg -cfg_print_dot -input [https://gitlab.com/nomadic-labs/smart-contracts]/tezos-contracts/attic/infinite_loop.tz
  * get optimized-unrolled-cfg of optimized-cfg (RECOMMENDED, remove some redundant vertices)
      dune exec -- micse.utils.print_unrolled_cfg -cfgopt -input [https://gitlab.com/nomadic-labs/smart-contracts]/tezos-contracts/attic/infinite_loop.tz
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

  (*let _ = Pre.pre_process (!Utils.Options.input_file) in*)
  Stdlib.exit 0
