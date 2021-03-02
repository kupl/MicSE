module Lib = PreLib
open Lib

module Analyzer = Analyzer

module Translator = Translator

type pre_ret = {
  cfg : PreLib.Cfg.t;
  cfgcon_counter : PreLib.Cfg.cfgcon_ctr;
  init_stg_opt : Mich.data Mich.t option;
  number_of_inst : int;
}

let pre_process : string -> pre_ret
=fun filepath -> begin
  (* Parse Michelson File *)
  let adt = Adt.parse filepath |> Mich.subst_standard_macro_all_pgm |> Mich.optm_all_pgm |> Mich.fill_position_all_pgm ~update_loc:false in
  let number_of_inst = adt |> Mich.count_inst_pgm in

  (* FLAGS - Parsed Michelson File *)
  let _ : unit = (if (!Utils.Options.flag_adt_print) then (Mich.string_of_pgm_ol adt |> Stdlib.print_endline) else ()) in

  (* Parse Initial Storage *)
  let init_stg_opt = (if (!Utils.Options.initial_storage_file) = "" then None else Some (Adt.parse_data !Utils.Options.initial_storage_file)) in

  (* Construct control flow graph *)
  let cfg_first, cfgcon_counter = Translator.adt_to_cfg_counter_included (adt, None) in
  let cfg_rssov_optimized = (if (!Utils.Options.flag_cfgopt_rssov) then (CfgUtil.remove_simple_stack_operation_vertices cfg_first) else (cfg_first)) in
  let cfg_rsv_optimized = (if (!Utils.Options.flag_cfgopt_rsv) then (CfgUtil.remove_meaningless_skip_vertices_fixpoint cfg_rssov_optimized) else (cfg_rssov_optimized)) in
  let cfg_rfv_optimized = (if (!Utils.Options.flag_cfgopt_rfv) then (CfgUtil.remove_meaningless_fail_vertices_fixpoint cfg_rsv_optimized) else (cfg_rsv_optimized)) in
  let cfg_optimized = cfg_rfv_optimized in
  
  let _ : unit = (if (!Utils.Options.flag_cfg_print_dot) then print_endline (cfg_optimized |> CfgUtil.Dot.of_cfg |> CfgUtil.Dot.to_string) else ()) in

  {
    cfg=cfg_optimized;
    cfgcon_counter=cfgcon_counter;
    init_stg_opt=init_stg_opt;
    number_of_inst=number_of_inst;
  }
end