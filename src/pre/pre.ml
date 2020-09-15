module Lib = PreLib
open Lib

module Analyzer = Analyzer

module Translator = Translator

let pre_process : string -> (Cfg.t * (Mich.data Mich.t option))
=fun filepath -> begin
  (* Parse Michelson File *)
  let adt = Adt.parse filepath |> Mich.subst_standard_macro_all_pgm |> Mich.optm_all_pgm |> Mich.fill_position_all_pgm ~update_loc:false in

  (* FLAGS - Parsed Michelson File *)
  let _ : unit = (if (!Utils.Options.flag_adt_print) then (Mich.string_of_pgm_ol adt |> Stdlib.print_endline) else ()) in

  (* Parse Initial Storage *)
  let init_stg_opt = (if (!Utils.Options.initial_storage_file) = "" then None else Some (Adt.parse_data !Utils.Options.initial_storage_file)) in

  (* Construct control flow graph *)
  let cfg_first = Translator.adt_to_cfg adt in
  let cfg_rsv_optimized = (if (!Utils.Options.flag_cfgopt_rsv) then (CfgUtil.remove_meaningless_skip_vertices_fixpoint cfg_first) else (cfg_first)) in
  let cfg_rfv_optimized = (if (!Utils.Options.flag_cfgopt_rfv) then (CfgUtil.remove_meaningless_fail_vertices_fixpoint cfg_rsv_optimized) else (cfg_rsv_optimized)) in
  let cfg_optimized = cfg_rfv_optimized in
  
  let _ : unit = (if (!Utils.Options.flag_cfg_print_dot) then print_endline (CfgUtil.cfg_to_dotformat cfg_optimized) else ()) in

  (cfg_optimized, init_stg_opt)
end