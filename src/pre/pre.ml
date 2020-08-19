module Lib = PreLib


module Analyzer = Analyzer

module Translator = Translator


let pre_process : string -> Lib.Cfg.t
=fun filepath -> begin
  (* Parse Michelson File *)
  let adt = Lib.Adt.parse filepath |> Lib.Mich.subst_standard_macro_all_pgm |> Lib.Mich.fill_position_all_pgm ~update_loc:false in

  (* FLAGS - Parsed Michelson File *)
  let _ : unit = (if (!Utils.Options.flag_adt_print) then (Lib.Mich.string_of_pgm_ol adt |> Stdlib.print_endline) else ()) in

  (* Construct control flow graph *)
  let cfg_first = Translator.adt_to_cfg adt in
  let cfg_rsv_optimized = (if (!Utils.Options.flag_cfgopt_rsv) then (Lib.Cfg.remove_meaningless_skip_vertices_fixpoint cfg_first) else (cfg_first)) in
  let cfg_rfv_optimized = (if (!Utils.Options.flag_cfgopt_rfv) then (Lib.Cfg.remove_meaningless_fail_vertices_fixpoint cfg_rsv_optimized) else (cfg_rsv_optimized)) in
  let cfg_optimized = cfg_rfv_optimized in
  
  let _ : unit = (if (!Utils.Options.flag_cfg_print_dot) then print_endline (Lib.Cfg.cfg_to_dotformat cfg_optimized) else ()) in

  cfg_optimized
end