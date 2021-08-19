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

val pre_process : string -> pre_ret