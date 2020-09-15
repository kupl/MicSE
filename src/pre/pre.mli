module Lib = PreLib
open Lib

module Analyzer = Analyzer

module Translator = Translator


val pre_process : string -> (Cfg.t * (Mich.data Mich.t option))