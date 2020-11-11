module Lib = ProverLib
open Lib

module Extractor = Extractor

module Converter = Converter

module Generator = Generator

module Verifier = Verifier


type timer = Utils.Timer.t ref


val prove : Pre.Lib.Cfg.t -> Pre.Lib.Mich.data Pre.Lib.Mich.t option -> unit

val work : Inv.WorkList.t -> Bp.lst -> Pre.Lib.Cfg.t -> Pre.Lib.Mich.data Pre.Lib.Mich.t option -> timer -> Query.t list

val read_query_location : Pre.Lib.Cfg.t -> Query.t -> string

val read_line_of_file : in_channel -> int -> string