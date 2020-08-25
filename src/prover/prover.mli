module Lib = ProverLib


module Extractor = Extractor

module Converter = Converter

module Generator = Generator

module Verifier = Verifier


val prove : Pre.Lib.Cfg.t -> unit

val work : Lib.Inv.WorkList.t -> Lib.Bp.raw_t_list -> Pre.Lib.Cfg.t -> Lib.Query.t list

val read_query_location : Pre.Lib.Cfg.t -> Lib.Query.t -> string

val read_line_of_file : in_channel -> int -> string