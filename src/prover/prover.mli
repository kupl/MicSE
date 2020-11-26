module Lib = ProverLib
module Extractor = Extractor
module Converter = Converter
module Generator = Generator
module Verifier = Verifier

type timer = Utils.Timer.t ref

module ProverUtil : sig
  val read_line_of_file : in_channel -> int -> string
  val read_query_location : Pre.Lib.Cfg.t -> ProverLib.Query.t -> string
  val read_param_storage : ProverLib.Smt.ZModel.t -> cfg:Pre.Lib.Cfg.t -> (string * string)
  val read_post_storage : ProverLib.Smt.ZModel.t -> bp_list:ProverLib.Bp.lst -> cfg:Pre.Lib.Cfg.t -> string
end

val work : ProverLib.Inv.WorkList.t -> ProverLib.Bp.lst -> Pre.Lib.Cfg.t -> Pre.Lib.Mich.data Pre.Lib.Mich.t option -> timer -> ProverLib.Query.t list
val prove : Pre.Lib.Cfg.t -> Pre.Lib.Mich.data Pre.Lib.Mich.t option -> unit