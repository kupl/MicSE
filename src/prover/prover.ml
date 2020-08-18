module Lib = ProverLib


module Extractor = Extractor

module Converter = Converter

module Generator = Generator

module Verifier = Verifier


let prove : Pre.Lib.Cfg.t -> unit
=fun cfg -> begin
  (* Construct basic path *)
  let raw_bp_list = Extractor.extract cfg in
  let bp_list = Generator.generate raw_bp_list cfg in

  (* Collect all queries from basic path *)
  let queries = Core.List.fold_right bp_list ~f:(fun bp qs -> (
    let _, queries = Converter.convert bp cfg in
    queries@qs
  )) ~init:[] in

  (* Verify each basic path *)
  let _ = Core.List.iter queries ~f:(fun q -> (
    let result, _ = Verifier.verify q.query cfg in
    if result
    then begin
      print_endline ("- proven query   [" ^ (Lib.Bp.string_of_category q.typ) ^ "] \t(" ^ (string_of_int q.loc.entry) ^ ":" ^ (string_of_int q.loc.exit) ^ ")")
    end else begin
      print_endline ("- unproven query [" ^ (Lib.Bp.string_of_category q.typ) ^ "] \t(" ^ (string_of_int q.loc.entry) ^ ":" ^ (string_of_int q.loc.exit) ^ ")")
    end
  )) in
  ()
end