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
    let result, param_storage_opt = Verifier.verify q.query cfg in
    if result
    then begin
      let _ = print_endline ("- Proven query\t\t[" ^ (Lib.Bp.string_of_category q.typ) ^ "] \t(" ^ (string_of_int q.loc.entry) ^ ":" ^ (string_of_int q.loc.exit) ^ ")") in
      ()
    end else begin
      let _ = print_endline ("- Unproven query\t[" ^ (Lib.Bp.string_of_category q.typ) ^ "] \t(" ^ (string_of_int q.loc.entry) ^ ":" ^ (string_of_int q.loc.exit) ^ ")") in
      match param_storage_opt with
      | None -> let _ = print_endline ("\t- Something Wrong") in ()
      | Some (param, storage) -> begin
          if !Utils.Options.flag_param_storage
          then begin
            let _ = print_endline ("\t- Parameter:\t" ^ (Lib.Smt.string_of_expr param)) in
            let _ = print_endline ("\t- Storage:\t" ^ (Lib.Smt.string_of_expr storage)) in
            ()
          end else ()
        end
    end
  )) in
  ()
end