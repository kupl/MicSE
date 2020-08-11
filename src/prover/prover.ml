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

  (* Verify each basic path *)
  let _ = Core.List.iter bp_list ~f:(fun bp -> (
    let vlang_vc = Converter.convert bp cfg in
    let verify_result, param_storage_opt = Verifier.verify vlang_vc cfg in
    print_endline (
      if verify_result
      then "- Safe path"
      else begin
        match param_storage_opt with
        | None -> "Something Wrong"
        | Some (param, storage) -> begin
            "- Unsafe path" ^ (
              if !Utils.Options.flag_param_storage
              then "\n" ^
                  "  - Parameter: " ^ (Lib.Smt.string_of_expr param) ^ "\n" ^
                  "  - Storage:   " ^ (Lib.Smt.string_of_expr storage)
              else ""
            )
          end
      end
    )
  )) in
  ()
end