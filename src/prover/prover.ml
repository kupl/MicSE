module Lib = ProverLib


module Extractor = Extractor

module Converter = Converter

module Generator = Generator

module Verifier = Verifier


let rec prove : Pre.Lib.Cfg.t -> unit
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
    let _ = print_endline ("- Query line " ^ (read_query_location cfg q)) in
    if result
    then begin
      let _ = print_endline ("\t- Status: Proven") in
      ()
    end else begin
      let _ = print_endline ("\t- Status: Unproven") in
      let _ = print_endline ("\t- Category: " ^ (Lib.Bp.string_of_category q.typ)) in
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

and read_query_location : Pre.Lib.Cfg.t -> Lib.Query.t -> string
=fun cfg q -> begin
  try
    let file = open_in (!Utils.Options.input_file) in
    let entry_loc = Pre.Lib.Cfg.CPMap.find_exn cfg.pos_info q.loc.entry in
    let exit_loc = Pre.Lib.Cfg.CPMap.find_exn cfg.pos_info q.loc.exit in
    match entry_loc, exit_loc with
    | Pos (etr_pos, _), Pos (_, ext_pos) -> begin
        let line = read_line_of_file file etr_pos.lin in
        (string_of_int etr_pos.lin) ^ ": " ^ (Core.String.slice line (etr_pos.col - 1) (ext_pos.col))
      end
    | _ -> raise (Failure "")
  with
  | _ -> "?: Cannot Find Query Location"
end

and read_line_of_file : in_channel -> int -> string
=fun file line -> begin
  if line < 0 then raise (Failure "") else
  match line with
  | 1 -> input_line file
  | _ -> let _ = input_line file in read_line_of_file file (line - 1)
end