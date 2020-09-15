module Lib = ProverLib
open Lib

module Extractor = Extractor

module Converter = Converter

module Generator = Generator

module Verifier = Verifier


let rec prove : Pre.Lib.Cfg.t -> Pre.Lib.Mich.data Pre.Lib.Mich.t option -> unit
=fun cfg init_stg_opt -> begin
  (* Construct basic path *)
  let raw_bp_list = Extractor.extract cfg in

  (* Verify all of basic path *)
  let initial_worklist = Generator.initial_inv_worklist raw_bp_list.trx_inv_vtx raw_bp_list.loop_inv_vtx in
  let queries = work initial_worklist raw_bp_list cfg init_stg_opt in

  (* Print out result *)
  let _ = Core.List.iter queries ~f:(fun q -> (
    let _ = print_endline ("- Query line " ^ (read_query_location cfg q)) in
    match q.status with
    | Q_proven -> begin
        let _ = print_endline ("\t- Status: Proven") in
        ()
      end
    | Q_unproven param_storage_opt -> begin
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
    | Q_nonproven -> raise (Failure "Prover.prove: Non-proved query exists")
  )) in
  ()
end

and work : Inv.WorkList.t -> Bp.raw_t_list -> Pre.Lib.Cfg.t -> Pre.Lib.Mich.data Pre.Lib.Mich.t option -> Query.t list
=fun w raw_bps cfg init_stg_opt -> begin
  let _ = init_stg_opt in
  (* Choose a candidate invariant *)
  let inv_map, _ = Inv.WorkList.pop w in

  (* Verify Queries *)
  let bp_list = Generator.apply inv_map raw_bps.bps in
  let _, proven, unproven = Core.List.fold_right bp_list ~f:(fun bp (inductive, proven, unproven) -> (
    let path_vc, queries = Converter.convert bp cfg in
    let path_inductive, _ = Verifier.verify path_vc cfg in
    let proven, unproven = Core.List.fold_right queries ~f:(fun q (p, up) -> (
      let result, param_storage_opt = Verifier.verify q.query cfg in
      if result
      then begin
        let p_q = Query.update_status q (Lib.Query.create_status_proven) in
        (p_q::p, up)
      end else begin
        let up_q = Query.update_status q (Lib.Query.create_status_unproven param_storage_opt) in
        (p, up_q::up)
      end
    )) ~init:(proven, unproven) in
    (inductive&&path_inductive, proven, unproven)
  )) ~init:(true, [], []) in
  proven@unproven
end

and read_query_location : Pre.Lib.Cfg.t -> Query.t -> string
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