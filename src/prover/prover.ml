module Lib = ProverLib
open Lib

module Extractor = Extractor

module Converter = Converter

module Generator = Generator

module Verifier = Verifier


type timer = Utils.Timer.t ref


let rec prove : Pre.Lib.Cfg.t -> Pre.Lib.Mich.data Pre.Lib.Mich.t option -> unit
=fun cfg init_stg_opt -> begin
  (* Set the Z3 context *)
  let _ = Smt.Ctx.create () in

  (* Construct basic path *)
  let bp_list = Extractor.extract cfg in
  let _ = if !Utils.Options.flag_bp_print
          then print_endline (":: Basic Paths" ^ 
                              (Core.List.foldi bp_list.bps ~init:"" ~f:(fun idx str bp -> (
                                str ^ "\nBasic Path #" ^ (string_of_int idx) ^ "\n" ^ (Bp.to_string bp))
                              ))) in

  (* Verify all of basic path *)
  let initial_worklist = Generator.W.create ~bp_list:bp_list in
  let time = Utils.Timer.create ~budget:(!Utils.Options.prover_time_budget) in
  let raw_queries = work initial_worklist bp_list cfg init_stg_opt time in

  (* Print out result *)
  let queries = Core.List.sort (Core.List.fold_right raw_queries ~f:(fun raw_q qs -> ( (* only when proven@unproven *)
    if Core.List.mem qs raw_q ~equal:(fun q raw_q -> (Bp.compare_loc q.loc raw_q.loc) = 0)
    then qs
    else raw_q::qs
  )) ~init:[]) ~compare:(fun q1 q2 -> Bp.compare_loc q1.loc q2.loc) in
  let _ = Core.List.iter queries ~f:(fun q -> (
    let _ = print_endline ("- Query line " ^ (read_query_location cfg q)) in
    match q.status with
    | Q_proven -> begin
        let _ = print_endline ("\t- Status: Proven") in
        let _ = if !Utils.Options.flag_vc_print
                then print_endline ("\t- VC: " ^ (Vlang.string_of_formula q.query)) in
        ()
      end
    | Q_unproven param_storage_opt -> begin
        let _ = print_endline ("\t- Status: Unproven") in
        let _ = print_endline ("\t- Category: " ^ (Bp.string_of_category q.typ)) in
        let _ = match param_storage_opt with
                | None -> let _ = print_endline ("\t- Something Wrong") in ()
                | Some (param, storage) -> begin
                    if !Utils.Options.flag_param_storage
                    then begin
                      let _ = print_endline ("\t- Parameter:\t" ^ (Smt.string_of_expr param)) in
                      let _ = print_endline ("\t- Storage:\t" ^ (Smt.string_of_expr storage)) in
                      ()
                    end else ()
                  end in
        let _ = if !Utils.Options.flag_vc_print
                then print_endline ("\t- VC: " ^ (Vlang.string_of_formula q.query)) in
        ()
      end
    | Q_nonproven -> raise (Failure "Prover.prove: Non-proved query exists")
  )) in
  ()
end

and work : Inv.WorkList.t -> Bp.lst -> Pre.Lib.Cfg.t -> Pre.Lib.Mich.data Pre.Lib.Mich.t option -> timer -> Query.t list
=fun w bp_list cfg init_stg_opt time -> begin
  (* Choose a candidate invariant *)
  let inv_map, cur_wlst = Inv.WorkList.pop w in

  (* Verify Queries *)
  let bps = Generator.apply inv_map bp_list.bps in
  let inductiveness, proven, unproven = Core.List.fold_right bps ~f:(fun bp (inductive, proven, unproven) -> (
    let path_vc, queries = Converter.convert bp cfg in
    let path_inductive, _ = Verifier.verify path_vc cfg in
    let proven, unproven = Core.List.fold_right queries ~f:(fun q (p, up) -> (
      let result, param_storage_opt = Verifier.verify q.query cfg in
      if result
      then begin
        let p_q = Query.update_status q (Query.create_status_proven) in
        (p_q::p, up)
      end else begin
        let up_q = Query.update_status q (Query.create_status_unproven param_storage_opt) in
        (p, up_q::up)
      end
    )) ~init:(proven, unproven) in
    (inductive&&path_inductive, proven, unproven)
  )) ~init:(true, [], []) in
  if Core.List.is_empty unproven
  then proven
  else begin
    (* Generate invariants *)
    let generated_wlst = Generator.W.update ~bp_list:bp_list ~cfg:cfg ~init_stg:init_stg_opt ~wlst:cur_wlst in
    let next_wlst = if inductiveness then (Generator.W.join ~inv:inv_map ~wlst:generated_wlst) else generated_wlst in
    if (Inv.WorkList.is_empty next_wlst) || (Utils.Timer.is_timeout time)
    then proven@unproven
    else work next_wlst bp_list cfg init_stg_opt time
  end
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