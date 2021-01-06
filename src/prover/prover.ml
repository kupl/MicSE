module Lib = ProverLib
open Lib

module Extractor = Extractor
module Converter = Converter
module Generator = Generator
module Verifier = Verifier

type timer = Utils.Timer.t ref

exception Error of string

module ProverUtil = struct

  exception Error of string

  let rec read_line_of_file : in_channel -> int -> string
  =fun file line -> begin
    if line < 0
    then Error "read_line_of_file: Wrong line input for reading." |> raise
    else match line with
          | 1 -> input_line file
          | _ -> let _ = input_line file in read_line_of_file file (line - 1)
  end

  let read_query_location : Pre.Lib.Cfg.t -> Query.t -> string
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
      | _ -> Error "read_query_location: Wrong query location information." |> raise
    with
    | Core.Not_found_s _
    | Not_found -> "?: Cannot Find Query Location"
    | e -> e |> raise
  end

  let read_param_storage : Smt.ZModel.t -> cfg:Pre.Lib.Cfg.t -> (string * string)
  =fun model ~cfg -> begin
    try
      let param_stg = Pre.Lib.Cfg.param_storage_name |>
                      Pre.Lib.Cfg.CPMap.find_exn (cfg.type_info) |>
                      Vlang.TypeUtil.ty_of_mty |>
                      Verifier.smtsort_of_vlangtyp |>
                      Smt.ZExpr.create_var ~name:Pre.Lib.Cfg.param_storage_name in
      let param_opt, stg_opt = (
        (param_stg |> Smt.ZPair.read_fst |> Smt.ZModel.eval ~model:model),
        (param_stg |> Smt.ZPair.read_snd |> Smt.ZModel.eval ~model:model)) in
      match param_opt, stg_opt with
      | Some param, Some stg -> ((param |> Smt.ZExpr.to_string), (stg |> Smt.ZExpr.to_string))
      | _, _ -> Error "read_param_storage: Wrong evaluation of parameter and storage" |> raise
    with
    | Core.Not_found_s _
    | Not_found -> Error "read_param_storage: Type of param_storage is not found" |> raise
    | e -> e |> raise
  end

  let read_post_storage : Smt.ZModel.t -> bp_list:Bp.lst -> cfg:Pre.Lib.Cfg.t -> string
  =fun model ~bp_list ~cfg -> begin
    try
      let operation_stg = bp_list.exit.var |>
                          Option.get |>
                          Pre.Lib.Cfg.CPMap.find_exn (cfg.type_info) |>
                          Vlang.TypeUtil.ty_of_mty |>
                          Verifier.smtsort_of_vlangtyp |>
                          Smt.ZExpr.create_var ~name:("operation_storage") in
      match (operation_stg |> Smt.ZPair.read_snd |> Smt.ZModel.eval ~model:model) with
      | Some stg -> stg |> Smt.ZExpr.to_string
      | _ -> Error "read_post_storage: wrong evaluation of post storage" |> raise
    with
    | Core.Not_found_s _
    | Not_found -> Error "read_post_storage: Type of param_storage is not found" |> raise
    | e -> e |> raise
  end
end

let rec work : Inv.WorkList.t -> Bp.lst -> Pre.Lib.Cfg.t -> Pre.Lib.Mich.data Pre.Lib.Mich.t option -> timer -> Query.t list
=fun w bp_list cfg init_stg_opt time -> begin
  let entry_var, exit_var = ((bp_list.entry.var |> Option.get), (bp_list.exit.var |> Option.get)) in

  (* Choose a candidate invariant *)
  let inv_map, cur_wlst = Inv.WorkList.pop w in

  (* Generate Verification Conditions *)
  let bps = Generator.apply inv_map bp_list.bps in
  let path_vcs, queries = Core.List.fold_right bps ~f:(fun bp (pvcl, ql) -> (begin
    let path_vc, queries = Converter.convert bp cfg ~entry_var:entry_var ~exit_var:exit_var in
    (path_vc::pvcl, queries@ql)
  end)) ~init:([], []) in

  (* Verify Inductiveness *)
  let inductiveness = Core.List.fold_right path_vcs ~f:(fun path_vc inductiveness -> (begin
    if Utils.Timer.is_timeout time then begin
      false
    end else begin
      let path_inductive, _ = Verifier.verify path_vc in
      inductiveness&&(path_inductive |> Smt.ZSolver.is_valid)
    end
  end)) ~init:true in

  (* Verify Queries *)
  if inductiveness then begin
    let proven, unproven = Core.List.fold_right queries ~f:(fun q (p, up) -> (begin
      if Utils.Timer.is_timeout time then begin
        let up_q = Query.update_status q (Query.Q_unproven None) in
        (p, up_q::up)
      end else begin
        let result, model = Verifier.verify q.query in
        if result |> Smt.ZSolver.is_valid
        then begin
          let p_q = Query.update_status q (Query.Q_proven) in
          (p_q::p, up)
        end else begin
          let up_q = Query.update_status q (Query.Q_unproven model) in
          (p, up_q::up)
        end
      end
    end)) ~init:([], []) in
    if Core.List.is_empty unproven
    then proven@unproven
    else begin
      let generated_wlst = Generator.W.update ~bp_list:bp_list ~cfg:cfg ~init_stg:init_stg_opt ~wlst:cur_wlst in
      let next_wlst = Generator.W.join ~inv:inv_map ~wlst:generated_wlst in
      if Utils.Timer.is_timeout time || Inv.WorkList.is_empty next_wlst then begin
        proven@unproven
      end else begin
        let next_work = work next_wlst bp_list cfg init_stg_opt time in
        if next_work = [] then proven@unproven else next_work
      end
    end
  end else begin
    if (Utils.Timer.is_timeout time) || (Inv.WorkList.is_empty cur_wlst) then begin
      []
    end else begin
      work cur_wlst bp_list cfg init_stg_opt time
    end
  end
end

let prove : Pre.Lib.Cfg.t -> Pre.Lib.Mich.data Pre.Lib.Mich.t option -> unit
=fun cfg init_stg_opt -> begin
  try
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
      let _ = print_endline ("- Query line " ^ (ProverUtil.read_query_location cfg q)) in
      match q.status with
      | Q_proven -> begin
          let _ = print_endline ("\t- Status: Proven") in
          let _ = if !Utils.Options.flag_vc_print
                  then print_endline ("\t- VC: " ^ (Vlang.Formula.to_string q.query)) in
          ()
        end
      | Q_unproven model_opt -> begin
          let _ = print_endline ("\t- Status: Unproven") in
          let _ = print_endline ("\t- Category: " ^ (Bp.string_of_category q.typ)) in
          let _ = match model_opt with
                  | None -> let _ = print_endline ("\t- Something Wrong") in ()
                  | Some model -> begin
                      if !Utils.Options.flag_param_storage
                      then begin
                        let param, stg = model |> ProverUtil.read_param_storage ~cfg:cfg in
                        let _ = print_endline ("\t- Parameter:\t" ^ param) in
                        let _ = print_endline ("\t- Storage:\t" ^ stg) in
                        ()
                      end else ()
                    end in
          let _ = if !Utils.Options.flag_vc_print
                  then print_endline ("\t- VC: " ^ (Vlang.string_of_formula q.query)) in
          ()
        end
      | Q_nonproven -> Error "prove: Non-proved query exists" |> raise
    )) in
    ()
  with
  | e -> e |> raise
end
