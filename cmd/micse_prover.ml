
module JsonRep = struct
  exception ParseErr of Yojson.Basic.t

  let of_inv : PreLib.Cfg.t -> ProverLib.Inv.t -> Yojson.Basic.t
  = let module CPMap = Core.Map.Poly in
    let open PreLib in
    let open ProverLib in
    fun cfg inv -> begin
    `Assoc [
      ("transaction", `String (inv.trx_inv |> Inv.inv_to_formula |> VlangUtil.NaiveOpt.run |> Vlang.Formula.to_string));
      ("loop", `List (inv.loop_inv
                      |> CPMap.to_alist ~key_order:`Increasing
                      |> Core.List.map ~f:(fun (vtx, f) -> (
                        `Assoc [
                          ("vertex", `Int (vtx));
                          ("position", `String (vtx
                                                |> Cfg.t_map_find ~errtrace:("Prover.main : result-pq : " ^ Stdlib.string_of_int vtx) cfg.pos_info
                                                |> Mich.string_of_loc));
                          ("formula", `String (f |> Inv.inv_to_formula |> VlangUtil.NaiveOpt.run |> Vlang.Formula.to_string))]))));]
  end

  let of_queries : PreLib.Cfg.t -> (ProverLib.Inv.t option * ProverLib.Bp.query_category * PreLib.Cfg.vertex) list -> Yojson.Basic.t
  = let module CPSet = Core.Set.Poly in
    let open PreLib in
    let open ProverLib in
    fun cfg queries -> begin
    let i = ref 0 in
    `List (queries |>
           Core.List.map ~f:(fun (inv, category, vtx) -> (
            let content = ("id", `Int (Stdlib.incr i; !i))::
                          ("state", `String (if (Option.is_some inv) then "Proven" else "Unproven"))::
                          ("vertex", `Int (vtx))::
                          ("position", `String (vtx
                                                |> Cfg.t_map_find ~errtrace:("Prover.main : result-pq : " ^ Stdlib.string_of_int vtx) cfg.pos_info
                                                |> Mich.string_of_loc))::
                          ("category", (category |> Bp.JsonRep.of_query_category))::[] in
            let content = if (Option.is_some inv) then content@[("invariant", (inv |> Option.get |> of_inv cfg))] else content in
            `Assoc content)))
  end

  let of_prover_ret : PreLib.Cfg.t -> Prover.run_ret -> Yojson.Basic.t
  = let module CPSet = Core.Set.Poly in
    fun cfg prover_ret -> begin
    `Assoc [
      ("total", `Int (CPSet.length prover_ret.all_qs));
      ("proved", `Int (CPSet.length prover_ret.proved_qs));
      ("unproved", `Int (CPSet.length prover_ret.unproved_qs));
      ("queries", (
        ((prover_ret.proved_qs |> CPSet.to_list |> Core.List.map ~f:(fun (inv, (category, vtx)) -> ((Some inv), category, vtx)))
          @(prover_ret.unproved_qs |> CPSet.to_list |> Core.List.map ~f:(fun (category, vtx) -> (None, category, vtx)))) 
        |> of_queries cfg));]
  end

  let of_result : Pre.pre_ret -> Prover.run_ret -> Yojson.Basic.t
  = fun pre_ret prover_ret -> begin
    `Assoc [
      ("file", `String (!Utils.Options.input_file));
      ("instructions", `Int (pre_ret.number_of_inst));
      ("result", `Assoc ([
        ("prover", (prover_ret |> of_prover_ret pre_ret.cfg))]));
      ("cfg", `Assoc (
        let g = pre_ret.cfg |> PreLib.CfgUtil.Dot.of_cfg in
        [("edge", `List (g.flow |> Core.List.map ~f:(fun s -> `String s)));
         ("vertex", `List (g.vi |> Core.List.map ~f:(fun s -> `String s)))]));]
  end
end

let main : unit -> unit
= let module CPSet = Core.Set.Poly in
  let module CPMap = Core.Map.Poly in
  let open ProverLib in
  fun () -> begin
    (* 1. Cfg Construction *)
    let pre_ret : Pre.pre_ret = Pre.pre_process (!Utils.Options.input_file) in
    (* 2. Run Prover *)
    (* The proved query-ids are stored as the side-effect of the values in Prover.Results. *)
    (* Standard-output prints specific prover results *)
    let prover_ret_opt = Prover.main pre_ret.cfg pre_ret.init_stg_opt in
    match prover_ret_opt with
    | None -> print_endline "Failure to create invariant that satisfies inductiveness. This log means that Z3 timeout is set too short to prove inductiveness of invariant-True."
    | Some {all_qs; proved_qs; unproved_qs} -> 
      if !Utils.Options.json_output_flag then (
        let out_channel = open_out !Utils.Options.json_output_file in
        Yojson.Basic.pretty_to_channel out_channel (JsonRep.of_result pre_ret {all_qs; proved_qs; unproved_qs}));
      print_endline ("# of Instructions : " ^ (Stdlib.string_of_int (pre_ret.number_of_inst)));
      print_endline ("# of Total Queries : " ^ (Stdlib.string_of_int (CPSet.length all_qs)));
      print_endline ("# of Proved Queries : " ^ (Stdlib.string_of_int (CPSet.length proved_qs)));
      print_endline ("# of Unproved Queries : " ^ (Stdlib.string_of_int (CPSet.length unproved_qs)));
      print_endline ("================ Proved Queries ::");
      let i = ref 0 in
      CPSet.iter proved_qs ~f:(fun (inv, (category, vtxnum)) -> (
        Stdlib.incr i;
        print_endline ("======== Proved Query #" ^ (Stdlib.string_of_int !i)
                        ^ ", vtx=" ^ (Stdlib.string_of_int vtxnum)
                        ^ ", pos=" ^ (PreLib.Cfg.t_map_find ~errtrace:("Prover.main : result-pq : " ^ Stdlib.string_of_int vtxnum) pre_ret.cfg.pos_info vtxnum |> PreLib.Mich.string_of_loc) 
                        ^ ", category=" ^ (ProverLib.Bp.JsonRep.of_query_category category |> Yojson.Basic.to_string));
        print_endline ("==== Transaction Invariant (printed in optimized form):");
        print_endline (Vlang.Formula.to_string (VlangUtil.NaiveOpt.run (Inv.inv_to_formula inv.trx_inv)));
        print_endline "==== Loop Invariant (printed in optimized form):";
        CPMap.iteri 
          inv.loop_inv 
          ~f:(fun ~key ~data -> (
              print_endline (
                "vtx=" ^ (Stdlib.string_of_int key)
                ^ ", pos=" ^ (PreLib.Cfg.t_map_find ~errtrace:("Prover.main : result-pq-li : " ^ Stdlib.string_of_int key) pre_ret.cfg.pos_info key |> PreLib.Mich.string_of_loc) 
                ^ " : " ^ (Vlang.Formula.to_string (VlangUtil.NaiveOpt.run (Inv.inv_to_formula data))))));));
      print_endline ("================ Unproved Queries ::");
      let i = ref 0 in
      CPSet.iter unproved_qs ~f:(fun (category, vtxnum) -> (
        Stdlib.incr i;
        print_endline ("======== Unproved Query #" ^ (Stdlib.string_of_int !i) 
                        ^ ", vtx=" ^ (Stdlib.string_of_int vtxnum) 
                        ^ ", pos=" ^ (PreLib.Cfg.t_map_find ~errtrace:("Prover.main : result-uq : " ^ Stdlib.string_of_int vtxnum) pre_ret.cfg.pos_info vtxnum |> PreLib.Mich.string_of_loc)
                        ^ ", category=" ^ (ProverLib.Bp.JsonRep.of_query_category category |> Yojson.Basic.to_string))););
end

let _ = begin
  let _ = Utils.Options.create_options () in
  let _ = Printexc.record_backtrace true in
  try
    if !Utils.Options.input_file <> ""
    then let _ = main () in ()
    else raise (Failure "No_Input")
  with
  | exc -> prerr_endline (Printexc.to_string exc);
end