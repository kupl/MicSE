let rec main : unit -> unit
= fun () -> begin
  (* 1. Cfg Construction *)
  let pre_ret : Pre.pre_ret = Pre.pre_process (!Utils.Options.input_file) in
  (* 2. Run Prover *)
  (* The proved query-ids are stored as the side-effect of the values in Prover.Results. *)
  (* Standard-output prints specific prover results *)
  let prover_ret_opt = Prover.main pre_ret.cfg pre_ret.init_stg_opt in
  let _ = print_prover_ret pre_ret prover_ret_opt in
  (* 3. Run Refuter *)
  (* The refuted query-ids are stored as the side-effect of the value "Refuter.refuted_queries" *)
  (* Standard-output prints specific refuter results *)
  let _ = Refuter.run_multiple (pre_ret.cfg, pre_ret.cfgcon_counter) pre_ret.init_stg_opt in
  ()
end

and print_prover_ret : Pre.pre_ret -> Prover.prover_ret option -> unit
= let module CPSet = Core.Set.Poly in
  let module CPMap = Core.Map.Poly in
  let open ProverLib in
  fun pre_ret prover_ret_opt -> begin
  match prover_ret_opt with
    | None -> print_endline "Failure to create invariant that satisfies inductiveness. This log means that Z3 timeout is set too short to prove inductiveness of invariant-True."
    | Some {all_qs; proved_qs; unproved_qs} -> 
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
    then
      let _ = main () in
      ()
    else
      raise (Failure "No_Input")
  with
  | exc -> prerr_endline (Printexc.to_string exc);
end