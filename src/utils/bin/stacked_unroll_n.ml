(*  It can take same options as MicSE, but the options unrelated with (Preprocessing & Refuter) will be ignored.
    Process returns 0 if normally executed.
*)

(* Example Usages
  * If you want to make sure it's working
      dune exec -- micse.utils.stacked_unroll_n -input [PROJECT-ROOT]/benchmarks/toy/add1.tz
*)

let loop_unroll_NUM = 2

let main : unit -> unit
= let open Stacked in
  fun () -> begin
  let pgmfilecontent : PreLib.Adt.t = !Utils.Options.input_file |> PreLib.Adt.parse in
  let strgfilecontentopt : PreLib.Adt.data option = 
    if !Utils.Options.initial_storage_file = "" 
      then None 
      else Some (PreLib.Adt.parse_data !Utils.Options.initial_storage_file)
  in
  let (tz_init_stg_opt, init_ss, cache, sset) = 
    Stacked.Prove.gen_sset pgmfilecontent strgfilecontentopt
  in
  let _ = Stdlib.ignore (tz_init_stg_opt, cache) in
  let {bro_visited=_; bro_r; bro_q} : LoopUr.bake_routes_output =
    LoopUr.bake_routes {
      brp_trx_entry_mci = init_ss.ss_entry_mci;
      brp_trx_exit_mci = init_ss.ss_block_mci;
      brp_sset = sset;
    }
  in
  (* DEBUG-ROUTE START *)
  (*
  let _ = 
    print_endline "[[[ROUTES]]]";
    Tz.PMap.iteri bro_r ~f:(
      fun ~key ~data -> 
        TzCvt.T2J.cv_mich_cut_info key |> Yojson.Safe.pretty_to_string |> (fun s -> (print_endline ("[MCI]\n" ^ s ^ " : #" ^ (Tz.PSet.length data |> string_of_int))));
        Tz.PSet.iter data ~f:(fun rte -> LoopUr.R2Jomci.cv_route rte |> Yojson.Safe.pretty_to_string |> (fun s -> print_endline s; print_newline ()));
        print_newline ()
    )
  in
  *)
  (* DEBUG-ROUTE END *)
  let {unno_visited=_; unno_p; unno_q;} : LoopUr.unroll_n_naive_output =
    LoopUr.unroll_n_naive {
      unnp_num = loop_unroll_NUM;
      unnp_trx_entry_mci = init_ss.ss_entry_mci;
      unnp_r = bro_r;
      unnp_q = bro_q;
    }
  in
  let _ = Stdlib.ignore (unno_p, unno_q) in
  (* DEBUG-PATH START *)
  (*
  let _ = 
    print_endline "[[[PATHS]]]";
    Tz.PMap.iteri unno_p ~f:(
      fun ~key ~data -> 
        TzCvt.T2J.cv_mich_cut_info key |> Yojson.Safe.pretty_to_string |> (fun s -> (print_endline ("[MCI]\n" ^ s ^ " : #" ^ (Tz.PSet.length data |> string_of_int))));
        Tz.PSet.iter data ~f:(fun pth -> LoopUr.P2Jomci.cv_path pth |> Yojson.Safe.pretty_to_string |> (fun s -> print_endline s; print_newline ()));
        print_newline ()
    )
  in
  *)
  (* DEBUG-PATH END *)
  ()
end (* function main end *)

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