(* It only takes refuter functions for the given Michelson code.
    It can take same options as MicSE, but the options unrelated with (Preprocessing & Refuter) will be ignored.
    Process returns 0 if normally executed.
*)

(* Example Usages
  * If you want to make sure it's working
      dune exec -- micse.utils.refuter_only -input [PROJECT-ROOT]/benchmarks/toy/add1.tz
*)

let unroll_NUM = 2
let transaction_seq_NUM = 2

let main : unit -> unit
= let open PreLib in
  fun () -> begin
  (* 1. Cfg Construction *)
  let ((cfg, cfgcon_counter), init_stg_opt) : (Cfg.t * Cfg.cfgcon_ctr) * (Adt.data option) = Pre.pre_process (!Utils.Options.input_file) in
  (* 2. Run Refuter *)
  let _ = Refuter.main (cfg, cfgcon_counter) init_stg_opt transaction_seq_NUM unroll_NUM in
  ()
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