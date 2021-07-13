(*****************************************************************************)
(*****************************************************************************)
(* Options Variables                                                         *)
(*****************************************************************************)
(*****************************************************************************)

let input_file : string ref
=ref ""

(* STRING - Initial Storage Value *)
let initial_storage_file : string ref
=ref ""

(* FLAGS - Log *)
let flag_verbose : bool ref
=ref false (* print log level info *)
let flag_debug : bool ref
=ref false (* print log level info and debug *)

(* FLAGS - Control Flow Graph *)
let flag_cfgopt_rsv : bool ref
=ref false (* remove-skip-vertices *)
let flag_cfgopt_rfv : bool ref
=ref false (* remove-fail-vertices *)
let flag_cfgopt_rssov : bool ref
=ref false (* remove-simple-stack-operation-vertices *)
let flag_cfgopt_all : bool ref
=ref false (* it will set all cfg-optimization *)

(* FLAGS - Basic Path *)
let flag_bpopt_rsi : bool ref
=ref false (* remove-skip-instructions *)
let flag_bp_print_pretty : bool ref
=ref true (* turn-on pretty-print *)

(* FLAGS - Result of solver *)
let flag_param_storage : bool ref
=ref false (* print counter-example on unsafe-path *)

(* FLAGS - Print components *)
let flag_inst_count_print : bool ref
=ref false
let flag_adt_print : bool ref
=ref false
let flag_cfg_print_dot : bool ref
=ref false
let flag_bp_print : bool ref
=ref false
let flag_vc_print : bool ref
=ref false

(* INT - Time Budgets *)
let z3_time_budget : int ref
=ref 30 (* z3 time budgets in seconds *)
let prover_time_budget : int ref
=ref 180 (* prover time budgets in seconds *)
let refuter_time_budget : int ref
=ref 180 (* refuter time budgets in seconds *)
let refuter_total_time_budget : int ref
=ref 180 (* refuter time budgets in seconds *)
let refuter_sub_time_budget : int ref
=ref 180 (* Time budget for each "Refuter.main" function call. *)
let queryid_time_budget : int ref
=ref 180 (* Time budget for each Query-Id prove or refuting *)
let total_time_budget : int ref
=ref 360 (* Time budget for total program execution. Used for special cases *)
let total_time_budget_set_flag : bool ref
=ref false (* Flag for checking total time budget is set *)


(* INT - Cfg Unrolling *)
let loop_unroll_num : int ref
=ref 1
let transaction_unroll_num : int ref
=ref 1

(* FLAGS - Refuter *)
let refuter_sub_time_budget_manually_set : bool ref
=ref false  (* If the user set the "refuter_sub_time_budget" option, then set it true. This is used to automatically calculate "refuter_sub_time_budget" if it not set manually. *)

(* FLAGS - MicSE Mode *)
let micse_baseline_mode : bool ref
= ref false (* If the user set the "baseline_mode" option, then set it true. Othercase, MicSE run on synergetic mode. *)
let micse_legacy_mode : bool ref
= ref false (* If the user set the "legacy_mode" option, then set it true. Othercase, MicSE run depend on baseline_mode flag. *)
let micse_sequential_mode : bool ref
= ref false (* If the user set the "sequential" option, then it will be set true. Othercase MicSE run on sequential mode. *)

(* INT - Query Filtering (for dev) *)
let target_query_line : int ref
= ref (-1)


(*****************************************************************************)
(*****************************************************************************)
(* Option Settings                                                           *)
(*****************************************************************************)
(*****************************************************************************)

let set_all_cfg_opt : unit -> unit
=fun () -> begin
  if (not !flag_cfgopt_all) then () else (
    flag_cfgopt_rsv := true;
    flag_cfgopt_rfv := true;
    flag_cfgopt_rssov := true;
  )
end

let set_timeout : queries:int -> unit
= (* funciton set_timeout start *)
  fun ~queries -> begin
  if !total_time_budget_set_flag then (
    if !queryid_time_budget > (!total_time_budget / queries) then (queryid_time_budget := !total_time_budget / queries)
    else ())
  else total_time_budget := !queryid_time_budget * queries * 11 / 10
end (* function set_timeout end *)

let activate_detector : string -> unit
=fun s -> begin
  match s with
  | _ -> invalid_arg ("invalid option : " ^ s)
end

let options : (Arg.key * Arg.spec * Arg.doc) list
= [
    ("-input", (Arg.String (fun s -> input_file := s)), "File path for input michelson program.");
    ("--inst-count", (Arg.Set flag_inst_count_print), "Print count of instruction in Michelson file.");
    ("--verbose", (Arg.Set flag_verbose), "Print log level info.");
    ("--debug", (Arg.Set flag_debug), "Print log level info and debug.");
    ("-adt_print", (Arg.Set flag_adt_print), "Print parsed Michelson file.");
    ("-cfgopt", (Arg.Set flag_cfgopt_all), "Set all cfg optimization options");
    ("-cfgopt_rsv", (Arg.Set flag_cfgopt_rsv), "Remove all trivial skip vertices in control flow graph. WARNING: It does not remove any vertex-information in Cfg");
    ("-cfgopt_rfv", (Arg.Set flag_cfgopt_rfv), "Remove all trivial fail vertices in control flow graph. WARNING: It does not remove any vertex-information in Cfg");
    ("-cfgopt_rssov", (Arg.Set flag_cfgopt_rssov), "Remove vertex with simple stack operations (Cfg_drop, Cfg_swap, Cfg_dig, Cfg_dug statements). If that vertex has one pred-edge and one succ-edge, remove that vertex. WARNING: It might overwrite vertex-information in Cfg");
    ("-cfg_print_dot", (Arg.Set flag_cfg_print_dot), "Print control flow graph in 'dot' format.");
    ("-bpopt_rsi", (Arg.Set flag_bpopt_rsi), "Remove all trivial skip instructions in bp printing.");
    ("-bp_print", (Arg.Set flag_bp_print), "Print all basic paths.");
    ("-bp_print_unpretty", (Arg.Clear flag_bp_print_pretty), "Do not pretty-printing for \"bp_print\" option.");
    ("-vc_print", (Arg.Set flag_vc_print), "Print all verification conditions.");
    ("-param_storage", (Arg.Set flag_param_storage), "Print counter-example from unsafe-path");
    ("-initial_storage", (Arg.String (fun s -> initial_storage_file := s)), "File path for initial storage of input michelson program");
    ("-z3_timeout", (Arg.Int (fun i -> z3_time_budget := i)), "Time budget for z3 solver in seconds. (default: 30s)");
    ("-prover_timeout", (Arg.Int (fun i -> prover_time_budget := i)), "Time budget for prover in seconds. (default: 180s)");
    ("-refuter_timeout", (Arg.Int (fun i -> refuter_time_budget := i)), "Time budget for refuter in seconds. (default: 180s)");
    ("-refuter_timeout_t", (Arg.Int (fun i -> refuter_total_time_budget := i)), "Timebudget for refuter total-time in seconds. (default: 180s)");
    ("-refuter_timeout_s", (Arg.Int (fun i -> refuter_sub_time_budget_manually_set := true; refuter_sub_time_budget := i)), "Timebudget for \"Refuter.main\" function in seconds. If not set, it'll be automatically calculated. (default: 180s)");
    ("--queryid_timeout", (Arg.Int (fun i -> queryid_time_budget := i)), "Time budget for query-id prove/refute in seconds. (default: 180s)");
    ("--total_timeout", (Arg.Int (fun i -> total_time_budget := i; total_time_budget_set_flag := true)), "Time budget for entire program execution in seconds (in special case only). (default: 360s)");
    ("-unroll_l", (Arg.Int (fun i -> loop_unroll_num := i)), "Set the number of loop unrolling. (default 1)");
    ("-unroll_t", (Arg.Int (fun i -> transaction_unroll_num := i)), "Set the maximum number of transaction scenario length to find. (default 1)");
    ("--sequential", (Arg.Set micse_sequential_mode), "Set the MicSE run as a sequential mode. (default: false)");
    ("--baseline", (Arg.Set micse_baseline_mode), "Set the MicSE run as a baseline mode. (default: false)");
    ("--legacy", (Arg.Set micse_legacy_mode), "Set the MicSE run as a legacy mode. (default: false)");
    ("--target", (Arg.Int (fun i -> target_query_line := i)), "DEV - Filter all queries if query line is not same with target line when this parameter is set.");
  ]

let create_options : unit -> unit
=fun () -> begin
  let usageMsg = "micse -input filename" in
  let _ = Arg.parse options activate_detector usageMsg in

  (* Set custom options *)
  let _ = set_all_cfg_opt () in
  
  let _ =  
    if !refuter_sub_time_budget_manually_set then () else (refuter_sub_time_budget := !refuter_total_time_budget / !transaction_unroll_num)
  in

  ()
end