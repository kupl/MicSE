(*****************************************************************************)
(*****************************************************************************)
(* Options Variables                                                         *)
(*****************************************************************************)
(*****************************************************************************)

(* String - Input *)
let input_file : string ref
=ref ""

(* STRING - Output *)
let json_output_file : string ref
=ref ""
let json_output_flag : bool ref
=ref false

(* STRING - Initial Storage Value *)
let initial_storage_file : string ref
=ref ""

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
let refuter_total_time_budget : int ref
=ref 180 (* refuter time budgets in seconds *)
let refuter_sub_time_budget : int ref
=ref 180 (* Time budget for each "Refuter.main" function call. *)

(* INT - Cfg Unrolling *)
let loop_unroll_num : int ref
=ref 1
let transaction_unroll_num : int ref
=ref 1

(* FLAGS - Refuter *)
let refuter_sub_time_budget_manually_set : bool ref
=ref false  (* If the user set the "refuter_sub_time_budget" option, then set it true. This is used to automatically calculate "refuter_sub_time_budget" if it not set manually. *)

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

let activate_detector : string -> unit
=fun s -> begin
  match s with
  | _ -> invalid_arg "invalid option"
end

let options : (Arg.key * Arg.spec * Arg.doc) list
= [
    ("--input", (Arg.String (fun s -> input_file := s)), "File path for input michelson program.");
    ("--json-out", (Arg.String (fun s -> (json_output_flag := true; json_output_file := s))), "File path for output json file.");
    ("--inst-count", (Arg.Set flag_inst_count_print), "Print count of instruction in Michelson file.");
    ("--adt-print", (Arg.Set flag_adt_print), "Print parsed Michelson file.");
    ("--cfgopt", (Arg.Set flag_cfgopt_all), "Set all cfg optimization options");
    ("--cfgopt-rsv", (Arg.Set flag_cfgopt_rsv), "Remove all trivial skip vertices in control flow graph. WARNING: It does not remove any vertex-information in Cfg");
    ("--cfgopt-rfv", (Arg.Set flag_cfgopt_rfv), "Remove all trivial fail vertices in control flow graph. WARNING: It does not remove any vertex-information in Cfg");
    ("--cfgopt-rssov", (Arg.Set flag_cfgopt_rssov), "Remove vertex with simple stack operations (Cfg_drop, Cfg_swap, Cfg_dig, Cfg_dug statements). If that vertex has one pred-edge and one succ-edge, remove that vertex. WARNING: It might overwrite vertex-information in Cfg");
    ("--cfg-print-dot", (Arg.Set flag_cfg_print_dot), "Print control flow graph in 'dot' format.");
    ("--bpopt-rsi", (Arg.Set flag_bpopt_rsi), "Remove all trivial skip instructions in bp printing.");
    ("--bp-print", (Arg.Set flag_bp_print), "Print all basic paths.");
    ("--bp-print-unpretty", (Arg.Clear flag_bp_print_pretty), "Do not pretty-printing for \"bp_print\" option.");
    ("--vc-print", (Arg.Set flag_vc_print), "Print all verification conditions.");
    ("--param-storage", (Arg.Set flag_param_storage), "Print counter-example from unsafe-path");
    ("--initial-storage", (Arg.String (fun s -> initial_storage_file := s)), "File path for initial storage of input michelson program");
    ("--z3-timeout", (Arg.Int (fun i -> z3_time_budget := i)), "Time budget for z3 solver in seconds. (default: 30s)");
    ("--prover-timeout", (Arg.Int (fun i -> prover_time_budget := i)), "Time budget for prover in seconds. (default: 180s)");
    ("--refuter-timeout-t", (Arg.Int (fun i -> refuter_total_time_budget := i)), "Timebudget for refuter total-time in seconds. (default: 180s)");
    ("--refuter-timeout-s", (Arg.Int (fun i -> refuter_sub_time_budget_manually_set := true; refuter_sub_time_budget := i)), "Timebudget for \"Refuter.main\" function in seconds. If not set, it'll be automatically calculated. (default: 180s)");
    ("--unroll-l", (Arg.Int (fun i -> loop_unroll_num := i)), "Set the number of loop unrolling. (default 1)");
    ("--unroll-t", (Arg.Int (fun i -> transaction_unroll_num := i)), "Set the maximum number of transaction scenario length to find. (default 1)");
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