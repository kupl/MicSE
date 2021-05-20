(*****************************************************************************)
(*****************************************************************************)
(* Options Variables                                                         *)
(*****************************************************************************)
(*****************************************************************************)

val input_file : string ref

(* STRING - Initial Storage Value *)
val initial_storage_file : string ref

(* FLAGS - Log *)
val flag_verbose : bool ref (* print log level info *)
val flag_debug : bool ref (* print log level info and debug *)

(* FLAGS - Control Flow Graph *)
val flag_cfgopt_rsv : bool ref (* remove-skip-vertices *)
val flag_cfgopt_rfv : bool ref (* remove-fail-vertices *)
val flag_cfgopt_rssov : bool ref (* remove-simple-stack-operation-vertices *)
val flag_cfgopt_all : bool ref (* it will set all cfg-optimization *)

(* FLAGS - Basic Path *)
val flag_bpopt_rsi : bool ref (* remove-skip-instructions *)
val flag_bp_print_pretty : bool ref (* turn on pretty-print *)

(* FLAGS - Result of solver *)
val flag_param_storage : bool ref (* print counter-example on unsafe-path *)

(* FLAGS - Print components *)
val flag_adt_print : bool ref
val flag_cfg_print_dot : bool ref
val flag_bp_print : bool ref
val flag_vc_print : bool ref

(* INT - Time Budgets *)
val z3_time_budget : int ref
val prover_time_budget : int ref
val refuter_total_time_budget : int ref
val refuter_sub_time_budget : int ref
val queryid_time_budget : int ref
val total_time_budget : int ref

(* INT - Cfg Unrolling *)
val loop_unroll_num : int ref
val transaction_unroll_num : int ref

(* FLAGS - Refuter *)
val refuter_sub_time_budget_manually_set : bool ref

(*****************************************************************************)
(*****************************************************************************)
(* Option Settings                                                           *)
(*****************************************************************************)
(*****************************************************************************)

val set_all_cfg_opt : unit -> unit

val activate_detector : string -> unit

val options : (Arg.key * Arg.spec * Arg.doc) list

val create_options : unit -> unit