(*****************************************************************************)
(*****************************************************************************)
(* Options Variables                                                         *)
(*****************************************************************************)
(*****************************************************************************)

val input_file : string ref

(* STRING - Initial Storage Value *)
val initial_storage_file : string ref

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

(*****************************************************************************)
(*****************************************************************************)
(* Option Settings                                                           *)
(*****************************************************************************)
(*****************************************************************************)

val set_all_cfg_opt : unit -> unit

val activate_detector : string -> unit

val options : (Arg.key * Arg.spec * Arg.doc) list

val create_options : unit -> unit