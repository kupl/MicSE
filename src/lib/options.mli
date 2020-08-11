(*****************************************************************************)
(*****************************************************************************)
(* Options                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

val input_file : string ref

(* FLAGS - Parsed Michelson File *)
val flag_adt_print : bool ref

(* FLAGS - Control Flow Graph *)
val flag_cfgopt_rsv : bool ref (* remove-skip-vertices *)
val flag_cfgopt_rfv : bool ref (* remove-fail-vertices *)
val flag_cfgopt_all : bool ref (* it will set all cfg-optimization *)

val set_all_cfg_opt : unit -> unit

val flag_cfg_print_dot : bool ref

val activate_detector : string -> unit

val options : (Arg.key * Arg.spec * Arg.doc) list