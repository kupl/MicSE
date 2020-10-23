(*****************************************************************************)
(*****************************************************************************)
(* Options                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

let input_file : string ref
=ref ""

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

(* FLAGS - Result of solver *)
let flag_param_storage : bool ref
=ref false (* print counter-example on unsafe-path *)

let set_all_cfg_opt : unit -> unit
=fun () -> begin
  if (not !flag_cfgopt_all) then () else (
    flag_cfgopt_rsv := true;
    flag_cfgopt_rfv := true;
    flag_cfgopt_rssov := true;
  )
end

(* FLAGS - Print components *)
let flag_adt_print : bool ref
=ref false
let flag_cfg_print_dot : bool ref
=ref false
let flag_bp_print : bool ref
=ref false
let flag_vc_print : bool ref
=ref false

let activate_detector : string -> unit
=fun s -> begin
  match s with
  | _ -> invalid_arg "invalid option"
end

let options : (Arg.key * Arg.spec * Arg.doc) list
= [
    ("-input", (Arg.String (fun s -> input_file := s)), "File path for input michelson program.");
    ("-adt_print", (Arg.Set flag_adt_print), "Print parsed Michelson file.");
    ("-cfgopt", (Arg.Set flag_cfgopt_all), "Set all cfg optimization options");
    ("-cfgopt_rsv", (Arg.Set flag_cfgopt_rsv), "Remove all trivial skip vertices in control flow graph. WARNING: It does not remove any vertex-information in Cfg");
    ("-cfgopt_rfv", (Arg.Set flag_cfgopt_rfv), "Remove all trivial fail vertices in control flow graph. WARNING: It does not remove any vertex-information in Cfg");
    ("-cfgopt_rssov", (Arg.Set flag_cfgopt_rssov), "Remove vertex with simple stack operations (Cfg_drop, Cfg_swap, Cfg_dig, Cfg_dug statements). If that vertex has one pred-edge and one succ-edge, remove that vertex. WARNING: It might overwrite vertex-information in Cfg");
    ("-cfg_print_dot", (Arg.Set flag_cfg_print_dot), "Print control flow graph in 'dot' format.");
    ("-bpopt_rsi", (Arg.Set flag_bpopt_rsi), "Remove all trivial skip instructions in bp printing.");
    ("-bp_print", (Arg.Set flag_bp_print), "Print all basic paths.");
    ("-vc_print", (Arg.Set flag_vc_print), "Print all verification conditions.");
    ("-param_storage", (Arg.Set flag_param_storage), "Print counter-example from unsafe-path");
    ("-initial_storage", (Arg.String (fun s -> initial_storage_file := s)), "File path for initial storage of input michelson program");
  ]

let create_options : unit -> unit
=fun () -> begin
  let usageMsg = "micse -input filename" in
  let _ = Arg.parse options activate_detector usageMsg in

  (* Set custom options *)
  let _ = set_all_cfg_opt () in

  ()
end