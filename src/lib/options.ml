(*****************************************************************************)
(*****************************************************************************)
(* Options                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

let input_file : string ref
=ref ""

(* FLAGS - Parsed Michelson File *)
let flag_adt_print : bool ref
=ref false

(* FLAGS - Control Flow Graph *)
let flag_cfgopt_rsv : bool ref
=ref false (* remove-skip-vertices *)
let flag_cfgopt_rfv : bool ref
=ref false (* remove-fail-vertices *)
let flag_cfgopt_all : bool ref
=ref false (* it will set all cfg-optimization *)

let set_all_cfg_opt : unit -> unit
=fun () -> begin
  if (not !flag_cfgopt_all) then () else (
    flag_cfgopt_rsv := true;
    flag_cfgopt_rfv := true;
  )
end

let flag_cfg_print_dot : bool ref
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
    ("-cfg_print_dot", (Arg.Set flag_cfg_print_dot), "Print control flow graph in 'dot' format.");
  ]
