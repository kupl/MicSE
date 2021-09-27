(* Argument Parser *)

open! Core

(******************************************************************************)
(******************************************************************************)
(* Setting                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Setting = struct
  type 'a t = {
    value : 'a ref;
    arg_lst : (Arg.key * Arg.spec * Arg.doc) list;
  }

  type require = bool ref

  let required_set : require Core.Set.Poly.t ref = ref Core.Set.Poly.empty

  let init_required : unit -> unit =
    (fun () -> Core.Set.Poly.iter !required_set ~f:(fun rf -> rf := false))

  let add_required : unit -> require =
    fun () ->
    let rf : require = ref false in
    let _ = required_set := Core.Set.Poly.add !required_set rf in
    rf

  let check_required : unit -> bool =
    (fun () -> Core.Set.for_all !required_set ~f:( ! ))

  let create_arg_lst :
      Arg.key list -> Arg.spec -> Arg.doc -> (Arg.key * Arg.spec * Arg.doc) list
      =
    fun key_lst spec doc ->
    Core.List.map key_lst ~f:(fun key -> (key, spec, doc))
  (* function create_arg_lst end *)

  (****************************************************************************)
  (* MicSE Behaviors                                                          *)
  (****************************************************************************)

  let input_file : string t =
     let (required : require) = add_required () in
     let (value : string ref) = ref "" in
     let (spec : Arg.spec) =
        Arg.String
          (fun s ->
          required := true;
          value := s)
     in
     let (doc : Arg.doc) = "file path for input michelson program (REQUIRED)" in
     { value; arg_lst = create_arg_lst [ "--input"; "-I" ] spec doc }

  let input_storage_file : string t =
     let (required : require) = add_required () in
     let (value : string ref) = ref "" in
     let (spec : Arg.spec) =
        Arg.String
          (fun s ->
          required := true;
          value := s)
     in
     let (doc : Arg.doc) =
        "file path for input initial storage information (REQUIRED)"
     in
     { value; arg_lst = create_arg_lst [ "--initial-storage"; "-S" ] spec doc }

  let memory_bound : int t =
     let (value : int ref) = ref 5 in
     let (spec : Arg.spec) = Arg.Set_int value in
     let (doc : Arg.doc) =
        "memory budget for entire MicSE in GB (default: 5)"
     in
     { value; arg_lst = create_arg_lst [ "--memory-bound"; "-M" ] spec doc }

  let total_timeout : int t =
     let (value : int ref) = ref 360 in
     let (spec : Arg.spec) = Arg.Set_int value in
     let (doc : Arg.doc) =
        "time budget for entire MicSE execution in seconds. (default: 360)"
     in
     { value; arg_lst = create_arg_lst [ "--total-timeout"; "-T" ] spec doc }

  let z3_timeout : int t =
     let (value : int ref) = ref 30 in
     let (spec : Arg.spec) = Arg.Set_int value in
     let (doc : Arg.doc) =
        "time budget for Z3 solver in seconds (default: 30)"
     in
     { value; arg_lst = create_arg_lst [ "--z3-timeout"; "-Z" ] spec doc }

  (****************************************************************************)
  (* Dev Mode                                                                 *)
  (****************************************************************************)

  let debug_mode : bool t =
     let (value : bool ref) = ref false in
     let (spec : Arg.spec) = Arg.Set value in
     let (doc : Arg.doc) = "print log over debug level" in
     { value; arg_lst = create_arg_lst [ "--debug"; "-d" ] spec doc }

  let inst_count : bool t =
     let (value : bool ref) = ref false in
     let (spec : Arg.spec) = Arg.Set value in
     let (doc : Arg.doc) =
        "print count of instructions in input Michelson file"
     in
     { value; arg_lst = create_arg_lst [ "--inst-count" ] spec doc }

  let verbose_mode : bool t =
     let (value : bool ref) = ref false in
     let (spec : Arg.spec) = Arg.Set value in
     let (doc : Arg.doc) = "print log over info level" in
     { value; arg_lst = create_arg_lst [ "--verbose"; "-v" ] spec doc }

  (****************************************************************************)
  (* Arguments Settings                                                       *)
  (****************************************************************************)

  let usage_msg : string = "micse --input [filepath] [options]"

  let option_list : (Arg.key * Arg.spec * Arg.doc) list =
     input_file.arg_lst
     @ input_storage_file.arg_lst
     @ memory_bound.arg_lst
     @ total_timeout.arg_lst
     @ z3_timeout.arg_lst
     @ debug_mode.arg_lst
     @ inst_count.arg_lst
     @ verbose_mode.arg_lst

  let anon_fun : string -> unit =
    (fun _ -> raise (Arg.Help "wrong anonymous argument(s)"))

  let finalize_parse : unit -> unit =
    fun () ->
    if (* 1. check required arguments *)
       not (check_required ())
    then raise (Arg.Bad "all required argument are not set")
    else ()
end

(******************************************************************************)
(******************************************************************************)
(* Functions                                                                  *)
(******************************************************************************)
(******************************************************************************)

let create : string array option -> unit =
  fun argv_opt ->
  let _ = Setting.init_required () in
  let _ =
     match argv_opt with
     | None      ->
       (* Normal Execution Case *)
       Arg.parse Setting.option_list Setting.anon_fun Setting.usage_msg
     | Some argv ->
       (* If argument is given from string, not command line argument *)
       Arg.parse_argv ~current:(ref (-1)) argv Setting.option_list
         Setting.anon_fun Setting.usage_msg
  in
  let _ = Setting.finalize_parse () in
  ()

let input_file : string ref = Setting.input_file.value

let input_storage_file : string ref = Setting.input_storage_file.value

let memory_bound : int ref = Setting.memory_bound.value

let total_timeout : int ref = Setting.total_timeout.value

let z3_timeout : int ref = Setting.z3_timeout.value

let debug_mode : bool ref = Setting.debug_mode.value

let inst_count : bool ref = Setting.inst_count.value

let verbose_mode : bool ref = Setting.verbose_mode.value
