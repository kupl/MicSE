(* Argument Parser *)

(******************************************************************************)
(******************************************************************************)
(* Setting                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Setting = struct
  type 'a t = {
    value : 'a Stdlib.ref;
    arg_lst : (Arg.key * Arg.spec * Arg.doc) list;
  }

  type require = bool Stdlib.ref

  let required_set : require Core.Set.Poly.t Stdlib.ref =
     Stdlib.ref Core.Set.Poly.empty

  let add_required : unit -> require =
    fun () ->
    let rf : require = Stdlib.ref false in
    let _ = required_set := Core.Set.Poly.add !required_set rf in
    rf

  let check_required : unit -> bool =
    (fun () -> Core.Set.for_all !required_set ~f:Stdlib.( ! ))

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
     let (value : string Stdlib.ref) = Stdlib.ref "" in
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
     let (value : string Stdlib.ref) = Stdlib.ref "" in
     let (spec : Arg.spec) =
        Arg.String
          (fun s ->
          required := true;
          value := s)
     in
     let (doc : Arg.doc) = "file path for input initial storage information" in
     { value; arg_lst = create_arg_lst [ "--initial-storage"; "-S" ] spec doc }

  let memory_bound : int t =
     let (value : int Stdlib.ref) = Stdlib.ref 5 in
     let (spec : Arg.spec) = Arg.Set_int value in
     let (doc : Arg.doc) =
        "memory budget for entire MicSE in GB (default: 5GB)"
     in
     { value; arg_lst = create_arg_lst [ "--memory-bound"; "-M" ] spec doc }

  let total_timeout : int t =
     let (value : int Stdlib.ref) = Stdlib.ref 360 in
     let (spec : Arg.spec) = Arg.Set_int value in
     let (doc : Arg.doc) =
        "time budget for entire MicSE execution in seconds. (default: 360s)"
     in
     { value; arg_lst = create_arg_lst [ "--total-timeout"; "-T" ] spec doc }

  let z3_timeout : int t =
     let (value : int Stdlib.ref) = Stdlib.ref 30 in
     let (spec : Arg.spec) = Arg.Set_int value in
     let (doc : Arg.doc) =
        "time budget for Z3 solver in seconds (default: 30s)"
     in
     { value; arg_lst = create_arg_lst [ "--z3-timeout"; "-Z" ] spec doc }

  (****************************************************************************)
  (* Debug Mode                                                               *)
  (****************************************************************************)

  let debug_mode : bool t =
     let (value : bool Stdlib.ref) = Stdlib.ref false in
     let (spec : Arg.spec) = Arg.Set value in
     let (doc : Arg.doc) = "print log over debug level" in
     { value; arg_lst = create_arg_lst [ "--debug"; "-d" ] spec doc }

  let inst_count : bool t =
     let (value : bool Stdlib.ref) = Stdlib.ref false in
     let (spec : Arg.spec) = Arg.Set value in
     let (doc : Arg.doc) =
        "print count of instructions in input Michelson file"
     in
     { value; arg_lst = create_arg_lst [ "--inst-count" ] spec doc }

  let verbose_mode : bool t =
     let (value : bool Stdlib.ref) = Stdlib.ref false in
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
    (fun _ -> Stdlib.raise (Arg.Help "wrong anonymous argument(s)"))

  let finalize_parse : unit -> unit =
    fun () ->
    (* 1. check required arguments *)
    if Stdlib.not (check_required ())
    then Stdlib.raise (Arg.Bad "all required argument are not set")
    else ()
end

(******************************************************************************)
(******************************************************************************)
(* Functions                                                                  *)
(******************************************************************************)
(******************************************************************************)

let create : unit -> unit =
  fun () ->
  let _ = Arg.parse Setting.option_list Setting.anon_fun Setting.usage_msg in
  let _ = Setting.finalize_parse () in
  ()

let input_file : string Stdlib.ref = Setting.input_file.value

let input_storage_file : string Stdlib.ref = Setting.input_storage_file.value

let memory_bound : int Stdlib.ref = Setting.memory_bound.value

let total_timeout : int Stdlib.ref = Setting.total_timeout.value

let z3_timeout : int Stdlib.ref = Setting.z3_timeout.value

let debug_mode : bool Stdlib.ref = Setting.debug_mode.value

let inst_count : bool Stdlib.ref = Setting.inst_count.value

let verbose_mode : bool Stdlib.ref = Setting.verbose_mode.value
