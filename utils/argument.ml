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

  let query_pick : (int * int) option t =
     let (value : (int * int) option ref) = ref None in
     let (spec : Arg.spec) =
        Arg.Tuple
          [
            Arg.Int
              (fun i1 ->
              value := Some (i1, snd (Option.value !value ~default:(0, 0))));
            Arg.Int
              (fun i2 ->
              value := Some (fst (Option.value !value ~default:(0, 0)), i2));
          ]
     in
     let (doc : Arg.doc) =
        "operate MicSE with only picked query (format: [lin] [col])"
     in
     { value; arg_lst = create_arg_lst [ "--query-pick"; "-q" ] spec doc }

  let verbose_mode : bool t =
     let (value : bool ref) = ref false in
     let (spec : Arg.spec) = Arg.Set value in
     let (doc : Arg.doc) = "print log over info level" in
     { value; arg_lst = create_arg_lst [ "--verbose"; "-v" ] spec doc }

  (****************************************************************************)
  (* Experiment Mode                                                          *)
  (****************************************************************************)

  let prec_random_rate : int t =
     let (value : int ref) = ref 50 in
     let (spec : Arg.spec) = Arg.Set_int value in
     let (doc : Arg.doc) =
        "set random choice rate of precondition (interval: [0, 100])"
     in
     {
       value;
       arg_lst = create_arg_lst [ "--prec_random_rate"; "-pr" ] spec doc;
     }

  let status_interval : int option t =
     let (value : int option ref) = ref None in
     let (spec : Arg.spec) = Arg.Int (fun v -> value := Some v) in
     let (doc : Arg.doc) =
        "set interval of printing intermediate status (interval: [0, ∞)"
     in
     { value; arg_lst = create_arg_lst [ "--status-interval"; "-si" ] spec doc }

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
     @ query_pick.arg_lst
     @ verbose_mode.arg_lst
     @ prec_random_rate.arg_lst
     @ status_interval.arg_lst

  let anon_fun : string -> unit =
    (fun _ -> raise (Arg.Help "wrong anonymous argument(s)"))

  let finalize_parse : unit -> unit =
    fun () ->
    if (* 1. check required arguments *)
       not (check_required ())
    then raise (Arg.Bad "all required argument are not set")
    else if (* 2. check validity of query pick *)
            Option.is_some !(query_pick.value)
            && (fst (Option.value_exn !(query_pick.value)) < 0
               || snd (Option.value_exn !(query_pick.value)) < 0
               )
    then raise (Arg.Bad "invalid query picking is inputed")
    else if (* 3. check range validity of prec_random_rate *)
            not
              (0 <= !(prec_random_rate.value)
              && !(prec_random_rate.value) <= 100
              )
    then raise (Arg.Bad "bad range of precondition-random-rate")
    else if (* 4. check range validity of status_interval *)
            Option.is_some !(status_interval.value)
            && not (0 <= Option.value_exn !(status_interval.value))
    then raise (Arg.Bad "bad range of status-interval")
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

(****************************************************************************)
(* MicSE Behaviors                                                          *)
(****************************************************************************)

let input_file : string ref = Setting.input_file.value

let input_storage_file : string ref = Setting.input_storage_file.value

let memory_bound : int ref = Setting.memory_bound.value

let total_timeout : int ref = Setting.total_timeout.value

let z3_timeout : int ref = Setting.z3_timeout.value

(****************************************************************************)
(* Dev Mode                                                                 *)
(****************************************************************************)

let debug_mode : bool ref = Setting.debug_mode.value

let inst_count : bool ref = Setting.inst_count.value

let query_pick : (int * int) option ref = Setting.query_pick.value

let verbose_mode : bool ref = Setting.verbose_mode.value

(****************************************************************************)
(* Experiment Mode                                                          *)
(****************************************************************************)

let prec_random_rate : int ref = Setting.prec_random_rate.value

let status_interval : int option ref = Setting.status_interval.value
