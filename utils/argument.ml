(* Argument Parser *)

(******************************************************************************)
(******************************************************************************)
(* Setting                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Setting = struct
  type 'a t = {
    value : 'a Stdlib.ref;
    spec : Arg.key * Arg.spec * Arg.doc;
  }

  let input_file : string t =
     let (value : string Stdlib.ref) = Stdlib.ref "" in
     {
       value;
       spec =
         ( "--input",
           Arg.String (fun s -> value := s),
           "File path for input michelson program"
         );
     }

  let input_storage_file : string t =
     let (value : string Stdlib.ref) = Stdlib.ref "" in
     {
       value;
       spec =
         ( "--initial_storage",
           Arg.String (fun s -> value := s),
           "Initial-storage file path for input michelson program"
         );
     }

  let verbose_mode : bool t =
     let (value : bool Stdlib.ref) = Stdlib.ref false in
     { value; spec = ("--verbose", Arg.Set value, "Print log over info level") }

  let debug_mode : bool t =
     let (value : bool Stdlib.ref) = Stdlib.ref false in
     { value; spec = ("--debug", Arg.Set value, "Print log over debug level") }

  let usage_msg : string = "micse -input [filename] [options]"

  let option_list : (Arg.key * Arg.spec * Arg.doc) list =
     [
       input_file.spec;
       input_storage_file.spec;
       verbose_mode.spec;
       debug_mode.spec;
     ]

  let anon_fun : string -> unit = (fun _ -> ())

  let finalize_parse : unit -> unit = (fun () -> ())
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

let verbose_mode : bool Stdlib.ref = Setting.verbose_mode.value

let debug_mode : bool Stdlib.ref = Setting.debug_mode.value
