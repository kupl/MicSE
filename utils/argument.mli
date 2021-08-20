(* Argument Parser *)

(******************************************************************************)
(******************************************************************************)
(* Setting                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Setting : sig
  type 'a t = {
    value : 'a Stdlib.ref;
    arg_lst : (Arg.key * Arg.spec * Arg.doc) list;
  }

  type require = bool Stdlib.ref

  val required_set : require Core.Set.Poly.t Stdlib.ref

  val add_required : require -> unit

  val check_required : unit -> bool

  val create_arg_lst :
    Arg.key list -> Arg.spec -> Arg.doc -> (Arg.key * Arg.spec * Arg.doc) list

  (****************************************************************************)
  (* MicSE Behaviors                                                          *)
  (****************************************************************************)

  val input_file : string t

  val input_storage_file : string t

  val memory_bound : int t

  val total_timeout : int t

  val z3_timeout : int t

  (****************************************************************************)
  (* Debug Mode                                                               *)
  (****************************************************************************)

  val verbose_mode : bool t

  val inst_count : bool t

  val debug_mode : bool t

  (****************************************************************************)
  (* Arguments Settings                                                       *)
  (****************************************************************************)

  val usage_msg : string

  val option_list : (Arg.key * Arg.spec * Arg.doc) list

  val anon_fun : string -> unit

  val finalize_parse : unit -> unit
end

(******************************************************************************)
(******************************************************************************)
(* Functions                                                                  *)
(******************************************************************************)
(******************************************************************************)

val create : unit -> unit

val input_file : string Stdlib.ref

val input_storage_file : string Stdlib.ref

val memory_bound : int Stdlib.ref

val total_timeout : int Stdlib.ref

val z3_timeout : int Stdlib.ref

val debug_mode : bool Stdlib.ref

val inst_count : bool Stdlib.ref

val verbose_mode : bool Stdlib.ref
