(* Argument Parser *)

(******************************************************************************)
(******************************************************************************)
(* Setting                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Setting : sig
  type 'a t = {
    value : 'a ref;
    arg_lst : (Arg.key * Arg.spec * Arg.doc) list;
  }

  type require = bool ref

  val required_set : require Core.Set.Poly.t ref

  val add_required : unit -> require

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

  val path_limit : int t

  (****************************************************************************)
  (* Dev Mode                                                                 *)
  (****************************************************************************)

  val debug_mode : bool t

  val inst_count : bool t

  val query_pick : (int * int) option t

  val verbose_mode : bool t

  (****************************************************************************)
  (* Experiment Mode                                                          *)
  (****************************************************************************)

  val set_random_seed : bool t

  val status_interval : int option t

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

val create : string array option -> unit

(****************************************************************************)
(* MicSE Behaviors                                                          *)
(****************************************************************************)

val input_file : string ref

val input_storage_file : string ref

val memory_bound : int ref

val total_timeout : int ref

val z3_timeout : int ref

val path_limit : int ref

(****************************************************************************)
(* Dev Mode                                                                 *)
(****************************************************************************)

val debug_mode : bool ref

val inst_count : bool ref

val query_pick : (int * int) option ref

val verbose_mode : bool ref

(****************************************************************************)
(* Experiment Mode                                                          *)
(****************************************************************************)

val set_random_seed : bool ref

val status_interval : int option ref
