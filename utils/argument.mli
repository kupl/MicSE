(* Argument Parser *)

(******************************************************************************)
(******************************************************************************)
(* Setting                                                                    *)
(******************************************************************************)
(******************************************************************************)

module Setting : sig
  type 'a t = {
    value : 'a Stdlib.ref;
    spec : Arg.key * Arg.spec * Arg.doc;
  }

  val input_file : string t

  val verbose_mode : bool t

  val debug_mode : bool t

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

val verbose_mode : bool Stdlib.ref

val debug_mode : bool Stdlib.ref
