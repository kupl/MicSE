(*****************************************************************************)
(*****************************************************************************)
(* Exceptions                                                                *)
(*****************************************************************************)
(*****************************************************************************)

exception Exn_Translator of string

val fail : string -> 'a


(*****************************************************************************)
(*****************************************************************************)
(* Stack information (internal datatype)                                     *)
(*****************************************************************************)
(*****************************************************************************)

type stack_info_t =
| NS of string list (* Normal Stack info *)
| ES of string      (* Error  Stack info *)

val ns_hd       : stack_info_t -> string
val ns_tl       : stack_info_t -> stack_info_t
val ns_nth      : stack_info_t -> int -> string
val ns_rev      : stack_info_t -> stack_info_t
val ns_cons     : string -> stack_info_t -> stack_info_t
val ns_append   : stack_info_t -> stack_info_t -> stack_info_t
val ns_unlift   : stack_info_t -> string list
val ns_split_n  : stack_info_t -> int -> (stack_info_t * stack_info_t)
val ns_rev_append : stack_info_t -> stack_info_t -> stack_info_t

val is_es       : stack_info_t -> bool
val get_es_str  : stack_info_t -> string


(*****************************************************************************)
(*****************************************************************************)
(* Adt to Cfg                                                                *)
(*****************************************************************************)
(*****************************************************************************)

(* "adt_to_cfg_counter_included" is recommended for advanced users who know what Cfg.cfgcon_ctr do *)
val adt_to_cfg_counter_included : (PreLib.Adt.t * (PreLib.Cfg.cfgcon_ctr option)) -> (PreLib.Cfg.t * PreLib.Cfg.cfgcon_ctr)

val adt_to_cfg : PreLib.Adt.t -> PreLib.Cfg.t
