(*****************************************************************************)
(*****************************************************************************)
(* Exceptions                                                                *)
(*****************************************************************************)
(*****************************************************************************)

exception Exn_Translator of string

val fail : string -> 'a


(*****************************************************************************)
(*****************************************************************************)
(* Tezla_cfg to Cfg                                                          *)
(*****************************************************************************)
(*****************************************************************************)

val tcfg_cast_stmt : PreLib.TezlaCfg.Node.stmt -> PreLib.Cfg.stmt
val tcfg_cast_edge_label : PreLib.TezlaCfg.edge_label -> PreLib.Cfg.edge_label
val tcfg_get_id : PreLib.TezlaCfg.Node.t -> int
val tcfg_get_stmt : PreLib.TezlaCfg.Node.t -> PreLib.TezlaCfg.Node.stmt

val of_tezlaCfg : PreLib.TezlaCfg.t -> PreLib.Cfg.t


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

val adt_to_cfg : PreLib.Adt.t -> PreLib.Cfg.t