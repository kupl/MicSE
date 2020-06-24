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

val tcfg_cast_stmt : ProverLib.TezlaCfg.Node.stmt -> ProverLib.Cfg.stmt
val tcfg_cast_edge_label : ProverLib.TezlaCfg.edge_label -> ProverLib.Cfg.edge_label
val tcfg_get_id : ProverLib.TezlaCfg.Node.t -> int
val tcfg_get_stmt : ProverLib.TezlaCfg.Node.t -> ProverLib.TezlaCfg.Node.stmt

val of_tezlaCfg : ProverLib.TezlaCfg.t -> ProverLib.Cfg.t