(* Refuter procedure *)

(* 
  [INPUT]
  - Cfg: Control flow graph of the given smart contract
  - Cfg-construction Counter (Type "PreLib.Cfg.cfgcon_ctr")
  - Initial Storage (optional)
  - # of Transaction-Unrolling
  - # of Loop-Unrolling 
*)
val main : (PreLib.Cfg.t * PreLib.Cfg.cfgcon_ctr) -> PreLib.Adt.data option -> int -> int -> unit
