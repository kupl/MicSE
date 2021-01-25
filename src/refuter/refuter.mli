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

(* "run_multiple n" runs the "main" function n times, changing the number of transaction-unrolling number *)
val run_multiple : (PreLib.Cfg.t * PreLib.Cfg.cfgcon_ctr) -> PreLib.Adt.data option -> unit