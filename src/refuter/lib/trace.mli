(* Transaction Traces *)

exception Error of string

(* Type for each transaction information *)
type trx = {
  index : int;
  storage : ProverLib.Smt_deprecated.ZExpr.t;    (* storage of contract before the transaction *)
  parameter : ProverLib.Smt_deprecated.ZExpr.t;  (* parameter value of the transaction *)
  sender : ProverLib.Smt_deprecated.ZExpr.t;     (* sender address of the transaction *)
  source : ProverLib.Smt_deprecated.ZExpr.t;     (* source address of the transaction *)
  amount : ProverLib.Smt_deprecated.ZExpr.t;     (* amount of the transaction *)
  balance : ProverLib.Smt_deprecated.ZExpr.t;    (* updated balance of contract by the transaction *)
}

(* Type for Transaction Trace *)
type t = {
  index : int;                                (* scenario index of trace *)
  query : Prover.VcGen.query_vc;              (* query information which violated by this trace *)
  loc : PreLib.Mich.loc;                      (* location information which safety property is violated *)
  length : int;                               (* length of transaction trace *)
  trx_list : trx list;                        (* transaction information of this trace *)
}

val gen : PreLib.Cfg.t -> (PreLib.Cfg.vertex -> PreLib.Mich.loc) -> int -> int -> Prover.VcGen.query_vc -> ProverLib.Smt_deprecated.ZModel.t -> t

val to_string : t -> string