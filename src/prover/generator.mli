open ProverLib

(************************************************)
(************************************************)

val apply : Inv.Map.t -> Bp.t list -> Bp.t list


(*****************************************************************************)
(*****************************************************************************)
(* Worklist Management                                                       *)
(*****************************************************************************)
(*****************************************************************************)

module Stg : sig
  type t = Vlang.Expr.t
  type data = Pre.Lib.Mich.data Pre.Lib.Mich.t
  type cfg = Pre.Lib.Cfg.t

  exception Error of string

  val read_typt : cfg:cfg -> Vlang.typ

  val create : cfg:cfg -> t

  val create_comp : cfg:cfg -> Comps.t

  val equal_to_expr : expr:t -> cfg:cfg -> Vlang.t

  val equal_to_data : data:data -> cfg:cfg -> Vlang.t

  val equal_to_ps_or_os : ps_os:Vlang.var -> cfg:cfg -> Vlang.t
end

module TrxInv : sig
  type m = Inv.Map.t
  type formula = Vlang.t
  type cfg = Pre.Lib.Cfg.t

  exception Error of string

  val formula_mutez_equal : comp:Comps.t -> formula list

  val wrap_formula : bp_list:Bp.lst -> cfg:cfg -> (f:(comp:Comps.t -> formula list) -> (formula * formula) list)
  
  val create : bp_list:Bp.lst -> cfg:cfg -> m list
end

(*
module LoopInv : sig
end
*)

(*****************************************************************************)
(*****************************************************************************)
(* Worklist Management                                                       *)
(*****************************************************************************)
(*****************************************************************************)

module W : sig
  type t = Inv.WorkList.t
  type m = Inv.Map.t
  type cfg = Pre.Lib.Cfg.t

  exception Error of string

  val create : bp_list:Bp.lst -> t

  val update : bp_list:Bp.lst -> cfg:cfg -> init_stg:Stg.data option -> wlst:t -> t

  val join : inv:m -> wlst:t -> t

  val last_worklist : wlst:t -> t
end
