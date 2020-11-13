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
  type t = Vlang.v_obj
  type data = Pre.Lib.Mich.data Pre.Lib.Mich.t
  type cfg = Pre.Lib.Cfg.t

  val read_typt : cfg:cfg -> Vlang.typ

  val create : cfg:cfg -> t

  val create_comp : cfg:cfg -> Comps.t

  val equal_to_obj : obj:t -> cfg:cfg -> Vlang.t

  val equal_to_data : data:data -> cfg:cfg -> Vlang.t

  val equal_to_ps_or_os : ps_os:Vlang.var -> cfg:cfg -> Vlang.t
end

module TrxInv : sig
  type m = Inv.Map.t
  type formula = Vlang.t
  type cfg = Pre.Lib.Cfg.t

  val formula_mutez_equal : comp:Comps.t -> formula list

  val wrap_formula : cfg:cfg -> entry:Vlang.var -> exit:Vlang.var -> comp:Comps.t -> f:(comp:Comps.t -> formula list) -> (formula * formula) list

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

  val create : bp_list:Bp.lst -> t

  val update : bp_list:Bp.lst -> cfg:cfg -> init_stg:Stg.data option -> inv:m -> wlst:t -> t

  val join : inv:m -> wlst:t -> t
end