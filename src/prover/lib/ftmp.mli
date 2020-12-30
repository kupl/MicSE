
(*****************************************************************************)
(*****************************************************************************)
(* SigmaEqualUtils                                                           *)
(*****************************************************************************)
(*****************************************************************************)

module SigmaEqualUtils : sig
  exception Error of string

  module PolySet = Core.Set.Poly

  type idx = Vlang.Expr.t PolySet.t
  type partition = idx PolySet.t

  val read_index : Vlang.Expr.t -> idx
  val read_origin_map : Vlang.Expr.t -> Vlang.Expr.t
  val read_partition_expr : partition -> map:Vlang.Expr.t -> Vlang.Expr.t list

  val create_remain_var : Vlang.Expr.t -> Vlang.Expr.t
  val create_partition : Vlang.Expr.t -> partition list
  val create_partitioning_formula : partition -> Vlang.t
  val create_idx_exist_formula : Vlang.Expr.t -> Vlang.t
  val create_fst_formula : partition -> map:Vlang.Expr.t -> value:Vlang.Expr.t -> Vlang.t
  val create_snd_formula : partition -> map:Vlang.Expr.t -> Vlang.t

  val create_pre_formula : map:Vlang.Expr.t -> value:Vlang.Expr.t -> Vlang.t
  val create_post_formula : map:Vlang.Expr.t -> value:Vlang.Expr.t -> Vlang.t
end

(*****************************************************************************)
(*****************************************************************************)
(* Templates                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

module Tmp : sig
  exception Error of string

  val sigma_equal_template : Vlang.t -> Vlang.t
end