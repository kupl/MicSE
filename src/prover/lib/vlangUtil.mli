(* Naive Formula Optimizations *)
module NaiveOpt : sig
  val run : Vlang.t -> Vlang.t
end


module FormulaUtils : sig
  val optimize_var : Vlang.Expr.t -> Vlang.Expr.t
end
