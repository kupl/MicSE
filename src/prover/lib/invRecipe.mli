module CPSet = Core.Set.Poly (* sugar *)

val combination_rfl : 'a CPSet.t -> ('a * 'a) CPSet.t

val combination : 'a CPSet.t -> 'b CPSet.t -> ('a * 'b) CPSet.t

val combination_self_two_diff : 'a list -> ('a * 'a) list



(* "formula_mutez_equal" : every component should be mutez type. *)
val mutez_equal : Vlang.Component.t -> Vlang.Formula.t CPSet.t
(* "mutez_ge" : every component should be mutez type. *)
val mutez_ge : Vlang.Component.t -> Vlang.Formula.t CPSet.t

(* "mtzmap_partial_sum" : components should be "('k, mutez)map", "('k)", and "mutez" types
  "mtzmap_partial_sum c1 c2 c3" will generate
    "|c1| * (2^|c2|) * |c3|" formulas, so be careful.
*)
val mtzmap_partial_sum : Vlang.Component.t -> Vlang.Component.t -> Vlang.Component.t -> Vlang.Formula.t CPSet.t

(* "int_ge" : every component should be integer type. *)
val int_ge : Vlang.Component.t -> Vlang.Formula.t CPSet.t

(* "nat_ge" : every component should be natural type. *)
val nat_ge : Vlang.Component.t -> Vlang.Formula.t CPSet.t

(* "all_equal" : a generalized version of "formula_mutez_equal". *)
val all_equal : Vlang.Component.t -> Vlang.Formula.t CPSet.t
