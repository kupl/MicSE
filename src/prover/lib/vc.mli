type typ = Adt.typ
and data = Adt.data
and operation = Adt.operation

type var = Cfg.ident
and exp = Cfg.expr

type v_formula = Vlang.v_formula
and v_exp = Vlang.v_exp

val unit_sort : Z3.Sort.sort
val option_symbol : Z3.Symbol.symbol
val option_none_symbol : Z3.Symbol.symbol
val option_some_symbol : Z3.Symbol.symbol
val option_sort : Z3.Sort.sort
val or_symbol : Z3.Symbol.symbol
val or_left_symbol : Z3.Symbol.symbol
val or_right_symbol : Z3.Symbol.symbol
val or_sort : Z3.Sort.sort

val sort_of_typt : typ -> Z3.Sort.sort

val zexp_of_vformula : v_formula -> Z3.Expr.expr

val solver : unit -> Z3.Solver.solver

val add : Z3.Solver.solver -> Z3.Expr.expr list -> unit

val check : Z3.Solver.solver -> (bool * Z3.Model.model option)

val string_of_solver : Z3.Solver.solver -> string

val string_of_model : Z3.Model.model -> string
