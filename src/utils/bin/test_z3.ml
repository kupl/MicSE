(* Settings *)
let ctx = [] |> Z3.mk_context in
let solver = Z3.Solver.mk_solver ctx None in

(* Sort Generation *)
let int_sort = Z3.Arithmetic.Integer.mk_sort ctx in

(* Expressions *)
let a = Z3.Arithmetic.Integer.mk_numeral_s ctx "1" in
let e = Z3.Expr.mk_const_s ctx "X" int_sort in

(* Formulas *)
let f = Z3.Boolean.mk_eq ctx a e in

(* Formula Addition to Solver *)
let result = Z3.Solver.check solver [f] in
let model = Z3.Solver.get_model solver in

(* Print Result *)
match result with
| UNKNOWN -> begin
    print_endline ("> Proving Error");
    print_endline (solver |> Z3.Solver.get_reason_unknown)
  end
| UNSATISFIABLE -> begin
    print_endline ("> Unsatisfiable")
  end
| SATISFIABLE -> begin
    print_endline ("> Satisfiable");
    print_endline ("> SAT Model");
    print_endline (model |> Option.get |> Z3.Model.to_string)
  end