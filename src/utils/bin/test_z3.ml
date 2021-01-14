let open ProverLib.Smt in

(* Sort Generation *)
let str_sort = ZStr.sort () in
let mutez_sort = ZMutez.sort () in

(* Expressions *)
let a = ZStr.of_string "A" in
let b = ZStr.of_string "B" in

let one = ZMutez.of_int 1 in

let map = ZMap.create ~key_sort:str_sort ~value_sort:mutez_sort in
let map = ZMap.update ~key:a ~value:(ZOption.create_some ~content:one) ~map in

let a_val = ZMap.read_value ~key:a ~map in
let b_val = ZMap.read_value ~key:b ~map in

let em = ZExpr.create_var (map |> ZExpr.read_sort) ~name:"M" in
let ea = ZExpr.create_var (a_val |> ZExpr.read_sort) ~name:"A" in
let eb = ZExpr.create_var (b_val |> ZExpr.read_sort) ~name:"B" in

(* Formulas *)
let fm = ZFormula.create_eq em map in
let fa = ZFormula.create_eq ea a_val in
let fb = ZFormula.create_eq eb b_val in

let f1 = ZOption.is_some ea in
let f2 = ZMutez.create_eq (ZOption.read ea) (one) in
let f3 = ZOption.is_none eb in
let f4 = ZMap.read_exist ~key:(ZStr.of_string "A") ~map in

let ff = ZFormula.create_and [fm; fa; fb] in
let fs = ZFormula.create_and [f1; f2; f3; f4] in
let f = ZFormula.create_imply ff fs in

(* Formula Addition to Solver *)
let validity, model = ZSolver.check_validity [f] in

if validity |> ZSolver.is_valid then begin
  print_endline ("VAL")
end else begin
  print_endline ("INVALID") ;
  print_endline (model |> Option.get |> ZModel.to_string)
end