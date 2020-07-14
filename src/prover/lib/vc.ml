type typ = Adt.typ
and data = Adt.data
and operation = Adt.operation

type var = Cfg.ident
and exp = Cfg.expr

type v_formula = Vlang.v_formula
and v_exp = Vlang.v_exp

let ctx = ref (Z3.mk_context [])

let option_symbol = Z3.Symbol.mk_string !ctx "option"
let option_none_symbol = Z3.Symbol.mk_string !ctx "none"
let option_some_symbol = Z3.Symbol.mk_string !ctx "some"
let option_sort = Z3.Enumeration.mk_sort !ctx option_symbol [option_none_symbol; option_some_symbol]
let or_symbol = Z3.Symbol.mk_string !ctx "or"
let or_left_symbol = Z3.Symbol.mk_string !ctx "left"
let or_right_symbol = Z3.Symbol.mk_string !ctx "right"
let or_sort = Z3.Enumeration.mk_sort !ctx or_symbol [or_left_symbol; or_right_symbol]

let rec sort_of_typt : typ -> Z3.Sort.sort
=fun typ -> begin
  let mk_typt_symbol s tl = Z3.Symbol.mk_string !ctx (Core.List.fold_right tl ~init:s ~f:(fun t str -> str ^ "_" ^ (Adt.string_of_typt t))) in
  let mk_simple_symbol s = Z3.Symbol.mk_string !ctx s in
  match typ.d with
  | T_key -> Z3.Seq.mk_string_sort !ctx
  | T_unit -> Z3.Sort.mk_uninterpreted_s !ctx "unit" (* UNINTERPRETED *)
  | T_signature -> Z3.Seq.mk_string_sort !ctx
  | T_option t -> Z3.Tuple.mk_sort !ctx (mk_typt_symbol "option" [t]) [(mk_simple_symbol "exist"); (mk_simple_symbol "value")] [option_sort; (sort_of_typt t)]
  | T_list t -> Z3.Z3List.mk_sort !ctx (mk_typt_symbol "list" [t]) (sort_of_typt t)
  | T_set t -> Z3.Set.mk_sort !ctx (sort_of_typt t)
  | T_operation -> Z3.Sort.mk_uninterpreted_s !ctx "operation" (* UNINTERPRETED *)
  | T_contract _ ->  Z3.Sort.mk_uninterpreted_s !ctx "contract" (* UNINTERPRETED *)
  | T_pair (t1, t2) -> Z3.Tuple.mk_sort !ctx (mk_typt_symbol "pair" [t1; t2]) [(mk_simple_symbol "fst"); (mk_simple_symbol "snd")] [(sort_of_typt t1); (sort_of_typt t2)]
  | T_or (t1, t2) -> Z3.Tuple.mk_sort !ctx (mk_typt_symbol "or" [t1; t2]) [(mk_simple_symbol "loc"); (mk_simple_symbol "left"); (mk_simple_symbol "right")] [or_sort; (sort_of_typt t1); (sort_of_typt t2)]
  | T_lambda (t1, t2) -> Z3.Sort.mk_uninterpreted_s !ctx "lambda" (* UNINTERPRETED *)
  | T_map (t1, t2) -> Z3.Z3Array.mk_sort !ctx (sort_of_typt t1) (sort_of_typt t2)
  | T_big_map (t1, t2) -> Z3.Z3Array.mk_sort !ctx (sort_of_typt t1) (sort_of_typt t2)
  | T_chain_id -> Z3.Seq.mk_string_sort !ctx
  | T_int -> Z3.Arithmetic.Integer.mk_sort !ctx
  | T_nat -> Z3.Arithmetic.Integer.mk_sort !ctx
  | T_string -> Z3.Seq.mk_string_sort !ctx
  | T_bytes -> Z3.Seq.mk_string_sort !ctx
  | T_mutez -> Z3.BitVector.mk_sort !ctx 64
  | T_bool -> Z3.Boolean.mk_sort !ctx
  | T_key_hash -> Z3.Seq.mk_string_sort !ctx
  | T_timestamp -> Z3.Seq.mk_string_sort !ctx
  | T_address -> Z3.Seq.mk_string_sort !ctx
end



let solver : unit -> Z3.Solver.solver
=fun () -> Z3.Solver.mk_solver !ctx None

let add : Z3.Solver.solver -> Z3.Expr.expr list -> unit
=fun solver el -> Z3.Solver.add solver el

let check : Z3.Solver.solver -> (bool * Z3.Model.model option)
=fun solver -> begin
  let check = Z3.Solver.check solver [] in
  match check with
  |	UNSATISFIABLE -> (true, None)
  |	SATISFIABLE -> begin
      let model = Z3.Solver.get_model solver in
      (false, model)
    end
  |	UNKNOWN -> (false, None)
end

let string_of_solver : Z3.Solver.solver -> string
=fun solver -> Z3.Solver.to_string solver

let string_of_model : Z3.Model.model -> string
=fun model -> Z3.Model.to_string model
