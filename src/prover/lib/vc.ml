type typ = Adt.typ
and data = Adt.data
and operation = Adt.operation

type var = Cfg.ident
and exp = Cfg.expr

type v_formula = Vlang.v_formula
and v_exp = Vlang.v_exp

let ctx = ref (Z3.mk_context [])

let unit_sort = Z3.Sort.mk_uninterpreted_s !ctx "Unit" (* UNINTERPRETED *)
let option_symbol = Z3.Symbol.mk_string !ctx "Option"
let option_none_symbol = Z3.Symbol.mk_string !ctx "None"
let option_some_symbol = Z3.Symbol.mk_string !ctx "Some"
let option_sort = Z3.Enumeration.mk_sort !ctx option_symbol [option_none_symbol; option_some_symbol]
let or_symbol = Z3.Symbol.mk_string !ctx "Or"
let or_left_symbol = Z3.Symbol.mk_string !ctx "Left"
let or_right_symbol = Z3.Symbol.mk_string !ctx "Right"
let or_sort = Z3.Enumeration.mk_sort !ctx or_symbol [or_left_symbol; or_right_symbol]

let rec sort_of_typt : typ -> Z3.Sort.sort
=fun typ -> begin
  let mk_typt_symbol s tl = Z3.Symbol.mk_string !ctx (Core.List.fold_right tl ~init:s ~f:(fun t str -> str ^ "_" ^ (Adt.string_of_typt t))) in
  let mk_simple_symbol s = Z3.Symbol.mk_string !ctx s in
  match typ.d with
  | T_key -> Z3.Seq.mk_string_sort !ctx
  | T_unit -> unit_sort
  | T_signature -> Z3.Seq.mk_string_sort !ctx
  | T_option t -> Z3.Tuple.mk_sort !ctx (mk_typt_symbol "Option" [t]) [(mk_simple_symbol "exist"); (mk_simple_symbol "value")] [option_sort; (sort_of_typt t)]
  | T_list t -> Z3.Z3List.mk_sort !ctx (mk_typt_symbol "List" [t]) (sort_of_typt t)
  | T_set t -> Z3.Set.mk_sort !ctx (sort_of_typt t)
  | T_operation -> Z3.Sort.mk_uninterpreted_s !ctx "Operation" (* UNINTERPRETED *)
  | T_contract _ ->  Z3.Sort.mk_uninterpreted_s !ctx "Contract" (* UNINTERPRETED *)
  | T_pair (t1, t2) -> Z3.Tuple.mk_sort !ctx (mk_typt_symbol "Pair" [t1; t2]) [(mk_simple_symbol "fst"); (mk_simple_symbol "snd")] [(sort_of_typt t1); (sort_of_typt t2)]
  | T_or (t1, t2) -> Z3.Tuple.mk_sort !ctx (mk_typt_symbol "Or" [t1; t2]) [(mk_simple_symbol "loc"); (mk_simple_symbol "left"); (mk_simple_symbol "right")] [or_sort; (sort_of_typt t1); (sort_of_typt t2)]
  | T_lambda (t1, t2) -> Z3.Sort.mk_uninterpreted_s !ctx "Lambda" (* UNINTERPRETED *)
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

let sort_of_inner_type : typ -> Z3.Sort.sort list
=fun typ -> begin
  match typ.d with
  | T_option t -> [(sort_of_typt t)]
  | T_list t -> [(sort_of_typt t)]
  | T_set t -> [(sort_of_typt t)]
  | T_pair (t1, t2) -> [(sort_of_typt t1); (sort_of_typt t2)]
  | T_or (t1, t2) -> [(sort_of_typt t1); (sort_of_typt t2)]
  | T_lambda (t1, t2) -> [(sort_of_typt t1); (sort_of_typt t2)]
  | T_map (t1, t2) -> [(sort_of_typt t1); (sort_of_typt t2)]
  | T_big_map (t1, t2) -> [(sort_of_typt t1); (sort_of_typt t2)]
  | _ -> raise (Failure "Cannot get inner type on this type")
end

let rec zexp_of_vformula : v_formula -> Z3.Expr.expr
=fun vf -> begin
  match vf with
  | VF_true -> Z3.Boolean.mk_true !ctx
  | VF_false -> Z3.Boolean.mk_false !ctx
  | VF_not f -> Z3.Boolean.mk_not !ctx (zexp_of_vformula f)
  | VF_and (f1, f2) -> Z3.Boolean.mk_and !ctx [(zexp_of_vformula f1); (zexp_of_vformula f2)]
  | VF_or (f1, f2) -> Z3.Boolean.mk_or !ctx [(zexp_of_vformula f1); (zexp_of_vformula f2)]
  | VF_uni_rel (vur, e) -> begin
      let e' = zexp_of_vexp e in
      let sort_of_e = Z3.Expr.get_sort e' in
      match vur with
      | VF_is_true -> Z3.Boolean.mk_eq !ctx e' (Z3.Boolean.mk_true !ctx)
      | VF_is_none -> begin
          let fields = Z3.Tuple.get_field_decls sort_of_e in
          let exist_func = Core.List.nth_exn fields 0 in
          let exist = Z3.FuncDecl.apply exist_func [e'] in
          Z3.Boolean.mk_eq !ctx exist (Z3.Enumeration.get_const option_sort 0)
        end
      | VF_is_left -> begin
          let fields = Z3.Tuple.get_field_decls sort_of_e in
          let loc_func = Core.List.nth_exn fields 0 in
          let loc = Z3.FuncDecl.apply loc_func [e'] in
          Z3.Boolean.mk_eq !ctx loc (Z3.Enumeration.get_const or_sort 0)
        end
      | VF_is_cons -> begin
          let is_cons_func = Z3.Z3List.get_is_cons_decl sort_of_e in
          Z3.FuncDecl.apply is_cons_func [e']
        end
    end
  | VF_eq (e1, e2) -> Z3.Boolean.mk_eq !ctx (zexp_of_vexp e1) (zexp_of_vexp e2)
  | VF_imply (f1, f2) -> Z3.Boolean.mk_implies !ctx (zexp_of_vformula f1) (zexp_of_vformula f2)
  | VF_iff (f1, f2) -> Z3.Boolean.mk_iff !ctx (zexp_of_vformula f1) (zexp_of_vformula f2)
end

and zexp_of_vexp : v_exp -> Z3.Expr.expr
=fun ve -> begin
  let mk_simple_symbol s = Z3.Symbol.mk_string !ctx s in
  let get_inner_sort l id = Core.List.nth_exn l id in
  match ve with
  | VE_int n -> Z3.Arithmetic.Integer.mk_numeral_s !ctx (Z.to_string n)
  | VE_string s -> Z3.Seq.mk_string !ctx s
  | VE_bool f -> zexp_of_vformula f
  | VE_unit -> Z3.Expr.mk_const !ctx (mk_simple_symbol "UNIT") unit_sort
  | VE_none t -> begin
      let const_func = Z3.Tuple.get_mk_decl (sort_of_typt t) in
      let inner_sort = get_inner_sort (sort_of_inner_type t) 0 in
      Z3.FuncDecl.apply const_func [(Z3.Enumeration.get_const option_sort 0); (Z3.Expr.mk_const !ctx (mk_simple_symbol "DUMMY") inner_sort)]
    end
  | VE_uni_cont (vuc, e, t) -> begin
      let const_func = Z3.Tuple.get_mk_decl (sort_of_typt t) in
      let inner_sorts = sort_of_inner_type t in
      match vuc with
      | VE_left -> begin
          let right_sort = get_inner_sort inner_sorts 1 in
          Z3.FuncDecl.apply const_func [(Z3.Enumeration.get_const or_sort 0); (zexp_of_vexp e); (Z3.Expr.mk_const !ctx (mk_simple_symbol "DUMMY") right_sort)]
        end
      | VE_right -> begin
          let left_sort = get_inner_sort inner_sorts 0 in
          Z3.FuncDecl.apply const_func [(Z3.Enumeration.get_const or_sort 1); (Z3.Expr.mk_const !ctx (mk_simple_symbol "DUMMY") left_sort); (zexp_of_vexp e)]
        end
      | VE_some -> begin
          Z3.FuncDecl.apply const_func [(Z3.Enumeration.get_const option_sort 1); (zexp_of_vexp e)]
        end
    end
  (*
  | VE_bin_cont of v_bin_cont * v_exp * v_exp
  | VE_list of v_exp list
  | VE_var of var
  | VE_read of v_exp * v_exp (* A[i] in RHS *)
  | VE_write of v_exp * v_exp * v_exp (* A[i] = v *)
  | VE_nul_op of v_nul_op
  | VE_uni_op of v_uni_op * v_exp
  | VE_bin_op of v_bin_op * v_exp * v_exp
  | VE_ter_op of v_tri_op * v_exp * v_exp * v_exp
  | VE_lambda
  | VE_operation of v_operation *)
  | _ -> Z3.Boolean.mk_true !ctx (* DUMMY EXPR *)
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
