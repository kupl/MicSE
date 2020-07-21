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

let dummy_tmp = ref 0
let dummy_symbol () = begin
  let _ = dummy_tmp := !dummy_tmp + 1 in
  Z3.Symbol.mk_string !ctx ("DUMMY" ^ (string_of_int !dummy_tmp))
end

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
  let mk_option_of e sort = begin
    let opt_sort = Z3.Tuple.mk_sort !ctx (mk_simple_symbol "Option") [(mk_simple_symbol "exist"); (mk_simple_symbol "value")] [option_sort; sort] in
    let const_func = Z3.Tuple.get_mk_decl opt_sort in
    match e with
    | None -> Z3.FuncDecl.apply const_func [(Z3.Enumeration.get_const option_sort 0); (Z3.Expr.mk_const !ctx (dummy_symbol ()) sort)]
    | Some x -> Z3.FuncDecl.apply const_func [(Z3.Enumeration.get_const option_sort 1); x]
  end in
  let mk_pair_of e1 e2 = begin
    let pair_sort = Z3.Tuple.mk_sort !ctx (mk_simple_symbol "Pair") [(mk_simple_symbol "fst"); (mk_simple_symbol "snd")] [(Z3.Expr.get_sort e1); (Z3.Expr.get_sort e2)] in
    let const_func = Z3.Tuple.get_mk_decl pair_sort in
    Z3.FuncDecl.apply const_func [e1; e2]
  end in
  let get_nth l id = Core.List.nth_exn l id in
  match ve with
  | VE_int n -> Z3.Arithmetic.Integer.mk_numeral_s !ctx (Z.to_string n)
  | VE_string s -> Z3.Seq.mk_string !ctx s
  | VE_bool f -> zexp_of_vformula f
  | VE_unit -> Z3.Expr.mk_const !ctx (mk_simple_symbol "UNIT") unit_sort
  | VE_none t -> begin
      let const_func = Z3.Tuple.get_mk_decl (sort_of_typt t) in
      let inner_sort = get_nth (sort_of_inner_type t) 0 in
      Z3.FuncDecl.apply const_func [(Z3.Enumeration.get_const option_sort 0); (Z3.Expr.mk_const !ctx (dummy_symbol ()) inner_sort)]
    end
  | VE_uni_cont (vuc, e, t) -> begin
      let const_func = Z3.Tuple.get_mk_decl (sort_of_typt t) in
      let inner_sorts = sort_of_inner_type t in
      match vuc with
      | VE_left -> begin
          let right_sort = get_nth inner_sorts 1 in
          Z3.FuncDecl.apply const_func [(Z3.Enumeration.get_const or_sort 0); (zexp_of_vexp e); (Z3.Expr.mk_const !ctx (dummy_symbol ()) right_sort)]
        end
      | VE_right -> begin
          let left_sort = get_nth inner_sorts 0 in
          Z3.FuncDecl.apply const_func [(Z3.Enumeration.get_const or_sort 1); (Z3.Expr.mk_const !ctx (dummy_symbol ()) left_sort); (zexp_of_vexp e)]
        end
      | VE_some -> begin
          Z3.FuncDecl.apply const_func [(Z3.Enumeration.get_const option_sort 1); (zexp_of_vexp e)]
        end
    end
  | VE_bin_cont (vbc, e1, e2, t) -> begin
    match vbc with
    | VE_pair -> begin
        let const_func = Z3.Tuple.get_mk_decl (sort_of_typt t) in
        Z3.FuncDecl.apply const_func [(zexp_of_vexp e1); (zexp_of_vexp e2)]
      end
    | VE_elt -> begin
        let array_sort = sort_of_typt t in
        let default_array = Z3.Z3Array.mk_const !ctx (dummy_symbol ()) (Z3.Z3Array.get_domain array_sort) (Z3.Z3Array.get_range array_sort) in
        Z3.Z3Array.mk_store !ctx default_array (zexp_of_vexp e1) (zexp_of_vexp e2)
      end
    end
  | VE_list (vel, t) -> begin
      let array_sort = sort_of_typt t in
      let nil = Z3.Z3List.nil array_sort in
      let cons_func = Z3.Z3List.get_cons_decl array_sort in
      Core.List.fold_right vel ~f:(fun e l -> (Z3.FuncDecl.apply cons_func [(zexp_of_vexp e); l])) ~init:nil
    end
  | VE_var (v, t) -> Z3.Expr.mk_const !ctx (mk_simple_symbol v) (sort_of_typt t)
  | VE_read (e1, e2) -> begin
      let idx, arr = ((zexp_of_vexp e1), (zexp_of_vexp e2)) in
      let item = Z3.Z3Array.mk_select !ctx arr idx in
      let sort_of_item = Z3.Expr.get_sort item in
      let default_value = Z3.Z3Array.mk_term_array !ctx arr in
      Z3.Boolean.mk_ite !ctx (Z3.Boolean.mk_eq !ctx item default_value) (mk_option_of None sort_of_item) (mk_option_of (Some item) sort_of_item)
    end
  | VE_write (e1, e2, e3) -> Z3.Z3Array.mk_store !ctx (zexp_of_vexp e3) (zexp_of_vexp e1) (zexp_of_vexp e2)
  | VE_nul_op (vno, t) -> begin
      match vno with
      | VE_self -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
      | VE_now -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
      | VE_amount -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
      | VE_balance -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
      | VE_steps_to_quota -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
      | VE_source -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
      | VE_sender -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
      | VE_chain_id -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
    end
  | VE_uni_op (vuo, e1, t) -> begin
      let zero = Z3.Arithmetic.Integer.mk_numeral_i !ctx 0 in
      match vuo with
      | VE_car -> begin
          let fst_func = get_nth (Z3.Tuple.get_field_decls (sort_of_typt t)) 0 in
          Z3.FuncDecl.apply fst_func [(zexp_of_vexp e1)]
        end
      | VE_cdr -> begin
          let snd_func = get_nth (Z3.Tuple.get_field_decls (sort_of_typt t)) 1 in
          Z3.FuncDecl.apply snd_func [(zexp_of_vexp e1)]
        end
      | VE_abs -> begin
          let ze1 = zexp_of_vexp e1 in
          Z3.Boolean.mk_ite !ctx (Z3.Arithmetic.mk_ge !ctx ze1 zero) ze1 (Z3.Arithmetic.mk_unary_minus !ctx ze1)
        end
      | VE_neg -> Z3.Arithmetic.mk_unary_minus !ctx (zexp_of_vexp e1)
      | VE_not -> Z3.Boolean.mk_not !ctx (zexp_of_vexp e1)
      | VE_eq -> Z3.Boolean.mk_eq !ctx (zexp_of_vexp e1) zero
      | VE_neq -> Z3.Boolean.mk_not !ctx (Z3.Boolean.mk_eq !ctx (zexp_of_vexp e1) zero)
      | VE_lt -> Z3.Arithmetic.mk_lt !ctx (zexp_of_vexp e1) zero
      | VE_gt -> Z3.Arithmetic.mk_gt !ctx (zexp_of_vexp e1) zero
      | VE_leq -> Z3.Arithmetic.mk_le !ctx (zexp_of_vexp e1) zero
      | VE_geq -> Z3.Arithmetic.mk_ge !ctx (zexp_of_vexp e1) zero
      | VE_cast -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
      | VE_concat -> Z3.Seq.mk_seq_concat !ctx [(zexp_of_vexp e1)]
      | VE_pack -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
      | VE_unpack -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
      | VE_contract -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
      | VE_account -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
      | VE_blake2b -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
      | VE_sha256 -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
      | VE_sha512 -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
      | VE_hash_key -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
      | VE_address -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
      | VE_un_opt -> begin
          let value_func = get_nth (Z3.Tuple.get_field_decls (sort_of_typt t)) 1 in
          Z3.FuncDecl.apply value_func [(zexp_of_vexp e1)]
        end
      | VE_un_or -> begin
          let ze = zexp_of_vexp e1 in
          let loc_func = get_nth (Z3.Tuple.get_field_decls (sort_of_typt t)) 0 in
          let left_func = get_nth (Z3.Tuple.get_field_decls (sort_of_typt t)) 1 in
          let right_func = get_nth (Z3.Tuple.get_field_decls (sort_of_typt t)) 2 in
          let loc = Z3.FuncDecl.apply loc_func [ze] in
          Z3.Boolean.mk_ite !ctx (Z3.Boolean.mk_eq !ctx loc (Z3.Enumeration.get_const or_sort 0)) (Z3.FuncDecl.apply left_func [ze]) (Z3.FuncDecl.apply right_func [ze])
        end
      | VE_hd -> begin
          let hd_func = Z3.Z3List.get_head_decl (sort_of_typt t) in
          Z3.FuncDecl.apply hd_func [(zexp_of_vexp e1)]
        end
      | VE_tl -> begin
          let tl_func = Z3.Z3List.get_tail_decl (sort_of_typt t) in
          Z3.FuncDecl.apply tl_func [(zexp_of_vexp e1)]
        end
      | VE_size -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
      | VE_isnat -> Z3.Boolean.mk_const !ctx (dummy_symbol ())
      | VE_int -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
    end
  | VE_bin_op (vbo, e1, e2, t) -> begin
      let zero = Z3.Arithmetic.Integer.mk_numeral_i !ctx 0 in
      let two = Z3.Arithmetic.Integer.mk_numeral_i !ctx 2 in
      match vbo with
      | VE_add -> Z3.Arithmetic.mk_add !ctx [(zexp_of_vexp e1); (zexp_of_vexp e2)]
      | VE_sub -> Z3.Arithmetic.mk_sub !ctx [(zexp_of_vexp e1); (zexp_of_vexp e2)]
      | VE_mul -> Z3.Arithmetic.mk_mul !ctx [(zexp_of_vexp e1); (zexp_of_vexp e2)]
      | VE_ediv -> begin
          let dividend, divisor = ((zexp_of_vexp e1), (zexp_of_vexp e2)) in
          let x = mk_pair_of (Z3.Arithmetic.mk_div !ctx dividend divisor) (Z3.Arithmetic.Integer.mk_mod !ctx dividend divisor) in
          let sort_of_x = Z3.Expr.get_sort x in
          Z3.Boolean.mk_ite !ctx (Z3.Boolean.mk_eq !ctx divisor zero) (mk_option_of None sort_of_x) (mk_option_of (Some x) sort_of_x)
        end
      | VE_div -> Z3.Arithmetic.mk_div !ctx (zexp_of_vexp e1) (zexp_of_vexp e2)
      | VE_mod -> Z3.Arithmetic.Integer.mk_mod !ctx (zexp_of_vexp e1) (zexp_of_vexp e2)
      | VE_lsl -> begin
          let powered = Z3.Arithmetic.mk_power !ctx two (zexp_of_vexp e2) in
          Z3.Arithmetic.mk_mul !ctx [(zexp_of_vexp e1); powered]
        end
      | VE_lsr -> begin
          let powered = Z3.Arithmetic.mk_power !ctx two (zexp_of_vexp e2) in
          Z3.Arithmetic.mk_div !ctx (zexp_of_vexp e1) powered
        end
      | VE_and -> Z3.Boolean.mk_and !ctx [(zexp_of_vexp e1); (zexp_of_vexp e2)]
      | VE_or -> Z3.Boolean.mk_or !ctx [(zexp_of_vexp e1); (zexp_of_vexp e2)]
      | VE_xor -> Z3.Boolean.mk_xor !ctx (zexp_of_vexp e1) (zexp_of_vexp e2)
      | VE_cmp -> Z3.Arithmetic.Integer.mk_numeral_i !ctx (Z3.Expr.compare (zexp_of_vexp e1) (zexp_of_vexp e2))
      | VE_cons -> begin
          let cons_func = Z3.Z3List.get_cons_decl (sort_of_typt t) in
          Z3.FuncDecl.apply cons_func [(zexp_of_vexp e1); (zexp_of_vexp e2)]
        end
      | VE_concat -> Z3.Seq.mk_seq_concat !ctx [(zexp_of_vexp e1); (zexp_of_vexp e2)]
      | VE_exec -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
      | VE_append -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
    end
  | VE_ter_op (vto, e1, e2, e3, t) -> begin
      match vto with
      | VE_slice -> begin
          let offset, length, s = ((zexp_of_vexp e1), (zexp_of_vexp e2), (zexp_of_vexp e3)) in
          Z3.Seq.mk_seq_extract !ctx s offset (Z3.Arithmetic.mk_add !ctx [offset; length])
        end
      | VE_check_signature -> Z3.Expr.mk_const !ctx (dummy_symbol ()) (sort_of_typt t)
    end
  (*
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
