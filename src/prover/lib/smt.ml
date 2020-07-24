type typ = Adt.typ
and data = Adt.data
and operation = Adt.operation

type var = Cfg.ident
and exp = Cfg.expr

type v_formula = Vlang.v_formula
and v_exp = Vlang.v_exp

let get_field : 'a list -> int -> 'a
=fun l n -> Core.List.nth_exn l n

let ctx = ref (Z3.mk_context [])


(*****************************************************************************)
(*****************************************************************************)
(* Symbols                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

type z_symbol = Z3.Symbol.symbol

let dummy_tmp = ref 0
let create_dummy_symbol : unit -> z_symbol
=fun () -> begin
  let _ = dummy_tmp := !dummy_tmp + 1 in
  Z3.Symbol.mk_string !ctx ("DUMMY" ^ (string_of_int !dummy_tmp))
end

let create_symbol : string -> z_symbol
=fun s -> Z3.Symbol.mk_string !ctx s


(*****************************************************************************)
(*****************************************************************************)
(* Constructors                                                              *)
(*****************************************************************************)
(*****************************************************************************)

type z_const = Z3.Datatype.Constructor.constructor

let list_nil_const : z_const
= Z3.Datatype.mk_constructor !ctx (create_symbol "Nil") (create_symbol "is_nil") [] [] []


(*****************************************************************************)
(*****************************************************************************)
(* Sorts                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

type z_sort = Z3.Sort.sort


let string_of_sort : z_sort -> string
=fun sort -> Z3.Sort.to_string sort

let create_option_symbol : z_sort -> z_symbol
=fun content_sort -> create_symbol ("Option_(" ^ (string_of_sort content_sort) ^ ")")

let create_pair_symbol : z_sort -> z_sort -> z_symbol
=fun fst_sort snd_sort -> create_symbol ("Pair_(" ^ (string_of_sort fst_sort) ^ ")_(" ^ (string_of_sort snd_sort) ^ ")")

let create_or_symbol : z_sort -> z_sort -> z_symbol
=fun left_sort right_sort -> create_symbol ("Or_(" ^ (string_of_sort left_sort) ^ ")_(" ^ (string_of_sort right_sort) ^ ")")

let create_list_symbol : z_sort -> z_symbol
=fun content_sort -> create_symbol ("List_(" ^ (string_of_sort content_sort) ^ ")")


(* UNINTERPRETED SORTS *)
let create_unit_sort : z_sort
=Z3.Sort.mk_uninterpreted_s !ctx "Unit" 

let create_operation_sort : z_sort
=Z3.Sort.mk_uninterpreted_s !ctx "Operation"

let create_contract_sort : z_sort
=Z3.Sort.mk_uninterpreted_s !ctx "Contract"

let create_lambda_sort : z_sort
=Z3.Sort.mk_uninterpreted_s !ctx "Lambda"


let create_bool_sort : z_sort
=Z3.Boolean.mk_sort !ctx

let create_int_sort : z_sort
=Z3.Arithmetic.Integer.mk_sort !ctx

let create_string_sort : z_sort
=Z3.Seq.mk_string_sort !ctx


let create_option_enum_sort : z_sort
=Z3.Enumeration.mk_sort !ctx (create_symbol "Option_Enum") [(create_symbol "None"); (create_symbol "Some")]

let create_option_sort : z_sort -> z_sort
=fun content_sort -> Z3.Tuple.mk_sort !ctx (create_option_symbol content_sort) [(create_symbol "exist"); (create_symbol "value")] [create_option_enum_sort; content_sort]


let create_pair_sort : z_sort -> z_sort -> z_sort
=fun fst_sort snd_sort -> begin
  let pair_const = Z3.Datatype.mk_constructor !ctx (create_symbol "Pair") (create_symbol "is_pair") [(create_symbol "fst"); (create_symbol "snd")] [(Some fst_sort); (Some snd_sort)] [1; 2] in
  Z3.Datatype.mk_sort !ctx (create_pair_symbol fst_sort snd_sort) [pair_const]
end


let create_or_enum_sort : z_sort
=Z3.Enumeration.mk_sort !ctx (create_symbol "Or_Enum") [(create_symbol "Left"); (create_symbol "Right")]

let create_or_sort : z_sort -> z_sort -> z_sort
=fun left_sort right_sort -> Z3.Tuple.mk_sort !ctx (create_or_symbol left_sort right_sort) [(create_symbol "loc"); (create_symbol "left"); (create_symbol "right")] [create_or_enum_sort; left_sort; right_sort]


let create_list_sort : z_sort -> z_sort
=fun content_sort -> begin
  let list_cons_const = Z3.Datatype.mk_constructor !ctx (create_symbol "Cons") (create_symbol "is_cons") [(create_symbol "head"); (create_symbol "tail")] [(Some content_sort); None] [1; 0] in
  Z3.Datatype.mk_sort !ctx (create_list_symbol content_sort) [list_nil_const; list_cons_const]
end


let create_map_sort : z_sort -> z_sort -> z_sort
=fun key_sort value_sort -> Z3.Z3Array.mk_sort !ctx key_sort value_sort


(*****************************************************************************)
(*****************************************************************************)
(* Expressions                                                               *)
(*****************************************************************************)
(*****************************************************************************)

type ('a, 'b) or_type = | Left of 'a | Right of 'b

type z_expr = Z3.Expr.expr
and z_func = Z3.FuncDecl.func_decl

let string_of_expr : z_expr -> string
=fun e -> Z3.Expr.to_string e

let string_of_func : z_func -> string
=fun f -> Z3.FuncDecl.to_string f


let create_dummy_expr : z_sort -> z_expr
=fun sort -> Z3.Expr.mk_const !ctx (create_dummy_symbol ()) sort

let read_sort_of_expr : z_expr -> z_sort
=fun e -> Z3.Expr.get_sort e

let read_var : z_symbol -> z_sort -> z_expr
=fun v t -> Z3.Expr.mk_const !ctx v t


let create_ite : z_expr -> z_expr -> z_expr -> z_expr
=fun cond true_expr false_expr -> Z3.Boolean.mk_ite !ctx cond true_expr false_expr

let create_cmp : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Arithmetic.Integer.mk_numeral_i !ctx (Z3.Expr.compare e1 e2)


let create_tuple : z_sort -> z_expr list -> z_expr
=fun sort contents -> Z3.FuncDecl.apply (Z3.Tuple.get_mk_decl sort) contents

let create_tuple_enum : z_sort -> int -> z_expr
=fun sort n -> Z3.Enumeration.get_const sort n

let read_tuple_content : z_expr -> int -> z_expr
=fun e n -> begin
  let sort_of_e = read_sort_of_expr e in
  let access_func = get_field (Z3.Tuple.get_field_decls sort_of_e) n in
  Z3.FuncDecl.apply access_func [e]
end


let create_unit : z_expr
=Z3.Expr.mk_const !ctx (create_symbol "UNIT") create_unit_sort


let create_bool_true : z_expr
=Z3.Boolean.mk_true !ctx

let create_bool_false : z_expr
=Z3.Boolean.mk_false !ctx

let create_bool_not : z_expr -> z_expr
=fun e -> Z3.Boolean.mk_not !ctx e

let create_bool_and : z_expr list -> z_expr
=fun el -> Z3.Boolean.mk_and !ctx el

let create_bool_or : z_expr list -> z_expr
=fun el -> Z3.Boolean.mk_or !ctx el

let create_bool_xor : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Boolean.mk_xor !ctx e1 e2

let create_bool_eq : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Boolean.mk_eq !ctx e1 e2

let create_bool_imply : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Boolean.mk_implies !ctx e1 e2

let create_bool_iff : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Boolean.mk_iff !ctx e1 e2

let create_bool_list_is_nil : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_recognizers (read_sort_of_expr e)) 0) [e]

let create_bool_list_is_cons : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_recognizers (read_sort_of_expr e)) 1) [e]

let create_bool_int_lt : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Arithmetic.mk_lt !ctx e1 e2

let create_bool_int_le : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Arithmetic.mk_le !ctx e1 e2

let create_bool_int_gt : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Arithmetic.mk_gt !ctx e1 e2

let create_bool_int_ge : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Arithmetic.mk_ge !ctx e1 e2


let create_int_from_zarith : Z.t -> z_expr
=fun n -> Z3.Arithmetic.Integer.mk_numeral_s !ctx (Z.to_string n)

let create_int : int -> z_expr
=fun n -> Z3.Arithmetic.Integer.mk_numeral_i !ctx n

let create_int_neg : z_expr -> z_expr
=fun e -> Z3.Arithmetic.mk_unary_minus !ctx e

let create_int_add : z_expr list -> z_expr
=fun el -> Z3.Arithmetic.mk_add !ctx el

let create_int_sub : z_expr list -> z_expr
=fun el -> Z3.Arithmetic.mk_sub !ctx el

let create_int_mul : z_expr list -> z_expr
=fun el -> Z3.Arithmetic.mk_mul !ctx el

let create_int_div : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Arithmetic.mk_div !ctx e1 e2

let create_int_mod : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Arithmetic.Integer.mk_mod !ctx e1 e2

let create_int_power : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Arithmetic.mk_power !ctx e1 e2


let create_string : string -> z_expr
=fun s -> Z3.Seq.mk_string !ctx s

let create_string_concat : z_expr list -> z_expr
=fun sl -> Z3.Seq.mk_seq_concat !ctx sl

let create_string_slice : z_expr -> z_expr -> z_expr -> z_expr
=fun s lo hi -> Z3.Seq.mk_seq_extract !ctx s lo hi


let create_option_enum_none : z_expr
=create_tuple_enum create_option_enum_sort 0

let create_option_enum_some : z_expr
=create_tuple_enum create_option_enum_sort 1

let create_option : z_sort -> z_expr option -> z_expr
=fun sort_of_e opt_e -> begin
  match opt_e with
  | None -> create_tuple (create_option_sort sort_of_e) [create_option_enum_none; (create_dummy_expr sort_of_e)]
  | Some e -> create_tuple (create_option_sort sort_of_e) [create_option_enum_some; e]
end

let read_option_exist : z_expr -> z_expr
=fun e -> read_tuple_content e 0

let read_option_content : z_expr -> z_expr
=fun e -> read_tuple_content e 1


let create_pair : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_constructors (create_pair_sort (read_sort_of_expr e1) (read_sort_of_expr e2))) 0) [e1; e2]

let read_pair_fst : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (get_field (Z3.Datatype.get_accessors (read_sort_of_expr e)) 0) 0) [e]

let read_pair_snd : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (get_field (Z3.Datatype.get_accessors (read_sort_of_expr e)) 0) 1) [e]


let create_or_enum_left : z_expr
=create_tuple_enum create_or_enum_sort 0

let create_or_enum_right : z_expr
=create_tuple_enum create_or_enum_sort 1

let create_or : z_sort -> (z_expr, z_expr) or_type -> z_expr
=fun sort_of_dummy or_e -> begin
  let dummy_e = create_dummy_expr sort_of_dummy in
  match or_e with
  | Left e -> create_tuple (create_or_sort (read_sort_of_expr e) sort_of_dummy) [create_or_enum_left; e; dummy_e]
  | Right e -> create_tuple (create_or_sort sort_of_dummy (read_sort_of_expr e)) [create_or_enum_right; dummy_e; e]
end

let read_or_location : z_expr -> z_expr
=fun e -> read_tuple_content e 0

let read_or_left_content : z_expr -> z_expr
=fun e -> read_tuple_content e 1

let read_or_right_content : z_expr -> z_expr
=fun e -> read_tuple_content e 2

let read_or_content : z_expr -> z_expr
=fun e -> create_ite (create_bool_eq (read_or_location e) create_or_enum_left) (read_or_left_content e) (read_or_right_content e)


let create_list : z_sort -> z_expr
=fun item_sort -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_constructors (create_list_sort item_sort)) 0) []

let read_list_head : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (get_field (Z3.Datatype.get_accessors (read_sort_of_expr e)) 1) 0) [e]

let read_list_tail : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (get_field (Z3.Datatype.get_accessors (read_sort_of_expr e)) 1) 1) [e]

let update_list_cons : z_expr -> z_expr -> z_expr
=fun item e -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_constructors (read_sort_of_expr e)) 0) [item; e]


let create_map : z_sort -> z_expr
=fun sort -> begin
  let key_sort, value_sort = ((Z3.Z3Array.get_domain sort), (Z3.Z3Array.get_range sort)) in
  Z3.Z3Array.mk_const !ctx (create_dummy_symbol ()) key_sort value_sort
end

let read_map : z_expr -> z_expr -> z_expr
=fun map key -> Z3.Z3Array.mk_select !ctx map key

let read_default_term : z_expr -> z_expr
=fun map -> Z3.Z3Array.mk_term_array !ctx map

let update_map : z_expr -> z_expr -> z_expr -> z_expr
=fun map key value -> Z3.Z3Array.mk_store !ctx map key value


(*****************************************************************************)
(*****************************************************************************)
(* Solver                                                                    *)
(*****************************************************************************)
(*****************************************************************************)

type solver = Z3.Solver.solver
and model = Z3.Model.model

let create_solver : unit -> solver
=fun () -> Z3.Solver.mk_solver !ctx None

let update_solver_add : solver -> z_expr list -> unit
=fun solver el -> Z3.Solver.add solver el

let create_check : solver -> (bool * model option)
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

let string_of_solver : solver -> string
=fun solver -> Z3.Solver.to_string solver

let string_of_model : model -> string
=fun model -> Z3.Model.to_string model
