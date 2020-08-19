type typ = Pre.Lib.Adt.typ
and data = Pre.Lib.Adt.data
and operation = Pre.Lib.Cfg.operation

type var = Pre.Lib.Cfg.ident
and exp = Pre.Lib.Cfg.expr

type v_formula = Vlang.v_formula
and v_exp = Vlang.v_exp

exception Z3Error = Z3.Error

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

let option_none_const : z_const
= Z3.Datatype.mk_constructor !ctx (create_symbol "None") (create_symbol "is_none") [] [] []

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


let create_option_sort : z_sort -> z_sort
=fun content_sort -> begin
  let option_some_const = Z3.Datatype.mk_constructor !ctx (create_symbol "Some") (create_symbol "is_some") [(create_symbol "content")] [(Some content_sort)] [1] in
  Z3.Datatype.mk_sort !ctx (create_option_symbol content_sort) [option_none_const; option_some_const]
end


let create_pair_sort : z_sort -> z_sort -> z_sort
=fun fst_sort snd_sort -> begin
  let pair_const = Z3.Datatype.mk_constructor !ctx (create_symbol "Pair") (create_symbol "is_pair") [(create_symbol "fst"); (create_symbol "snd")] [(Some fst_sort); (Some snd_sort)] [1; 2] in
  Z3.Datatype.mk_sort !ctx (create_pair_symbol fst_sort snd_sort) [pair_const]
end


let create_or_sort : z_sort -> z_sort -> z_sort
=fun left_sort right_sort -> begin
  let or_left_const = Z3.Datatype.mk_constructor !ctx (create_symbol "Left") (create_symbol "is_left") [(create_symbol "content")] [(Some left_sort)] [1] in
  let or_right_const = Z3.Datatype.mk_constructor !ctx (create_symbol "Right") (create_symbol "is_right") [(create_symbol "content")] [(Some right_sort)] [1] in
  Z3.Datatype.mk_sort !ctx (create_or_symbol left_sort right_sort) [or_left_const; or_right_const]
end


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


let create_unit : z_expr
=Z3.Expr.mk_const !ctx (create_symbol "UNIT") create_unit_sort


let create_forall : z_expr list -> z_expr -> z_expr
=fun vl f -> Z3.Quantifier.expr_of_quantifier (Z3.Quantifier.mk_forall_const !ctx vl f None [] [] None None)


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

let create_bool_option_is_none : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_recognizers (read_sort_of_expr e)) 0) [e]

let create_bool_option_is_some : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_recognizers (read_sort_of_expr e)) 1) [e]

let create_bool_option_is_left : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_recognizers (read_sort_of_expr e)) 0) [e]

let create_bool_option_is_right : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_recognizers (read_sort_of_expr e)) 1) [e]


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


let create_option : z_sort -> z_expr option -> z_expr
=fun sort_of_e opt_e -> begin
  match opt_e with
  | None -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_constructors (create_option_sort sort_of_e)) 0) []
  | Some e -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_constructors (create_option_sort sort_of_e)) 1) [e]
end

let read_option_content : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (get_field (Z3.Datatype.get_accessors (read_sort_of_expr e)) 1) 0) [e]


let create_pair : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_constructors (create_pair_sort (read_sort_of_expr e1) (read_sort_of_expr e2))) 0) [e1; e2]

let read_pair_fst : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (get_field (Z3.Datatype.get_accessors (read_sort_of_expr e)) 0) 0) [e]

let read_pair_snd : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (get_field (Z3.Datatype.get_accessors (read_sort_of_expr e)) 0) 1) [e]


let create_or : z_sort -> (z_expr, z_expr) or_type -> z_expr
=fun sort_of_dummy or_e -> begin
  match or_e with
  | Left e -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_constructors (create_or_sort (read_sort_of_expr e) sort_of_dummy)) 0) [e]
  | Right e -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_constructors (create_or_sort sort_of_dummy (read_sort_of_expr e))) 1) [e]
end

let read_or_left_content : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (get_field (Z3.Datatype.get_accessors (read_sort_of_expr e)) 0) 0) [e]

let read_or_right_content : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (get_field (Z3.Datatype.get_accessors (read_sort_of_expr e)) 1) 0) [e]


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


let create_cmp : z_expr -> z_expr -> z_expr
=fun e1 e2 -> create_ite (create_bool_int_lt e1 e2) (create_int (-1)) (create_ite (create_bool_int_gt e1 e2) (create_int 1) (create_int 0))


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


(*****************************************************************************)
(*****************************************************************************)
(* Model                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

let create_evaluation : model -> z_expr -> z_expr option
=fun m e -> Z3.Model.eval m e true

let string_of_model : model -> string
=fun m -> Z3.Model.to_string m
