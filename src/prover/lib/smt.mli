type typ = Adt.typ
and data = Adt.data
and operation = Adt.operation

type var = Cfg.ident
and exp = Cfg.expr

type v_formula = Vlang.v_formula
and v_exp = Vlang.v_exp


(*****************************************************************************)
(*****************************************************************************)
(* Symbols                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

type z_symbol = Z3.Symbol.symbol

val dummy_tmp : int ref
val create_dummy_symbol : unit -> z_symbol

val create_symbol : string -> z_symbol


(*****************************************************************************)
(*****************************************************************************)
(* Constructors                                                              *)
(*****************************************************************************)
(*****************************************************************************)

type z_const = Z3.Datatype.Constructor.constructor

val option_none_const : z_const

val list_nil_const : z_const


(*****************************************************************************)
(*****************************************************************************)
(* Sorts                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

type z_sort = Z3.Sort.sort


val string_of_sort : z_sort -> string

val create_option_symbol : z_sort -> z_symbol

val create_pair_symbol : z_sort -> z_sort -> z_symbol

val create_or_symbol : z_sort -> z_sort -> z_symbol

val create_list_symbol : z_sort -> z_symbol


val create_unit_sort : z_sort

val create_operation_sort : z_sort

val create_contract_sort : z_sort

val create_lambda_sort : z_sort


val create_bool_sort : z_sort

val create_int_sort : z_sort

val create_string_sort : z_sort


val create_option_sort : z_sort -> z_sort


val create_pair_sort : z_sort -> z_sort -> z_sort


val create_or_sort : z_sort -> z_sort -> z_sort


val create_list_sort : z_sort -> z_sort


val create_map_sort : z_sort -> z_sort -> z_sort


(*****************************************************************************)
(*****************************************************************************)
(* Expressions                                                               *)
(*****************************************************************************)
(*****************************************************************************)

type ('a, 'b) or_type = | Left of 'a | Right of 'b

type z_expr = Z3.Expr.expr
and z_func = Z3.FuncDecl.func_decl

val string_of_expr : z_expr -> string

val string_of_func : z_func -> string


val create_dummy_expr : z_sort -> z_expr

val read_sort_of_expr : z_expr -> z_sort

val read_var : z_symbol -> z_sort -> z_expr


val create_ite : z_expr -> z_expr -> z_expr -> z_expr

val create_cmp : z_expr -> z_expr -> z_expr


val create_unit : z_expr


val create_bool_true : z_expr

val create_bool_false : z_expr

val create_bool_not : z_expr -> z_expr

val create_bool_and : z_expr list -> z_expr

val create_bool_or : z_expr list -> z_expr

val create_bool_xor : z_expr -> z_expr -> z_expr

val create_bool_eq : z_expr -> z_expr -> z_expr

val create_bool_imply : z_expr -> z_expr -> z_expr

val create_bool_iff : z_expr -> z_expr -> z_expr

val create_bool_list_is_nil : z_expr -> z_expr

val create_bool_list_is_cons : z_expr -> z_expr

val create_bool_int_lt : z_expr -> z_expr -> z_expr

val create_bool_int_le : z_expr -> z_expr -> z_expr

val create_bool_int_gt : z_expr -> z_expr -> z_expr

val create_bool_int_ge : z_expr -> z_expr -> z_expr

val create_bool_option_is_none : z_expr -> z_expr

val create_bool_option_is_some : z_expr -> z_expr

val create_bool_option_is_left : z_expr -> z_expr

val create_bool_option_is_right : z_expr -> z_expr


val create_int_from_zarith : Z.t -> z_expr

val create_int : int -> z_expr

val create_int_neg : z_expr -> z_expr

val create_int_add : z_expr list -> z_expr

val create_int_sub : z_expr list -> z_expr

val create_int_mul : z_expr list -> z_expr

val create_int_div : z_expr -> z_expr -> z_expr

val create_int_mod : z_expr -> z_expr -> z_expr

val create_int_power : z_expr -> z_expr -> z_expr


val create_string : string -> z_expr

val create_string_concat : z_expr list -> z_expr

val create_string_slice : z_expr -> z_expr -> z_expr -> z_expr


val create_option : z_sort -> z_expr option -> z_expr

val read_option_content : z_expr -> z_expr


val create_pair : z_expr -> z_expr -> z_expr

val read_pair_fst : z_expr -> z_expr

val read_pair_snd : z_expr -> z_expr


val create_or : z_sort -> (z_expr, z_expr) or_type -> z_expr

val read_or_left_content : z_expr -> z_expr

val read_or_right_content : z_expr -> z_expr

val read_or_content : z_expr -> z_expr


val create_list : z_sort -> z_expr

val read_list_head : z_expr -> z_expr

val read_list_tail : z_expr -> z_expr

val update_list_cons : z_expr -> z_expr -> z_expr


val create_map : z_sort -> z_expr

val read_map : z_expr -> z_expr -> z_expr

val read_default_term : z_expr -> z_expr

val update_map : z_expr -> z_expr -> z_expr -> z_expr


(*****************************************************************************)
(*****************************************************************************)
(* Solver                                                                    *)
(*****************************************************************************)
(*****************************************************************************)

type solver = Z3.Solver.solver
and model = Z3.Model.model

val create_solver : unit -> solver

val update_solver_add : solver -> z_expr list -> unit

val create_check : solver -> (bool * model option)

val string_of_solver : solver -> string

val string_of_model : model -> string