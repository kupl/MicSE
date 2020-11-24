(*
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


(*****************************************************************************)
(*****************************************************************************)
(* Context                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

module Ctx = struct
  type t = Z3.context
  type component = (string * string)

  let obj : t ref
  =ref (Z3.mk_context []) 

  let create_timeout : unit -> component
  =fun () -> begin
    let budget = !Utils.Options.z3_time_budget * 1000 in
    ("timeout", (string_of_int (budget)))
  end

  let create : unit -> unit
  =fun () -> begin
    let c = [] in
    let c = (create_timeout ())::c in
    obj := Z3.mk_context c
  end
  
  let read : t
  =(!obj)
end


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
  Z3.Symbol.mk_string (Ctx.read) ("DUMMY" ^ (string_of_int !dummy_tmp))
end

let create_symbol : string -> z_symbol
=fun s -> Z3.Symbol.mk_string (Ctx.read) s


(*****************************************************************************)
(*****************************************************************************)
(* Constructors                                                              *)
(*****************************************************************************)
(*****************************************************************************)

type z_const = Z3.Datatype.Constructor.constructor

let option_none_const : z_const
= Z3.Datatype.mk_constructor (Ctx.read) (create_symbol "None") (create_symbol "is_none") [] [] []

let list_nil_const : z_const
= Z3.Datatype.mk_constructor (Ctx.read) (create_symbol "Nil") (create_symbol "is_nil") [] [] []


(*****************************************************************************)
(*****************************************************************************)
(* Sorts                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

type z_sort = Z3.Sort.sort


let read_constructor_domain_sort : z_sort -> const_idx:int -> sort_idx:int -> z_sort
=fun const_sort ~const_idx ~sort_idx -> begin
  get_field (Z3.FuncDecl.get_domain (get_field (Z3.Datatype.get_constructors const_sort) const_idx)) sort_idx
end


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

let create_elt_symbol : key_sort:z_sort -> value_sort:z_sort -> z_symbol
=fun ~key_sort ~value_sort -> create_symbol ("Elt_(" ^ (string_of_sort key_sort) ^ ")_(" ^ (string_of_sort value_sort) ^ ")")

let create_map_symbol : key_sort:z_sort -> value_sort:z_sort -> z_symbol
=fun ~key_sort ~value_sort -> create_symbol ("Map_(" ^ (string_of_sort key_sort) ^ ")_(" ^ (string_of_sort value_sort) ^ ")")


(* UNINTERPRETED SORTS *)
let create_unit_sort : z_sort
=Z3.Sort.mk_uninterpreted_s (Ctx.read) "Unit" 

let create_operation_sort : z_sort
=Z3.Sort.mk_uninterpreted_s (Ctx.read) "Operation"

let create_contract_sort : z_sort
=Z3.Sort.mk_uninterpreted_s (Ctx.read) "Contract"

let create_lambda_sort : z_sort
=Z3.Sort.mk_uninterpreted_s (Ctx.read) "Lambda"


let create_bool_sort : z_sort
=Z3.Boolean.mk_sort (Ctx.read)

let create_int_sort : z_sort
=Z3.Arithmetic.Integer.mk_sort (Ctx.read)

let create_string_sort : z_sort
=Z3.Seq.mk_string_sort (Ctx.read)

let create_mutez_sort : z_sort
=Z3.BitVector.mk_sort (Ctx.read) 63


let create_option_sort : z_sort -> z_sort
=fun content_sort -> begin
  let option_some_const = Z3.Datatype.mk_constructor (Ctx.read) (create_symbol "Some") (create_symbol "is_some") [(create_symbol "content")] [(Some content_sort)] [1] in
  Z3.Datatype.mk_sort (Ctx.read) (create_option_symbol content_sort) [option_none_const; option_some_const]
end


let create_pair_sort : z_sort -> z_sort -> z_sort
=fun fst_sort snd_sort -> begin
  let pair_const = Z3.Datatype.mk_constructor (Ctx.read) (create_symbol "Pair") (create_symbol "is_pair") [(create_symbol "fst"); (create_symbol "snd")] [(Some fst_sort); (Some snd_sort)] [1; 2] in
  Z3.Datatype.mk_sort (Ctx.read) (create_pair_symbol fst_sort snd_sort) [pair_const]
end


let create_or_sort : z_sort -> z_sort -> z_sort
=fun left_sort right_sort -> begin
  let or_left_const = Z3.Datatype.mk_constructor (Ctx.read) (create_symbol "Left") (create_symbol "is_left") [(create_symbol "content")] [(Some left_sort)] [1] in
  let or_right_const = Z3.Datatype.mk_constructor (Ctx.read) (create_symbol "Right") (create_symbol "is_right") [(create_symbol "content")] [(Some right_sort)] [1] in
  Z3.Datatype.mk_sort (Ctx.read) (create_or_symbol left_sort right_sort) [or_left_const; or_right_const]
end


let create_list_sort : z_sort -> z_sort
=fun content_sort -> begin
  let list_cons_const = Z3.Datatype.mk_constructor (Ctx.read) (create_symbol "Cons") (create_symbol "is_cons") [(create_symbol "head"); (create_symbol "tail")] [(Some content_sort); None] [1; 0] in
  Z3.Datatype.mk_sort (Ctx.read) (create_list_symbol content_sort) [list_nil_const; list_cons_const]
end


let create_elt_sort : key_sort:z_sort -> value_sort:z_sort -> z_sort
=fun ~key_sort ~value_sort -> begin
  let elt_const = Z3.Datatype.mk_constructor (Ctx.read) (create_symbol "Elt") (create_symbol "is_elt") [(create_symbol "key"); (create_symbol "value")] [Some key_sort; Some value_sort] [1; 2] in
  Z3.Datatype.mk_sort (Ctx.read) (create_elt_symbol ~key_sort:key_sort ~value_sort:value_sort) [elt_const]
end

let create_map_sort : elt_sort:z_sort -> z_sort
=fun ~elt_sort -> begin
  let key_sort = read_constructor_domain_sort elt_sort ~const_idx:0 ~sort_idx:0 in
  let value_sort = read_constructor_domain_sort elt_sort ~const_idx:0 ~sort_idx:1 in
  Z3.Z3Array.mk_sort (Ctx.read) key_sort value_sort
end


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
=fun sort -> Z3.Expr.mk_const (Ctx.read) (create_dummy_symbol ()) sort

let read_sort_of_expr : z_expr -> z_sort
=fun e -> Z3.Expr.get_sort e

let read_var : z_symbol -> z_sort -> z_expr
=fun v t -> Z3.Expr.mk_const (Ctx.read) v t


let create_ite : z_expr -> z_expr -> z_expr -> z_expr
=fun cond true_expr false_expr -> Z3.Boolean.mk_ite (Ctx.read) cond true_expr false_expr


let create_unit : z_expr
=Z3.Expr.mk_const (Ctx.read) (create_symbol "UNIT") create_unit_sort


let create_forall : z_expr list -> z_expr -> z_expr
=fun vl f -> Z3.Quantifier.expr_of_quantifier (Z3.Quantifier.mk_forall_const (Ctx.read) vl f None [] [] None None)


let create_bool_true : z_expr
=Z3.Boolean.mk_true (Ctx.read)

let create_bool_false : z_expr
=Z3.Boolean.mk_false (Ctx.read)

let create_bool_not : z_expr -> z_expr
=fun e -> Z3.Boolean.mk_not (Ctx.read) e

let create_bool_and : z_expr list -> z_expr
=fun el -> Z3.Boolean.mk_and (Ctx.read) el

let create_bool_or : z_expr list -> z_expr
=fun el -> Z3.Boolean.mk_or (Ctx.read) el

let create_bool_xor : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Boolean.mk_xor (Ctx.read) e1 e2

let create_bool_eq : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Boolean.mk_eq (Ctx.read) e1 e2

let create_bool_imply : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Boolean.mk_implies (Ctx.read) e1 e2

let create_bool_iff : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Boolean.mk_iff (Ctx.read) e1 e2

let create_bool_list_is_nil : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_recognizers (read_sort_of_expr e)) 0) [e]

let create_bool_list_is_cons : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_recognizers (read_sort_of_expr e)) 1) [e]

let create_bool_int_lt : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Arithmetic.mk_lt (Ctx.read) e1 e2

let create_bool_int_le : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Arithmetic.mk_le (Ctx.read) e1 e2

let create_bool_int_gt : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Arithmetic.mk_gt (Ctx.read) e1 e2

let create_bool_int_ge : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Arithmetic.mk_ge (Ctx.read) e1 e2

let create_bool_mutez_lt : v1:z_expr -> v2:z_expr -> z_expr (* v1 < v2 *)
=fun ~v1 ~v2 -> Z3.BitVector.mk_ult (Ctx.read) v1 v2

let create_bool_mutez_le : v1:z_expr -> v2:z_expr -> z_expr (* v1 ≦ v2 *)
=fun ~v1 ~v2 -> Z3.BitVector.mk_ule (Ctx.read) v1 v2

let create_bool_mutez_gt : v1:z_expr -> v2:z_expr -> z_expr (* v1 > v2 *)
=fun ~v1 ~v2 -> Z3.BitVector.mk_ugt (Ctx.read) v1 v2

let create_bool_mutez_ge : v1:z_expr -> v2:z_expr -> z_expr (* v1 ≧ v2 *)
=fun ~v1 ~v2 -> Z3.BitVector.mk_uge (Ctx.read) v1 v2

let create_bool_option_is_none : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_recognizers (read_sort_of_expr e)) 0) [e]

let create_bool_option_is_some : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_recognizers (read_sort_of_expr e)) 1) [e]

let create_bool_option_is_left : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_recognizers (read_sort_of_expr e)) 0) [e]

let create_bool_option_is_right : z_expr -> z_expr
=fun e -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_recognizers (read_sort_of_expr e)) 1) [e]


let create_int_from_zarith : Z.t -> z_expr
=fun n -> Z3.Arithmetic.Integer.mk_numeral_s (Ctx.read) (Z.to_string n)

let create_int : int -> z_expr
=fun n -> Z3.Arithmetic.Integer.mk_numeral_i (Ctx.read) n

let create_int_neg : z_expr -> z_expr
=fun e -> Z3.Arithmetic.mk_unary_minus (Ctx.read) e

let create_int_add : z_expr list -> z_expr
=fun el -> Z3.Arithmetic.mk_add (Ctx.read) el

let create_int_sub : z_expr list -> z_expr
=fun el -> Z3.Arithmetic.mk_sub (Ctx.read) el

let create_int_mul : z_expr list -> z_expr
=fun el -> Z3.Arithmetic.mk_mul (Ctx.read) el

let create_int_div : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Arithmetic.mk_div (Ctx.read) e1 e2

let create_int_mod : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Arithmetic.Integer.mk_mod (Ctx.read) e1 e2

let create_int_power : z_expr -> z_expr -> z_expr
=fun e1 e2 -> Z3.Arithmetic.mk_power (Ctx.read) e1 e2


let create_string : string -> z_expr
=fun s -> Z3.Seq.mk_string (Ctx.read) s

let create_string_concat : z_expr list -> z_expr
=fun sl -> Z3.Seq.mk_seq_concat (Ctx.read) sl

let create_string_slice : z_expr -> z_expr -> z_expr -> z_expr
=fun s lo hi -> Z3.Seq.mk_seq_extract (Ctx.read) s lo hi


let create_mutez_from_zarith : value:Z.t -> z_expr
=fun ~value -> Z3.BitVector.mk_numeral (Ctx.read) (Z.to_string value) 63

let create_mutez : value:int -> z_expr
=fun ~value -> Z3.BitVector.mk_numeral (Ctx.read) (string_of_int value) 63

let create_mutez_add : v1:z_expr -> v2:z_expr -> z_expr
=fun ~v1 ~v2 -> Z3.BitVector.mk_add (Ctx.read) v1 v2

let create_mutez_sub : v1:z_expr -> v2:z_expr -> z_expr
=fun ~v1 ~v2 -> Z3.BitVector.mk_sub (Ctx.read) v1 v2

let create_mutez_mul : v1:z_expr -> v2:z_expr -> z_expr
=fun ~v1 ~v2 -> Z3.BitVector.mk_mul (Ctx.read) v1 v2

let create_mutez_div : v1:z_expr -> v2:z_expr -> z_expr
=fun ~v1 ~v2 -> Z3.BitVector.mk_udiv (Ctx.read) v1 v2

let create_mutez_mod : v1:z_expr -> v2:z_expr -> z_expr
=fun ~v1 ~v2 -> Z3.BitVector.mk_urem (Ctx.read) v1 v2


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
=fun item e -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_constructors (read_sort_of_expr e)) 1) [item; e]


let create_elt : key:z_expr -> value:z_expr -> z_expr
=fun ~key ~value -> Z3.FuncDecl.apply (get_field (Z3.Datatype.get_constructors (create_elt_sort ~key_sort:(read_sort_of_expr key) ~value_sort:(read_sort_of_expr value))) 0) [key; value]

let read_elt_key : elt:z_expr -> z_expr
=fun ~elt -> Z3.FuncDecl.apply (get_field (get_field (Z3.Datatype.get_accessors (read_sort_of_expr elt)) 0) 0) [elt]

let read_elt_value : elt:z_expr -> z_expr
=fun ~elt -> Z3.FuncDecl.apply (get_field (get_field (Z3.Datatype.get_accessors (read_sort_of_expr elt)) 0) 1) [elt]


let create_map : key_sort:z_sort -> value_sort:z_sort -> z_expr
=fun ~key_sort ~value_sort -> Z3.Z3Array.mk_const (Ctx.read) (create_map_symbol ~key_sort:key_sort ~value_sort:value_sort) key_sort value_sort

let read_map_elt_content : key:z_expr -> map:z_expr -> z_expr
=fun ~key ~map -> begin
  let map_sort = (read_sort_of_expr map) in
  let value_sort = Z3.Z3Array.get_range map_sort in
  let value = Z3.Z3Array.mk_select (Ctx.read) map key in
  let default_value = Z3.Z3Array.mk_term_array (Ctx.read) map in
  create_ite
    (create_bool_eq value default_value)
    (create_option value_sort None)
    (create_option value_sort (Some value))
end

let read_map_elt_exists : key:z_expr -> map:z_expr -> z_expr
=fun ~key ~map -> begin
  let value = Z3.Z3Array.mk_select (Ctx.read) map key in
  let default_value = Z3.Z3Array.mk_term_array (Ctx.read) map in
  create_ite
    (create_bool_eq value default_value)
    (create_bool_false)
    (create_bool_true)
end

let read_map_sigma : map:z_expr -> z_expr
=fun ~map -> Z3.Expr.mk_const (Ctx.read) (create_symbol ("Sigma_of_" ^ (string_of_expr map))) (create_int_sort)

let update_map : key:z_expr -> value_opt:z_expr -> map:z_expr -> z_expr
=fun ~key ~value_opt ~map -> begin
  let value = read_option_content value_opt in
  let default_value = Z3.Z3Array.mk_term_array (Ctx.read) map in
  create_ite
    (create_bool_option_is_none value_opt)
    (Z3.Z3Array.mk_store (Ctx.read) map key default_value)
    (Z3.Z3Array.mk_store (Ctx.read) map key value)
end

let create_int_cmp : v1:z_expr -> v2:z_expr -> z_expr
=fun ~v1 ~v2 -> create_ite (create_bool_int_lt v1 v2) (create_int (-1)) (create_ite (create_bool_int_gt v1 v2) (create_int 1) (create_int 0))

let create_mutez_cmp : v1:z_expr -> v2:z_expr -> z_expr
=fun ~v1 ~v2 -> create_ite (create_bool_mutez_lt ~v1:v1 ~v2:v2) (create_int (-1)) (create_ite (create_bool_mutez_gt ~v1:v1 ~v2:v2) (create_int 1) (create_int 0))

let create_string_cmp : v1:z_expr -> v2:z_expr -> z_expr
=fun ~v1 ~v2 -> create_ite (create_bool_eq v1 v2) (create_int 0) (create_int 1) (* String compare is not supported in z3 *)

(*****************************************************************************)
(*****************************************************************************)
(* Solver                                                                    *)
(*****************************************************************************)
(*****************************************************************************)

type solver = Z3.Solver.solver
and model = Z3.Model.model

let create_solver : unit -> solver
=fun () -> Z3.Solver.mk_solver (Ctx.read) None

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
*)