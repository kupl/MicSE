exception ZError of string

module CONST : sig
  val _name_dummy : string
  val _name_unit : string
  val _name_map : string

  val _sort_unit : string
  val _sort_operation : string
  val _sort_contract : string
  val _sort_lambda : string
  val _sort_bytes : string
  val _sort_option : string
  val _sort_pair : string
  val _sort_or : string
  val _sort_list : string

  val _const_bytes_bytstr : string
  val _const_bytes_pack : string
  val _const_bytes_concatenated : string
  val _const_bytes_sliced : string
  val _const_bytes_blake2b : string
  val _const_bytes_sha256 : string
  val _const_bytes_sha512 : string
  val _const_option_none : string
  val _const_option_some : string
  val _const_pair : string
  val _const_or_left : string
  val _const_or_right : string
  val _const_list_nil : string
  val _const_list_cons : string

  val _recog_bytes_bytstr : string
  val _recog_bytes_pack : string
  val _recog_bytes_concatenated : string
  val _recog_bytes_sliced : string
  val _recog_bytes_blake2b : string
  val _recog_bytes_sha256 : string
  val _recog_bytes_sha512 : string
  val _recog_option_none : string
  val _recog_option_some : string
  val _recog_pair : string
  val _recog_or_left : string
  val _recog_or_right : string
  val _recog_list_nil : string
  val _recog_list_cons : string

  val _field_content : string
  val _field_pair_fst : string
  val _field_pair_snd : string
  val _field_list_head : string
  val _field_list_tail : string

  val _bit_mutez : int
end

(*****************************************************************************)
(*****************************************************************************)
(* Context                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

module ZCtx : sig
  type body = (string * string)
  type t = Z3.context
  type t_ref = t option ref

  val _obj : t_ref

  val body_timeout : unit -> body
  val create : unit -> unit
  val read : unit -> t
end


(*****************************************************************************)
(*****************************************************************************)
(* Symbols                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

module ZSym : sig
  type t = Z3.Symbol.symbol

  val _name_dummy : string
  val _count_dummy : int ref

  val create : string -> t
  val create_dummy : unit -> t
  
  val to_string : t -> string
end


(*****************************************************************************)
(*****************************************************************************)
(* Sorts                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

module ZSort : sig
  type t = Z3.Sort.sort

  val create_dummy : unit -> t
  val create : name:string -> t

  val to_string : t -> string
end


(*****************************************************************************)
(*****************************************************************************)
(* Expressions                                                               *)
(*****************************************************************************)
(*****************************************************************************)

module ZExpr : sig
  type t = Z3.Expr.expr

  val create_dummy : ZSort.t -> t
  val create_var : ZSort.t -> name:string -> t

  val create_ite : cond:t -> t:t -> f:t -> t

  val read_sort : t -> ZSort.t

  val to_string : t -> string
end


(*****************************************************************************)
(*****************************************************************************)
(* FuncDecls                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

module ZFunc : sig
  type t = Z3.FuncDecl.func_decl

  val get_idx : 'a list -> idx:int -> 'a

  val apply : t -> params:ZExpr.t list -> ZExpr.t

  val sort_of_domain : t -> idx:int -> ZSort.t
end


(*****************************************************************************)
(*****************************************************************************)
(* Datatypes                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

module ZDatatype : sig
  type const = Z3.Datatype.Constructor.constructor

  val get_idx : 'a list -> idx:int -> 'a

  val create_const : name:string -> recog_func_name:string -> field_names:string list -> field_sorts:ZSort.t option list -> field_sort_refs:int list -> const
  val create_sort : name:string -> const_list:const list -> ZSort.t
  val create_const_func : ZSort.t -> const_idx:int -> ZFunc.t
  val create_recog_func : ZSort.t -> const_idx:int -> ZFunc.t
  val create_access_func : ZSort.t -> const_idx:int -> field_idx:int -> ZFunc.t
  val read_field_sort : ZSort.t -> const_idx:int -> field_idx:int -> ZSort.t

  val create : ZSort.t -> const_idx:int -> expr_list:ZExpr.t list -> ZExpr.t
  val read : ZExpr.t -> const_idx:int -> field_idx:int -> ZExpr.t

  val is_field : ZExpr.t -> const_idx:int -> ZExpr.t
end


(*****************************************************************************)
(*****************************************************************************)
(* Formulae                                                                  *)
(*****************************************************************************)
(*****************************************************************************)

module ZFormula : sig
  type t = ZExpr.t

  val sort : ZSort.t
  
  val true_ : t
  val false_ : t
  val uninterpreted_ : t

  val create_not : t -> t
  val create_and : t list -> t
  val create_or : t list -> t
  val create_xor : t -> t -> t
  val create_eq : t -> t -> t
  val create_neq : t -> t -> t
  val create_imply : t -> t -> t
  val create_iff : t -> t -> t
end


(*****************************************************************************)
(*****************************************************************************)
(* Unit                                                                      *)
(*****************************************************************************)
(*****************************************************************************)

module ZUnit : sig
  type t = ZExpr.t

  val sort : ZSort.t

  val create : t
end


(*****************************************************************************)
(*****************************************************************************)
(* Booleans                                                                  *)
(*****************************************************************************)
(*****************************************************************************)

module ZBool : sig
  type t = ZExpr.t

  val sort : ZSort.t

  val of_bool : bool -> t
  
  val true_ : t
  val false_ : t

  val create_not : t -> t
  val create_and : t -> t -> t
  val create_or : t -> t -> t
  val create_xor : t -> t -> t

  val create_eq : t -> t -> t
  val create_neq : t -> t -> t
end


(*****************************************************************************)
(*****************************************************************************)
(* Integers                                                                  *)
(*****************************************************************************)
(*****************************************************************************)

module ZInt : sig
  type t = ZExpr.t

  val sort : ZSort.t

  val of_zarith : Z.t -> t
  val of_int : int -> t

  val minus_one_ : t
  val zero_ : t
  val one_ : t

  val create_neg : t -> t
  val create_add : t list -> t
  val create_sub : t list -> t
  val create_mul : t list -> t
  val create_div : t -> t -> t
  val create_mod : t -> t -> t
  val create_power : t -> t -> t

  val create_eq : t -> t -> ZBool.t
  val create_neq : t -> t -> ZBool.t
  val create_lt : t -> t -> ZBool.t
  val create_le : t -> t -> ZBool.t
  val create_gt : t -> t -> ZBool.t
  val create_ge : t -> t -> ZBool.t

  val create_cmp : t -> t -> t
  val create_abs : t -> t

  val to_zmutez : t -> ZExpr.t
end


(*****************************************************************************)
(*****************************************************************************)
(* Mutez                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

module ZMutez : sig
  type t = ZExpr.t

  val sort : ZSort.t

  val of_zarith : Z.t -> t
  val of_int : int -> t

  val zero_ : t

  val create_add : t -> t -> t
  val create_sub : t -> t -> t
  val create_mul : t -> t -> t
  val create_div : t -> t -> t
  val create_mod : t -> t -> t

  val create_eq : t -> t -> ZBool.t
  val create_neq : t -> t -> ZBool.t
  val create_lt : t -> t -> ZBool.t
  val create_le : t -> t -> ZBool.t
  val create_gt : t -> t -> ZBool.t
  val create_ge : t -> t -> ZBool.t

  val create_cmp : t -> t -> ZInt.t

  val to_zint : t -> ZInt.t

  val check_add_no_overflow : t -> t -> ZBool.t
  val check_mul_no_overflow : t -> t -> ZBool.t
  val check_sub_no_underflow : t -> t -> ZBool.t
end


(*****************************************************************************)
(*****************************************************************************)
(* Strings                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

module ZStr : sig
  type t = ZExpr.t

  val sort : ZSort.t

  val of_string : string -> t

  val create_concat : t list -> t
  val create_slice : t -> low:ZInt.t -> high:ZInt.t -> t
  val create_length : t -> ZInt.t

  val create_eq : t -> t -> ZBool.t
  val create_neq : t -> t -> ZBool.t

  val create_cmp : t -> t -> ZInt.t
end


(*****************************************************************************)
(*****************************************************************************)
(* Options                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

module ZOption : sig
  type t = ZExpr.t

  val _create_const_of_none : ZDatatype.const
  val _create_const_of_some : content_sort:ZSort.t -> ZDatatype.const
  val _create_sort_name : content_sort:ZSort.t -> string

  val create_sort : content_sort:ZSort.t -> ZSort.t

  val create_none : content_sort:ZSort.t -> t
  val create_some : content:ZExpr.t -> t
  val read : t -> ZExpr.t

  val create_eq : t -> t -> ZBool.t
  val create_neq : t -> t -> ZBool.t

  val is_none : t -> ZBool.t
  val is_some : t -> ZBool.t
end


(*****************************************************************************)
(*****************************************************************************)
(* Pairs                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

module ZPair : sig
  type t = ZExpr.t

  val _create_const_of_pair : fst_sort:ZSort.t -> snd_sort:ZSort.t -> ZDatatype.const
  val _create_sort_name : fst_sort:ZSort.t -> snd_sort:ZSort.t -> string

  val create_sort : fst_sort:ZSort.t -> snd_sort:ZSort.t -> ZSort.t

  val create : fst:ZExpr.t -> snd:ZExpr.t -> t
  val read_fst : t -> ZExpr.t
  val read_snd : t -> ZExpr.t

  val create_eq : t -> t -> ZBool.t
  val create_neq : t -> t -> ZBool.t
end


(*****************************************************************************)
(*****************************************************************************)
(* Bytes                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

module ZBytes : sig
  type t = ZExpr.t

  val _create_const_of_bytstr : ZDatatype.const
  val _create_const_of_pack : content_sort:ZSort.t -> ZDatatype.const
  val _create_const_of_concatenated : bytes_pair_sort:ZSort.t -> ZDatatype.const
  (* let _create_const_of_sliced : content_sort:ZSort.t -> ZDatatype.const *) (* TODO : after ZNat completed *)
  val _create_const_of_blake2b : ZDatatype.const
  val _create_const_of_sha256 : ZDatatype.const
  val _create_const_of_sha512 : ZDatatype.const

  val create_sort : content_sort:ZSort.t -> ZSort.t

  val of_string : string -> t
  val create_bytstr : ZExpr.t -> t
  val create_pack : ZExpr.t -> t

  (* "create_concatenated" does not check that the ~fst_bytes and ~snd_bytes has real bytes type expression *)
  val create_concatenated : fst_bytes:ZExpr.t -> snd_bytes:ZExpr.t -> t

  (* val create_sliced : ZExpr.t -> t *) (* TODO : after _create_const_of_sliced finished *)

  val create_blake2b : ZExpr.t -> t
  val create_sha256 : ZExpr.t -> t
  val create_sha512 : ZExpr.t -> t
end


(*****************************************************************************)
(*****************************************************************************)
(* Ors                                                                       *)
(*****************************************************************************)
(*****************************************************************************)

module ZOr : sig
  type t = ZExpr.t

  val _create_const_of_left : left_sort:ZSort.t -> ZDatatype.const
  val _create_const_of_right : right_sort:ZSort.t -> ZDatatype.const
  val _create_sort_name : left_sort:ZSort.t -> right_sort:ZSort.t -> string

  val create_sort : left_sort:ZSort.t -> right_sort:ZSort.t -> ZSort.t

  val create_left : left_content:ZExpr.t -> right_sort:ZSort.t -> t
  val create_right : left_sort:ZSort.t -> right_content:ZExpr.t -> t
  val read_left : t -> ZExpr.t
  val read_right : t -> ZExpr.t

  val create_eq : t -> t -> ZBool.t
  val create_neq : t -> t -> ZBool.t

  val is_left : t -> ZBool.t
  val is_right : t -> ZBool.t
end


(*****************************************************************************)
(*****************************************************************************)
(* Lists                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

module ZList : sig
  type t = ZExpr.t

  val _create_const_of_nil : ZDatatype.const
  val _create_const_of_cons : content_sort:ZSort.t -> ZDatatype.const
  val _create_sort_name : content_sort:ZSort.t -> string

  val create_sort : content_sort:ZSort.t -> ZSort.t

  val create : content_sort:ZSort.t -> t
  val read_head : t -> ZExpr.t
  val read_tail : t -> t
  val update : t -> content:ZExpr.t -> t

  val create_eq : t -> t -> ZBool.t
  val create_neq : t -> t -> ZBool.t

  val is_nil : t -> ZBool.t
  val is_cons : t -> ZBool.t
end


(*****************************************************************************)
(*****************************************************************************)
(* Maps                                                                      *)
(*****************************************************************************)
(*****************************************************************************)

module ZMap : sig
  type t = ZExpr.t

  val _count_map : int ref
  val _create_name : key_sort:ZSort.t -> value_sort:ZSort.t -> string

  val create_sort : key_sort:ZSort.t -> value_sort:ZSort.t -> ZSort.t

  val read_default_value : t -> ZExpr.t

  val create : key_sort:ZSort.t -> value_sort:ZSort.t -> t
  val read_value : key:ZExpr.t -> map:t -> ZExpr.t
  val read_exist : key:ZExpr.t -> map:t -> ZBool.t
  val update : key:ZExpr.t -> value:ZExpr.t -> map:t -> t

  val create_eq : t -> t -> ZBool.t
  val create_neq : t -> t -> ZBool.t
end


(*****************************************************************************)
(*****************************************************************************)
(* Sets                                                                      *)
(*****************************************************************************)
(*****************************************************************************)


(*****************************************************************************)
(*****************************************************************************)
(* Operations                                                                *)
(*****************************************************************************)
(*****************************************************************************)

module ZOperation : sig
  type t = ZExpr.t
  
  val sort : ZSort.t

  val create_eq : t -> t -> ZBool.t
  val create_neq : t -> t -> ZBool.t
end


(*****************************************************************************)
(*****************************************************************************)
(* Contracts                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

module ZContract : sig
  type t = ZExpr.t
  
  val sort : ZSort.t

  val create_eq : t -> t -> ZBool.t
  val create_neq : t -> t -> ZBool.t
end


(*****************************************************************************)
(*****************************************************************************)
(* Lambdas                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

module ZLambda : sig
  type t = ZExpr.t
  
  val sort : ZSort.t

  val create_eq : t -> t -> ZBool.t
  val create_neq : t -> t -> ZBool.t
end


(*****************************************************************************)
(*****************************************************************************)
(* Model                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

module ZModel : sig
  type t = Z3.Model.model

  val eval : ZExpr.t -> model:t -> ZExpr.t option

  val to_string : t -> string
end


(*****************************************************************************)
(*****************************************************************************)
(* Solver                                                                    *)
(*****************************************************************************)
(*****************************************************************************)

module ZSolver : sig
  type t = Z3.Solver.solver
  type validity = VAL | INVAL | UNKNOWN
  type satisfiability = SAT | UNSAT | UNKNOWN

  val _create : unit -> t
  val _formula_add : t -> ZFormula.t list -> unit
  
  val check_satisfiability : ZFormula.t list -> (satisfiability * ZModel.t option)
  val check_validity : ZFormula.t list -> (validity * ZModel.t option)

  val is_unknown_sat : satisfiability -> bool
  val is_sat : satisfiability -> bool
  val is_unsat : satisfiability -> bool
  val is_unknown_val : validity -> bool
  val is_valid : validity -> bool
  val is_invalid : validity -> bool

  val to_string : t -> string
  val string_of_satisfiability : satisfiability -> string
  val string_of_validity : validity -> string
end