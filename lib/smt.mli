(* Smt: Interface to the Z3 SMT solver *)

exception SmtError of string

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(* Map of Tz.mich_t Tz.cc *)
module MTMap : module type of Core.Map.Make (Tz.MichTCC_cmp)

(* Map of Tz.mich_v Tz.cc *)
module MVMap : module type of Core.Map.Make (Tz.MichVCC_cmp)

type constructor =
  | CST_unit
  | CST_key
  | CST_keyhash_str
  | CST_keyhash_key
  | CST_option_none
  | CST_option_some      of Tz.mich_t Tz.cc
  | CST_pair             of Tz.mich_t Tz.cc * Tz.mich_t Tz.cc
  | CST_bytes_nil
  | CST_bytes_str
  | CST_bytes_concat
  | CST_bytes_blake2b
  | CST_bytes_sha256
  | CST_bytes_sha512
  | CST_signature_str
  | CST_signature_signed
  | CST_address
  | CST_or_left          of Tz.mich_t Tz.cc
  | CST_or_right         of Tz.mich_t Tz.cc
  | CST_operation
  | CST_contract
  | CST_lambda
[@@deriving sexp, compare, equal]

module CST_cmp : sig
  type t = constructor [@@deriving sexp, compare]
end

module CSTMap : module type of Core.Map.Make (CST_cmp)

(******************************************************************************)
(******************************************************************************)
(* Z3 Interfaces                                                              *)
(******************************************************************************)
(******************************************************************************)

(******************************************************************************)
(* Context                                                                    *)
(******************************************************************************)

module Ctx : sig
  type body = string * string

  type t = private {
    id : int;
    ctx : (Z3.context[@sexp.opaque] [@ignore]);
    const_map :
      (Z3.Datatype.Constructor.constructor[@sexp.opaque] [@ignore]) CSTMap.t ref;
    sort_map : (Z3.Sort.sort[@sexp.opaque] [@ignore]) MTMap.t ref;
    expr_map : (Z3.Expr.expr[@sexp.opaque] [@ignore]) MVMap.t ref;
  }
  [@@deriving sexp, compare, equal]

  val body_timeout : unit -> body

  val create : unit -> t

  val read : t -> Z3.context

  val read_id : t -> int

  val read_const :
    t ->
    constructor ->
    f:(unit -> Z3.Datatype.Constructor.constructor) ->
    Z3.Datatype.Constructor.constructor

  val read_sort :
    t -> Tz.mich_t Tz.cc -> f:(unit -> Z3.Sort.sort) -> Z3.Sort.sort

  val read_expr :
    t -> Tz.mich_v Tz.cc -> f:(unit -> Z3.Expr.expr) -> Z3.Expr.expr
end

(******************************************************************************)
(* Symbol                                                                     *)
(******************************************************************************)

module Sym : sig
  type t = private Z3.Symbol.symbol

  val create : Ctx.t -> string -> t

  val create_dummy : Ctx.t -> t

  val create_lst : Ctx.t -> string list -> t list

  val to_string : t -> string

  val compare : t -> t -> int

  val equal : t -> t -> bool
end

(******************************************************************************)
(* Sort                                                                       *)
(******************************************************************************)

module Sort : sig
  type t = Z3.Sort.sort

  val create : Ctx.t -> name:string -> t

  val create_dummy : Ctx.t -> t

  val to_string : t -> string

  val compare : t -> t -> int

  val equal : t -> t -> bool
end

(******************************************************************************)
(* Formula                                                                    *)
(******************************************************************************)

module Formula : sig
  type t = private Z3.Expr.expr

  val sort : Ctx.t -> Sort.t

  val create_true : Ctx.t -> t

  val create_false : Ctx.t -> t

  val create_uninterpreted : Ctx.t -> t

  val create_not : Ctx.t -> t -> t

  val create_and : Ctx.t -> t list -> t

  val create_or : Ctx.t -> t list -> t

  val create_xor : Ctx.t -> t -> t -> t

  val create_imply : Ctx.t -> t -> t -> t

  val create_iff : Ctx.t -> t -> t -> t

  val create_eq : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t

  val create_neq : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t

  val create_is_true : Ctx.t -> Z3.Expr.expr -> t

  val create_is_false : Ctx.t -> Z3.Expr.expr -> t

  val create_is_none : Ctx.t -> Z3.Expr.expr -> t

  val create_is_some : Ctx.t -> Z3.Expr.expr -> t

  val create_is_left : Ctx.t -> Z3.Expr.expr -> t

  val create_is_right : Ctx.t -> Z3.Expr.expr -> t

  val create_is_nil : Ctx.t -> Z3.Expr.expr -> t

  val create_is_cons : Ctx.t -> Z3.Expr.expr -> t

  val create_int_lt : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t

  val create_int_le : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t

  val create_int_gt : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t

  val create_int_ge : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t

  val create_str_lt : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t

  val create_str_le : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t

  val create_str_gt : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t

  val create_str_ge : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t

  val create_convertable_to_finite_bv : Ctx.t -> Z3.Expr.expr -> t

  val create_nat_bound : Ctx.t -> Z3.Expr.expr -> t

  val create_mutez_bound : Ctx.t -> Z3.Expr.expr -> t

  val create_add_no_overflow : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t

  val create_mul_no_overflow : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t

  val create_sub_no_underflow : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t

  val to_sat_check : Ctx.t -> t -> Z3.Expr.expr list

  val to_val_check : Ctx.t -> t -> Z3.Expr.expr list

  val compare : t -> t -> int

  val equal : t -> t -> bool
end

(******************************************************************************)
(* Expression                                                                 *)
(******************************************************************************)

module Expr : sig
  type t = Z3.Expr.expr

  val create_var : Ctx.t -> Sort.t -> name:string -> t

  val create_dummy : Ctx.t -> Sort.t -> t

  val read_sort : t -> Sort.t

  val to_string : t -> string

  val if_then_else : Ctx.t -> if_:t -> then_:t -> else_:t -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool
end

(******************************************************************************)
(* Model & Solver                                                             *)
(******************************************************************************)

(* Model **********************************************************************)

module Model : sig
  type t = (Z3.Model.model[@sexp.opaque]) [@@deriving sexp]

  val eval : t -> Expr.t -> Expr.t option

  val to_string : t -> string

  val compare : t -> t -> int

  val equal : t -> t -> bool
end

(* Solver *********************************************************************)

module Solver : sig
  type t = private {
    id : int;
    solver : (Z3.Solver.solver[@sexp.opaque] [@ignore]);
  }
  [@@deriving sexp, compare, equal]

  type validity =
    | VAL
    | INVAL
    | UNKNOWN
  [@@deriving sexp, compare, equal]

  type satisfiability =
    | SAT
    | UNSAT
    | UNKNOWN
  [@@deriving sexp, compare, equal]

  val create : Ctx.t -> t

  val read : t -> Z3.Solver.solver

  val read_id : t -> int

  val check_sat : t -> Ctx.t -> Formula.t -> satisfiability * Model.t option

  val check_val : t -> Ctx.t -> Formula.t -> validity * Model.t option

  val is_sat_unknown : satisfiability -> bool

  val is_sat : satisfiability -> bool

  val is_unsat : satisfiability -> bool

  val is_val_unknown : validity -> bool

  val is_val : validity -> bool

  val is_inval : validity -> bool

  val string_of_sat : satisfiability -> string

  val string_of_val : validity -> string
end

(******************************************************************************)
(* Function Declarations & Data Types                                         *)
(******************************************************************************)

val get : 'a list -> idx:int -> 'a

(* Function Declarations ******************************************************)

module Func : sig
  type t = Z3.FuncDecl.func_decl

  val apply : t -> params:Expr.t list -> Expr.t

  val read_num_of_domain : t -> int

  val read_sort_of_domain : t -> idx:int -> Sort.t

  val read_sort_of_range : t -> Sort.t
end

(* Data Type Constructor ******************************************************)

module DataConst : sig
  type t = Z3.Datatype.Constructor.constructor

  type info = {
    (* Name of constructor *)
    name : string;
    (* Name of function for recognizing constructor *)
    recog_func_name : string;
    (* Pairs of name and sort for each field *)
    field : (string * Sort.t option) list;
  }

  val create_constructor : Ctx.t -> info -> t

  val read_num_of_field : t -> int

  val make_func_for_constructor : t -> Func.t

  val get_func_for_constructor : t -> idx:int -> Func.t

  val recog_func_for_constructor : t -> Func.t
end

(* Data Type ******************************************************************)

module DataType : sig
  val create_sort : Ctx.t -> name:string -> DataConst.t list -> Sort.t

  val read_num_of_constructor : Sort.t -> int

  val make_func_for_type : Sort.t -> const_idx:int -> Func.t

  val get_func_for_type : Sort.t -> const_idx:int -> field_idx:int -> Func.t

  val recog_func_for_type : Sort.t -> const_idx:int -> Func.t

  val create_expr : Sort.t -> const_idx:int -> Expr.t list -> Expr.t

  val read_sort_of_field : Sort.t -> const_idx:int -> field_idx:int -> Sort.t

  val read_expr_of_field : Expr.t -> const_idx:int -> field_idx:int -> Expr.t

  val read_expr_is_const : Expr.t -> const_idx:int -> Expr.t
end

(******************************************************************************)
(* Data Expressions                                                           *)
(******************************************************************************)

module type BuiltInDataExpr = sig
  type elt

  val create_sort : Ctx.t -> Sort.t

  val create_expr : Ctx.t -> elt -> Expr.t
end

module type CustomDataExpr = sig
  val create_const : Ctx.t -> DataConst.t

  val create_sort : Ctx.t -> Sort.t

  val create_expr : Ctx.t -> Expr.t
end

(* Arithmetic *****************************************************************)

module Arithmetic (Typ : sig
  val mt_cc : Tz.mich_t Tz.cc

  val gen_mv_cc : Bigint.t -> Tz.mich_v Tz.cc
end) : sig
  include BuiltInDataExpr

  val create_expr_of_bigint : Ctx.t -> Bigint.t -> Expr.t

  val to_finite_bv : Ctx.t -> Expr.t -> Expr.t

  val create_add : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_mul : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_div : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_cmp : Ctx.t -> Expr.t -> Expr.t -> Expr.t
end

module ZInt : sig
  module Typ : sig
    val mt_cc : Tz.mich_t Tz.cc

    val gen_mv_cc : Bigint.t -> Tz.mich_v Tz.cc
  end

  include module type of Arithmetic (Typ)

  val create_neg : Ctx.t -> Expr.t -> Expr.t

  val create_sub : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_not : Ctx.t -> Expr.t -> Expr.t
end

module ZNat : sig
  module Typ : sig
    val mt_cc : Tz.mich_t Tz.cc

    val gen_mv_cc : Bigint.t -> Tz.mich_v Tz.cc
  end

  include module type of Arithmetic (Typ)

  val create_abs : Ctx.t -> Expr.t -> Expr.t

  val create_mod : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_power : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_shift_l : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_shift_r : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_and : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_or : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_xor : Ctx.t -> Expr.t -> Expr.t -> Expr.t
end

module ZMutez : sig
  module Typ : sig
    val mt_cc : Tz.mich_t Tz.cc

    val gen_mv_cc : Bigint.t -> Tz.mich_v Tz.cc
  end

  include module type of Arithmetic (Typ)
end

(* Boolean ********************************************************************)

module ZBool : sig
  include BuiltInDataExpr

  val mt_cc : Tz.mich_t Tz.cc

  val gen_mv_cc : bool -> Tz.mich_v Tz.cc

  val create_not : Ctx.t -> Expr.t -> Expr.t

  val create_and : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_or : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_xor : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_cmp : Ctx.t -> Expr.t -> Expr.t -> Expr.t
end

(* String *********************************************************************)

module ZStr : sig
  include BuiltInDataExpr

  val mt_cc : Tz.mich_t Tz.cc

  val gen_mv_cc : string -> Tz.mich_v Tz.cc

  val create_concat : Ctx.t -> Expr.t list -> Expr.t

  val create_slice : Ctx.t -> Expr.t -> Expr.t -> Expr.t -> Expr.t

  val create_size : Ctx.t -> Expr.t -> Expr.t

  val create_cmp : Ctx.t -> Expr.t -> Expr.t -> Expr.t
end

(* Unit ***********************************************************************)

module ZUnit : sig
  include CustomDataExpr
end

(******************************************************************************)
(******************************************************************************)
(* Utility Functions                                                          *)
(******************************************************************************)
(******************************************************************************)

val get_version_of_z3 : unit -> string

val make_log : string -> unit

val close_log : unit -> unit
