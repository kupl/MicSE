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

type const_delim =
  | CST_unit
  | CST_key
  | CST_keyhash_str
  | CST_keyhash_key
  | CST_option_none
  | CST_option_some               of Tz.mich_t Tz.cc
  | CST_pair                      of Tz.mich_t Tz.cc * Tz.mich_t Tz.cc
  | CST_bytes_nil
  | CST_bytes_str
  | CST_bytes_concat
  | CST_bytes_blake2b
  | CST_bytes_sha256
  | CST_bytes_sha512
  | CST_bytes_pack
  | CST_bytes_sliced
  | CST_signature_str
  | CST_signature_signed
  | CST_address
  | CST_or_left                   of Tz.mich_t Tz.cc
  | CST_or_right                  of Tz.mich_t Tz.cc
  | CST_operation_create_contract
  | CST_operation_transfer_tokens
  | CST_operation_set_delegate
  | CST_contract                  of Tz.mich_t Tz.cc
  | CST_lambda                    of Tz.mich_t Tz.cc * Tz.mich_t Tz.cc
[@@deriving sexp, compare, equal]

module CST_cmp : sig
  type t = const_delim [@@deriving sexp, compare]
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
    const_delim ->
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

  (* Helper Function **********************************************************)

  val gen_sort_name : string -> t list -> string
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

  val compare : t -> t -> int

  val equal : t -> t -> bool
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
    (* Indicator of constructor *)
    const_idx : int;
    const_delim : const_delim;
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

  (* Helper Function **********************************************************)

  val gen_const_list : Ctx.t -> info list -> t list
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

(* Arithmetic *****************************************************************)

module Arithmetic (Typ : sig
  type elt = int

  val mt_cc : Tz.mich_t Tz.cc

  val gen_mv_lit_cc : elt -> Tz.mich_v Tz.cc

  val gen_mv_lit_cc_of_bigint : Bigint.t -> Tz.mich_v Tz.cc
end) : sig
  include module type of Typ

  val create_sort : Ctx.t -> Sort.t

  val create_expr : Ctx.t -> Typ.elt -> Expr.t

  val create_expr_of_bigint : Ctx.t -> Bigint.t -> Expr.t

  val create_add : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_sub : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_mul : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_div : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_mod : Ctx.t -> Expr.t -> Expr.t -> Expr.t
end

module ZInt : sig
  module Typ : sig
    type elt = int

    val mt_cc : Tz.mich_t Tz.cc

    val gen_mv_lit_cc : elt -> Tz.mich_v Tz.cc

    val gen_mv_lit_cc_of_bigint : Bigint.t -> Tz.mich_v Tz.cc
  end

  include module type of Arithmetic (Typ)

  val create_neg : Ctx.t -> Expr.t -> Expr.t

  val create_not : Ctx.t -> Expr.t -> Expr.t
end

module ZNat : sig
  module Typ : sig
    type elt = int

    val mt_cc : Tz.mich_t Tz.cc

    val gen_mv_lit_cc : elt -> Tz.mich_v Tz.cc

    val gen_mv_lit_cc_of_bigint : Bigint.t -> Tz.mich_v Tz.cc
  end

  include module type of Arithmetic (Typ)

  val to_finite_bv : Ctx.t -> Expr.t -> Expr.t

  val create_convertable_to_finite_bv : Ctx.t -> Expr.t -> Expr.t

  val create_abs : Ctx.t -> Expr.t -> Expr.t

  val create_power : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_shift_l : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_shift_r : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_and : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_or : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_xor : Ctx.t -> Expr.t -> Expr.t -> Expr.t
end

module ZMutez : sig
  module Typ : sig
    type elt = int

    val mt_cc : Tz.mich_t Tz.cc

    val gen_mv_lit_cc : elt -> Tz.mich_v Tz.cc

    val gen_mv_lit_cc_of_bigint : Bigint.t -> Tz.mich_v Tz.cc
  end

  include module type of Arithmetic (Typ)
end

module ZTimestamp : sig
  module Typ : sig
    type elt = int

    val mt_cc : Tz.mich_t Tz.cc

    val gen_mv_lit_cc : elt -> Tz.mich_v Tz.cc

    val gen_mv_lit_cc_of_bigint : Bigint.t -> Tz.mich_v Tz.cc
  end

  include module type of Arithmetic (Typ)
end

(* Boolean ********************************************************************)

module ZBool : sig
  type elt = bool

  val mt_cc : Tz.mich_t Tz.cc

  val gen_mv_lit_cc : elt -> Tz.mich_v Tz.cc

  val create_sort : Ctx.t -> Sort.t

  val create_expr : Ctx.t -> elt -> Expr.t

  val create_not : Ctx.t -> Expr.t -> Expr.t

  val create_and : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_or : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_xor : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_eq : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_neq : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_int_lt : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_int_gt : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_int_leq : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_int_geq : Ctx.t -> Expr.t -> Expr.t -> Expr.t
end

(* String *********************************************************************)

module ZStr : sig
  type elt = string

  val mt_cc : Tz.mich_t Tz.cc

  val gen_mv_lit_cc : elt -> Tz.mich_v Tz.cc

  val create_sort : Ctx.t -> Sort.t

  val create_expr : Ctx.t -> elt -> Expr.t

  val create_concat : Ctx.t -> Expr.t list -> Expr.t

  val create_slice : Ctx.t -> offset:Expr.t -> len:Expr.t -> Expr.t -> Expr.t

  val create_size : Ctx.t -> Expr.t -> Expr.t
end

(* Unit ***********************************************************************)

module ZUnit : sig
  type elt = unit

  val mt_cc : Tz.mich_t Tz.cc

  val gen_mv_lit_cc : elt -> Tz.mich_v Tz.cc

  val create_sort : Ctx.t -> Sort.t

  val create_expr : Ctx.t -> Expr.t
end

(* Key ************************************************************************)

module ZKey : sig
  type elt = string

  val mt_cc : Tz.mich_t Tz.cc

  val gen_mv_lit_cc : elt -> Tz.mich_v Tz.cc

  val create_sort : Ctx.t -> Sort.t

  val create_expr : Ctx.t -> elt -> Expr.t

  val read_content : Expr.t -> Expr.t
end

(* Key Hash *******************************************************************)

module ZKeyHash : sig
  type elt = string

  val mt_cc : Tz.mich_t Tz.cc

  val gen_mv_lit_cc : elt -> Tz.mich_v Tz.cc

  val create_sort : Ctx.t -> Sort.t

  val create_expr : Ctx.t -> elt -> Expr.t

  val create_hashkey : Ctx.t -> Expr.t -> Expr.t

  val read_content_str : Expr.t -> Expr.t

  val read_content_key : Expr.t -> Expr.t
end

(* Option *********************************************************************)

module ZOption : sig
  val create_sort :
    Ctx.t -> typ:Tz.mich_t Tz.cc -> content_sort:Sort.t -> Sort.t

  val create_expr_none : Sort.t -> Expr.t

  val create_expr_some : Sort.t -> Expr.t -> Expr.t

  val read_content : Expr.t -> Expr.t
end

(* Pair ***********************************************************************)

module ZPair : sig
  val gen_mt_cc : Tz.mich_t Tz.cc * Tz.mich_t Tz.cc -> Tz.mich_t Tz.cc

  val create_sort :
    Ctx.t -> typ:Tz.mich_t Tz.cc -> content_sort:Sort.t * Sort.t -> Sort.t

  val create_expr : Sort.t -> Expr.t * Expr.t -> Expr.t

  val read_content_fst : Expr.t -> Expr.t

  val read_content_snd : Expr.t -> Expr.t
end

(* Bytes **********************************************************************)

module ZBytes : sig
  type elt = string

  val mt_cc : Tz.mich_t Tz.cc

  val gen_mv_lit_cc : elt -> Tz.mich_v Tz.cc

  val create_sort : Ctx.t -> Sort.t

  val create_expr : Ctx.t -> elt -> Expr.t

  val create_concat : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val create_blake2b : Ctx.t -> Expr.t -> Expr.t

  val create_sha256 : Ctx.t -> Expr.t -> Expr.t

  val create_sha512 : Ctx.t -> Expr.t -> Expr.t

  val create_pack : Ctx.t -> int -> Expr.t

  val create_slice : Ctx.t -> offset:Expr.t -> len:Expr.t -> Expr.t -> Expr.t

  val read_str : Expr.t -> Expr.t

  val read_content_blake2b : Expr.t -> Expr.t

  val read_content_sha256 : Expr.t -> Expr.t

  val read_content_sha512 : Expr.t -> Expr.t

  val read_content_packed : Expr.t -> Expr.t

  val read_content_sliced : Expr.t -> Expr.t

  val read_offset_sliced : Expr.t -> Expr.t

  val read_length_sliced : Expr.t -> Expr.t
end

(* Signature **********************************************************************)

module ZSig : sig
  type elt = string

  val mt_cc : Tz.mich_t Tz.cc

  val gen_mv_lit_cc : elt -> Tz.mich_v Tz.cc

  val create_sort : Ctx.t -> Sort.t

  val create_expr : Ctx.t -> elt -> Expr.t

  val create_signed : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val read_str : Expr.t -> Expr.t

  val read_key_signed : Expr.t -> Expr.t

  val read_bytes_signed : Expr.t -> Expr.t
end

(* Address ********************************************************************)

module ZAddr : sig
  val mt_cc : Tz.mich_t Tz.cc

  val create_sort : Ctx.t -> Sort.t

  val create_expr : Ctx.t -> Expr.t -> Expr.t

  val read_content : Expr.t -> Expr.t
end

(* Or *************************************************************************)

module ZOr : sig
  val gen_mt_cc : Tz.mich_t Tz.cc * Tz.mich_t Tz.cc -> Tz.mich_t Tz.cc

  val create_sort :
    Ctx.t -> typ:Tz.mich_t Tz.cc -> content_sort:Sort.t * Sort.t -> Sort.t

  val create_expr_left : Sort.t -> Expr.t -> Expr.t

  val create_expr_right : Sort.t -> Expr.t -> Expr.t

  val read_content_left : Expr.t -> Expr.t

  val read_content_right : Expr.t -> Expr.t
end

(* List ***********************************************************************)

module ZList : sig
  val gen_mt_cc : Tz.mich_t Tz.cc -> Tz.mich_t Tz.cc

  val create_sort :
    Ctx.t -> typ:Tz.mich_t Tz.cc -> content_sort:Sort.t -> Sort.t

  val create_expr_nil : Sort.t -> Expr.t

  val create_cons : content:Expr.t -> Expr.t -> Expr.t

  val read_head : Expr.t -> Expr.t

  val read_tail : Expr.t -> Expr.t
end

(* Map ************************************************************************)

module ZMap : sig
  val gen_mt_cc : Tz.mich_t Tz.cc * Tz.mich_t Tz.cc -> Tz.mich_t Tz.cc

  val create_sort :
    Ctx.t ->
    typ:Tz.mich_t Tz.cc ->
    key_sort:Sort.t ->
    data_body_sort:Sort.t ->
    Sort.t

  val create_expr_empty_map : Ctx.t -> key_sort:Sort.t -> data_sort:Sort.t -> Expr.t

  val read_value : Ctx.t -> key:Expr.t -> Expr.t -> Expr.t

  val read_default_value : Ctx.t -> Expr.t -> Expr.t

  val update : Ctx.t -> key:Expr.t -> data:Expr.t -> Expr.t -> Expr.t

  val read_mem : Ctx.t -> key:Expr.t -> Expr.t -> Expr.t
end

(* Set ************************************************************************)

module ZSet : sig
  val gen_mt_cc : Tz.mich_t Tz.cc -> Tz.mich_t Tz.cc

  val create_sort :
    Ctx.t -> typ:Tz.mich_t Tz.cc -> content_sort:Sort.t -> Sort.t

  val create_expr_empty_set : Ctx.t -> Sort.t -> Expr.t

  val update : Ctx.t -> content:Expr.t -> flag:Expr.t -> Expr.t -> Expr.t

  val read_mem : Ctx.t -> content:Expr.t -> Expr.t -> Expr.t
end

(* Operation ******************************************************************)

module ZOperation : sig
  val mt_cc : Tz.mich_t Tz.cc

  val create_sort : Ctx.t -> Sort.t

  val create_expr_create_contract :
    Ctx.t -> Expr.t -> Expr.t -> Expr.t -> Expr.t

  val create_expr_transfer_tokens : Ctx.t -> Expr.t -> Expr.t

  val create_expr_set_delegate : Ctx.t -> Expr.t -> Expr.t

  val read_amount : Ctx.t -> Expr.t -> Expr.t
end

(* Contract *******************************************************************)

module ZContract : sig
  val gen_mt_cc : Tz.mich_t Tz.cc -> Tz.mich_t Tz.cc

  val create_sort :
    Ctx.t -> typ:Tz.mich_t Tz.cc -> content_sort:Sort.t -> Sort.t

  val create_expr : Ctx.t -> Expr.t -> Expr.t

  val create_expr_of_address : Sort.t -> Expr.t -> Expr.t

  val read_keyhash : Expr.t -> Expr.t
end

(* Lambda *********************************************************************)

module ZLambda : sig
  val gen_mt_cc : Tz.mich_t Tz.cc * Tz.mich_t Tz.cc -> Tz.mich_t Tz.cc

  val create_sort :
    Ctx.t ->
    typ:Tz.mich_t Tz.cc ->
    domain_sort:Sort.t ->
    range_sort:Sort.t ->
    Sort.t

  val create_expr_domain : Ctx.t -> Sort.t -> Expr.t

  val create_expr : Sort.t -> Expr.t -> Expr.t -> Expr.t

  val create_exec : Expr.t -> Expr.t -> Expr.t

  val create_apply : Ctx.t -> Expr.t -> Expr.t -> Expr.t

  val read_expr_domain : Expr.t -> Expr.t
end

(******************************************************************************)
(* Formula                                                                    *)
(******************************************************************************)

module Formula : sig
  type t = private Z3.Expr.expr

  val sort : Ctx.t -> Sort.t

  val if_then_else : Ctx.t -> if_:t -> then_:Expr.t -> else_:Expr.t -> Expr.t

  val create_true : Ctx.t -> t

  val create_false : Ctx.t -> t

  val create_uninterpreted : Ctx.t -> t

  val create_not : Ctx.t -> t -> t

  val create_and : Ctx.t -> t list -> t

  val create_or : Ctx.t -> t list -> t

  val create_xor : Ctx.t -> t -> t -> t

  val create_imply : Ctx.t -> t -> t -> t

  val create_iff : Ctx.t -> t -> t -> t

  val create_eq : Ctx.t -> Expr.t -> Expr.t -> t

  val create_neq : Ctx.t -> Expr.t -> Expr.t -> t

  val create_is_true : Ctx.t -> Expr.t -> t

  val create_is_false : Ctx.t -> Expr.t -> t

  val create_is_unit : Expr.t -> t

  val create_is_key : Expr.t -> t

  val create_is_keyhash_str : Expr.t -> t

  val create_is_keyhash_key : Expr.t -> t

  val create_is_option_none : Expr.t -> t

  val create_is_option_some : Expr.t -> t

  val create_is_pair : Expr.t -> t

  val create_is_bytes_nil : Expr.t -> t

  val create_is_bytes_str : Expr.t -> t

  val create_is_bytes_concat : Expr.t -> t

  val create_is_bytes_blake2b : Expr.t -> t

  val create_is_bytes_sha256 : Expr.t -> t

  val create_is_bytes_sha512 : Expr.t -> t

  val create_is_bytes_pack : Expr.t -> t

  val create_is_bytes_sliced : Expr.t -> t

  val create_is_signature_str : Expr.t -> t

  val create_is_signature_signed : Expr.t -> t

  val create_is_address : Expr.t -> t

  val create_is_or_left : Expr.t -> t

  val create_is_or_right : Expr.t -> t

  val create_is_list_nil : Expr.t -> t

  val create_is_list_cons : Expr.t -> t

  val create_is_operation_create_contract : Expr.t -> t

  val create_is_operation_transfer_tokens : Expr.t -> t

  val create_is_operation_set_delegate : Expr.t -> t

  val create_is_contract : Expr.t -> t

  val create_is_lambda : Expr.t -> t

  val create_is_mem_of_map : Ctx.t -> Expr.t -> Expr.t -> t

  val create_is_not_mem_of_map : Ctx.t -> Expr.t -> Expr.t -> t

  val create_is_mem_of_set : Ctx.t -> Expr.t -> Expr.t -> t

  val create_is_not_mem_of_set : Ctx.t -> Expr.t -> Expr.t -> t

  val create_is_expr_lambda_domain : Ctx.t -> Expr.t -> Expr.t -> t

  val create_arith_lt : Ctx.t -> Expr.t -> Expr.t -> t

  val create_arith_le : Ctx.t -> Expr.t -> Expr.t -> t

  val create_arith_gt : Ctx.t -> Expr.t -> Expr.t -> t

  val create_arith_ge : Ctx.t -> Expr.t -> Expr.t -> t

  val create_str_lt : Ctx.t -> Expr.t -> Expr.t -> t

  val create_str_le : Ctx.t -> Expr.t -> Expr.t -> t

  val create_str_gt : Ctx.t -> Expr.t -> Expr.t -> t

  val create_str_ge : Ctx.t -> Expr.t -> Expr.t -> t

  val create_nat_bound : Ctx.t -> Expr.t -> t

  val create_mutez_bound : Ctx.t -> Expr.t -> t

  val create_add_no_overflow : Ctx.t -> Expr.t -> Expr.t -> t

  val create_mul_no_overflow : Ctx.t -> Expr.t -> Expr.t -> t

  val create_sub_no_underflow : Ctx.t -> Expr.t -> Expr.t -> t

  val create_shift_l_rhs_in_256 : Ctx.t -> Expr.t -> t

  val create_shift_r_rhs_in_256 : Ctx.t -> Expr.t -> t

  val to_sat_check : Ctx.t -> t -> Expr.t list

  val to_val_check : Ctx.t -> t -> Expr.t list

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

  val reset : t -> unit

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
(******************************************************************************)
(* Utility Functions                                                          *)
(******************************************************************************)
(******************************************************************************)

val get_version_of_z3 : unit -> string

val make_log : string -> unit

val close_log : unit -> unit
