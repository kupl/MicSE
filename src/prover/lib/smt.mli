exception ZError of string

module CONST : sig
  val _name_dummy : string
  val _name_unit : string
  val _name_map : string
  
  val _tmpname_source : string
  val _tmpname_sender : string

  val _sort_unit : string
  val _sort_bool : string
  val _sort_int : string
  val _sort_str : string
  val _sort_key : string
  val _sort_unit : string
  val _sort_operation : string
  val _sort_contract : string
  val _sort_lambda : string
  val _sort_bytes : string
  val _sort_option : string
  val _sort_pair : string
  val _sort_or : string
  val _sort_list : string
  val _sort_map : string

  val _const_key_keystr : string
  val _const_bytes_nil : string
  val _const_bytes_bytstr : string
  val _const_bytes_pack : string
  val _const_bytes_sliced : string
  val _const_bytes_blake2b : string
  val _const_bytes_sha256 : string
  val _const_bytes_sha512 : string
  val _const_signature_sigstr : string
  val _const_signature_signed : string
  val _const_address_addrkh : string
  val _const_option_none : string
  val _const_option_some : string
  val _const_pair : string
  val _const_or_left : string
  val _const_or_right : string
  val _const_list_nil : string
  val _const_list_cons : string

  val _recog_key_keystr : string
  val _recog_bytes_bytnil : string
  val _recog_bytes_bytstr : string
  val _recog_bytes_pack : string
  val _recog_bytes_sliced : string
  val _recog_bytes_blake2b : string
  val _recog_bytes_sha256 : string
  val _recog_bytes_sha512 : string
  val _recog_signature_sigstr : string
  val _recog_signature_signed : string
  val _recog_address_addrkh : string
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

  val _int2bv_precision : int
end

(******************************************************************************)
(******************************************************************************)
(* Context                                                                    *)
(******************************************************************************)
(******************************************************************************)

module ZCtx : sig
  type body = (string * string)
  type t = Z3.context

  val body_timeout : unit -> body

  val create  : unit -> t
end


(******************************************************************************)
(******************************************************************************)
(* Symbols                                                                    *)
(******************************************************************************)
(******************************************************************************)

module ZSym : sig
  type t = Z3.Symbol.symbol

  val _name_dummy : string
  val _count_dummy : int ref

  val create : ZCtx.t -> string -> t
  val create_dummy : ZCtx.t -> t
  
  val to_string : t -> string
end


(******************************************************************************)
(******************************************************************************)
(* Sorts                                                                      *)
(******************************************************************************)
(******************************************************************************)

module ZSort : sig
  type t = Z3.Sort.sort

  val create_dummy : ZCtx.t -> t
  val create : ZCtx.t -> name:string -> t

  val to_string : t -> string

  (****************************************************************************)
  (* Sort of Types                                                            *)
  (****************************************************************************)
  val sort_map : (ZCtx.t, (string, t) Core.Map.Poly.t) Core.Map.Poly.t Stdlib.ref
  val _read_x_sort : ZCtx.t -> string -> cst:(ZCtx.t -> t) -> t

  val read_unit_sort : ZCtx.t -> t
  val read_bool_sort : ZCtx.t -> t
  val read_int_sort : ZCtx.t -> t
  val read_str_sort : ZCtx.t -> t
end


(******************************************************************************)
(******************************************************************************)
(* Expressions                                                                *)
(******************************************************************************)
(******************************************************************************)

module ZExpr : sig
  type t = Z3.Expr.expr

  val create_dummy : ZCtx.t -> ZSort.t -> t
  val create_var : ZCtx.t -> ZSort.t -> name:string -> t

  val create_ite : ZCtx.t -> cond:t -> t:t -> f:t -> t

  val read_sort : t -> ZSort.t

  val to_string : t -> string

  (****************************************************************************)
  (* Expression of Literals                                                   *)
  (****************************************************************************)
  type 'a _lit_map = (ZCtx.t, ('a, t) Core.Map.Poly.t) Core.Map.Poly.t Stdlib.ref
  
  val _read_x_lit : 'a _lit_map -> ZCtx.t -> 'a -> cst:(ZCtx.t -> t) -> t

  (* Unit Literals ************************************************************)
  val _unit_lit_map : unit _lit_map
  val read_unit : ZCtx.t -> t

  (* Boolean Literals *********************************************************)
  val _bool_lit_map : bool _lit_map
  val read_bool : ZCtx.t -> bool -> t

  (* Integer Literals *********************************************************)
  val _int_lit_map : Z.t _lit_map
  val read_int : ZCtx.t -> int -> t
  val read_zint : ZCtx.t -> Z.t -> t

  (* String Literals **********************************************************)
  val _str_lit_map : string _lit_map
  val read_str : ZCtx.t -> string -> t
end


(******************************************************************************)
(******************************************************************************)
(* FuncDecls                                                                  *)
(******************************************************************************)
(******************************************************************************)

module ZFunc : sig
  type t = Z3.FuncDecl.func_decl

  val get_idx : 'a list -> idx:int -> 'a

  val apply : t -> params:ZExpr.t list -> ZExpr.t

  val sort_of_domain : t -> idx:int -> ZSort.t
end


(******************************************************************************)
(******************************************************************************)
(* Datatypes                                                                  *)
(******************************************************************************)
(******************************************************************************)

module ZDatatype : sig
  type const = Z3.Datatype.Constructor.constructor

  val get_idx : 'a list -> idx:int -> 'a

  val create_const : ZCtx.t -> name:string -> recog_func_name:string -> field_names:string list -> field_sorts:ZSort.t option list -> field_sort_refs:int list -> const
  val create_sort : ZCtx.t -> name:string -> const_list:const list -> ZSort.t
  val create_const_func : ZSort.t -> const_idx:int -> ZFunc.t
  val create_recog_func : ZSort.t -> const_idx:int -> ZFunc.t
  val create_access_func : ZSort.t -> const_idx:int -> field_idx:int -> ZFunc.t
  val read_field_sort : ZSort.t -> const_idx:int -> field_idx:int -> ZSort.t

  val create : ZSort.t -> const_idx:int -> expr_list:ZExpr.t list -> ZExpr.t
  val read : ZExpr.t -> const_idx:int -> field_idx:int -> ZExpr.t

  val is_field : ZExpr.t -> const_idx:int -> ZExpr.t

  (****************************************************************************)
  (* Pre-defined Datatypes and Its Sort                                       *)
  (****************************************************************************)
  type typ =
    | Key
    | KeyHash_str
    | KeyHash_key
    | Option_none
    | Option_some       of ZSort.t
    | Pair              of ZSort.t * ZSort.t
    | Bytes_nil
    | Bytes_str
    | Bytes_concat
    | Bytes_blake2b
    | Bytes_sha256
    | Bytes_sha512
    | Signature_str
    | Signature_signed
    | Address
    | Or_left           of ZSort.t
    | Or_right          of ZSort.t
    | List_nil
    | List_cons         of ZSort.t

  type _typ_map = (ZCtx.t, (typ, const) Core.Map.Poly.t) Core.Map.Poly.t Stdlib.ref

  val _const_map : _typ_map
  val _read_x_const : _typ_map -> ZCtx.t -> typ -> cst:(ZCtx.t -> const) -> const

  (* Key Type *****************************************************************)
  val read_key_const : ZCtx.t -> const
  val read_key_sort : ZCtx.t -> ZSort.t

  (* Key Hash Type ************************************************************)
  val read_keyhash_const_of_str : ZCtx.t -> const
  val read_keyhash_const_of_key : ZCtx.t -> const
  val read_keyhash_sort : ZCtx.t -> ZSort.t

  (* Option Type **************************************************************)
  val read_option_const_of_none : ZCtx.t -> const
  val read_option_const_of_some : ZCtx.t -> content_sort:ZSort.t -> const
  val _create_option_sort_name : content_sort:ZSort.t -> string
  val read_option_sort : ZCtx.t -> content_sort:ZSort.t -> ZSort.t

  (* Pair Type ****************************************************************)
  val read_pair_const : ZCtx.t -> fst_sort:ZSort.t -> snd_sort:ZSort.t -> const
  val _create_pair_sort_name : fst_sort:ZSort.t -> snd_sort:ZSort.t -> string
  val read_pair_sort : ZCtx.t -> fst_sort:ZSort.t -> snd_sort:ZSort.t -> ZSort.t

  (* Bytes Type ***************************************************************)
  val read_bytes_const_of_nil : ZCtx.t -> const
  val read_bytes_const_of_str : ZCtx.t -> const
  val read_bytes_const_of_concat : ZCtx.t -> const
  val read_bytes_const_of_blake2b : ZCtx.t -> const
  val read_bytes_const_of_sha256 : ZCtx.t -> const
  val read_bytes_const_of_sha512 : ZCtx.t -> const
  val read_bytes_sort : ZCtx.t -> ZSort.t

  (* Signature Type ***********************************************************)
  val read_sig_const_of_str : ZCtx.t -> const
  val read_sig_const_of_signed : ZCtx.t -> const
  val read_sig_sort : ZCtx.t -> ZSort.t

  (* Address Type *************************************************************)
  val read_addr_const : ZCtx.t -> const
  val read_addr_sort : ZCtx.t -> ZSort.t

  (* Or Type ******************************************************************)
  val read_or_const_of_left : ZCtx.t -> left_sort:ZSort.t -> const
  val read_or_const_of_right : ZCtx.t -> right_sort:ZSort.t -> const
  val _create_or_sort_name : left_sort:ZSort.t -> right_sort:ZSort.t -> string
  val read_or_sort : ZCtx.t -> left_sort:ZSort.t -> right_sort:ZSort.t -> ZSort.t

  (* List Type ****************************************************************)
  val _create_list_sort_name : content_sort:ZSort.t -> string
  val read_list_sort : ZCtx.t -> content_sort:ZSort.t -> ZSort.t

  (* Map Type *****************************************************************)
  val _create_map_sort_name : key_sort:ZSort.t -> value_sort:ZSort.t -> string
  val read_map_sort : ZCtx.t -> key_sort:ZSort.t -> value_sort:ZSort.t -> ZSort.t

  (* Operation Type ***********************************************************)
  val read_operation_sort : ZCtx.t -> ZSort.t

  (* Contract Type ************************************************************)
  val read_contract_sort : ZCtx.t -> ZSort.t

  (* Lambda Type **************************************************************)
  val read_lambda_sort : ZCtx.t -> ZSort.t
end


(******************************************************************************)
(******************************************************************************)
(* Formulae                                                                   *)
(******************************************************************************)
(******************************************************************************)

module ZFormula : sig
  type t = ZExpr.t

  val sort : ZCtx.t -> ZSort.t
  
  val true_ : ZCtx.t -> t
  val false_ : ZCtx.t -> t
  val uninterpreted_ : ZCtx.t -> t

  val create_not : ZCtx.t -> t -> t
  val create_and : ZCtx.t -> t list -> t
  val create_or : ZCtx.t -> t list -> t
  val create_xor : ZCtx.t -> t -> t -> t
  val create_eq : ZCtx.t -> t -> t -> t
  val create_neq : ZCtx.t -> t -> t -> t
  val create_imply : ZCtx.t -> t -> t -> t
  val create_iff : ZCtx.t -> t -> t -> t
end


(******************************************************************************)
(******************************************************************************)
(* Unit                                                                       *)
(******************************************************************************)
(******************************************************************************)

module ZUnit : sig
  type t = ZExpr.t

  val sort : ZCtx.t -> ZSort.t

  val create : ZCtx.t -> t
end


(******************************************************************************)
(******************************************************************************)
(* Booleans                                                                   *)
(******************************************************************************)
(******************************************************************************)

module ZBool : sig
  type t = ZExpr.t

  val sort : ZCtx.t -> ZSort.t

  val of_bool : ZCtx.t -> bool -> t

  val _minus_one_ : ZCtx.t -> t
  val _zero_ : ZCtx.t -> t
  val _one_ : ZCtx.t -> t
  
  val true_ : ZCtx.t -> t
  val false_ : ZCtx.t -> t

  val create_not : ZCtx.t -> t -> t
  val create_and : ZCtx.t -> t -> t -> t
  val create_or : ZCtx.t -> t -> t -> t
  val create_xor : ZCtx.t -> t -> t -> t

  val create_eq : ZCtx.t -> t -> t -> t
  val create_neq : ZCtx.t -> t -> t -> t
  
  val create_cmp : ZCtx.t -> t -> t -> t
end


(******************************************************************************)
(******************************************************************************)
(* Integers                                                                   *)
(******************************************************************************)
(******************************************************************************)

module ZInt : sig
  type t = ZExpr.t

  val sort : ZCtx.t -> ZSort.t

  val of_zarith : ZCtx.t -> Z.t -> t
  val of_int : ZCtx.t -> int -> t

  val minus_one_ : ZCtx.t -> t
  val zero_ : ZCtx.t -> t
  val one_ : ZCtx.t -> t

  val mutez_max_ : ZCtx.t -> t

  val create_neg : ZCtx.t -> t -> t
  val create_add : ZCtx.t -> t list -> t
  val create_sub : ZCtx.t -> t list -> t
  val create_mul : ZCtx.t -> t list -> t
  val create_div : ZCtx.t -> t -> t -> t
  val create_mod : ZCtx.t -> t -> t -> t
  val create_power : ZCtx.t -> t -> t -> t

  val create_eq : ZCtx.t -> t -> t -> ZBool.t
  val create_neq : ZCtx.t -> t -> t -> ZBool.t
  val create_lt : ZCtx.t -> t -> t -> ZBool.t
  val create_le : ZCtx.t -> t -> t -> ZBool.t
  val create_gt : ZCtx.t -> t -> t -> ZBool.t
  val create_ge : ZCtx.t -> t -> t -> ZBool.t

  (* bitwise operations *)
  val _to_finite_bv : ZCtx.t -> t -> ZExpr.t
  val _create_finite_bv_expressible : ZCtx.t -> t -> ZBool.t
  val create_shiftL : ZCtx.t -> t -> t -> t
  val create_shiftR : ZCtx.t -> t -> t -> t
  val create_not : ZCtx.t -> t -> t
  val create_and : ZCtx.t -> t -> t -> t
  val create_or : ZCtx.t -> t -> t -> t
  val create_xor : ZCtx.t -> t -> t -> t

  val create_cmp : ZCtx.t -> t -> t -> t
  val create_abs : ZCtx.t -> t -> t

  val to_zmutez : t -> ZExpr.t
end


(******************************************************************************)
(******************************************************************************)
(* Natural Number                                                             *)
(******************************************************************************)
(******************************************************************************)

module ZNat = ZInt


(******************************************************************************)
(******************************************************************************)
(* Mutez                                                                      *)
(******************************************************************************)
(******************************************************************************)

module ZMutez : sig
  type t = ZExpr.t

  val sort : ZCtx.t -> ZSort.t

  val of_zarith : ZCtx.t -> Z.t -> t
  val of_int : ZCtx.t -> int -> t

  val max_ : ZCtx.t -> t
  val zero_ : ZCtx.t -> t

  val create_add : ZCtx.t -> t -> t -> t
  val create_sub : ZCtx.t -> t -> t -> t
  val create_mul : ZCtx.t -> t -> t -> t
  val create_div : ZCtx.t -> t -> t -> t
  val create_mod : ZCtx.t -> t -> t -> t

  val create_eq : ZCtx.t -> t -> t -> ZBool.t
  val create_neq : ZCtx.t -> t -> t -> ZBool.t
  val create_lt : ZCtx.t -> t -> t -> ZBool.t
  val create_le : ZCtx.t -> t -> t -> ZBool.t
  val create_gt : ZCtx.t -> t -> t -> ZBool.t
  val create_ge : ZCtx.t -> t -> t -> ZBool.t

  val create_cmp : ZCtx.t -> t -> t -> ZInt.t

  val to_zint : t -> ZInt.t

  val create_bound : ZCtx.t -> t -> ZBool.t
  val check_add_no_overflow : ZCtx.t -> t -> t -> ZBool.t
  val check_mul_no_overflow : ZCtx.t -> t -> t -> ZBool.t
  val check_sub_no_underflow : ZCtx.t -> t -> t -> ZBool.t
end


(******************************************************************************)
(******************************************************************************)
(* Strings                                                                    *)
(******************************************************************************)
(******************************************************************************)

module ZStr : sig
  type t = ZExpr.t

  val sort : ZCtx.t -> ZSort.t

  val of_string : ZCtx.t -> string -> t

  val create_concat : ZCtx.t -> t list -> t
  val create_slice : ZCtx.t -> t -> low:ZInt.t -> high:ZInt.t -> t
  val create_length : ZCtx.t -> t -> ZInt.t

  val create_eq : ZCtx.t -> t -> t -> ZBool.t
  val create_neq : ZCtx.t -> t -> t -> ZBool.t

  val create_cmp : ZCtx.t -> t -> t -> ZInt.t
end


(******************************************************************************)
(******************************************************************************)
(* Key                                                                        *)
(******************************************************************************)
(******************************************************************************)

module ZKey : sig
  type t = ZExpr.t

  val sort : ZCtx.t -> ZSort.t

  val of_string : ZCtx.t -> string -> t
  val create_keystr : ZCtx.t -> ZExpr.t -> t

  val _read_innerstr : t -> ZStr.t

  val create_cmp : ZCtx.t -> t -> t -> ZInt.t
  val create_eq : ZCtx.t -> t -> t -> ZBool.t
end


(******************************************************************************)
(******************************************************************************)
(* Key Hash                                                                   *)
(******************************************************************************)
(******************************************************************************)

module ZKeyHash : sig
  type t = ZExpr.t

  val sort : ZCtx.t -> ZSort.t

  val of_string : ZCtx.t -> string -> t
  val create_hashkey : ZCtx.t -> ZExpr.t -> t

  val _read_innerstr : t -> ZStr.t
  val _read_innerkey : t -> ZKey.t

  val create_cmp : ZCtx.t -> t -> t -> ZInt.t
  val create_eq : ZCtx.t -> t -> t -> ZBool.t
end


(******************************************************************************)
(******************************************************************************)
(* Options                                                                    *)
(******************************************************************************)
(******************************************************************************)

module ZOption : sig
  type t = ZExpr.t

  val sort : ZCtx.t -> content_sort:ZSort.t -> ZSort.t

  val create_none : ZCtx.t -> content_sort:ZSort.t -> t
  val create_some : ZCtx.t -> content:ZExpr.t -> t
  val read : t -> ZExpr.t

  val create_eq : ZCtx.t -> t -> t -> ZBool.t
  val create_neq : ZCtx.t -> t -> t -> ZBool.t

  val is_none : t -> ZBool.t
  val is_some : t -> ZBool.t
end


(******************************************************************************)
(******************************************************************************)
(* Pairs                                                                      *)
(******************************************************************************)
(******************************************************************************)

module ZPair : sig
  type t = ZExpr.t

  val sort : ZCtx.t -> fst_sort:ZSort.t -> snd_sort:ZSort.t -> ZSort.t

  val create : ZCtx.t -> fst:ZExpr.t -> snd:ZExpr.t -> t
  val read_fst : t -> ZExpr.t
  val read_snd : t -> ZExpr.t

  val create_eq : ZCtx.t -> t -> t -> ZBool.t
  val create_neq : ZCtx.t -> t -> t -> ZBool.t
end


(******************************************************************************)
(******************************************************************************)
(* Bytes                                                                      *)
(******************************************************************************)
(******************************************************************************)

module ZBytes : sig
  type t = ZExpr.t

  val sort : ZCtx.t -> ZSort.t

  (* val bytnil : ZCtx.t -> t *)

  val of_string : ZCtx.t -> string -> t
  val create_bytstr : ZCtx.t -> ZExpr.t -> t
  val create_pack : ZCtx.t -> t

  (* "create_concatenated" does not check that the ~fst_bytes and ~snd_bytes has real bytes type expression *)
  val create_concatenated : ZCtx.t -> fst_bytes:ZExpr.t -> snd_bytes:ZExpr.t -> t

  (* val create_sliced : ZExpr.t -> t *) (* TODO : after _create_const_of_sliced finished *)

  val create_blake2b : ZCtx.t -> ZExpr.t -> t
  val create_sha256 : ZCtx.t -> ZExpr.t -> t
  val create_sha512 : ZCtx.t -> ZExpr.t -> t
end


(******************************************************************************)
(******************************************************************************)
(* Signature                                                                  *)
(******************************************************************************)
(******************************************************************************)

module ZSignature : sig
  type t = ZExpr.t
  
  val sort : ZCtx.t -> ZSort.t
  
  val of_string : ZCtx.t -> string -> t
  val create_sigstr : ZCtx.t -> ZExpr.t -> t
  
  val create_signed : ZCtx.t -> key_data:ZExpr.t -> bytes_data:ZExpr.t -> t
end


(******************************************************************************)
(******************************************************************************)
(* Address                                                                    *)
(******************************************************************************)
(******************************************************************************)

module ZAddress : sig
  type t = ZExpr.t

  val sort : ZCtx.t -> ZSort.t

  val of_string : ZCtx.t -> string -> t

  val create_addrkh : ZCtx.t -> ZKeyHash.t -> t

  val _read_innerkh : t -> ZKeyHash.t
  
  val create_cmp : ZCtx.t -> t -> t -> ZInt.t
  val create_eq : ZCtx.t -> t -> t -> ZBool.t
end


(******************************************************************************)
(******************************************************************************)
(* Ors                                                                        *)
(******************************************************************************)
(******************************************************************************)

module ZOr : sig
  type t = ZExpr.t

  val sort : ZCtx.t -> left_sort:ZSort.t -> right_sort:ZSort.t -> ZSort.t

  val create_left : ZCtx.t -> left_content:ZExpr.t -> right_sort:ZSort.t -> t
  val create_right : ZCtx.t -> left_sort:ZSort.t -> right_content:ZExpr.t -> t
  val read_left : t -> ZExpr.t
  val read_right : t -> ZExpr.t

  val create_eq : ZCtx.t -> t -> t -> ZBool.t
  val create_neq : ZCtx.t -> t -> t -> ZBool.t

  val is_left : t -> ZBool.t
  val is_right : t -> ZBool.t
end


(******************************************************************************)
(******************************************************************************)
(* Lists                                                                      *)
(******************************************************************************)
(******************************************************************************)

module ZList : sig
  type t = ZExpr.t

  val create_sort : ZCtx.t -> content_sort:ZSort.t -> ZSort.t

  val create : ZCtx.t -> content_sort:ZSort.t -> t
  val read_head : t -> ZExpr.t
  val read_tail : t -> t
  val update : t -> content:ZExpr.t -> t

  val create_eq : ZCtx.t -> t -> t -> ZBool.t
  val create_neq : ZCtx.t -> t -> t -> ZBool.t

  val is_nil : t -> ZBool.t
  val is_cons : t -> ZBool.t
end


(******************************************************************************)
(******************************************************************************)
(* Maps                                                                       *)
(******************************************************************************)
(******************************************************************************)

module ZMap : sig
  type t = ZExpr.t

  val create_sort : ZCtx.t -> key_sort:ZSort.t -> value_sort:ZSort.t -> ZSort.t

  val read_default_value : ZCtx.t -> t -> ZExpr.t

  val create : ZCtx.t -> key_sort:ZSort.t -> value_sort:ZSort.t -> t
  val read_value : ZCtx.t -> key:ZExpr.t -> map:t -> ZExpr.t
  val read_exist : ZCtx.t -> key:ZExpr.t -> map:t -> ZBool.t
  val update : ZCtx.t -> key:ZExpr.t -> value:ZExpr.t -> map:t -> t

  val create_eq : ZCtx.t -> t -> t -> ZBool.t
  val create_neq : ZCtx.t -> t -> t -> ZBool.t
end


(******************************************************************************)
(******************************************************************************)
(* Sets                                                                       *)
(******************************************************************************)
(******************************************************************************)

module ZSet = ZMap


(******************************************************************************)
(******************************************************************************)
(* Operations                                                                 *)
(******************************************************************************)
(******************************************************************************)

module ZOperation : sig
  type t = ZExpr.t
  
  val sort : ZCtx.t -> ZSort.t

  val create_eq : ZCtx.t -> t -> t -> ZBool.t
  val create_neq : ZCtx.t -> t -> t -> ZBool.t
end


(******************************************************************************)
(******************************************************************************)
(* Contracts                                                                  *)
(******************************************************************************)
(******************************************************************************)

module ZContract : sig
  type t = ZExpr.t
  
  val sort : ZCtx.t -> ZSort.t

  val create_eq : ZCtx.t -> t -> t -> ZBool.t
  val create_neq : ZCtx.t -> t -> t -> ZBool.t
end


(******************************************************************************)
(******************************************************************************)
(* Lambdas                                                                    *)
(******************************************************************************)
(******************************************************************************)

module ZLambda : sig
  type t = ZExpr.t
  
  val sort : ZCtx.t -> ZSort.t

  val create_eq : ZCtx.t -> t -> t -> ZBool.t
  val create_neq : ZCtx.t -> t -> t -> ZBool.t
end


(******************************************************************************)
(******************************************************************************)
(* Model                                                                      *)
(******************************************************************************)
(******************************************************************************)

module ZModel : sig
  type t = Z3.Model.model

  val eval : ZExpr.t -> model:t -> ZExpr.t option

  val to_string : t -> string
end


(******************************************************************************)
(******************************************************************************)
(* Solver                                                                     *)
(******************************************************************************)
(******************************************************************************)

module ZSolver : sig
  type t = Z3.Solver.solver
  type validity = VAL | INVAL | UNKNOWN
  type satisfiability = SAT | UNSAT | UNKNOWN

  val create : ZCtx.t -> t
  val _formula_add : t -> ZFormula.t list -> unit
  
  val check_satisfiability : t -> ZCtx.t -> ZFormula.t list -> (satisfiability * ZModel.t option)
  val check_validity : t -> ZCtx.t -> ZFormula.t list -> (validity * ZModel.t option)

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