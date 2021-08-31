(* Smt: Interface to the Z3 SMT solver *)

exception SmtError = Z3.Error

open! Core

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(* Map of Tz.mich_t Tz.cc *)
module MTMap = Map.Make (Tz.MichTCC_cmp)

(* Map of Tz.mich_v Tz.cc *)
module MVMap = Map.Make (Tz.MichVCC_cmp)

type constructor =
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

module CST_cmp = struct
  type t = constructor [@@deriving sexp, compare]
end

module CSTMap = Map.Make (CST_cmp)

(******************************************************************************)
(******************************************************************************)
(* Constant Values                                                            *)
(******************************************************************************)
(******************************************************************************)

module Constant = struct
  (* Name of Expression *)
  let _name_lambda_domain_sym : string = "LAMBDA_DOMAIN"

  let _name_dummy_sym : string = "DUMMY_SYM"

  let _name_dummy_sort : string = "DUMMY_SORT"

  (* Name of Sort *)

  let _sort_formula : string = "Formula"

  let _sort_unit : string = "Unit"

  let _sort_key : string = "Key"

  let _sort_keyhash : string = "KeyHash"

  let _sort_option : string = "Option"

  let _sort_pair : string = "Pair"

  let _sort_bytes : string = "Bytes"

  let _sort_signature : string = "Signature"

  let _sort_address : string = "Address"

  let _sort_or : string = "Or"

  let _sort_list : string = "List"

  let _sort_map : string = "Map"

  let _sort_operation : string = "Operation"

  let _sort_contract : string = "Contract"

  let _sort_lambda : string = "Lambda"

  (* Index of Constructor *)

  let _idx_const_unit : int = 0

  let _idx_const_key : int = 0

  let _idx_const_keyhash_str : int = 0

  let _idx_const_keyhash_key : int = 1

  let _idx_const_option_none : int = 0

  let _idx_const_option_some : int = 1

  let _idx_const_pair : int = 0

  let _idx_const_bytes_nil : int = 0

  let _idx_const_bytes_str : int = 1

  let _idx_const_bytes_concat : int = 2

  let _idx_const_bytes_blake2b : int = 3

  let _idx_const_bytes_sha256 : int = 4

  let _idx_const_bytes_sha512 : int = 5

  let _idx_const_bytes_pack : int = 6

  let _idx_const_bytes_sliced : int = 7

  let _idx_const_signature_str : int = 0

  let _idx_const_signature_signed : int = 1

  let _idx_const_address : int = 0

  let _idx_const_or_left : int = 0

  let _idx_const_or_right : int = 1

  let _idx_const_operation_create_contract : int = 0

  let _idx_const_operation_transfer_tokens : int = 1

  let _idx_const_operation_set_delegate : int = 2

  let _idx_const_contract : int = 0

  let _idx_const_lambda : int = 0

  (* Name of Constructor Function *)

  let _const_unit : string = "const_unit"

  let _const_key : string = "const_key"

  let _const_keyhash_str : string = "const_keyhash_str"

  let _const_keyhash_key : string = "const_keyhash_key"

  let _const_option_none : string = "const_option_none"

  let _const_option_some : string = "const_option_some"

  let _const_pair : string = "const_pair"

  let _const_bytes_nil : string = "const_bytes_nil"

  let _const_bytes_str : string = "const_bytes_str"

  let _const_bytes_concat : string = "const_bytes_concat"

  let _const_bytes_blake2b : string = "const_bytes_blake2b"

  let _const_bytes_sha256 : string = "const_bytes_sha256"

  let _const_bytes_sha512 : string = "const_bytes_sha512"

  let _const_bytes_pack : string = "const_bytes_pack"

  let _const_bytes_sliced : string = "const_bytes_sliced"

  let _const_signature_str : string = "const_signature_str"

  let _const_signature_signed : string = "const_signature_signed"

  let _const_address : string = "const_address"

  let _const_or_left : string = "const_or_left"

  let _const_or_right : string = "const_or_right"

  let _const_operation_create_contract : string =
     "const_operation_create_contract"

  let _const_operation_transfer_tokens : string =
     "const_operation_transfer_tokens"

  let _const_operation_set_delegate : string = "const_operation_set_delegate"

  let _const_contract : string = "const_contract"

  let _const_lambda : string = "const_lambda"

  (* Name of Recognizer Function *)

  let _recog_unit : string = "is_unit"

  let _recog_key : string = "is_key"

  let _recog_keyhash_str : string = "is_keyhash_str"

  let _recog_keyhash_key : string = "is_keyhash_key"

  let _recog_option_none : string = "is_none"

  let _recog_option_some : string = "is_some"

  let _recog_pair : string = "is_pair"

  let _recog_bytes_nil : string = "is_bytes_nil"

  let _recog_bytes_str : string = "is_bytes_str"

  let _recog_bytes_concat : string = "is_bytes_concat"

  let _recog_bytes_blake2b : string = "is_bytes_blake2b"

  let _recog_bytes_sha256 : string = "is_bytes_sha256"

  let _recog_bytes_sha512 : string = "is_bytes_sha512"

  let _recog_bytes_pack : string = "is_bytes_pack"

  let _recog_bytes_sliced : string = "is_bytes_sliced"

  let _recog_signature_str : string = "is_signature_str"

  let _recog_signature_signed : string = "is_signature_signed"

  let _recog_address : string = "is_address"

  let _recog_or_left : string = "is_left"

  let _recog_or_right : string = "is_right"

  let _recog_operation_create_contract : string = "is_operation_create_contract"

  let _recog_operation_transfer_tokens : string = "is_operation_transfer_tokens"

  let _recog_operation_set_delegate : string = "is_operation_set_delegate"

  let _recog_contract : string = "is_contract"

  let _recog_lambda : string = "is_lambda"

  (* Index of Field *)

  let _idx_field_content : int = 0

  let _idx_field_fst : int = 0

  let _idx_field_snd : int = 1

  let _idx_field_bytes_offset : int = 1 (* [ content; offset; length; ] *)

  let _idx_field_bytes_length : int = 2 (* [ content; offset; length; ] *)

  (* Name of Field *)

  let _field_content : string = "content"

  let _field_fst : string = "fst"

  let _field_snd : string = "snd"

  let _field_bytes_offset : string = "offset"

  let _field_bytes_length : string = "length"

  (* Others *)

  let _bit_mutez : int = 63

  let _int2bv_precision : int = 128
end

(******************************************************************************)
(******************************************************************************)
(* Z3 Interfaces                                                              *)
(******************************************************************************)
(******************************************************************************)

(******************************************************************************)
(* Context                                                                    *)
(******************************************************************************)

module Ctx = struct
  type body = string * string

  type t = {
    id : int;
    ctx : (Z3.context[@sexp.opaque] [@ignore]);
    const_map :
      (Z3.Datatype.Constructor.constructor[@sexp.opaque] [@ignore]) CSTMap.t ref;
    sort_map : (Z3.Sort.sort[@sexp.opaque] [@ignore]) MTMap.t ref;
    expr_map : (Z3.Expr.expr[@sexp.opaque] [@ignore]) MVMap.t ref;
  }
  [@@deriving sexp, compare, equal]

  let body_timeout : unit -> body =
    fun () ->
    let budget = !Utils.Argument.z3_timeout * 1000 in
    ("timeout", string_of_int budget)
  (* function body_timeout end *)

  let create : unit -> t =
     let (id : int ref) = ref 0 in
     fun () ->
     let _ = incr id in
     {
       id = !id;
       ctx = [ body_timeout () ] |> Z3.mk_context;
       const_map = ref CSTMap.empty;
       sort_map = ref MTMap.empty;
       expr_map = ref MVMap.empty;
     }
  (* function create end *)

  let read : t -> Z3.context = (fun { ctx; _ } -> ctx)

  let read_id : t -> int = (fun { id; _ } -> id)

  let read_const :
      t ->
      constructor ->
      f:(unit -> Z3.Datatype.Constructor.constructor) ->
      Z3.Datatype.Constructor.constructor =
    fun { const_map; _ } cst ~f ->
    match CSTMap.find !const_map cst with
    | None     ->
      let (new_const : Z3.Datatype.Constructor.constructor) = f () in
      let _ = const_map := CSTMap.set !const_map ~key:cst ~data:new_const in
      new_const
    | Some ccc -> ccc
  (* function read_const end *)

  let read_sort :
      t -> Tz.mich_t Tz.cc -> f:(unit -> Z3.Sort.sort) -> Z3.Sort.sort =
    fun { sort_map; _ } mtcc ~f ->
    match MTMap.find !sort_map mtcc with
    | None     ->
      let (new_sort : Z3.Sort.sort) = f () in
      let _ = sort_map := MTMap.set !sort_map ~key:mtcc ~data:new_sort in
      new_sort
    | Some sss -> sss
  (* function read_sort end *)

  let read_expr :
      t -> Tz.mich_v Tz.cc -> f:(unit -> Z3.Expr.expr) -> Z3.Expr.expr =
    fun { expr_map; _ } mvcc ~f ->
    match MVMap.find !expr_map mvcc with
    | None     ->
      let (new_expr : Z3.Expr.expr) = f () in
      let _ = expr_map := MVMap.set !expr_map ~key:mvcc ~data:new_expr in
      new_expr
    | Some eee -> eee
  (* function read_expr end *)
end

(******************************************************************************)
(* Symbol                                                                     *)
(******************************************************************************)

module Sym = struct
  type t = Z3.Symbol.symbol

  let create : Ctx.t -> string -> t =
    (fun ctx name -> Z3.Symbol.mk_string (Ctx.read ctx) name)
  (* function create end *)

  let create_dummy : Ctx.t -> t =
     let (cnt : int ref) = ref 0 in
     fun ctx ->
     let _ = incr cnt in
     create ctx (Constant._name_dummy_sym ^ string_of_int !cnt)
  (* function create_dummy end *)

  let create_lst : Ctx.t -> string list -> t list =
    (fun ctx names -> Z3.Symbol.mk_strings (Ctx.read ctx) names)
  (* function create_lst end *)

  let to_string : t -> string = (fun sym -> Z3.Symbol.to_string sym)

  let compare : t -> t -> int =
    (fun sym1 sym2 -> compare_string (to_string sym1) (to_string sym2))
  (* function compare end *)

  let equal : t -> t -> bool =
    (fun sym1 sym2 -> equal_string (to_string sym1) (to_string sym2))
  (* function equal end *)
end

(******************************************************************************)
(* Sort                                                                       *)
(******************************************************************************)

module Sort = struct
  type t = Z3.Sort.sort

  let create : Ctx.t -> name:string -> t =
    (fun ctx ~name -> Z3.Sort.mk_uninterpreted_s (Ctx.read ctx) name)
  (* function create end *)

  let create_dummy : Ctx.t -> t =
     let (cnt : int ref) = ref 0 in
     fun ctx ->
     let _ = incr cnt in
     create ctx ~name:(Constant._name_dummy_sort ^ string_of_int !cnt)
  (* function create_dummy end *)

  let to_string : t -> string =
    fun sort ->
    Z3.Sort.to_string sort |> String.substr_replace_all ~pattern:"|" ~with_:""
  (* function to_string end *)

  let compare : t -> t -> int =
    fun sort1 sort2 ->
    compare_int (Z3.Sort.get_id sort1) (Z3.Sort.get_id sort2)
  (* function compare end *)

  let equal : t -> t -> bool = (fun sort1 sort2 -> Z3.Sort.equal sort1 sort2)
end

(******************************************************************************)
(* Expression                                                                 *)
(******************************************************************************)

module Expr = struct
  type t = Z3.Expr.expr

  let create_var : Ctx.t -> Sort.t -> name:string -> t =
    (fun ctx sort ~name -> Z3.Expr.mk_const_s (Ctx.read ctx) name sort)
  (* function create_var end *)

  let create_dummy : Ctx.t -> Sort.t -> t =
    (fun ctx sort -> Z3.Expr.mk_const (Ctx.read ctx) (Sym.create_dummy ctx) sort)
  (* function create_dummy end *)

  let read_sort : t -> Sort.t = (fun expr -> Z3.Expr.get_sort expr)

  let to_string : t -> string = (fun expr -> Z3.Expr.to_string expr)

  let compare : t -> t -> int = (fun expr1 expr2 -> Z3.Expr.compare expr1 expr2)

  let equal : t -> t -> bool = (fun expr1 expr2 -> Z3.Expr.equal expr1 expr2)
end

(******************************************************************************)
(* Function Declarations & Data Types                                         *)
(******************************************************************************)

let get : 'a list -> idx:int -> 'a =
  fun lst ~idx ->
  List.nth lst idx
  |> function
  | Some aaa -> aaa
  | None     ->
    SmtError
      ("get : idx #"
      ^ string_of_int idx
      ^ " is not in list length "
      ^ string_of_int (List.length lst)
      )
    |> raise
(* function get end *)

(* Function Declarations ******************************************************)

module Func = struct
  type t = Z3.FuncDecl.func_decl

  let apply : t -> params:Expr.t list -> Expr.t =
    (fun func ~params -> Z3.FuncDecl.apply func params)
  (* function apply end *)

  let read_num_of_domain : t -> int =
    (fun func -> Z3.FuncDecl.get_domain_size func)
  (* function read_num_of_domain end *)

  let read_sort_of_domain : t -> idx:int -> Sort.t =
    (fun func ~idx -> Z3.FuncDecl.get_domain func |> get ~idx)
  (* function read_sort_of_domain end *)

  let read_sort_of_range : t -> Sort.t = (fun func -> Z3.FuncDecl.get_range func)
  (* function read_sort_of_range end *)
end

(* Data Type Constructor ******************************************************)

module DataConst = struct
  type t = Z3.Datatype.Constructor.constructor

  type info = {
    (* Name of constructor *)
    name : string;
    (* Name of function for recognizing constructor *)
    recog_func_name : string;
    (* Pairs of name and sort for each field *)
    field : (string * Sort.t option) list;
  }

  let create_constructor : Ctx.t -> info -> t =
    fun ctx info ->
    let ((field_names : string list), (field_sorts : Sort.t option list)) =
       List.unzip info.field
    in
    let (field_refs : int list) =
       List.mapi field_sorts ~f:(fun idx sort_opt ->
           match (idx, sort_opt) with
           | (idx, Some _) -> idx + 1
           | (_, None)     -> 0
       )
    in
    Z3.Datatype.mk_constructor_s (Ctx.read ctx) info.name
      (Sym.create ctx info.recog_func_name)
      (Sym.create_lst ctx field_names)
      field_sorts field_refs
  (* function create_constructor end *)

  let read_num_of_field : t -> int =
    (fun const -> Z3.Datatype.Constructor.get_num_fields const)
  (* function read_num_of_field end *)

  let make_func_for_constructor : t -> Func.t =
    (fun const -> Z3.Datatype.Constructor.get_constructor_decl const)
  (* function make_func_for_constructor end *)

  let get_func_for_constructor : t -> idx:int -> Func.t =
    fun const ~idx ->
    Z3.Datatype.Constructor.get_accessor_decls const |> get ~idx
  (* function get_func_for_constructor end *)

  let recog_func_for_constructor : t -> Func.t =
    (fun const -> Z3.Datatype.Constructor.get_tester_decl const)
  (* function recog_func_for_constructor end *)
end

(* Data Type ******************************************************************)

module DataType = struct
  let create_sort : Ctx.t -> name:string -> DataConst.t list -> Sort.t =
    fun ctx ~name const_lst ->
    Z3.Datatype.mk_sort_s (Ctx.read ctx) name const_lst
  (* function create_sort end *)

  let read_num_of_constructor : Sort.t -> int =
    (fun sort -> Z3.Datatype.get_num_constructors sort)
  (* function read_num_of_constructor end *)

  let make_func_for_type : Sort.t -> const_idx:int -> Func.t =
    fun sort ~const_idx ->
    Z3.Datatype.get_constructors sort |> get ~idx:const_idx
  (* function make_func_for_type end *)

  let get_func_for_type : Sort.t -> const_idx:int -> field_idx:int -> Func.t =
    fun sort ~const_idx ~field_idx ->
    Z3.Datatype.get_accessors sort |> get ~idx:const_idx |> get ~idx:field_idx
  (* function get_func_for_type end *)

  let recog_func_for_type : Sort.t -> const_idx:int -> Func.t =
    fun sort ~const_idx ->
    Z3.Datatype.get_recognizers sort |> get ~idx:const_idx
  (* function recog_func_for_type end *)

  let create_expr : Sort.t -> const_idx:int -> Expr.t list -> Expr.t =
    fun sort ~const_idx params ->
    make_func_for_type sort ~const_idx |> Func.apply ~params
  (* function create_expr end *)

  let read_sort_of_field : Sort.t -> const_idx:int -> field_idx:int -> Sort.t =
    fun sort ~const_idx ~field_idx ->
    get_func_for_type sort ~const_idx ~field_idx |> Func.read_sort_of_range
  (* function read_sort_of_field end *)

  let read_expr_of_field : Expr.t -> const_idx:int -> field_idx:int -> Expr.t =
    fun expr ~const_idx ~field_idx ->
    Expr.read_sort expr
    |> get_func_for_type ~const_idx ~field_idx
    |> Func.apply ~params:[ expr ]
  (* function read_expr_of_field end *)

  let read_expr_is_const : Expr.t -> const_idx:int -> Expr.t =
    (* Sort of output expression is boolean sort *)
    fun expr ~const_idx ->
    Expr.read_sort expr
    |> recog_func_for_type ~const_idx
    |> Func.apply ~params:[ expr ]
  (* function read_expr_if_const end *)
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
end) =
struct
  include Typ

  let create_sort : Ctx.t -> Sort.t =
    fun ctx ->
    Ctx.read_sort ctx Typ.mt_cc ~f:(fun () ->
        Z3.Arithmetic.Integer.mk_sort (Ctx.read ctx)
    )
  (* function create_sort end *)

  let create_expr : Ctx.t -> int -> Expr.t =
    fun ctx value ->
    let (mv_int_cc : Tz.mich_v Tz.cc) = Typ.gen_mv_lit_cc value in
    Ctx.read_expr ctx mv_int_cc ~f:(fun () ->
        Z3.Arithmetic.Integer.mk_numeral_i (Ctx.read ctx) value
    )
  (* function create_expr end *)

  let create_expr_of_bigint : Ctx.t -> Bigint.t -> Expr.t =
    fun ctx value ->
    let (mv_int_cc : Tz.mich_v Tz.cc) = Typ.gen_mv_lit_cc_of_bigint value in
    Ctx.read_expr ctx mv_int_cc ~f:(fun () ->
        Z3.Arithmetic.Integer.mk_numeral_s (Ctx.read ctx)
          (Bigint.to_string value)
    )
  (* function create_expr_of_bigint end *)

  let create_add : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    (fun ctx expr1 expr2 -> Z3.Arithmetic.mk_add (Ctx.read ctx) [ expr1; expr2 ])
  (* function create_add end *)

  let create_sub : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    (fun ctx expr1 expr2 -> Z3.Arithmetic.mk_sub (Ctx.read ctx) [ expr1; expr2 ])
  (* function create_sub end *)

  let create_mul : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    (fun ctx expr1 expr2 -> Z3.Arithmetic.mk_mul (Ctx.read ctx) [ expr1; expr2 ])
  (* function create_mul end *)

  let create_div : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    (fun ctx expr1 expr2 -> Z3.Arithmetic.mk_div (Ctx.read ctx) expr1 expr2)
  (* function create_div end *)

  let create_mod : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    fun ctx expr1 expr2 ->
    Z3.Arithmetic.Integer.mk_mod (Ctx.read ctx) expr1 expr2
  (* function create_mode end *)
end

module ZInt = struct
  module Typ = struct
    type elt = int

    let (mt_cc : Tz.mich_t Tz.cc) = TzUtil.gen_dummy_cc Tz.MT_int

    let gen_mv_lit_cc : int -> Tz.mich_v Tz.cc =
      (fun value -> TzUtil.gen_dummy_cc (Tz.MV_lit_int (Bigint.of_int value)))
    (* function gen_mv_lit_cc end *)

    let gen_mv_lit_cc_of_bigint : Bigint.t -> Tz.mich_v Tz.cc =
      (fun value -> TzUtil.gen_dummy_cc (Tz.MV_lit_int value))
    (* function gen_mv_lit_cc_of_bigint end *)
  end

  include Arithmetic (Typ)

  let create_neg : Ctx.t -> Expr.t -> Expr.t =
    (fun ctx expr1 -> Z3.Arithmetic.mk_unary_minus (Ctx.read ctx) expr1)
  (* function create_neg end *)

  let create_not : Ctx.t -> Expr.t -> Expr.t =
    fun ctx expr1 ->
    let (neg : Expr.t) = create_neg ctx expr1 in
    create_sub ctx neg (create_expr ctx 1)
  (* function create_not end *)
end

module ZNat = struct
  module Typ = struct
    type elt = int

    let (mt_cc : Tz.mich_t Tz.cc) = TzUtil.gen_dummy_cc Tz.MT_nat

    let gen_mv_lit_cc : int -> Tz.mich_v Tz.cc =
      (fun value -> TzUtil.gen_dummy_cc (Tz.MV_lit_nat (Bigint.of_int value)))
    (* function gen_mv_lit_cc end *)

    let gen_mv_lit_cc_of_bigint : Bigint.t -> Tz.mich_v Tz.cc =
      (fun value -> TzUtil.gen_dummy_cc (Tz.MV_lit_nat value))
    (* function gen_mv_lit_cc_of_bigint end *)
  end

  include Arithmetic (Typ)

  let to_finite_bv : Ctx.t -> Expr.t -> Expr.t =
    fun ctx expr1 ->
    Z3.Arithmetic.Integer.mk_int2bv (Ctx.read ctx) Constant._int2bv_precision
      expr1
  (* function to_finite_bv end *)

  let create_convertable_to_finite_bv : Ctx.t -> Expr.t -> Expr.t =
     let (max_int2bv : Bigint.t) =
        Bigint.pow (Bigint.of_int 2) (Bigint.of_int Constant._int2bv_precision)
     in
     let (mv_max_int2bv : Tz.mich_v Tz.cc) =
        Tz.MV_lit_int max_int2bv |> TzUtil.gen_dummy_cc
     in
     (* Sort of output expression is boolean sort *)
     fun ctx expr1 ->
     let (max : Z3.Expr.expr) =
        Ctx.read_expr ctx mv_max_int2bv ~f:(fun () ->
            Z3.Arithmetic.Integer.mk_numeral_s (Ctx.read ctx)
              (Bigint.to_string max_int2bv)
        )
     in
     Z3.Arithmetic.mk_lt (Ctx.read ctx) expr1 max
  (* function check_convertable_to_finite_bv end *)

  let create_abs : Ctx.t -> Expr.t -> Expr.t =
    fun ctx expr1 ->
    Z3.Boolean.mk_ite (Ctx.read ctx)
      (Z3.Arithmetic.mk_lt (Ctx.read ctx) expr1 (create_expr ctx 0))
      (Z3.Arithmetic.mk_unary_minus (Ctx.read ctx) expr1)
      expr1
  (* function create_abs end *)

  let create_power : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    (fun ctx expr1 expr2 -> Z3.Arithmetic.mk_power (Ctx.read ctx) expr1 expr2)
  (* function create_power *)

  let create_shift_l : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    fun ctx expr1 expr2 ->
    let (exponent : Expr.t) = create_power ctx (create_expr ctx 2) expr2 in
    create_mul ctx expr1 exponent
  (* function create_shift_l end *)

  let create_shift_r : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    fun ctx expr1 expr2 ->
    let (exponent : Expr.t) = create_power ctx (create_expr ctx 2) expr2 in
    create_div ctx expr1 exponent
  (* function create_shift_r end *)

  let create_and : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    fun ctx expr1 expr2 ->
    let (f1 : Expr.t) = create_convertable_to_finite_bv ctx expr1 in
    let (f2 : Expr.t) = create_convertable_to_finite_bv ctx expr2 in
    Z3.Boolean.mk_ite (Ctx.read ctx)
      (Z3.Boolean.mk_and (Ctx.read ctx) [ f1; f2 ])
      (let (cvt_expr1 : Expr.t) = to_finite_bv ctx expr1 in
       let (cvt_expr2 : Expr.t) = to_finite_bv ctx expr2 in
       let (ret_expr : Expr.t) =
          Z3.BitVector.mk_and (Ctx.read ctx) cvt_expr1 cvt_expr2
       in
       Z3.BitVector.mk_bv2int (Ctx.read ctx) ret_expr false
      )
      (Expr.create_dummy ctx (create_sort ctx))
  (* function create_and end *)

  let create_or : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    fun ctx expr1 expr2 ->
    let (f1 : Expr.t) = create_convertable_to_finite_bv ctx expr1 in
    let (f2 : Expr.t) = create_convertable_to_finite_bv ctx expr2 in
    Z3.Boolean.mk_ite (Ctx.read ctx)
      (Z3.Boolean.mk_and (Ctx.read ctx) [ f1; f2 ])
      (let (cvt_expr1 : Expr.t) = to_finite_bv ctx expr1 in
       let (cvt_expr2 : Expr.t) = to_finite_bv ctx expr2 in
       let (ret_expr : Expr.t) =
          Z3.BitVector.mk_or (Ctx.read ctx) cvt_expr1 cvt_expr2
       in
       Z3.BitVector.mk_bv2int (Ctx.read ctx) ret_expr false
      )
      (Expr.create_dummy ctx (create_sort ctx))
  (* function create_or end *)

  let create_xor : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    fun ctx expr1 expr2 ->
    let (f1 : Expr.t) = create_convertable_to_finite_bv ctx expr1 in
    let (f2 : Expr.t) = create_convertable_to_finite_bv ctx expr2 in
    Z3.Boolean.mk_ite (Ctx.read ctx)
      (Z3.Boolean.mk_and (Ctx.read ctx) [ f1; f2 ])
      (let (cvt_expr1 : Expr.t) = to_finite_bv ctx expr1 in
       let (cvt_expr2 : Expr.t) = to_finite_bv ctx expr2 in
       let (ret_expr : Expr.t) =
          Z3.BitVector.mk_xor (Ctx.read ctx) cvt_expr1 cvt_expr2
       in
       Z3.BitVector.mk_bv2int (Ctx.read ctx) ret_expr false
      )
      (Expr.create_dummy ctx (create_sort ctx))
  (* function create_xor end *)
end

module ZMutez = struct
  module Typ = struct
    type elt = int

    let (mt_cc : Tz.mich_t Tz.cc) = TzUtil.gen_dummy_cc Tz.MT_mutez

    let gen_mv_lit_cc : int -> Tz.mich_v Tz.cc =
      (fun value -> TzUtil.gen_dummy_cc (Tz.MV_lit_mutez (Bigint.of_int value)))
    (* function gen_mv_lit_cc end *)

    let gen_mv_lit_cc_of_bigint : Bigint.t -> Tz.mich_v Tz.cc =
      (fun value -> TzUtil.gen_dummy_cc (Tz.MV_lit_mutez value))
    (* function gen_mv_lit_cc_of_bigint end *)
  end

  include Arithmetic (Typ)
end

module ZTimestamp = struct
  module Typ = struct
    type elt = int

    let (mt_cc : Tz.mich_t Tz.cc) = TzUtil.gen_dummy_cc Tz.MT_timestamp

    let gen_mv_lit_cc : int -> Tz.mich_v Tz.cc =
      fun value ->
      TzUtil.gen_dummy_cc (Tz.MV_lit_timestamp_sec (Bigint.of_int value))
    (* function gen_mv_lit_cc end *)

    let gen_mv_lit_cc_of_bigint : Bigint.t -> Tz.mich_v Tz.cc =
      (fun value -> TzUtil.gen_dummy_cc (Tz.MV_lit_timestamp_sec value))
    (* function gen_mv_lit_cc_of_bigint end *)
  end

  include Arithmetic (Typ)
end

(* Boolean ********************************************************************)

module ZBool = struct
  type elt = bool

  let (mt_cc : Tz.mich_t Tz.cc) = TzUtil.gen_dummy_cc Tz.MT_bool

  let gen_mv_lit_cc : elt -> Tz.mich_v Tz.cc =
    (fun value -> TzUtil.gen_dummy_cc (Tz.MV_lit_bool value))
  (* function gen_mv_lit_cc end *)

  let create_sort : Ctx.t -> Sort.t =
    fun ctx ->
    Ctx.read_sort ctx mt_cc ~f:(fun () -> Z3.Boolean.mk_sort (Ctx.read ctx))
  (* function create_sort end *)

  let create_expr : Ctx.t -> elt -> Expr.t =
    fun ctx value ->
    let (mv_bool_cc : Tz.mich_v Tz.cc) = gen_mv_lit_cc value in
    Ctx.read_expr ctx mv_bool_cc ~f:(fun () ->
        Z3.Boolean.mk_val (Ctx.read ctx) value
    )
  (* function create_expr end *)

  let create_not : Ctx.t -> Expr.t -> Expr.t =
    (fun ctx expr1 -> Z3.Boolean.mk_not (Ctx.read ctx) expr1)
  (* function create_not end *)

  let create_and : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    (fun ctx expr1 expr2 -> Z3.Boolean.mk_and (Ctx.read ctx) [ expr1; expr2 ])
  (* function create_and end *)

  let create_or : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    (fun ctx expr1 expr2 -> Z3.Boolean.mk_or (Ctx.read ctx) [ expr1; expr2 ])
  (* function create_or end *)

  let create_xor : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    (fun ctx expr1 expr2 -> Z3.Boolean.mk_xor (Ctx.read ctx) expr1 expr2)
  (* function create_xor end *)
end

(* String *********************************************************************)

module ZStr = struct
  type elt = string

  let (mt_cc : Tz.mich_t Tz.cc) = TzUtil.gen_dummy_cc Tz.MT_string

  let gen_mv_lit_cc : elt -> Tz.mich_v Tz.cc =
    (fun value -> TzUtil.gen_dummy_cc (Tz.MV_lit_string value))
  (* function gen_mv_lit_cc end *)

  let create_sort : Ctx.t -> Sort.t =
    fun ctx ->
    Ctx.read_sort ctx mt_cc ~f:(fun () -> Z3.Seq.mk_string_sort (Ctx.read ctx))
  (* function create_sort end *)

  let create_expr : Ctx.t -> elt -> Expr.t =
    fun ctx value ->
    let (mv_bool_cc : Tz.mich_v Tz.cc) = gen_mv_lit_cc value in
    Ctx.read_expr ctx mv_bool_cc ~f:(fun () ->
        Z3.Seq.mk_string (Ctx.read ctx) value
    )
  (* function create_expr end *)

  let create_concat : Ctx.t -> Expr.t list -> Expr.t =
    (fun ctx expr_lst -> Z3.Seq.mk_seq_concat (Ctx.read ctx) expr_lst)
  (* function create_concat end *)

  let create_slice : Ctx.t -> Expr.t -> Expr.t -> Expr.t -> Expr.t =
    (* Sort of input expressions expr_offset and expr_len are integer sort *)
    fun ctx expr_offset expr_len expr1 ->
    Z3.Seq.mk_seq_extract (Ctx.read ctx) expr1 expr_offset
      (ZInt.create_add ctx expr_offset expr_len)
  (* function create_slice end *)

  let create_size : Ctx.t -> Expr.t -> Expr.t =
    (* Sort of output expression is integer sort *)
    (fun ctx expr1 -> Z3.Seq.mk_seq_length (Ctx.read ctx) expr1)
  (* function create_size end *)
end

(* Unit ***********************************************************************)

module ZUnit = struct
  type elt = unit

  let (mt_cc : Tz.mich_t Tz.cc) = TzUtil.gen_dummy_cc Tz.MT_unit

  let gen_mv_lit_cc : elt -> Tz.mich_v Tz.cc =
    (fun () -> TzUtil.gen_dummy_cc Tz.MV_unit)
  (* function gen_mv_lit_cc end *)

  let create_const : Ctx.t -> DataConst.t list =
    fun ctx ->
    let (const_unit : DataConst.t) =
       Ctx.read_const ctx CST_unit ~f:(fun () ->
           DataConst.create_constructor ctx
             {
               name = Constant._const_unit;
               recog_func_name = Constant._recog_unit;
               field = [];
             }
       )
    in
    [ const_unit ]
  (* function create_const end *)

  let create_sort : Ctx.t -> Sort.t =
    fun ctx ->
    Ctx.read_sort ctx mt_cc ~f:(fun () ->
        let (const_lst : DataConst.t list) = create_const ctx in
        DataType.create_sort ctx ~name:Constant._sort_unit const_lst
    )
  (* function create_sort end *)

  let create_expr : Ctx.t -> Expr.t =
    fun ctx ->
    let (mv_unit_cc : Tz.mich_v Tz.cc) = gen_mv_lit_cc () in
    Ctx.read_expr ctx mv_unit_cc ~f:(fun () ->
        let (sort : Sort.t) = create_sort ctx in
        DataType.create_expr sort ~const_idx:Constant._idx_const_unit []
    )
  (* function create_expr end *)
end

(* Key ************************************************************************)

module ZKey = struct
  type elt = string

  let (mt_cc : Tz.mich_t Tz.cc) = TzUtil.gen_dummy_cc Tz.MT_key

  let gen_mv_lit_cc : elt -> Tz.mich_v Tz.cc =
    (fun value -> TzUtil.gen_dummy_cc (Tz.MV_lit_key value))
  (* function gen_mv_lit_cc end *)

  let create_const : Ctx.t -> DataConst.t list =
    fun ctx ->
    let (const_key : DataConst.t) =
       Ctx.read_const ctx CST_key ~f:(fun () ->
           DataConst.create_constructor ctx
             {
               name = Constant._const_key;
               recog_func_name = Constant._recog_key;
               field =
                 [ (Constant._field_content, Some (ZStr.create_sort ctx)) ];
             }
       )
    in
    [ const_key ]
  (* function create_const end *)

  let create_sort : Ctx.t -> Sort.t =
    fun ctx ->
    Ctx.read_sort ctx mt_cc ~f:(fun () ->
        let (const_lst : DataConst.t list) = create_const ctx in
        DataType.create_sort ctx ~name:Constant._sort_key const_lst
    )
  (* function create_sort end *)

  let create_expr : Ctx.t -> elt -> Expr.t =
    fun ctx value ->
    let (mv_key_cc : Tz.mich_v Tz.cc) = gen_mv_lit_cc value in
    let (expr_value : Expr.t) = ZStr.create_expr ctx value in
    Ctx.read_expr ctx mv_key_cc ~f:(fun () ->
        let (sort : Sort.t) = create_sort ctx in
        DataType.create_expr sort ~const_idx:Constant._idx_const_key
          [ expr_value ]
    )
  (* function create_expr end *)

  let read_content : Expr.t -> Expr.t =
    fun expr1 ->
    DataType.read_expr_of_field expr1 ~const_idx:Constant._idx_const_key
      ~field_idx:Constant._idx_field_content
  (* function read_content end *)
end

(* Key Hash *******************************************************************)

module ZKeyHash = struct
  type elt = string

  let (mt_cc : Tz.mich_t Tz.cc) = TzUtil.gen_dummy_cc Tz.MT_key_hash

  let gen_mv_lit_cc : elt -> Tz.mich_v Tz.cc =
    (fun value -> TzUtil.gen_dummy_cc (Tz.MV_lit_key_hash value))
  (* function gen_mv_lit_cc end *)

  let create_const : Ctx.t -> DataConst.t list =
    fun ctx ->
    let (const_keyhash_str : DataConst.t) =
       Ctx.read_const ctx CST_keyhash_str ~f:(fun () ->
           DataConst.create_constructor ctx
             {
               name = Constant._const_keyhash_str;
               recog_func_name = Constant._recog_keyhash_str;
               field =
                 [ (Constant._field_content, Some (ZStr.create_sort ctx)) ];
             }
       )
    in
    let (const_keyhash_key : DataConst.t) =
       Ctx.read_const ctx CST_keyhash_key ~f:(fun () ->
           DataConst.create_constructor ctx
             {
               name = Constant._const_keyhash_key;
               recog_func_name = Constant._recog_keyhash_key;
               field =
                 [ (Constant._field_content, Some (ZKey.create_sort ctx)) ];
             }
       )
    in
    [ const_keyhash_str; const_keyhash_key ]
  (* function create_const end *)

  let create_sort : Ctx.t -> Sort.t =
    fun ctx ->
    Ctx.read_sort ctx mt_cc ~f:(fun () ->
        let (const_lst : DataConst.t list) = create_const ctx in
        DataType.create_sort ctx ~name:Constant._sort_keyhash const_lst
    )
  (* function create_sort end *)

  let create_expr : Ctx.t -> elt -> Expr.t =
    fun ctx value ->
    let (mv_keyhash_cc : Tz.mich_v Tz.cc) = gen_mv_lit_cc value in
    let (expr_value : Expr.t) = ZStr.create_expr ctx value in
    Ctx.read_expr ctx mv_keyhash_cc ~f:(fun () ->
        let (sort : Sort.t) = create_sort ctx in
        DataType.create_expr sort ~const_idx:Constant._idx_const_keyhash_str
          [ expr_value ]
    )
  (* function create_expr end *)

  let create_hashkey : Ctx.t -> Expr.t -> Expr.t =
    fun ctx expr1 ->
    let (sort : Sort.t) = create_sort ctx in
    DataType.create_expr sort ~const_idx:Constant._idx_const_keyhash_key
      [ expr1 ]
  (* function create_hashkey end *)

  let read_content_str : Expr.t -> Expr.t =
    fun expr1 ->
    DataType.read_expr_of_field expr1 ~const_idx:Constant._idx_const_keyhash_str
      ~field_idx:Constant._idx_field_content
  (* function read_content_str end *)

  let read_content_key : Expr.t -> Expr.t =
    fun expr1 ->
    DataType.read_expr_of_field expr1 ~const_idx:Constant._idx_const_keyhash_key
      ~field_idx:Constant._idx_field_content
  (* function read_content_key end *)
end

(* Option *********************************************************************)

module ZOption = struct
  let gen_mt_cc : Tz.mich_t Tz.cc -> Tz.mich_t Tz.cc =
    (fun typ -> TzUtil.gen_dummy_cc (Tz.MT_option typ))
  (* function gen_mt_cc end *)

  let gen_mv_none_cc : Tz.mich_t Tz.cc -> Tz.mich_v Tz.cc =
    (fun typ -> TzUtil.gen_dummy_cc (Tz.MV_none typ))
  (* function gen_mv_none_cc end *)

  let gen_mv_some_cc : Tz.mich_v Tz.cc -> Tz.mich_v Tz.cc =
    (fun value -> TzUtil.gen_dummy_cc (Tz.MV_some value))
  (* function gen_mv_some_cc end *)

  let create_const : Ctx.t -> Tz.mich_t Tz.cc -> DataConst.t list =
     let read_sort_of_content : Ctx.t -> Tz.mich_t Tz.cc -> Sort.t =
       fun ctx content_typ ->
       Ctx.read_sort ctx content_typ ~f:(fun () ->
           SmtError
             ("ZOption : create_const : read_sort_of_content : sort of content type not defined : "
             ^ Sexp.to_string (Tz.sexp_of_mich_t content_typ.cc_v)
             )
           |> raise
       )
     in
     (* inner-function read_sort_of_content end *)
     let gen_const_name : string -> Sort.t list -> string =
       fun const_name field_lst ->
       let (field_names : string) =
          List.map field_lst ~f:Sort.to_string |> String.concat ~sep:" "
       in
       "(" ^ const_name ^ " " ^ field_names ^ ")"
     in
     (* inner-function gen_const_name end *)
     let gen_recog_func_name : string -> Sort.t list -> string =
       fun recog_name field_lst ->
       let (field_names : string) =
          List.map field_lst ~f:Sort.to_string |> String.concat ~sep:" "
       in
       recog_name ^ "_" ^ field_names
     in
     (* inner-function gen_const_name end *)
     fun ctx content_typ ->
     let (const_option_none : DataConst.t) =
        Ctx.read_const ctx CST_option_none ~f:(fun () ->
            DataConst.create_constructor ctx
              {
                name = Constant._const_option_none;
                recog_func_name = Constant._recog_option_none;
                field = [];
              }
        )
     in
     let (const_option_some : DataConst.t) =
        Ctx.read_const ctx (CST_option_some content_typ) ~f:(fun () ->
            let (content_sort : Sort.t) =
               read_sort_of_content ctx content_typ
            in
            DataConst.create_constructor ctx
              {
                name =
                  gen_const_name Constant._const_option_some [ content_sort ];
                recog_func_name =
                  gen_recog_func_name Constant._recog_option_some
                    [ content_sort ];
                field = [ (Constant._field_content, Some content_sort) ];
              }
        )
     in
     [ const_option_none; const_option_some ]
  (* function create_const end *)

  let create_sort : Ctx.t -> content_typ:Tz.mich_t Tz.cc -> Sort.t =
     let read_sort_of_content : Ctx.t -> Tz.mich_t Tz.cc -> Sort.t =
       fun ctx content_typ ->
       Ctx.read_sort ctx content_typ ~f:(fun () ->
           SmtError
             ("ZOption : create_const : read_sort_of_content : sort of content type not defined : "
             ^ Sexp.to_string (Tz.sexp_of_mich_t content_typ.cc_v)
             )
           |> raise
       )
     in
     (* inner-function read_sort_of_content end *)
     let gen_sort_name : string -> Sort.t list -> string =
       fun sort_name field_lst ->
       let (field_names : string) =
          List.map field_lst ~f:Sort.to_string |> String.concat ~sep:" "
       in
       "(" ^ sort_name ^ " " ^ field_names ^ ")"
     in
     (* inner-function gen_sort_name end *)
     fun ctx ~content_typ ->
     Ctx.read_sort ctx (gen_mt_cc content_typ) ~f:(fun () ->
         let (content_sort : Sort.t) = read_sort_of_content ctx content_typ in
         let (name : string) =
            gen_sort_name Constant._sort_option [ content_sort ]
         in
         let (const_lst : DataConst.t list) = create_const ctx content_typ in
         DataType.create_sort ctx ~name const_lst
     )
  (* function create_sort end *)

  let create_expr_none : Ctx.t -> content_typ:Tz.mich_t Tz.cc -> Expr.t =
    fun ctx ~content_typ ->
    let (mv_none_cc : Tz.mich_v Tz.cc) = gen_mv_none_cc content_typ in
    Ctx.read_expr ctx mv_none_cc ~f:(fun () ->
        let (sort : Sort.t) = create_sort ctx ~content_typ in
        DataType.create_expr sort ~const_idx:Constant._idx_const_option_none []
    )
  (* function create_expr_none end *)

  let create_expr_some :
      Ctx.t -> content_value:Tz.mich_v Tz.cc -> Expr.t -> Expr.t =
    fun ctx ~content_value expr1 ->
    let (content_typ : Tz.mich_t Tz.cc) = TzUtil.typ_of_val content_value in
    let (mv_some_cc : Tz.mich_v Tz.cc) = gen_mv_some_cc content_value in
    Ctx.read_expr ctx mv_some_cc ~f:(fun () ->
        let (sort : Sort.t) = create_sort ctx ~content_typ in
        DataType.create_expr sort ~const_idx:Constant._idx_const_option_some
          [ expr1 ]
    )
  (* function create_expr_some end *)

  let read_content : Expr.t -> Expr.t =
    fun expr1 ->
    DataType.read_expr_of_field expr1 ~const_idx:Constant._idx_const_option_some
      ~field_idx:Constant._idx_field_content
  (* function read_content end *)
end

(* Pair ***********************************************************************)

module ZPair = struct
  let gen_mt_cc : Tz.mich_t Tz.cc * Tz.mich_t Tz.cc -> Tz.mich_t Tz.cc =
    (fun (typ1, typ2) -> TzUtil.gen_dummy_cc (Tz.MT_pair (typ1, typ2)))
  (* function gen_mt_cc end *)

  let create_const :
      Ctx.t -> Tz.mich_t Tz.cc * Tz.mich_t Tz.cc -> DataConst.t list =
     let read_sort_of_content : Ctx.t -> Tz.mich_t Tz.cc -> Sort.t =
       fun ctx content_typ ->
       Ctx.read_sort ctx content_typ ~f:(fun () ->
           SmtError
             ("ZPair : create_const : read_sort_of_content : sort of content type not defined : "
             ^ Sexp.to_string (Tz.sexp_of_mich_t content_typ.cc_v)
             )
           |> raise
       )
     in
     (* inner-function read_sort_of_content end *)
     let gen_const_name : string -> Sort.t list -> string =
       fun const_name field_lst ->
       let (field_names : string) =
          List.map field_lst ~f:Sort.to_string |> String.concat ~sep:" "
       in
       "(" ^ const_name ^ " " ^ field_names ^ ")"
     in
     (* inner-function gen_const_name end *)
     let gen_recog_func_name : string -> Sort.t list -> string =
       fun recog_name field_lst ->
       let (field_names : string) =
          List.map field_lst ~f:Sort.to_string |> String.concat ~sep:" "
       in
       recog_name ^ "_" ^ field_names
     in
     (* inner-function gen_const_name end *)
     fun ctx (content_typ1, content_typ2) ->
     let (const_pair : DataConst.t) =
        Ctx.read_const ctx
          (CST_pair (content_typ1, content_typ2))
          ~f:(fun () ->
            let (content_sort1 : Sort.t) =
               read_sort_of_content ctx content_typ1
            in
            let (content_sort2 : Sort.t) =
               read_sort_of_content ctx content_typ2
            in
            DataConst.create_constructor ctx
              {
                name =
                  gen_const_name Constant._const_pair
                    [ content_sort1; content_sort2 ];
                recog_func_name =
                  gen_recog_func_name Constant._recog_pair
                    [ content_sort1; content_sort2 ];
                field =
                  [
                    (Constant._field_fst, Some content_sort1);
                    (Constant._field_snd, Some content_sort2);
                  ];
              })
     in
     [ const_pair ]
  (* function create_const end *)

  let create_sort :
      Ctx.t -> content_typ:Tz.mich_t Tz.cc * Tz.mich_t Tz.cc -> Sort.t =
     let read_sort_of_content : Ctx.t -> Tz.mich_t Tz.cc -> Sort.t =
       fun ctx content_typ ->
       Ctx.read_sort ctx content_typ ~f:(fun () ->
           SmtError
             ("ZPair : create_const : read_sort_of_content : sort of content type not defined : "
             ^ Sexp.to_string (Tz.sexp_of_mich_t content_typ.cc_v)
             )
           |> raise
       )
     in
     (* inner-function read_sort_of_content end *)
     let gen_sort_name : string -> Sort.t list -> string =
       fun sort_name field_lst ->
       let (field_names : string) =
          List.map field_lst ~f:Sort.to_string |> String.concat ~sep:" "
       in
       "(" ^ sort_name ^ " " ^ field_names ^ ")"
     in
     (* inner-function gen_sort_name end *)
     fun ctx ~content_typ ->
     Ctx.read_sort ctx (gen_mt_cc content_typ) ~f:(fun () ->
         let (content_sort1 : Sort.t) =
            read_sort_of_content ctx (fst content_typ)
         in
         let (content_sort2 : Sort.t) =
            read_sort_of_content ctx (snd content_typ)
         in
         let (name : string) =
            gen_sort_name Constant._sort_pair [ content_sort1; content_sort2 ]
         in
         let (const_lst : DataConst.t list) = create_const ctx content_typ in
         DataType.create_sort ctx ~name const_lst
     )
  (* function create_sort end *)

  let create_expr :
      Ctx.t ->
      content_typ:Tz.mich_t Tz.cc * Tz.mich_t Tz.cc ->
      Expr.t * Expr.t ->
      Expr.t =
    fun ctx ~content_typ (expr1, expr2) ->
    let (sort : Sort.t) = create_sort ctx ~content_typ in
    DataType.create_expr sort ~const_idx:Constant._idx_const_pair
      [ expr1; expr2 ]
  (* function create_expr end *)

  let read_content_fst : Expr.t -> Expr.t =
    fun expr1 ->
    DataType.read_expr_of_field expr1 ~const_idx:Constant._idx_const_pair
      ~field_idx:Constant._idx_field_fst
  (* function read_content_fst end *)

  let read_content_snd : Expr.t -> Expr.t =
    fun expr1 ->
    DataType.read_expr_of_field expr1 ~const_idx:Constant._idx_const_pair
      ~field_idx:Constant._idx_field_snd
  (* function read_content_snd end *)
end

(* Bytes **********************************************************************)

module ZBytes = struct
  type elt = string

  let (mt_cc : Tz.mich_t Tz.cc) = TzUtil.gen_dummy_cc Tz.MT_bytes

  let gen_mv_lit_cc : elt -> Tz.mich_v Tz.cc =
    (fun value -> TzUtil.gen_dummy_cc (Tz.MV_lit_bytes value))
  (* function gen_mv_lit_cc end *)

  let create_const : Ctx.t -> DataConst.t list =
    fun ctx ->
    let (const_bytes_nil : DataConst.t) =
       Ctx.read_const ctx CST_bytes_nil ~f:(fun () ->
           DataConst.create_constructor ctx
             {
               name = Constant._const_bytes_nil;
               recog_func_name = Constant._recog_bytes_nil;
               field = [];
             }
       )
    in
    let (const_bytes_str : DataConst.t) =
       Ctx.read_const ctx CST_bytes_str ~f:(fun () ->
           DataConst.create_constructor ctx
             {
               name = Constant._const_bytes_str;
               recog_func_name = Constant._recog_bytes_str;
               field =
                 [ (Constant._field_content, Some (ZStr.create_sort ctx)) ];
             }
       )
    in
    let (const_bytes_concat : DataConst.t) =
       Ctx.read_const ctx CST_bytes_concat ~f:(fun () ->
           DataConst.create_constructor ctx
             {
               name = Constant._const_bytes_concat;
               recog_func_name = Constant._recog_bytes_concat;
               field =
                 [ (Constant._field_fst, None); (Constant._field_snd, None) ];
             }
       )
    in
    let (const_bytes_blake2b : DataConst.t) =
       Ctx.read_const ctx CST_bytes_blake2b ~f:(fun () ->
           DataConst.create_constructor ctx
             {
               name = Constant._const_bytes_blake2b;
               recog_func_name = Constant._recog_bytes_blake2b;
               field = [ (Constant._field_content, None) ];
             }
       )
    in
    let (const_bytes_sha256 : DataConst.t) =
       Ctx.read_const ctx CST_bytes_sha256 ~f:(fun () ->
           DataConst.create_constructor ctx
             {
               name = Constant._const_bytes_sha256;
               recog_func_name = Constant._recog_bytes_sha256;
               field = [ (Constant._field_content, None) ];
             }
       )
    in
    let (const_bytes_sha512 : DataConst.t) =
       Ctx.read_const ctx CST_bytes_sha512 ~f:(fun () ->
           DataConst.create_constructor ctx
             {
               name = Constant._const_bytes_sha512;
               recog_func_name = Constant._recog_bytes_sha512;
               field = [ (Constant._field_content, None) ];
             }
       )
    in
    let (const_bytes_pack : DataConst.t) =
       Ctx.read_const ctx CST_bytes_pack ~f:(fun () ->
           DataConst.create_constructor ctx
             {
               name = Constant._const_bytes_pack;
               recog_func_name = Constant._recog_bytes_pack;
               field =
                 [ (Constant._field_content, Some (ZInt.create_sort ctx)) ];
             }
       )
    in
    let (const_bytes_sliced : DataConst.t) =
       Ctx.read_const ctx CST_bytes_sliced ~f:(fun () ->
           DataConst.create_constructor ctx
             {
               name = Constant._const_bytes_sliced;
               recog_func_name = Constant._recog_bytes_sliced;
               field =
                 [
                   (Constant._field_content, None);
                   (Constant._field_bytes_offset, Some (ZInt.create_sort ctx));
                   (Constant._field_bytes_length, Some (ZInt.create_sort ctx));
                 ];
             }
       )
    in
    [
      const_bytes_nil;
      const_bytes_str;
      const_bytes_concat;
      const_bytes_blake2b;
      const_bytes_sha256;
      const_bytes_sha512;
      const_bytes_pack;
      const_bytes_sliced;
    ]
  (* function create_const end *)

  let create_sort : Ctx.t -> Sort.t =
    fun ctx ->
    Ctx.read_sort ctx mt_cc ~f:(fun () ->
        let (const_lst : DataConst.t list) = create_const ctx in
        DataType.create_sort ctx ~name:Constant._sort_bytes const_lst
    )
  (* function create_sort end *)

  let create_expr : Ctx.t -> elt -> Expr.t =
    fun ctx value ->
    let (mv_bytes_cc : Tz.mich_v Tz.cc) = gen_mv_lit_cc value in
    let (expr_value : Expr.t) = ZStr.create_expr ctx value in
    Ctx.read_expr ctx mv_bytes_cc ~f:(fun () ->
        let (sort : Sort.t) = create_sort ctx in
        DataType.create_expr sort ~const_idx:Constant._idx_const_bytes_str
          [ expr_value ]
    )
  (* function create_expr end *)

  let create_concat : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    fun ctx expr1 expr2 ->
    let (sort : Sort.t) = create_sort ctx in
    DataType.create_expr sort ~const_idx:Constant._idx_const_bytes_concat
      [ expr1; expr2 ]
  (* function create_concat end *)

  let create_blake2b : Ctx.t -> Expr.t -> Expr.t =
    fun ctx expr1 ->
    let (sort : Sort.t) = create_sort ctx in
    DataType.create_expr sort ~const_idx:Constant._idx_const_bytes_blake2b
      [ expr1 ]
  (* function create_blake2b end *)

  let create_sha256 : Ctx.t -> Expr.t -> Expr.t =
    fun ctx expr1 ->
    let (sort : Sort.t) = create_sort ctx in
    DataType.create_expr sort ~const_idx:Constant._idx_const_bytes_sha256
      [ expr1 ]
  (* function create_sha256 end *)

  let create_sha512 : Ctx.t -> Expr.t -> Expr.t =
    fun ctx expr1 ->
    let (sort : Sort.t) = create_sort ctx in
    DataType.create_expr sort ~const_idx:Constant._idx_const_bytes_sha512
      [ expr1 ]
  (* function create_sha512 end *)

  let create_pack : Ctx.t -> int -> Expr.t =
    fun ctx expr_idx ->
    let (sort : Sort.t) = create_sort ctx in
    DataType.create_expr sort ~const_idx:Constant._idx_const_bytes_pack
      [ ZInt.create_expr ctx expr_idx ]
  (* function create_pack end *)

  let create_slice : Ctx.t -> Expr.t -> Expr.t -> Expr.t -> Expr.t =
    (* Sort of input expressions expr_offset and create_sort are integer sort *)
    fun ctx expr1 expr_offset expr_length ->
    let (sort : Sort.t) = create_sort ctx in
    DataType.create_expr sort ~const_idx:Constant._idx_const_bytes_sliced
      [ expr1; expr_offset; expr_length ]
  (* function create_slice end *)

  let read_str : Expr.t -> Expr.t =
    fun expr1 ->
    DataType.read_expr_of_field expr1 ~const_idx:Constant._idx_const_bytes_str
      ~field_idx:Constant._idx_field_content
  (* function read_str end *)

  let read_content_blake2b : Expr.t -> Expr.t =
    fun expr1 ->
    DataType.read_expr_of_field expr1
      ~const_idx:Constant._idx_const_bytes_blake2b
      ~field_idx:Constant._idx_field_content
  (* function read_content_blake2b end *)

  let read_content_sha256 : Expr.t -> Expr.t =
    fun expr1 ->
    DataType.read_expr_of_field expr1
      ~const_idx:Constant._idx_const_bytes_sha256
      ~field_idx:Constant._idx_field_content
  (* function read_content_sha256 end *)

  let read_content_sha512 : Expr.t -> Expr.t =
    fun expr1 ->
    DataType.read_expr_of_field expr1
      ~const_idx:Constant._idx_const_bytes_sha512
      ~field_idx:Constant._idx_field_content
  (* function read_content_sha512 end *)

  let read_content_packed : Expr.t -> Expr.t =
    fun expr1 ->
    DataType.read_expr_of_field expr1 ~const_idx:Constant._idx_const_bytes_pack
      ~field_idx:Constant._idx_field_content
  (* function read_content_packed end *)

  let read_content_sliced : Expr.t -> Expr.t =
    fun expr1 ->
    DataType.read_expr_of_field expr1
      ~const_idx:Constant._idx_const_bytes_sliced
      ~field_idx:Constant._idx_field_content
  (* function read_content_sliced end *)

  let read_offset_sliced : Expr.t -> Expr.t =
    (* Sort of output expression is integer sort *)
    fun expr1 ->
    DataType.read_expr_of_field expr1
      ~const_idx:Constant._idx_const_bytes_sliced
      ~field_idx:Constant._idx_field_bytes_offset
  (* function read_offset_sliced end *)

  let read_length_sliced : Expr.t -> Expr.t =
    (* Sort of output expression is integer sort *)
    fun expr1 ->
    DataType.read_expr_of_field expr1
      ~const_idx:Constant._idx_const_bytes_sliced
      ~field_idx:Constant._idx_field_bytes_length
  (* function read_length_sliced end *)
end

(* Signature ******************************************************************)

module ZSig = struct
  type elt = string

  let (mt_cc : Tz.mich_t Tz.cc) = TzUtil.gen_dummy_cc Tz.MT_signature

  let gen_mv_lit_cc : elt -> Tz.mich_v Tz.cc =
    (fun value -> TzUtil.gen_dummy_cc (Tz.MV_lit_signature_str value))
  (* function gen_mv_lit_cc end *)

  let create_const : Ctx.t -> DataConst.t list =
    fun ctx ->
    let (const_signature_str : DataConst.t) =
       Ctx.read_const ctx CST_signature_str ~f:(fun () ->
           DataConst.create_constructor ctx
             {
               name = Constant._const_signature_str;
               recog_func_name = Constant._recog_signature_str;
               field =
                 [ (Constant._field_content, Some (ZStr.create_sort ctx)) ];
             }
       )
    in
    let (const_signature_signed : DataConst.t) =
       Ctx.read_const ctx CST_signature_signed ~f:(fun () ->
           DataConst.create_constructor ctx
             {
               name = Constant._const_signature_signed;
               recog_func_name = Constant._recog_signature_signed;
               field =
                 [
                   (Constant._field_fst, Some (ZKey.create_sort ctx));
                   (Constant._field_snd, Some (ZBytes.create_sort ctx));
                 ];
             }
       )
    in
    [ const_signature_str; const_signature_signed ]
  (* function create_const end *)

  let create_sort : Ctx.t -> Sort.t =
    fun ctx ->
    Ctx.read_sort ctx mt_cc ~f:(fun () ->
        let (const_lst : DataConst.t list) = create_const ctx in
        DataType.create_sort ctx ~name:Constant._sort_signature const_lst
    )
  (* function create_sort end *)

  let create_expr : Ctx.t -> elt -> Expr.t =
    fun ctx value ->
    let (mv_sig_cc : Tz.mich_v Tz.cc) = gen_mv_lit_cc value in
    let (expr_value : Expr.t) = ZStr.create_expr ctx value in
    Ctx.read_expr ctx mv_sig_cc ~f:(fun () ->
        let (sort : Sort.t) = create_sort ctx in
        DataType.create_expr sort ~const_idx:Constant._idx_const_signature_str
          [ expr_value ]
    )
  (* function create_expr end *)

  let create_signed : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    (* Sort of input expressions expr_key expr_bytes are key sort and bytes sort *)
    fun ctx expr_key expr_bytes ->
    let (sort : Sort.t) = create_sort ctx in
    DataType.create_expr sort ~const_idx:Constant._idx_const_signature_signed
      [ expr_key; expr_bytes ]
  (* function create_signed end *)

  let read_str : Expr.t -> Expr.t =
    (* Sort of output expression is string sort *)
    fun expr1 ->
    DataType.read_expr_of_field expr1
      ~const_idx:Constant._idx_const_signature_str
      ~field_idx:Constant._idx_field_content
  (* function read_str *)

  let read_key_signed : Expr.t -> Expr.t =
    (* Sort of output expression is key sort *)
    fun expr1 ->
    DataType.read_expr_of_field expr1
      ~const_idx:Constant._idx_const_signature_signed
      ~field_idx:Constant._idx_field_fst

  (* function read_key_signed end *)
  let read_bytes_sliced : Expr.t -> Expr.t =
    (* Sort of output expression is bytes sort *)
    fun expr1 ->
    DataType.read_expr_of_field expr1
      ~const_idx:Constant._idx_const_signature_signed
      ~field_idx:Constant._idx_field_snd
  (* function read_bytes_sliced end *)
end

(* Address ********************************************************************)

module ZAddr = struct
  let (mt_cc : Tz.mich_t Tz.cc) = TzUtil.gen_dummy_cc Tz.MT_address

  let create_const : Ctx.t -> DataConst.t list =
    fun ctx ->
    let (const_address : DataConst.t) =
       Ctx.read_const ctx CST_address ~f:(fun () ->
           DataConst.create_constructor ctx
             {
               name = Constant._const_address;
               recog_func_name = Constant._recog_address;
               field =
                 [ (Constant._field_content, Some (ZKeyHash.create_sort ctx)) ];
             }
       )
    in
    [ const_address ]
  (* function create_const end *)

  let create_sort : Ctx.t -> Sort.t =
    fun ctx ->
    Ctx.read_sort ctx mt_cc ~f:(fun () ->
        let (const_lst : DataConst.t list) = create_const ctx in
        DataType.create_sort ctx ~name:Constant._sort_address const_lst
    )
  (* function create_sort end *)

  let create_expr : Ctx.t -> Expr.t -> Expr.t =
    (* Sort of input expression expr_keyhash is keyhash sort *)
    fun ctx expr_keyhash ->
    let (sort : Sort.t) = create_sort ctx in
    DataType.create_expr sort ~const_idx:Constant._idx_const_address
      [ expr_keyhash ]
  (* function create_expr end *)

  let read_content : Expr.t -> Expr.t =
    (* Sort of output expression is keyhash sort *)
    fun expr1 ->
    DataType.read_expr_of_field expr1 ~const_idx:Constant._idx_const_address
      ~field_idx:Constant._idx_field_content
  (* function read_content end *)
end

(* Or *************************************************************************)

module ZOr = struct
  let gen_mt_cc : Tz.mich_t Tz.cc * Tz.mich_t Tz.cc -> Tz.mich_t Tz.cc =
    (fun (typ1, typ2) -> TzUtil.gen_dummy_cc (Tz.MT_or (typ1, typ2)))
  (* function gen_mt_cc end *)

  let gen_mv_left_cc :
      Tz.mich_t Tz.cc * Tz.mich_t Tz.cc -> Tz.mich_v Tz.cc -> Tz.mich_v Tz.cc =
    (fun typ value -> TzUtil.gen_dummy_cc (Tz.MV_left (gen_mt_cc typ, value)))
  (* function gen_mv_left_cc end *)

  let gen_mv_right_cc :
      Tz.mich_t Tz.cc * Tz.mich_t Tz.cc -> Tz.mich_v Tz.cc -> Tz.mich_v Tz.cc =
    (fun typ value -> TzUtil.gen_dummy_cc (Tz.MV_right (gen_mt_cc typ, value)))
  (* function gen_mv_right_cc end *)

  let create_const :
      Ctx.t -> Tz.mich_t Tz.cc * Tz.mich_t Tz.cc -> DataConst.t list =
     let read_sort_of_content : Ctx.t -> Tz.mich_t Tz.cc -> Sort.t =
       fun ctx content_typ ->
       Ctx.read_sort ctx content_typ ~f:(fun () ->
           SmtError
             ("ZOr : create_const : read_sort_of_content : sort of content type not defined : "
             ^ Sexp.to_string (Tz.sexp_of_mich_t content_typ.cc_v)
             )
           |> raise
       )
     in
     (* inner-function read_sort_of_content end *)
     let gen_const_name : string -> Sort.t list -> string =
       fun const_name field_lst ->
       let (field_names : string) =
          List.map field_lst ~f:Sort.to_string |> String.concat ~sep:" "
       in
       "(" ^ const_name ^ " " ^ field_names ^ ")"
     in
     (* inner-function gen_const_name end *)
     let gen_recog_func_name : string -> Sort.t list -> string =
       fun recog_name field_lst ->
       let (field_names : string) =
          List.map field_lst ~f:Sort.to_string |> String.concat ~sep:" "
       in
       recog_name ^ "_" ^ field_names
     in
     (* inner-function gen_const_name end *)
     fun ctx (content_typ1, content_typ2) ->
     let (const_or_left : DataConst.t) =
        let (content_sort1 : Sort.t) = read_sort_of_content ctx content_typ1 in
        Ctx.read_const ctx (CST_or_left content_typ1) ~f:(fun () ->
            DataConst.create_constructor ctx
              {
                name = gen_const_name Constant._const_or_left [ content_sort1 ];
                recog_func_name =
                  gen_recog_func_name Constant._recog_or_left [ content_sort1 ];
                field = [ (Constant._field_content, Some content_sort1) ];
              }
        )
     in
     let (const_or_right : DataConst.t) =
        let (content_sort2 : Sort.t) = read_sort_of_content ctx content_typ2 in
        Ctx.read_const ctx (CST_or_right content_typ2) ~f:(fun () ->
            DataConst.create_constructor ctx
              {
                name = gen_const_name Constant._const_or_right [ content_sort2 ];
                recog_func_name =
                  gen_recog_func_name Constant._recog_or_right [ content_sort2 ];
                field = [ (Constant._field_content, Some content_sort2) ];
              }
        )
     in
     [ const_or_left; const_or_right ]
  (* function create_const end *)

  let create_sort :
      Ctx.t -> content_typ:Tz.mich_t Tz.cc * Tz.mich_t Tz.cc -> Sort.t =
     let read_sort_of_content : Ctx.t -> Tz.mich_t Tz.cc -> Sort.t =
       fun ctx content_typ ->
       Ctx.read_sort ctx content_typ ~f:(fun () ->
           SmtError
             ("ZOr : create_const : read_sort_of_content : sort of content type not defined : "
             ^ Sexp.to_string (Tz.sexp_of_mich_t content_typ.cc_v)
             )
           |> raise
       )
     in
     (* inner-function read_sort_of_content end *)
     let gen_sort_name : string -> Sort.t list -> string =
       fun sort_name field_lst ->
       let (field_names : string) =
          List.map field_lst ~f:Sort.to_string |> String.concat ~sep:" "
       in
       "(" ^ sort_name ^ " " ^ field_names ^ ")"
     in
     (* inner-function gen_sort_name end *)
     fun ctx ~content_typ ->
     Ctx.read_sort ctx (gen_mt_cc content_typ) ~f:(fun () ->
         let (content_sort1 : Sort.t) =
            read_sort_of_content ctx (fst content_typ)
         in
         let (content_sort2 : Sort.t) =
            read_sort_of_content ctx (snd content_typ)
         in
         let (name : string) =
            gen_sort_name Constant._sort_or [ content_sort1; content_sort2 ]
         in
         let (const_lst : DataConst.t list) = create_const ctx content_typ in
         DataType.create_sort ctx ~name const_lst
     )
  (* function create_sort end *)

  let create_expr_left :
      Ctx.t ->
      left_value:Tz.mich_v Tz.cc ->
      right_typ:Tz.mich_t Tz.cc ->
      Expr.t ->
      Expr.t =
    fun ctx ~left_value ~right_typ expr_left ->
    let (left_typ : Tz.mich_t Tz.cc) = TzUtil.typ_of_val left_value in
    let (content_typ : Tz.mich_t Tz.cc * Tz.mich_t Tz.cc) =
       (left_typ, right_typ)
    in
    let (mv_left_cc : Tz.mich_v Tz.cc) =
       gen_mv_left_cc content_typ left_value
    in
    Ctx.read_expr ctx mv_left_cc ~f:(fun () ->
        let (sort : Sort.t) = create_sort ctx ~content_typ in
        DataType.create_expr sort ~const_idx:Constant._idx_const_or_left
          [ expr_left ]
    )
  (* function create_expr_left end *)

  let create_expr_right :
      Ctx.t ->
      left_typ:Tz.mich_t Tz.cc ->
      right_value:Tz.mich_v Tz.cc ->
      Expr.t ->
      Expr.t =
    fun ctx ~left_typ ~right_value expr_right ->
    let (right_typ : Tz.mich_t Tz.cc) = TzUtil.typ_of_val right_value in
    let (content_typ : Tz.mich_t Tz.cc * Tz.mich_t Tz.cc) =
       (left_typ, right_typ)
    in
    let (mv_right_cc : Tz.mich_v Tz.cc) =
       gen_mv_right_cc content_typ right_value
    in
    Ctx.read_expr ctx mv_right_cc ~f:(fun () ->
        let (sort : Sort.t) = create_sort ctx ~content_typ in
        DataType.create_expr sort ~const_idx:Constant._idx_const_or_right
          [ expr_right ]
    )
  (* function create_expr_right end *)

  let read_content_left : Expr.t -> Expr.t =
    fun expr1 ->
    DataType.read_expr_of_field expr1 ~const_idx:Constant._idx_const_or_left
      ~field_idx:Constant._idx_field_content

  (* function read_content_left end *)
  let read_content_right : Expr.t -> Expr.t =
    fun expr1 ->
    DataType.read_expr_of_field expr1 ~const_idx:Constant._idx_const_or_right
      ~field_idx:Constant._idx_field_content
  (* function read_content_right end *)
end

(* List ***********************************************************************)

module ZList = struct
  let gen_mt_cc : Tz.mich_t Tz.cc -> Tz.mich_t Tz.cc =
    (fun typ -> TzUtil.gen_dummy_cc (Tz.MT_list typ))
  (* function gen_mt_cc end *)

  let gen_mv_nil_cc : Tz.mich_t Tz.cc -> Tz.mich_v Tz.cc =
    (fun typ -> TzUtil.gen_dummy_cc (Tz.MV_nil typ))
  (* function gen_mv_nil_cc end *)

  let create_sort : Ctx.t -> content_typ:Tz.mich_t Tz.cc -> Sort.t =
    fun ctx ~content_typ ->
    Ctx.read_sort ctx (gen_mt_cc content_typ) ~f:(fun () ->
        let (content_sort : Sort.t) =
           Ctx.read_sort ctx content_typ ~f:(fun () ->
               SmtError "ZList : create_sort : sort of content type not defined"
               |> raise
           )
        in
        let (name : string) =
           "(" ^ Constant._sort_list ^ " " ^ Sort.to_string content_sort ^ ")"
        in
        Z3.Z3List.mk_list_s (Ctx.read ctx) name content_sort
    )
  (* function create_sort end *)

  let create_expr_nil : Ctx.t -> content_typ:Tz.mich_t Tz.cc -> Expr.t =
    fun ctx ~content_typ ->
    let (mv_nil_cc : Tz.mich_v Tz.cc) = gen_mv_nil_cc content_typ in
    Ctx.read_expr ctx mv_nil_cc ~f:(fun () ->
        let (sort : Sort.t) = create_sort ctx ~content_typ in
        Z3.Z3List.nil sort
    )
  (* function create_expr_nil end *)

  let create_cons : Expr.t -> Expr.t -> Expr.t =
    fun expr_content expr1 ->
    Z3.Z3List.get_cons_decl (Expr.read_sort expr1)
    |> Func.apply ~params:[ expr_content; expr1 ]
  (* function create_cons end *)

  let read_head : Expr.t -> Expr.t =
    fun expr1 ->
    Z3.Z3List.get_head_decl (Expr.read_sort expr1)
    |> Func.apply ~params:[ expr1 ]
  (* function read_head end *)

  let read_tail : Expr.t -> Expr.t =
    fun expr1 ->
    Z3.Z3List.get_tail_decl (Expr.read_sort expr1)
    |> Func.apply ~params:[ expr1 ]
  (* function read_tail end *)
end

(* Map ************************************************************************)

module ZMap = struct
  let gen_mt_cc : Tz.mich_t Tz.cc * Tz.mich_t Tz.cc -> Tz.mich_t Tz.cc =
    (fun (typ1, typ2) -> TzUtil.gen_dummy_cc (Tz.MT_map (typ1, typ2)))
  (* function gen_mt_cc end *)

  let gen_mv_empty_map_cc : Tz.mich_t Tz.cc * Tz.mich_t Tz.cc -> Tz.mich_v Tz.cc
      =
    (fun (typ1, typ2) -> TzUtil.gen_dummy_cc (Tz.MV_empty_map (typ1, typ2)))
  (* function gen_mv_empty_map_cc end *)

  let create_sort :
      Ctx.t -> key_typ:Tz.mich_t Tz.cc -> data_typ:Tz.mich_t Tz.cc -> Sort.t =
    fun ctx ~key_typ ~data_typ ->
    Ctx.read_sort ctx
      (gen_mt_cc (key_typ, data_typ))
      ~f:(fun () ->
        let (key_sort : Sort.t) =
           Ctx.read_sort ctx key_typ ~f:(fun () ->
               SmtError "ZMap : create_sort : sort of key type not defined"
               |> raise
           )
        in
        let (data_sort : Sort.t) =
           ZOption.create_sort ctx ~content_typ:data_typ
        in
        Z3.Z3Array.mk_sort (Ctx.read ctx) key_sort data_sort)
  (* function create_sort end *)

  let create_expr_empty_map :
      Ctx.t -> key_typ:Tz.mich_t Tz.cc -> data_typ:Tz.mich_t Tz.cc -> Expr.t =
    fun ctx ~key_typ ~data_typ ->
    let (mv_empty_map_cc : Tz.mich_v Tz.cc) =
       gen_mv_empty_map_cc (key_typ, data_typ)
    in
    Ctx.read_expr ctx mv_empty_map_cc ~f:(fun () ->
        let (key_sort : Sort.t) =
           Ctx.read_sort ctx key_typ ~f:(fun () ->
               SmtError
                 "ZMap : create_expr_empty_map : sort of key type not defined"
               |> raise
           )
        in
        let (default_value : Expr.t) =
           ZOption.create_expr_none ctx ~content_typ:data_typ
        in
        Z3.Z3Array.mk_const_array (Ctx.read ctx) key_sort default_value
    )
  (* function create_expr_empty_map end *)

  let read_value : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    fun ctx expr_key expr1 ->
    Z3.Z3Array.mk_select (Ctx.read ctx) expr1 expr_key
  (* function read_value end *)

  let read_default_value : Ctx.t -> Expr.t -> Expr.t =
    (fun ctx expr1 -> Z3.Z3Array.mk_term_array (Ctx.read ctx) expr1)
  (* function read_default_value end *)

  let update : Ctx.t -> Expr.t -> Expr.t -> Expr.t -> Expr.t =
    fun ctx expr_key expr_value expr1 ->
    Z3.Z3Array.mk_store (Ctx.read ctx) expr1 expr_key expr_value
  (* function update end *)
end

(* Set ************************************************************************)

module ZSet = struct
  let gen_mt_cc : Tz.mich_t Tz.cc -> Tz.mich_t Tz.cc =
    (fun typ -> TzUtil.gen_dummy_cc (Tz.MT_set typ))
  (* function gen_mt_cc end *)

  let gen_mv_empty_set_cc : Tz.mich_t Tz.cc -> Tz.mich_v Tz.cc =
    (fun typ -> TzUtil.gen_dummy_cc (Tz.MV_empty_set typ))
  (* function gen_mv_empty_map_cc end *)

  let create_sort : Ctx.t -> content_typ:Tz.mich_t Tz.cc -> Sort.t =
    fun ctx ~content_typ ->
    Ctx.read_sort ctx (gen_mt_cc content_typ) ~f:(fun () ->
        let (content_sort : Sort.t) =
           Ctx.read_sort ctx content_typ ~f:(fun () ->
               SmtError "ZSet : create_sort : sort of content type not defined"
               |> raise
           )
        in
        Z3.Z3Array.mk_sort (Ctx.read ctx) content_sort (ZBool.create_sort ctx)
    )
  (* function create_sort end *)

  let create_expr_empty_set : Ctx.t -> content_typ:Tz.mich_t Tz.cc -> Expr.t =
    fun ctx ~content_typ ->
    let (mv_empty_set_cc : Tz.mich_v Tz.cc) = gen_mv_empty_set_cc content_typ in
    Ctx.read_expr ctx mv_empty_set_cc ~f:(fun () ->
        let (content_sort : Sort.t) =
           Ctx.read_sort ctx content_typ ~f:(fun () ->
               SmtError "ZSet : create_sort : sort of content type not defined"
               |> raise
           )
        in
        let (default_value : Expr.t) = ZBool.create_expr ctx false in
        Z3.Z3Array.mk_const_array (Ctx.read ctx) content_sort default_value
    )
  (* function create_expr_empty_set end *)

  let update : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    fun ctx expr_content expr1 ->
    Z3.Z3Array.mk_store (Ctx.read ctx) expr1 expr_content
      (ZBool.create_expr ctx true)
  (* function update end *)
end

(* Operation ******************************************************************)

module ZOperation = struct
  let (mt_cc : Tz.mich_t Tz.cc) = TzUtil.gen_dummy_cc Tz.MT_operation

  let create_const : Ctx.t -> DataConst.t list =
     let (mt_keyhash_cc : Tz.mich_t Tz.cc) =
        TzUtil.gen_dummy_cc Tz.MT_key_hash
     in
     fun ctx ->
     let (const_operation_create_contract : DataConst.t) =
        Ctx.read_const ctx CST_operation_create_contract ~f:(fun () ->
            DataConst.create_constructor ctx
              {
                name = Constant._const_operation_create_contract;
                recog_func_name = Constant._recog_operation_create_contract;
                field =
                  [
                    ( Constant._field_fst,
                      Some (ZOption.create_sort ctx ~content_typ:mt_keyhash_cc)
                    );
                    (Constant._field_snd, Some (ZMutez.create_sort ctx));
                  ];
              }
        )
     in
     let (const_operation_transfer_tokens : DataConst.t) =
        Ctx.read_const ctx CST_operation_transfer_tokens ~f:(fun () ->
            DataConst.create_constructor ctx
              {
                name = Constant._const_operation_transfer_tokens;
                recog_func_name = Constant._recog_operation_transfer_tokens;
                field =
                  [ (Constant._field_content, Some (ZMutez.create_sort ctx)) ];
              }
        )
     in
     let (const_operation_set_delegate : DataConst.t) =
        Ctx.read_const ctx CST_operation_set_delegate ~f:(fun () ->
            DataConst.create_constructor ctx
              {
                name = Constant._const_operation_set_delegate;
                recog_func_name = Constant._recog_operation_set_delegate;
                field =
                  [
                    ( Constant._field_content,
                      Some (ZOption.create_sort ctx ~content_typ:mt_keyhash_cc)
                    );
                  ];
              }
        )
     in
     [
       const_operation_create_contract;
       const_operation_transfer_tokens;
       const_operation_set_delegate;
     ]
  (* function create_const end *)

  let create_sort : Ctx.t -> Sort.t =
    fun ctx ->
    Ctx.read_sort ctx mt_cc ~f:(fun () ->
        let (const_lst : DataConst.t list) = create_const ctx in
        DataType.create_sort ctx ~name:Constant._sort_operation const_lst
    )
  (* function create_sort end *)

  let create_expr_create_contract : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    fun ctx expr_keyhash_opt expr_mutez ->
    let (sort : Sort.t) = create_sort ctx in
    DataType.create_expr sort
      ~const_idx:Constant._idx_const_operation_create_contract
      [ expr_keyhash_opt; expr_mutez ]
  (* function create_expr_create_contract end *)

  let create_expr_transfer_tokens : Ctx.t -> Expr.t -> Expr.t =
    fun ctx expr_mutez ->
    let (sort : Sort.t) = create_sort ctx in
    DataType.create_expr sort
      ~const_idx:Constant._idx_const_operation_transfer_tokens [ expr_mutez ]
  (* function create_expr_transfer_tokens end *)

  let create_expr_set_delegate : Ctx.t -> Expr.t -> Expr.t =
    fun ctx expr_keyhash_opt ->
    let (sort : Sort.t) = create_sort ctx in
    DataType.create_expr sort
      ~const_idx:Constant._idx_const_operation_set_delegate [ expr_keyhash_opt ]
  (* function create_expr_set_delegate end *)

  let read_amount : Ctx.t -> Expr.t -> Expr.t =
    fun ctx expr1 ->
    Z3.Boolean.mk_ite (Ctx.read ctx)
      (DataType.read_expr_is_const
         ~const_idx:Constant._idx_const_operation_set_delegate expr1
      )
      (ZMutez.create_expr ctx 0)
      (Z3.Boolean.mk_ite (Ctx.read ctx)
         (DataType.read_expr_is_const expr1
            ~const_idx:Constant._idx_const_operation_transfer_tokens
         )
         (DataType.read_expr_of_field expr1
            ~const_idx:Constant._idx_const_operation_transfer_tokens
            ~field_idx:Constant._idx_field_content
         )
         (DataType.read_expr_of_field expr1
            ~const_idx:Constant._idx_const_operation_create_contract
            ~field_idx:Constant._idx_field_snd
         )
      )
  (* function read_amount end *)
end

(* Contract *******************************************************************)

module ZContract = struct
  let gen_mt_cc : Tz.mich_t Tz.cc -> Tz.mich_t Tz.cc =
    (fun typ -> TzUtil.gen_dummy_cc (Tz.MT_contract typ))
  (* function gen_mt_cc end *)

  let create_const : Ctx.t -> Tz.mich_t Tz.cc -> DataConst.t list =
     let read_sort_of_content : Ctx.t -> Tz.mich_t Tz.cc -> Sort.t =
       fun ctx content_typ ->
       Ctx.read_sort ctx content_typ ~f:(fun () ->
           SmtError
             ("ZContract : create_const : read_sort_of_content : sort of content type not defined : "
             ^ Sexp.to_string (Tz.sexp_of_mich_t content_typ.cc_v)
             )
           |> raise
       )
     in
     (* inner-function read_sort_of_content end *)
     let gen_const_name : string -> Sort.t list -> string =
       fun const_name field_lst ->
       let (field_names : string) =
          List.map field_lst ~f:Sort.to_string |> String.concat ~sep:" "
       in
       "(" ^ const_name ^ " " ^ field_names ^ ")"
     in
     (* inner-function gen_const_name end *)
     let gen_recog_func_name : string -> Sort.t list -> string =
       fun recog_name field_lst ->
       let (field_names : string) =
          List.map field_lst ~f:Sort.to_string |> String.concat ~sep:" "
       in
       recog_name ^ "_" ^ field_names
     in
     (* inner-function gen_const_name end *)
     fun ctx content_typ ->
     let (const_contract : DataConst.t) =
        Ctx.read_const ctx (CST_contract content_typ) ~f:(fun () ->
            let (content_sort : Sort.t) =
               read_sort_of_content ctx content_typ
            in
            DataConst.create_constructor ctx
              {
                name = gen_const_name Constant._const_contract [ content_sort ];
                recog_func_name =
                  gen_recog_func_name Constant._recog_contract [ content_sort ];
                field =
                  [ (Constant._field_content, Some (ZKeyHash.create_sort ctx)) ];
              }
        )
     in
     [ const_contract ]
  (* function create_const end *)

  let create_sort : Ctx.t -> content_typ:Tz.mich_t Tz.cc -> Sort.t =
     let read_sort_of_content : Ctx.t -> Tz.mich_t Tz.cc -> Sort.t =
       fun ctx content_typ ->
       Ctx.read_sort ctx content_typ ~f:(fun () ->
           SmtError
             ("ZContract : create_const : read_sort_of_content : sort of content type not defined : "
             ^ Sexp.to_string (Tz.sexp_of_mich_t content_typ.cc_v)
             )
           |> raise
       )
     in
     (* inner-function read_sort_of_content end *)
     let gen_sort_name : string -> Sort.t list -> string =
       fun sort_name field_lst ->
       let (field_names : string) =
          List.map field_lst ~f:Sort.to_string |> String.concat ~sep:" "
       in
       "(" ^ sort_name ^ " " ^ field_names ^ ")"
     in
     (* inner-function gen_sort_name end *)
     fun ctx ~content_typ ->
     Ctx.read_sort ctx (gen_mt_cc content_typ) ~f:(fun () ->
         let (content_sort : Sort.t) = read_sort_of_content ctx content_typ in
         let (name : string) =
            gen_sort_name Constant._sort_contract [ content_sort ]
         in
         let (const_lst : DataConst.t list) = create_const ctx content_typ in
         DataType.create_sort ctx ~name const_lst
     )
  (* function create_sort end *)

  let create_expr : Ctx.t -> content_typ:Tz.mich_t Tz.cc -> Expr.t -> Expr.t =
    fun ctx ~content_typ expr_keyhash ->
    let (sort : Sort.t) = create_sort ctx ~content_typ in
    DataType.create_expr sort ~const_idx:Constant._idx_const_contract
      [ expr_keyhash ]
  (* function create_expr end *)

  let create_expr_of_address :
      Ctx.t -> content_typ:Tz.mich_t Tz.cc -> Expr.t -> Expr.t =
    fun ctx ~content_typ expr_address ->
    let (sort : Sort.t) = create_sort ctx ~content_typ in
    DataType.create_expr sort ~const_idx:Constant._idx_const_contract
      [ ZAddr.read_content expr_address ]
  (* function create_expr end *)
end

(* Lambda *********************************************************************)

module ZLambda = struct
  let gen_mt_cc : Tz.mich_t Tz.cc * Tz.mich_t Tz.cc -> Tz.mich_t Tz.cc =
    (fun (typ1, typ2) -> TzUtil.gen_dummy_cc (Tz.MT_lambda (typ1, typ2)))
  (* function gen_mt_cc end *)

  let create_const :
      Ctx.t ->
      domain_typ:Tz.mich_t Tz.cc ->
      range_typ:Tz.mich_t Tz.cc ->
      DataConst.t list =
     let read_sort_of_content : Ctx.t -> Tz.mich_t Tz.cc -> Sort.t =
       fun ctx content_typ ->
       Ctx.read_sort ctx content_typ ~f:(fun () ->
           SmtError
             ("ZLambda : create_const : read_sort_of_content : sort of content type not defined : "
             ^ Sexp.to_string (Tz.sexp_of_mich_t content_typ.cc_v)
             )
           |> raise
       )
     in
     (* inner-function read_sort_of_content end *)
     let gen_const_name : string -> Sort.t list -> string =
       fun const_name field_lst ->
       let (field_names : string) =
          List.map field_lst ~f:Sort.to_string |> String.concat ~sep:" "
       in
       "(" ^ const_name ^ " " ^ field_names ^ ")"
     in
     (* inner-function gen_const_name end *)
     let gen_recog_func_name : string -> Sort.t list -> string =
       fun recog_name field_lst ->
       let (field_names : string) =
          List.map field_lst ~f:Sort.to_string |> String.concat ~sep:" "
       in
       recog_name ^ "_" ^ field_names
     in
     (* inner-function gen_const_name end *)
     fun ctx ~domain_typ ~range_typ ->
     let (const_lambda : DataConst.t) =
        Ctx.read_const ctx
          (CST_lambda (domain_typ, range_typ))
          ~f:(fun () ->
            let (domain_sort : Sort.t) = read_sort_of_content ctx domain_typ in
            let (range_sort : Sort.t) = read_sort_of_content ctx range_typ in
            DataConst.create_constructor ctx
              {
                name =
                  gen_const_name Constant._const_lambda
                    [ domain_sort; range_sort ];
                recog_func_name =
                  gen_recog_func_name Constant._recog_lambda
                    [ domain_sort; range_sort ];
                field =
                  [
                    (Constant._field_fst, Some domain_sort);
                    (Constant._field_snd, Some range_sort);
                  ];
              })
     in
     [ const_lambda ]
  (* function create_const end *)

  let create_sort :
      Ctx.t -> domain_typ:Tz.mich_t Tz.cc -> range_typ:Tz.mich_t Tz.cc -> Sort.t
      =
     let read_sort_of_content : Ctx.t -> Tz.mich_t Tz.cc -> Sort.t =
       fun ctx content_typ ->
       Ctx.read_sort ctx content_typ ~f:(fun () ->
           SmtError
             ("ZLambda : create_const : read_sort_of_content : sort of content type not defined : "
             ^ Sexp.to_string (Tz.sexp_of_mich_t content_typ.cc_v)
             )
           |> raise
       )
     in
     (* inner-function read_sort_of_content end *)
     let gen_sort_name : string -> Sort.t list -> string =
       fun sort_name field_lst ->
       let (field_names : string) =
          List.map field_lst ~f:Sort.to_string |> String.concat ~sep:" "
       in
       "(" ^ sort_name ^ " " ^ field_names ^ ")"
     in
     (* inner-function gen_sort_name end *)
     fun ctx ~domain_typ ~range_typ ->
     Ctx.read_sort ctx
       (gen_mt_cc (domain_typ, range_typ))
       ~f:(fun () ->
         let (domain_sort : Sort.t) = read_sort_of_content ctx domain_typ in
         let (range_sort : Sort.t) = read_sort_of_content ctx range_typ in
         let (name : string) =
            gen_sort_name Constant._sort_lambda [ domain_sort; range_sort ]
         in
         let (const_lst : DataConst.t list) =
            create_const ctx ~domain_typ ~range_typ
         in
         DataType.create_sort ctx ~name const_lst)
  (* function create_sort end *)

  let create_expr_domain : Ctx.t -> domain_typ:Tz.mich_t Tz.cc -> Expr.t =
     let (cnt : int ref) = ref 0 in
     let read_sort_of_content : Ctx.t -> Tz.mich_t Tz.cc -> Sort.t =
       fun ctx content_typ ->
       Ctx.read_sort ctx content_typ ~f:(fun () ->
           SmtError
             ("ZLambda : create_const : read_sort_of_content : sort of content type not defined : "
             ^ Sexp.to_string (Tz.sexp_of_mich_t content_typ.cc_v)
             )
           |> raise
       )
     in
     (* inner-function read_sort_of_content end *)
     fun ctx ~domain_typ ->
     let _ = incr cnt in
     Expr.create_var ctx
       (read_sort_of_content ctx domain_typ)
       ~name:(Constant._name_lambda_domain_sym ^ string_of_int !cnt)
  (* function create_expr_domain end *)

  let create_expr :
      Ctx.t ->
      domain_typ:Tz.mich_t Tz.cc ->
      Expr.t ->
      range_typ:Tz.mich_t Tz.cc ->
      Expr.t ->
      Expr.t =
    fun ctx ~domain_typ expr_domain ~range_typ expr_range ->
    let (sort : Sort.t) = create_sort ctx ~domain_typ ~range_typ in
    DataType.create_expr sort ~const_idx:Constant._idx_const_lambda
      [ expr_domain; expr_range ]
  (* function create_expr end *)

  let create_exec : Expr.t -> Expr.t -> Expr.t =
    fun expr_value expr1 ->
    let (expr_domain : Expr.t) =
       DataType.read_expr_of_field expr1 ~const_idx:Constant._idx_const_lambda
         ~field_idx:Constant._idx_field_fst
    in
    let (expr_range : Expr.t) =
       DataType.read_expr_of_field expr1 ~const_idx:Constant._idx_const_lambda
         ~field_idx:Constant._idx_field_snd
    in
    Z3.Expr.substitute_one expr_range expr_domain expr_value
  (* function create_exec end *)

  let create_apply :
      Ctx.t ->
      Expr.t ->
      domain_typ:Tz.mich_t Tz.cc * Tz.mich_t Tz.cc ->
      range_typ:Tz.mich_t Tz.cc ->
      Expr.t ->
      Expr.t =
    fun ctx expr_value ~domain_typ ~range_typ expr1 ->
    let (expr_domain : Expr.t) =
       DataType.read_expr_of_field expr1 ~const_idx:Constant._idx_const_lambda
         ~field_idx:Constant._idx_field_fst
    in
    let (expr_range : Expr.t) =
       DataType.read_expr_of_field expr1 ~const_idx:Constant._idx_const_lambda
         ~field_idx:Constant._idx_field_snd
    in
    let (expr_new_domain : Expr.t) =
       create_expr_domain ctx ~domain_typ:(snd domain_typ)
    in
    let (expr_pair : Expr.t) =
       ZPair.create_expr ctx ~content_typ:domain_typ
         (expr_value, expr_new_domain)
    in
    let (expr_new_range : Expr.t) =
       Z3.Expr.substitute_one expr_range expr_domain expr_pair
    in
    create_expr ctx ~domain_typ:(snd domain_typ) expr_new_domain ~range_typ
      expr_new_range
  (* function create_apply end *)

  let read_expr_domain : Expr.t -> Expr.t =
    fun expr1 ->
    DataType.read_expr_of_field expr1 ~const_idx:Constant._idx_const_lambda
      ~field_idx:Constant._idx_field_fst
  (* function read_expr_domain end *)
end

(******************************************************************************)
(* Formula                                                                    *)
(******************************************************************************)

module Formula = struct
  type t = Z3.Expr.expr

  let sort : Ctx.t -> Sort.t = (fun ctx -> Z3.Boolean.mk_sort (Ctx.read ctx))

  let if_then_else : Ctx.t -> if_:t -> then_:Expr.t -> else_:Expr.t -> Expr.t =
    fun ctx ~if_ ~then_ ~else_ ->
    Z3.Boolean.mk_ite (Ctx.read ctx) if_ then_ else_
  (* function if_then_else end *)

  let create_true : Ctx.t -> t = (fun ctx -> Z3.Boolean.mk_true (Ctx.read ctx))

  let create_false : Ctx.t -> t = (fun ctx -> Z3.Boolean.mk_false (Ctx.read ctx))

  let create_uninterpreted : Ctx.t -> t =
    (fun ctx -> Z3.Boolean.mk_const (Ctx.read ctx) (Sym.create_dummy ctx))
  (* function create_uninterpreted end *)

  let create_not : Ctx.t -> t -> t =
    (fun ctx fmla -> Z3.Boolean.mk_not (Ctx.read ctx) fmla)
  (* function create_not end *)

  let create_and : Ctx.t -> t list -> t =
    (fun ctx fmla_lst -> Z3.Boolean.mk_and (Ctx.read ctx) fmla_lst)
  (* function create_and end *)

  let create_or : Ctx.t -> t list -> t =
    (fun ctx fmla_lst -> Z3.Boolean.mk_or (Ctx.read ctx) fmla_lst)
  (* function create_or end *)

  let create_xor : Ctx.t -> t -> t -> t =
    (fun ctx fmla1 fmla2 -> Z3.Boolean.mk_xor (Ctx.read ctx) fmla1 fmla2)
  (* function create_xor end *)

  let create_imply : Ctx.t -> t -> t -> t =
    (fun ctx fmla1 fmla2 -> Z3.Boolean.mk_implies (Ctx.read ctx) fmla1 fmla2)
  (* function create_imply end *)

  let create_iff : Ctx.t -> t -> t -> t =
    (fun ctx fmla1 fmla2 -> Z3.Boolean.mk_iff (Ctx.read ctx) fmla1 fmla2)
  (* function create_iff end *)

  let create_eq : Ctx.t -> Expr.t -> Expr.t -> t =
    (fun ctx expr1 expr2 -> Z3.Boolean.mk_eq (Ctx.read ctx) expr1 expr2)
  (* function create_eq end *)

  let create_neq : Ctx.t -> Expr.t -> Expr.t -> t =
    (fun ctx expr1 expr2 -> create_not ctx (create_eq ctx expr1 expr2))
  (* function create_neq end *)

  let create_is_true : Ctx.t -> Expr.t -> t =
    (fun ctx expr1 -> create_eq ctx expr1 (create_true ctx))
  (* function create_is_true end *)

  let create_is_false : Ctx.t -> Expr.t -> t =
    (fun ctx expr1 -> create_eq ctx expr1 (create_false ctx))
  (* function create_is_false end *)

  let create_is_unit : Ctx.t -> Expr.t -> t =
    fun ctx expr1 ->
    ZUnit.create_const ctx
    |> (fun const_lst -> List.nth_exn const_lst Constant._idx_const_unit)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_unit end *)

  let create_is_key : Ctx.t -> Expr.t -> t =
    fun ctx expr1 ->
    ZKey.create_const ctx
    |> (fun const_lst -> List.nth_exn const_lst Constant._idx_const_key)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_key end *)

  let create_is_keyhash_str : Ctx.t -> Expr.t -> t =
    fun ctx expr1 ->
    ZKeyHash.create_const ctx
    |> (fun const_lst -> List.nth_exn const_lst Constant._idx_const_keyhash_str)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_keyhash_str end *)

  let create_is_keyhash_key : Ctx.t -> Expr.t -> t =
    fun ctx expr1 ->
    ZKeyHash.create_const ctx
    |> (fun const_lst -> List.nth_exn const_lst Constant._idx_const_keyhash_key)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_keyhash_str end *)

  let create_is_option_none : Ctx.t -> Tz.mich_t Tz.cc -> Expr.t -> t =
    fun ctx typ expr1 ->
    ZOption.create_const ctx typ
    |> (fun const_lst -> List.nth_exn const_lst Constant._idx_const_option_none)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_option_none end *)

  let create_is_option_some : Ctx.t -> Tz.mich_t Tz.cc -> Expr.t -> t =
    fun ctx typ expr1 ->
    ZOption.create_const ctx typ
    |> (fun const_lst -> List.nth_exn const_lst Constant._idx_const_option_some)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_option_some end *)

  let create_is_pair : Ctx.t -> Tz.mich_t Tz.cc * Tz.mich_t Tz.cc -> Expr.t -> t
      =
    fun ctx typ expr1 ->
    ZPair.create_const ctx typ
    |> (fun const_lst -> List.nth_exn const_lst Constant._idx_const_pair)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_pair end *)

  let create_is_bytes_nil : Ctx.t -> Expr.t -> t =
    fun ctx expr1 ->
    ZBytes.create_const ctx
    |> (fun const_lst -> List.nth_exn const_lst Constant._idx_const_bytes_nil)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_bytes_nil end *)

  let create_is_bytes_str : Ctx.t -> Expr.t -> t =
    fun ctx expr1 ->
    ZBytes.create_const ctx
    |> (fun const_lst -> List.nth_exn const_lst Constant._idx_const_bytes_str)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_bytes_str end *)

  let create_is_bytes_concat : Ctx.t -> Expr.t -> t =
    fun ctx expr1 ->
    ZBytes.create_const ctx
    |> (fun const_lst ->
         List.nth_exn const_lst Constant._idx_const_bytes_concat)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_bytes_concat end *)

  let create_is_bytes_blake2b : Ctx.t -> Expr.t -> t =
    fun ctx expr1 ->
    ZBytes.create_const ctx
    |> (fun const_lst ->
         List.nth_exn const_lst Constant._idx_const_bytes_blake2b)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_bytes_blake2b end *)

  let create_is_bytes_sha256 : Ctx.t -> Expr.t -> t =
    fun ctx expr1 ->
    ZBytes.create_const ctx
    |> (fun const_lst ->
         List.nth_exn const_lst Constant._idx_const_bytes_sha256)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_bytes_sha256 end *)

  let create_is_bytes_sha512 : Ctx.t -> Expr.t -> t =
    fun ctx expr1 ->
    ZBytes.create_const ctx
    |> (fun const_lst ->
         List.nth_exn const_lst Constant._idx_const_bytes_sha512)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_bytes_sha512 end *)

  let create_is_bytes_pack : Ctx.t -> Expr.t -> t =
    fun ctx expr1 ->
    ZBytes.create_const ctx
    |> (fun const_lst -> List.nth_exn const_lst Constant._idx_const_bytes_pack)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_bytes_pack end *)

  let create_is_bytes_sliced : Ctx.t -> Expr.t -> t =
    fun ctx expr1 ->
    ZBytes.create_const ctx
    |> (fun const_lst ->
         List.nth_exn const_lst Constant._idx_const_bytes_sliced)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_bytes_sliced end *)

  let create_is_signature_str : Ctx.t -> Expr.t -> t =
    fun ctx expr1 ->
    ZSig.create_const ctx
    |> (fun const_lst ->
         List.nth_exn const_lst Constant._idx_const_signature_str)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_signature_str end *)

  let create_is_signature_signed : Ctx.t -> Expr.t -> t =
    fun ctx expr1 ->
    ZSig.create_const ctx
    |> (fun const_lst ->
         List.nth_exn const_lst Constant._idx_const_signature_signed)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_signature_signed end *)

  let create_is_address : Ctx.t -> Expr.t -> t =
    fun ctx expr1 ->
    ZAddr.create_const ctx
    |> (fun const_lst -> List.nth_exn const_lst Constant._idx_const_address)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_address end *)

  let create_is_or_left :
      Ctx.t -> Tz.mich_t Tz.cc * Tz.mich_t Tz.cc -> Expr.t -> t =
    fun ctx typ expr1 ->
    ZOr.create_const ctx typ
    |> (fun const_lst -> List.nth_exn const_lst Constant._idx_const_or_left)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_or_left end *)

  let create_is_or_right :
      Ctx.t -> Tz.mich_t Tz.cc * Tz.mich_t Tz.cc -> Expr.t -> t =
    fun ctx typ expr1 ->
    ZOr.create_const ctx typ
    |> (fun const_lst -> List.nth_exn const_lst Constant._idx_const_or_right)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_or_right end *)

  let create_is_list_nil : Expr.t -> t =
    fun expr1 ->
    Z3.Z3List.get_is_nil_decl (Expr.read_sort expr1)
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_list_nil end *)

  let create_is_list_cons : Expr.t -> t =
    fun expr1 ->
    Z3.Z3List.get_is_cons_decl (Expr.read_sort expr1)
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_list_cons end *)

  let create_is_operation_create_contract : Ctx.t -> Expr.t -> t =
    fun ctx expr1 ->
    ZOperation.create_const ctx
    |> (fun const_lst ->
         List.nth_exn const_lst Constant._idx_const_operation_create_contract)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_operation_create_contract end *)

  let create_is_operation_transfer_tokens : Ctx.t -> Expr.t -> t =
    fun ctx expr1 ->
    ZOperation.create_const ctx
    |> (fun const_lst ->
         List.nth_exn const_lst Constant._idx_const_operation_transfer_tokens)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_operation_transfer_tokens end *)

  let create_is_operation_set_delegate : Ctx.t -> Expr.t -> t =
    fun ctx expr1 ->
    ZOperation.create_const ctx
    |> (fun const_lst ->
         List.nth_exn const_lst Constant._idx_const_operation_set_delegate)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_operation_set_delegate end *)

  let create_is_contract : Ctx.t -> Tz.mich_t Tz.cc -> Expr.t -> t =
    fun ctx typ expr1 ->
    ZContract.create_const ctx typ
    |> (fun const_lst -> List.nth_exn const_lst Constant._idx_const_contract)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_contract end *)

  let create_is_lambda :
      Ctx.t -> Tz.mich_t Tz.cc * Tz.mich_t Tz.cc -> Expr.t -> t =
    fun ctx (domain_typ, range_typ) expr1 ->
    ZLambda.create_const ctx ~domain_typ ~range_typ
    |> (fun const_lst -> List.nth_exn const_lst Constant._idx_const_lambda)
    |> DataConst.recog_func_for_constructor
    |> Func.apply ~params:[ expr1 ]
  (* function create_is_lambda end *)

  let create_is_mem_of_map : Ctx.t -> Expr.t -> Expr.t -> t =
    fun ctx expr_key expr1 ->
    if_then_else ctx
      ~if_:
        (ZMap.read_value ctx expr_key expr1
        |> DataType.read_expr_is_const
             ~const_idx:Constant._idx_const_option_some
        )
      ~then_:(create_true ctx) ~else_:(create_false ctx)
  (* function create_is_data_in_map end *)

  let create_is_not_mem_of_map : Ctx.t -> Expr.t -> Expr.t -> t =
    fun ctx expr_key expr1 ->
    create_is_mem_of_map ctx expr_key expr1 |> create_not ctx
  (* function create_is_not_mem_of_map end *)

  let create_is_mem_of_set : Ctx.t -> Expr.t -> Expr.t -> t =
    (fun ctx expr_key expr1 -> ZMap.read_value ctx expr_key expr1)
  (* function create_is_data_in_set end *)

  let create_is_not_mem_of_set : Ctx.t -> Expr.t -> Expr.t -> t =
    fun ctx expr_key expr1 ->
    create_is_mem_of_set ctx expr_key expr1 |> create_not ctx
  (* function create_is_not_mem_of_set end *)

  let create_is_expr_lambda_domain : Ctx.t -> Expr.t -> Expr.t -> t =
    fun ctx expr_domain expr1 ->
    create_eq ctx expr_domain (ZLambda.read_expr_domain expr1)
  (* function create_is_lambda end *)

  let create_int_lt : Ctx.t -> Expr.t -> Expr.t -> t =
    (* Sort of input expressions are integer sort *)
    (fun ctx expr1 expr2 -> Z3.Arithmetic.mk_lt (Ctx.read ctx) expr1 expr2)
  (* function create_int_lt end *)

  let create_int_le : Ctx.t -> Expr.t -> Expr.t -> t =
    (* Sort of input expressions are integer sort *)
    (fun ctx expr1 expr2 -> Z3.Arithmetic.mk_le (Ctx.read ctx) expr1 expr2)
  (* function create_int_le end *)

  let create_int_gt : Ctx.t -> Expr.t -> Expr.t -> t =
    (* Sort of input expressions are integer sort *)
    (fun ctx expr1 expr2 -> Z3.Arithmetic.mk_gt (Ctx.read ctx) expr1 expr2)
  (* function create_int_gt end *)

  let create_int_ge : Ctx.t -> Expr.t -> Expr.t -> t =
    (* Sort of input expressions are integer sort *)
    (fun ctx expr1 expr2 -> Z3.Arithmetic.mk_ge (Ctx.read ctx) expr1 expr2)
  (* function create_int_ge end *)

  let create_str_lt : Ctx.t -> Expr.t -> Expr.t -> t =
    (* Sort of input expressions are string sort *)
    (fun ctx expr1 expr2 -> Z3.Seq.mk_str_lt (Ctx.read ctx) expr1 expr2)
  (* function create_str_lt end *)

  let create_str_le : Ctx.t -> Expr.t -> Expr.t -> t =
    (* Sort of input expressions are string sort *)
    (fun ctx expr1 expr2 -> Z3.Seq.mk_str_le (Ctx.read ctx) expr1 expr2)
  (* function create_str_le end *)

  let create_str_gt : Ctx.t -> Expr.t -> Expr.t -> t =
    (* Sort of input expressions are string sort *)
    (fun ctx expr1 expr2 -> create_not ctx (create_str_le ctx expr1 expr2))
  (* function create_str_gt end *)

  let create_str_ge : Ctx.t -> Expr.t -> Expr.t -> t =
    (* Sort of input expressions are string sort *)
    (fun ctx expr1 expr2 -> create_not ctx (create_str_lt ctx expr1 expr2))
  (* function create_str_ge end *)

  let create_nat_bound : Ctx.t -> Expr.t -> t =
    (* Sort of input expression is integer sort (natural number type) *)
    fun ctx expr1 ->
    let (min : Expr.t) = ZNat.create_expr ctx 0 in
    create_int_le ctx min expr1
  (* function create_nat_bound end *)

  let create_mutez_bound : Ctx.t -> Expr.t -> t =
     let (max_mtz : Bigint.t) =
        Bigint.pow (Bigint.of_int 2) (Bigint.of_int Constant._bit_mutez)
     in
     (* Sort of input expression is integer sort (mutez type) *)
     fun ctx expr1 ->
     let (min : Expr.t) = ZMutez.create_expr ctx 0 in
     let (max : Expr.t) = ZMutez.create_expr_of_bigint ctx max_mtz in
     create_and ctx [ create_int_le ctx min expr1; create_int_lt ctx expr1 max ]
  (* function create_mutez_bound end *)

  let create_add_no_overflow : Ctx.t -> Expr.t -> Expr.t -> t =
     let (max_mtz : Bigint.t) =
        Bigint.pow (Bigint.of_int 2) (Bigint.of_int Constant._bit_mutez)
     in
     (* Sort of input expressions are integer sort (mutez type) *)
     fun ctx expr1 expr2 ->
     let (add : Expr.t) =
        Z3.Arithmetic.mk_add (Ctx.read ctx) [ expr1; expr2 ]
     in
     let (max : Expr.t) = ZMutez.create_expr_of_bigint ctx max_mtz in
     create_int_lt ctx add max
  (* function create_add_no_overflow end *)

  let create_mul_no_overflow : Ctx.t -> Expr.t -> Expr.t -> t =
     let (max_mtz : Bigint.t) =
        Bigint.pow (Bigint.of_int 2) (Bigint.of_int Constant._bit_mutez)
     in
     (* Sort of input expressions are integer sort (mutez type) *)
     fun ctx expr1 expr2 ->
     let (mul : Expr.t) =
        Z3.Arithmetic.mk_mul (Ctx.read ctx) [ expr1; expr2 ]
     in
     let (max : Expr.t) = ZMutez.create_expr_of_bigint ctx max_mtz in
     create_int_lt ctx mul max
  (* function create_mul_no_overflow end *)

  let create_sub_no_underflow : Ctx.t -> Expr.t -> Expr.t -> t =
    (* Sort of input expressions are integer sort (mutez type) *)
    fun ctx expr1 expr2 ->
    let (sub : Expr.t) = Z3.Arithmetic.mk_sub (Ctx.read ctx) [ expr1; expr2 ] in
    let (min : Expr.t) = ZMutez.create_expr ctx 0 in
    create_int_le ctx min sub
  (* function create_sub_no_underflow end *)

  let to_sat_check : Ctx.t -> t -> Expr.t list = (fun _ fmla -> [ fmla ])
  (* function to_sat_check end *)

  let to_val_check : Ctx.t -> t -> Expr.t list =
    (fun ctx fmla -> [ create_not ctx fmla ])
  (* function to_val_check end *)

  let compare : t -> t -> int = (fun expr1 expr2 -> Z3.Expr.compare expr1 expr2)

  let equal : t -> t -> bool = (fun expr1 expr2 -> Z3.Expr.equal expr1 expr2)
end

(******************************************************************************)
(* Model & Solver                                                             *)
(******************************************************************************)

(* Model **********************************************************************)

module Model = struct
  type t = (Z3.Model.model[@sexp.opaque]) [@@deriving sexp]

  let eval : t -> Expr.t -> Expr.t option =
    (fun model expr -> Z3.Model.eval model expr true)
  (* function eval end *)

  let to_string : t -> string = (fun model -> Z3.Model.to_string model)

  let compare : t -> t -> int =
    (fun model1 model2 -> compare_string (to_string model1) (to_string model2))
  (* function compare end *)

  let equal : t -> t -> bool =
    (fun model1 model2 -> equal_string (to_string model1) (to_string model2))
  (* function equal end *)
end

(* Solver *********************************************************************)

module Solver = struct
  type t = {
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

  let create : Ctx.t -> t =
     let (id : int ref) = ref 0 in
     fun ctx ->
     let _ = incr id in
     {
       id = !id;
       solver = Z3.Solver.mk_solver_s (Ctx.read ctx) (string_of_int !id);
     }
  (* function create end *)

  let read : t -> Z3.Solver.solver = (fun { solver; _ } -> solver)

  let read_id : t -> int = (fun { id; _ } -> id)

  let check_sat : t -> Ctx.t -> Formula.t -> satisfiability * Model.t option =
    fun solver ctx fmla ->
    match Z3.Solver.check (read solver) (Formula.to_sat_check ctx fmla) with
    | UNKNOWN       -> (UNKNOWN, None)
    | UNSATISFIABLE -> (UNSAT, None)
    | SATISFIABLE   -> (SAT, Z3.Solver.get_model (read solver))
  (* function check_sat end *)

  let check_val : t -> Ctx.t -> Formula.t -> validity * Model.t option =
    fun solver ctx fmla ->
    match Z3.Solver.check (read solver) (Formula.to_val_check ctx fmla) with
    | UNKNOWN       -> (UNKNOWN, None)
    | UNSATISFIABLE -> (VAL, None)
    | SATISFIABLE   -> (INVAL, Z3.Solver.get_model (read solver))
  (* function check_val end *)

  let is_sat_unknown : satisfiability -> bool =
    (fun sat_flag -> equal_satisfiability sat_flag UNKNOWN)
  (* function is_sat_unknown end *)

  let is_sat : satisfiability -> bool =
    (fun sat_flag -> equal_satisfiability sat_flag SAT)
  (* function is_sat end *)

  let is_unsat : satisfiability -> bool =
    (fun sat_flag -> equal_satisfiability sat_flag UNSAT)
  (* function is_unsat end *)

  let is_val_unknown : validity -> bool =
    (fun val_flag -> equal_validity val_flag UNKNOWN)
  (* function is_val_unknown end *)

  let is_val : validity -> bool = (fun val_flag -> equal_validity val_flag VAL)
  (* function is_val end *)

  let is_inval : validity -> bool =
    (fun val_flag -> equal_validity val_flag INVAL)
  (* function is_inval end *)

  let string_of_sat : satisfiability -> string =
    fun sat_flag ->
    match sat_flag with
    | UNKNOWN -> "UNKNOWN"
    | SAT     -> "SATISFIABLE"
    | UNSAT   -> "UNSATISFIABLE"
  (* function string_of_sat end *)

  let string_of_val : validity -> string =
    fun val_flag ->
    match val_flag with
    | UNKNOWN -> "UNKNOWN"
    | VAL     -> "VALID"
    | INVAL   -> "INVALID"
  (* function string_of_val end *)
end

(******************************************************************************)
(******************************************************************************)
(* Utility Functions                                                          *)
(******************************************************************************)
(******************************************************************************)

let get_version_of_z3 : unit -> string = (fun () -> Z3.Version.to_string)

let make_log : string -> unit =
  fun filename ->
  if Z3.Log.open_ filename
  then ()
  else SmtError "create_log : log file open error" |> raise

let close_log : unit -> unit = (fun () -> Z3.Log.close ())
