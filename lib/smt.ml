(* Smt: Interface to the Z3 SMT solver *)

exception SmtError = Z3.Error

exception NotImplemented

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

  let _sort_operation : string = "Operation"

  let _sort_contract : string = "Contract"

  let _sort_lambda : string = "Lambda"

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

  let _const_signature_str : string = "const_signature_str"

  let _const_signature_signed : string = "const_signature_signed"

  let _const_address : string = "const_address"

  let _const_or_left : string = "const_or_left"

  let _const_or_right : string = "const_or_right"

  let _const_operation : string = "const_operation"

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

  let _recog_signature_str : string = "is_signature_str"

  let _recog_signature_signed : string = "is_signature_signed"

  let _recog_address : string = "is_address"

  let _recog_or_left : string = "is_left"

  let _recog_or_right : string = "is_right"

  let _recog_operation : string = "is_operation"

  let _recog_contract : string = "is_contract"

  let _recog_lambda : string = "is_lambda"

  (* Name of Field *)

  let _field_content : string = "content"

  let _field_pair_fst : string = "fst"

  let _field_pair_snd : string = "snd"

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
(* Formula                                                                    *)
(******************************************************************************)

module Formula = struct
  type t = Z3.Expr.expr

  let sort : Ctx.t -> Sort.t = (fun ctx -> Z3.Boolean.mk_sort (Ctx.read ctx))

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

  let create_eq : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t =
    (fun ctx expr1 expr2 -> Z3.Boolean.mk_eq (Ctx.read ctx) expr1 expr2)
  (* function create_eq end *)

  let create_neq : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t =
    (fun ctx expr1 expr2 -> create_not ctx (create_eq ctx expr1 expr2))
  (* function create_neq end *)

  let create_is_true : Ctx.t -> Z3.Expr.expr -> t =
    (fun _ _ -> raise NotImplemented)
  (* function create_is_true end *)

  let create_is_false : Ctx.t -> Z3.Expr.expr -> t =
    (fun _ _ -> raise NotImplemented)
  (* function create_is_false end *)

  let create_is_none : Ctx.t -> Z3.Expr.expr -> t =
    (fun _ _ -> raise NotImplemented)
  (* function create_is_none end *)

  let create_is_some : Ctx.t -> Z3.Expr.expr -> t =
    (fun _ _ -> raise NotImplemented)
  (* function create_is_some end *)

  let create_is_left : Ctx.t -> Z3.Expr.expr -> t =
    (fun _ _ -> raise NotImplemented)
  (* function create_is_left end *)

  let create_is_right : Ctx.t -> Z3.Expr.expr -> t =
    (fun _ _ -> raise NotImplemented)
  (* function create_is_right end *)

  let create_is_nil : Ctx.t -> Z3.Expr.expr -> t =
    (fun _ _ -> raise NotImplemented)
  (* function create_is_nil end *)

  let create_is_cons : Ctx.t -> Z3.Expr.expr -> t =
    (fun _ _ -> raise NotImplemented)
  (* function create_is_cons end *)

  let create_int_lt : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t =
    (* Sort of input expressions are integer sort *)
    (fun ctx expr1 expr2 -> Z3.Arithmetic.mk_lt (Ctx.read ctx) expr1 expr2)
  (* function create_int_lt end *)

  let create_int_le : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t =
    (* Sort of input expressions are integer sort *)
    (fun ctx expr1 expr2 -> Z3.Arithmetic.mk_le (Ctx.read ctx) expr1 expr2)
  (* function create_int_le end *)

  let create_int_gt : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t =
    (* Sort of input expressions are integer sort *)
    (fun ctx expr1 expr2 -> Z3.Arithmetic.mk_gt (Ctx.read ctx) expr1 expr2)
  (* function create_int_gt end *)

  let create_int_ge : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t =
    (* Sort of input expressions are integer sort *)
    (fun ctx expr1 expr2 -> Z3.Arithmetic.mk_ge (Ctx.read ctx) expr1 expr2)
  (* function create_int_ge end *)

  let create_convertable_to_finite_bv : Ctx.t -> Z3.Expr.expr -> t =
     let (max_int2bv : Bigint.t) =
        Bigint.pow (Bigint.of_int 2) (Bigint.of_int Constant._int2bv_precision)
     in
     let (mv_max_int2bv : Tz.mich_v Tz.cc) =
        Tz.MV_lit_int max_int2bv |> Tz.gen_dummy_cc
     in
     (* Sort of input expression is integer sort *)
     fun ctx expr1 ->
     let (max : Z3.Expr.expr) =
        Ctx.read_expr ctx mv_max_int2bv ~f:(fun () ->
            Z3.Arithmetic.Integer.mk_numeral_s (Ctx.read ctx)
              (Bigint.to_string max_int2bv)
        )
     in
     create_int_lt ctx expr1 max
  (* function check_convertable_to_finite_bv end *)

  let create_nat_bound : Ctx.t -> Z3.Expr.expr -> t =
     let (min_nat : Bigint.t) = Bigint.zero in
     let (mv_min_nat : Tz.mich_v Tz.cc) =
        Tz.MV_lit_nat min_nat |> Tz.gen_dummy_cc
     in
     (* Sort of input expression is integer sort (natural number type) *)
     fun ctx expr1 ->
     let (min : Z3.Expr.expr) =
        Ctx.read_expr ctx mv_min_nat ~f:(fun () ->
            Z3.Arithmetic.Integer.mk_numeral_s (Ctx.read ctx)
              (Bigint.to_string min_nat)
        )
     in
     create_int_le ctx min expr1
  (* function create_nat_bound end *)

  let create_mutez_bound : Ctx.t -> Z3.Expr.expr -> t =
     let (min_mtz : Bigint.t) = Bigint.zero in
     let (max_mtz : Bigint.t) =
        Bigint.pow (Bigint.of_int 2) (Bigint.of_int Constant._bit_mutez)
     in
     let (mv_min_mtz : Tz.mich_v Tz.cc) =
        Tz.MV_lit_mutez min_mtz |> Tz.gen_dummy_cc
     in
     let (mv_max_mtz : Tz.mich_v Tz.cc) =
        Tz.MV_lit_mutez max_mtz |> Tz.gen_dummy_cc
     in
     (* Sort of input expression is integer sort (mutez type) *)
     fun ctx expr1 ->
     let (min : Z3.Expr.expr) =
        Ctx.read_expr ctx mv_min_mtz ~f:(fun () ->
            Z3.Arithmetic.Integer.mk_numeral_s (Ctx.read ctx)
              (Bigint.to_string min_mtz)
        )
     in
     let (max : Z3.Expr.expr) =
        Ctx.read_expr ctx mv_max_mtz ~f:(fun () ->
            Z3.Arithmetic.Integer.mk_numeral_s (Ctx.read ctx)
              (Bigint.to_string max_mtz)
        )
     in
     create_and ctx [ create_int_le ctx min expr1; create_int_lt ctx expr1 max ]
  (* function create_mutez_bound end *)

  let create_add_no_overflow : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t =
     let (max_mtz : Bigint.t) =
        Bigint.pow (Bigint.of_int 2) (Bigint.of_int Constant._bit_mutez)
     in
     let (mv_max_mtz : Tz.mich_v Tz.cc) =
        Tz.MV_lit_mutez max_mtz |> Tz.gen_dummy_cc
     in
     fun ctx expr1 expr2 ->
     let (add : Z3.Expr.expr) =
        Z3.Arithmetic.mk_add (Ctx.read ctx) [ expr1; expr2 ]
     in
     let (max : Z3.Expr.expr) =
        Ctx.read_expr ctx mv_max_mtz ~f:(fun () ->
            Z3.Arithmetic.Integer.mk_numeral_s (Ctx.read ctx)
              (Bigint.to_string max_mtz)
        )
     in
     create_int_lt ctx add max
  (* function create_add_no_overflow end *)

  let create_mul_no_overflow : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t =
     let (max_mtz : Bigint.t) =
        Bigint.pow (Bigint.of_int 2) (Bigint.of_int Constant._bit_mutez)
     in
     let (mv_max_mtz : Tz.mich_v Tz.cc) =
        Tz.MV_lit_mutez max_mtz |> Tz.gen_dummy_cc
     in
     fun ctx expr1 expr2 ->
     let (mul : Z3.Expr.expr) =
        Z3.Arithmetic.mk_mul (Ctx.read ctx) [ expr1; expr2 ]
     in
     let (max : Z3.Expr.expr) =
        Ctx.read_expr ctx mv_max_mtz ~f:(fun () ->
            Z3.Arithmetic.Integer.mk_numeral_s (Ctx.read ctx)
              (Bigint.to_string max_mtz)
        )
     in
     create_int_lt ctx mul max
  (* function create_mul_no_overflow end *)

  let create_sub_no_underflow : Ctx.t -> Z3.Expr.expr -> Z3.Expr.expr -> t =
     let (min_mtz : Bigint.t) = Bigint.zero in
     let (mv_min_mtz : Tz.mich_v Tz.cc) =
        Tz.MV_lit_mutez min_mtz |> Tz.gen_dummy_cc
     in
     fun ctx expr1 expr2 ->
     let (sub : Z3.Expr.expr) =
        Z3.Arithmetic.mk_sub (Ctx.read ctx) [ expr1; expr2 ]
     in
     let (min : Z3.Expr.expr) =
        Ctx.read_expr ctx mv_min_mtz ~f:(fun () ->
            Z3.Arithmetic.Integer.mk_numeral_s (Ctx.read ctx)
              (Bigint.to_string min_mtz)
        )
     in
     create_int_le ctx min sub
  (* function create_sub_no_underflow end *)

  let to_sat_check : Ctx.t -> t -> Z3.Expr.expr list = (fun _ fmla -> [ fmla ])
  (* function to_sat_check end *)

  let to_val_check : Ctx.t -> t -> Z3.Expr.expr list =
    (fun ctx fmla -> [ create_not ctx fmla ])
  (* function to_val_check end *)

  let compare : t -> t -> int = (fun expr1 expr2 -> Z3.Expr.compare expr1 expr2)

  let equal : t -> t -> bool = (fun expr1 expr2 -> Z3.Expr.equal expr1 expr2)
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

  let if_then_else : Ctx.t -> if_:Formula.t -> then_:t -> else_:t -> t =
    fun ctx ~if_ ~then_ ~else_ ->
    Z3.Boolean.mk_ite (Ctx.read ctx) if_ then_ else_
  (* function if_then_else end *)

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
end) =
struct
  type elt = int

  let create_sort : Ctx.t -> Sort.t =
    fun ctx ->
    Ctx.read_sort ctx Typ.mt_cc ~f:(fun () ->
        Z3.Arithmetic.Integer.mk_sort (Ctx.read ctx)
    )
  (* function create_sort end *)

  let create_expr : Ctx.t -> int -> Expr.t =
    fun ctx value ->
    let (mv_int_cc : Tz.mich_v Tz.cc) = Typ.gen_mv_cc (Bigint.of_int value) in
    Ctx.read_expr ctx mv_int_cc ~f:(fun () ->
        Z3.Arithmetic.Integer.mk_numeral_i (Ctx.read ctx) value
    )
  (* function create_expr end *)

  let create_expr_of_bigint : Ctx.t -> Bigint.t -> Expr.t =
    fun ctx value ->
    let (mv_int_cc : Tz.mich_v Tz.cc) = Typ.gen_mv_cc value in
    Ctx.read_expr ctx mv_int_cc ~f:(fun () ->
        Z3.Arithmetic.Integer.mk_numeral_s (Ctx.read ctx)
          (Bigint.to_string value)
    )
  (* function create_expr_of_bigint end *)

  let to_finite_bv : Ctx.t -> Expr.t -> Expr.t =
    fun ctx expr1 ->
    Z3.Arithmetic.Integer.mk_int2bv (Ctx.read ctx) Constant._int2bv_precision
      expr1
  (* function to_finite_bv end *)

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

  let create_cmp : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    fun ctx expr1 expr2 ->
    Expr.if_then_else ctx
      ~if_:(Formula.create_eq ctx expr1 expr2)
      ~then_:(create_expr ctx 0)
      ~else_:
        (Expr.if_then_else ctx
           ~if_:(Formula.create_int_lt ctx expr1 expr2)
           ~then_:(create_expr ctx (-1)) ~else_:(create_expr ctx 1)
        )
  (* function create_cmp end *)
end

module Integer = struct
  module Typ = struct
    let (mt_cc : Tz.mich_t Tz.cc) = Tz.gen_dummy_cc Tz.MT_int

    let gen_mv_cc : Bigint.t -> Tz.mich_v Tz.cc =
      (fun value -> Tz.gen_dummy_cc (Tz.MV_lit_int value))
    (* function gen_mv_cc end *)
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

module NaturalNumber = struct
  module Typ = struct
    let (mt_cc : Tz.mich_t Tz.cc) = Tz.gen_dummy_cc Tz.MT_nat

    let gen_mv_cc : Bigint.t -> Tz.mich_v Tz.cc =
      (fun value -> Tz.gen_dummy_cc (Tz.MV_lit_nat value))
    (* function gen_mv_cc end *)
  end

  include Arithmetic (Typ)

  let create_abs : Ctx.t -> Expr.t -> Expr.t =
    fun ctx expr1 ->
    Expr.if_then_else ctx
      ~if_:(Formula.create_int_lt ctx expr1 (create_expr ctx 0))
      ~then_:(Z3.Arithmetic.mk_unary_minus (Ctx.read ctx) expr1)
      ~else_:expr1
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
    let (f1 : Formula.t) = Formula.create_convertable_to_finite_bv ctx expr1 in
    let (f2 : Formula.t) = Formula.create_convertable_to_finite_bv ctx expr2 in
    Expr.if_then_else ctx
      ~if_:(Formula.create_and ctx [ f1; f2 ])
      ~then_:
        (let (cvt_expr1 : Expr.t) = to_finite_bv ctx expr1 in
         let (cvt_expr2 : Expr.t) = to_finite_bv ctx expr2 in
         let (ret_expr : Expr.t) =
            Z3.BitVector.mk_and (Ctx.read ctx) cvt_expr1 cvt_expr2
         in
         Z3.BitVector.mk_bv2int (Ctx.read ctx) ret_expr false
        )
      ~else_:(Expr.create_dummy ctx (create_sort ctx))
  (* function create_and end *)

  let create_or : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    fun ctx expr1 expr2 ->
    let (f1 : Formula.t) = Formula.create_convertable_to_finite_bv ctx expr1 in
    let (f2 : Formula.t) = Formula.create_convertable_to_finite_bv ctx expr2 in
    Expr.if_then_else ctx
      ~if_:(Formula.create_and ctx [ f1; f2 ])
      ~then_:
        (let (cvt_expr1 : Expr.t) = to_finite_bv ctx expr1 in
         let (cvt_expr2 : Expr.t) = to_finite_bv ctx expr2 in
         let (ret_expr : Expr.t) =
            Z3.BitVector.mk_or (Ctx.read ctx) cvt_expr1 cvt_expr2
         in
         Z3.BitVector.mk_bv2int (Ctx.read ctx) ret_expr false
        )
      ~else_:(Expr.create_dummy ctx (create_sort ctx))
  (* function create_or end *)

  let create_xor : Ctx.t -> Expr.t -> Expr.t -> Expr.t =
    fun ctx expr1 expr2 ->
    let (f1 : Formula.t) = Formula.create_convertable_to_finite_bv ctx expr1 in
    let (f2 : Formula.t) = Formula.create_convertable_to_finite_bv ctx expr2 in
    Expr.if_then_else ctx
      ~if_:(Formula.create_and ctx [ f1; f2 ])
      ~then_:
        (let (cvt_expr1 : Expr.t) = to_finite_bv ctx expr1 in
         let (cvt_expr2 : Expr.t) = to_finite_bv ctx expr2 in
         let (ret_expr : Expr.t) =
            Z3.BitVector.mk_xor (Ctx.read ctx) cvt_expr1 cvt_expr2
         in
         Z3.BitVector.mk_bv2int (Ctx.read ctx) ret_expr false
        )
      ~else_:(Expr.create_dummy ctx (create_sort ctx))
  (* function create_xor end *)
end

module Mutez = struct
  module Typ = struct
    let (mt_cc : Tz.mich_t Tz.cc) = Tz.gen_dummy_cc Tz.MT_mutez

    let gen_mv_cc : Bigint.t -> Tz.mich_v Tz.cc =
      (fun value -> Tz.gen_dummy_cc (Tz.MV_lit_mutez value))
    (* function gen_mv_cc end *)
  end

  include Arithmetic (Typ)
end

(* Boolean ********************************************************************)

module Boolean = struct
  (* type elt = bool *)

  (* let (mt_bool_cc : Tz.mich_t Tz.cc) = Tz.gen_dummy_cc Tz.MT_bool *)

  (* let (mv_unit_cc : Tz.mich_v Tz.cc) = Tz.gen_dummy_cc Tz.MV_unit *)

  (* let create_sort : Ctx.t -> Sort.t =
     fun ctx ->
     let (const : DataConst.t) = create_const ctx in
     Ctx.read_sort ctx mt_unit_cc ~f:(fun () ->
         DataType.create_sort ctx ~name:Constant._sort_unit [ const ]
     ) *)
  (* function create_sort end *)

  (* let create_expr : Ctx.t -> Expr.t =
     fun ctx ->
     let (sort : Sort.t) = create_sort ctx in
     Ctx.read_expr ctx mv_unit_cc ~f:(fun () ->
         DataType.create_expr sort ~const_idx:0 []
     ) *)
  (* function create_expr end *)
end

(* Unit ***********************************************************************)

module Unit = struct
  let (mt_cc : Tz.mich_t Tz.cc) = Tz.gen_dummy_cc Tz.MT_unit

  let (mv_cc : Tz.mich_v Tz.cc) = Tz.gen_dummy_cc Tz.MV_unit

  let create_const : Ctx.t -> DataConst.t =
    fun ctx ->
    Ctx.read_const ctx CST_unit ~f:(fun () ->
        DataConst.create_constructor ctx
          {
            name = Constant._const_unit;
            recog_func_name = Constant._recog_unit;
            field = [];
          }
    )
  (* function create_const end *)

  let create_sort : Ctx.t -> Sort.t =
    fun ctx ->
    Ctx.read_sort ctx mt_cc ~f:(fun () ->
        let (const : DataConst.t) = create_const ctx in
        DataType.create_sort ctx ~name:Constant._sort_unit [ const ]
    )
  (* function create_sort end *)

  let create_expr : Ctx.t -> Expr.t =
    fun ctx ->
    Ctx.read_expr ctx mv_cc ~f:(fun _ ->
        let (sort : Sort.t) = create_sort ctx in
        DataType.create_expr sort ~const_idx:0 []
    )
  (* function create_expr end *)
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
