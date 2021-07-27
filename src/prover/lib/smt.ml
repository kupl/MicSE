exception ZError = Z3.Error

module CONST = struct
  let _name_dummy : string = "DUMMY"
  let _name_unit : string = "UNIT"
  let _name_map : string = "MAP"
  
  let _tmpname_source : string = "SOURCE"
  let _tmpname_sender : string = "SENDER"

  let _sort_key : string = "Key"
  let _sort_keyhash : string = "KeyHash"
  let _sort_unit : string = "Unit"
  let _sort_operation : string = "Operation"
  let _sort_contract : string = "Contract"
  let _sort_lambda : string = "Lambda"
  let _sort_bytes : string = "Bytes"
  let _sort_signature : string = "Signature"
  let _sort_address : string = "Address"
  let _sort_option : string = "Option"
  let _sort_pair : string = "Pair"
  let _sort_or : string = "Or"
  let _sort_list : string = "List"

  let _const_key_keystr : string = "KeyStr"
  let _const_keyhash_str : string = "KeyHashStr"
  let _const_keyhash_hashkey : string = "KeyHashKey"
  let _const_bytes_bytstr : string = "BytStr"
  let _const_bytes_pack : string = "Pack"
  let _const_bytes_concatenated : string = "BytConcat"
  let _const_bytes_sliced : string = "BytSlice"
  let _const_bytes_blake2b : string = "Blake2b"
  let _const_bytes_sha256 : string = "Sha256"
  let _const_bytes_sha512 : string = "Sha512"
  let _const_signature_sigstr : string = "SigStr"
  let _const_signature_signed : string = "Signed"
  let _const_address_addrkh : string = "AddrKh"
  let _const_option_none : string = "None"
  let _const_option_some : string = "Some"
  let _const_pair : string = "Pair"
  let _const_or_left : string = "Left"
  let _const_or_right : string = "Right"
  let _const_list_nil : string = "Nil"
  let _const_list_cons : string = "Cons"

  let _recog_key_keystr : string = "is_keystr"
  let _recog_keyhash_str : string = "is_keyhashStr"
  let _recog_keyhash_hashkey : string = "is_keyhashKey"
  let _recog_bytes_bytstr : string = "is_bytstr"
  let _recog_bytes_pack : string = "is_pack"
  let _recog_bytes_concatenated : string = "is_bytes_concatenated"
  let _recog_bytes_sliced : string = "is_bytes_sliced"
  let _recog_bytes_blake2b : string = "is_blake2b"
  let _recog_bytes_sha256 : string = "is_sha256"
  let _recog_bytes_sha512 : string = "is_sha512"
  let _recog_signature_sigstr : string = "is_sigstr"
  let _recog_signature_signed : string = "is_signed"
  let _recog_address_addrkh : string = "is_addrkh"
  let _recog_option_none : string = "is_none"
  let _recog_option_some : string = "is_some"
  let _recog_pair : string = "is_pair"
  let _recog_or_left : string = "is_left"
  let _recog_or_right : string = "is_right"
  let _recog_list_nil : string = "is_nil"
  let _recog_list_cons : string = "is_cons"

  let _field_content : string = "content"
  let _field_pair_fst : string = "fst"
  let _field_pair_snd : string = "snd"
  let _field_list_head : string = "head"
  let _field_list_tail : string = "tail"

  let _bit_mutez : int = 63

  let _int2bv_precision : int = 128
end

(*****************************************************************************)
(*****************************************************************************)
(* Context                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

module ZCtx = struct
  type body = (string * string)
  type t = Z3.context

  let body_timeout : unit -> body
  = fun () -> begin
    let budget = !Utils.Options.z3_time_budget * 1000 in
    ("timeout", (string_of_int (budget)))
  end
  
  let create : unit -> t
  = fun () -> [ (body_timeout ()); ] |> Z3.mk_context
end


(*****************************************************************************)
(*****************************************************************************)
(* Symbols                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

module ZSym = struct
  type t = Z3.Symbol.symbol

  let _name_dummy : string
  =CONST._name_dummy
  let _count_dummy : int ref
  =ref 0

  let create : ZCtx.t -> string -> t
  = fun ctx name -> name |> Z3.Symbol.mk_string ctx
  let create_dummy : ZCtx.t -> t
  = fun ctx -> begin
    _count_dummy := !_count_dummy + 1;
    (_name_dummy ^ (!_count_dummy |> string_of_int)) |> create ctx
  end

  let to_string : t -> string
  =Z3.Symbol.to_string
end

(*****************************************************************************)
(*****************************************************************************)
(* Sorts                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

module ZSort = struct
  type t = Z3.Sort.sort

  let create_dummy : ZCtx.t -> t
  =fun ctx -> ctx |> ZSym.create_dummy |> Z3.Sort.mk_uninterpreted ctx
  let create : ZCtx.t -> name:string -> t
  =fun ctx ~name -> name |> ZSym.create ctx |> Z3.Sort.mk_uninterpreted ctx

  let to_string : t -> string
  =Z3.Sort.to_string
end


(*****************************************************************************)
(*****************************************************************************)
(* Expressions                                                               *)
(*****************************************************************************)
(*****************************************************************************)

module ZExpr = struct
  type t = Z3.Expr.expr

  let create_dummy : ZCtx.t -> ZSort.t -> t
  =fun ctx sort -> sort |> (ZSym.create_dummy ctx |> Z3.Expr.mk_const ctx)
  let create_var : ZCtx.t -> ZSort.t -> name:string -> t
  =fun ctx sort ~name -> sort |> (name |> ZSym.create ctx |> Z3.Expr.mk_const ctx)

  let create_ite : ZCtx.t -> cond:t -> t:t -> f:t -> t
  =fun ctx ~cond ~t ~f -> Z3.Boolean.mk_ite ctx cond t f

  let read_sort : t -> ZSort.t
  =Z3.Expr.get_sort

  let to_string : t -> string
  =Z3.Expr.to_string
end


(*****************************************************************************)
(*****************************************************************************)
(* FuncDecls                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

module ZFunc = struct
  type t = Z3.FuncDecl.func_decl

  let get_idx : 'a list -> idx:int -> 'a
  =fun l ~idx -> try idx |> (l |> Core.List.nth_exn) with |_ -> ZError ("get_idx " ^ (idx |> string_of_int) ^ " called on list of length " ^ (l |> Core.List.length |> string_of_int)) |> raise

  let apply : t -> params:ZExpr.t list -> ZExpr.t
  =fun f ~params -> params |> (f |> Z3.FuncDecl.apply)

  let sort_of_domain : t -> idx:int -> ZSort.t
  =fun f ~idx -> f |> Z3.FuncDecl.get_domain |> get_idx ~idx:idx
end


(*****************************************************************************)
(*****************************************************************************)
(* Datatypes                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

module ZDatatype = struct
  type const = Z3.Datatype.Constructor.constructor

  let get_idx : 'a list -> idx:int -> 'a
  =fun l ~idx -> try idx |> (l |> Core.List.nth_exn) with |_ -> ZError ("get_idx " ^ (idx |> string_of_int) ^ " called on list of length " ^ (l |> Core.List.length |> string_of_int)) |> raise

  let create_const : ZCtx.t -> name:string -> recog_func_name:string -> field_names:string list -> field_sorts:ZSort.t option list -> field_sort_refs:int list -> const
  =fun ctx ~name ~recog_func_name ~field_names ~field_sorts ~field_sort_refs -> begin
    field_sort_refs |> (
    field_sorts |> (
    (field_names |> (Core.List.map ~f:(ZSym.create ctx))) |> (
    (recog_func_name |> (ZSym.create ctx)) |> (
    (name |> (ZSym.create ctx)) |> 
    Z3.Datatype.mk_constructor ctx))))
  end
  let create_sort : ZCtx.t -> name:string -> const_list:const list -> ZSort.t
  =fun ctx ~name ~const_list -> const_list |> (name |> ZSym.create ctx |> Z3.Datatype.mk_sort ctx)
  let create_const_func : ZSort.t -> const_idx:int -> ZFunc.t
  =fun sort ~const_idx -> sort |> Z3.Datatype.get_constructors |> get_idx ~idx:const_idx
  let create_recog_func : ZSort.t -> const_idx:int -> ZFunc.t
  =fun sort ~const_idx -> sort |> Z3.Datatype.get_recognizers |> get_idx ~idx:const_idx
  let create_access_func : ZSort.t -> const_idx:int -> field_idx:int -> ZFunc.t
  =fun sort ~const_idx ~field_idx -> sort |> Z3.Datatype.get_accessors |> get_idx ~idx:const_idx |> get_idx ~idx:field_idx
  let read_field_sort : ZSort.t -> const_idx:int -> field_idx:int -> ZSort.t
  =fun sort ~const_idx ~field_idx -> sort |> create_const_func ~const_idx:const_idx |> ZFunc.sort_of_domain ~idx:field_idx

  let create : ZSort.t -> const_idx:int -> expr_list:ZExpr.t list -> ZExpr.t
  =fun sort ~const_idx ~expr_list -> (sort |> create_const_func ~const_idx:const_idx) |> ZFunc.apply ~params:expr_list
  let read : ZExpr.t -> const_idx:int -> field_idx:int -> ZExpr.t
  =fun e ~const_idx ~field_idx -> e |> ZExpr.read_sort |> create_access_func ~const_idx:const_idx ~field_idx:field_idx |> ZFunc.apply ~params:[e]

  let is_field : ZExpr.t -> const_idx:int -> ZExpr.t
  =fun e ~const_idx -> e |> ZExpr.read_sort |> create_recog_func ~const_idx:const_idx |> ZFunc.apply ~params:[e]
end


(*****************************************************************************)
(*****************************************************************************)
(* Formulae                                                                  *)
(*****************************************************************************)
(*****************************************************************************)

module ZFormula = struct
  type t = ZExpr.t

  let sort : ZCtx.t -> ZSort.t
  =fun ctx -> Z3.Boolean.mk_sort ctx
  
  let true_ : ZCtx.t -> t
  =fun ctx -> Z3.Boolean.mk_true ctx
  let false_ : ZCtx.t -> t
  =fun ctx -> Z3.Boolean.mk_false ctx
  let uninterpreted_ : ZCtx.t -> t
  =fun ctx -> Z3.Boolean.mk_const ctx (ZSym.create_dummy ctx)

  let create_not : ZCtx.t -> t -> t
  =fun ctx f -> f |> Z3.Boolean.mk_not ctx
  let create_and : ZCtx.t -> t list -> t
  =fun ctx fl -> fl |> Z3.Boolean.mk_and ctx
  let create_or : ZCtx.t -> t list -> t
  =fun ctx fl -> fl |> Z3.Boolean.mk_or ctx
  let create_xor : ZCtx.t -> t -> t -> t
  =fun ctx f1 f2 -> Z3.Boolean.mk_xor ctx f1 f2
  let create_eq : ZCtx.t -> t -> t -> t
  =fun ctx f1 f2 -> Z3.Boolean.mk_eq ctx f1 f2
  let create_neq : ZCtx.t -> t -> t -> t
  =fun ctx e1 e2 -> create_eq ctx e1 e2 |> create_not ctx
  let create_imply : ZCtx.t -> t -> t -> t
  =fun ctx f1 f2 -> Z3.Boolean.mk_implies ctx f1 f2
  let create_iff : ZCtx.t -> t -> t -> t
  =fun ctx f1 f2 -> Z3.Boolean.mk_iff ctx f1 f2
end


(*****************************************************************************)
(*****************************************************************************)
(* Unit                                                                      *)
(*****************************************************************************)
(*****************************************************************************)

module ZUnit = struct
  type t = ZExpr.t

  let sort : ZCtx.t -> ZSort.t
  =fun ctx -> ZSort.create ctx ~name:CONST._name_unit

  let create : ZCtx.t -> t
  =fun ctx -> sort ctx |> ZExpr.create_var ctx ~name:CONST._sort_unit
end


(*****************************************************************************)
(*****************************************************************************)
(* Booleans                                                                  *)
(*****************************************************************************)
(*****************************************************************************)

module ZBool = struct
  type t = ZExpr.t

  let sort : ZCtx.t -> ZSort.t
  =fun ctx -> Z3.Boolean.mk_sort ctx

  let of_bool : ZCtx.t -> bool -> t
  =fun ctx e -> e |> Z3.Boolean.mk_val ctx

  let minus_one_ : ZCtx.t -> t
  =fun ctx -> Z3.Arithmetic.Integer.mk_numeral_i ctx (-1)
  let zero_ : ZCtx.t -> t
  =fun ctx -> Z3.Arithmetic.Integer.mk_numeral_i ctx (0)
  let one_ : ZCtx.t -> t
  =fun ctx -> Z3.Arithmetic.Integer.mk_numeral_i ctx (1)
  
  let true_ : ZCtx.t -> t
  =fun ctx -> Z3.Boolean.mk_true ctx
  let false_ : ZCtx.t -> t
  =fun ctx -> Z3.Boolean.mk_false ctx

  let create_not : ZCtx.t -> t -> t
  =fun ctx e -> e |> Z3.Boolean.mk_not ctx
  let create_and : ZCtx.t -> t -> t -> t
  =fun ctx e1 e2 -> Z3.Boolean.mk_and ctx [e1; e2]
  let create_or : ZCtx.t -> t -> t -> t
  =fun ctx e1 e2 -> Z3.Boolean.mk_or ctx [e1; e2]
  let create_xor : ZCtx.t -> t -> t -> t
  =fun ctx e1 e2 -> Z3.Boolean.mk_xor ctx e1 e2

  let create_eq : ZCtx.t -> t -> t -> t
  =fun ctx e1 e2 -> Z3.Boolean.mk_eq ctx e1 e2
  let create_neq : ZCtx.t -> t -> t -> t
  =fun ctx e1 e2 -> create_eq ctx e1 e2 |> create_not ctx

  let create_cmp : ZCtx.t -> t -> t -> t
  =fun ctx e1 e2 -> begin
    ZExpr.create_ite ctx
      ~cond:(create_eq ctx e1 e2)
      ~t:(zero_ ctx)
      ~f:(ZExpr.create_ite ctx
            ~cond:(e2)
            ~t:(minus_one_ ctx)
            ~f:(one_ ctx))
  end
end


(*****************************************************************************)
(*****************************************************************************)
(* Integers                                                                  *)
(*****************************************************************************)
(*****************************************************************************)

module ZInt = struct
  type t = ZExpr.t

  let sort : ZCtx.t -> ZSort.t
  =fun ctx -> Z3.Arithmetic.Integer.mk_sort ctx

  let of_zarith : ZCtx.t -> Z.t -> t
  =fun ctx n -> n |> Z.to_string |> Z3.Arithmetic.Integer.mk_numeral_s ctx
  let of_int : ZCtx.t -> int -> t
  =fun ctx i -> i |> Z3.Arithmetic.Integer.mk_numeral_i ctx

  let minus_one_ : ZCtx.t -> t
  =fun ctx -> of_int ctx (-1)
  let zero_ : ZCtx.t -> t
  =fun ctx -> of_int ctx (0)
  let one_ : ZCtx.t -> t
  =fun ctx -> of_int ctx (1)

  let mutez_max_ : ZCtx.t -> t
  =fun ctx -> of_zarith ctx (Z.shift_left Z.one (CONST._bit_mutez) |> Z.pred)

  let create_neg : ZCtx.t -> t -> t
  =fun ctx e -> e |> Z3.Arithmetic.mk_unary_minus ctx
  let create_add : ZCtx.t -> t list -> t
  =fun ctx el -> el |> Z3.Arithmetic.mk_add ctx
  let create_sub : ZCtx.t -> t list -> t
  =fun ctx el -> el |> Z3.Arithmetic.mk_sub ctx
  let create_mul : ZCtx.t -> t list -> t
  =fun ctx el -> el |> Z3.Arithmetic.mk_mul ctx
  let create_div : ZCtx.t -> t -> t -> t
  =fun ctx e1 e2 -> Z3.Arithmetic.mk_div ctx e1 e2
  let create_mod : ZCtx.t -> t -> t -> t
  =fun ctx e1 e2 -> Z3.Arithmetic.Integer.mk_mod ctx e1 e2
  let create_power : ZCtx.t -> t -> t -> t
  =fun ctx e1 e2 -> Z3.Arithmetic.mk_power ctx e1 e2

  let create_eq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_neq
  let create_lt : ZCtx.t -> t -> t -> ZBool.t
  =fun ctx e1 e2 -> Z3.Arithmetic.mk_lt ctx e1 e2
  let create_le : ZCtx.t -> t -> t -> ZBool.t
  =fun ctx e1 e2 -> Z3.Arithmetic.mk_le ctx e1 e2
  let create_gt : ZCtx.t -> t -> t -> ZBool.t
  =fun ctx e1 e2 -> Z3.Arithmetic.mk_gt ctx e1 e2
  let create_ge : ZCtx.t -> t -> t -> ZBool.t
  =fun ctx e1 e2 -> Z3.Arithmetic.mk_ge ctx e1 e2

  (* bitwise operations *)
  let _to_finite_bv : ZCtx.t -> t -> ZExpr.t
  =fun ctx e -> e |> Z3.Arithmetic.Integer.mk_int2bv ctx (CONST._int2bv_precision)
  let _create_finite_bv_expressible : ZCtx.t -> t -> ZBool.t
  =fun ctx x -> create_lt ctx x (create_power ctx (of_int ctx 2) (of_int ctx CONST._int2bv_precision))
    
  let create_shiftL : ZCtx.t -> t -> t -> t
  =fun ctx x1 x2 -> create_mul ctx [x1; create_power ctx (of_int ctx 2) x2]
  let create_shiftR : ZCtx.t -> t -> t -> t
  =fun ctx x1 x2 -> create_div ctx x1 (create_power ctx (of_int ctx 2) x2)
  let create_not : ZCtx.t -> t -> t
  =fun ctx x -> create_sub ctx [create_neg ctx x; (one_ ctx)] (* bitwise not is always (fun x -> -x-1) regardless of sign *)
  let create_and : ZCtx.t -> t -> t -> t
  =fun ctx e1 e2 -> begin
    let is_expressible = ZFormula.create_and ctx [_create_finite_bv_expressible ctx e1; _create_finite_bv_expressible ctx e2] in 
    ZExpr.create_ite ctx
      ~cond:is_expressible
      ~t:(
        let e1_bv, e2_bv = _to_finite_bv ctx e1, _to_finite_bv ctx e2 in
        let ret_bv = Z3.BitVector.mk_and ctx e1_bv e2_bv in
        Z3.BitVector.mk_bv2int ctx ret_bv false
        (* last arg, "false" indicates that the bitvector will be interpreted as unsigned integer (nat-number) *)
      )
      ~f:(ZExpr.create_dummy ctx (sort ctx))
  end
  let create_or : ZCtx.t -> t -> t -> t
  =fun ctx e1 e2 -> begin
    let is_expressible = ZFormula.create_and ctx [_create_finite_bv_expressible ctx e1; _create_finite_bv_expressible ctx e2] in 
    ZExpr.create_ite ctx 
      ~cond:is_expressible
      ~t:(
        let e1_bv, e2_bv = _to_finite_bv ctx e1, _to_finite_bv ctx e2 in
        let ret_bv = Z3.BitVector.mk_or ctx e1_bv e2_bv in
        Z3.BitVector.mk_bv2int ctx ret_bv false
        (* last arg, "false" indicates that the bitvector will be interpreted as unsigned integer (nat-number) *)
      )
      ~f:(ZExpr.create_dummy ctx (sort ctx))
  end
  let create_xor : ZCtx.t -> t -> t -> t
  =fun ctx e1 e2 -> begin
    let is_expressible = ZFormula.create_and ctx [_create_finite_bv_expressible ctx e1; _create_finite_bv_expressible ctx e2] in 
    ZExpr.create_ite ctx
      ~cond:is_expressible
      ~t:(
        let e1_bv, e2_bv = _to_finite_bv ctx e1, _to_finite_bv ctx e2 in
        let ret_bv = Z3.BitVector.mk_xor ctx e1_bv e2_bv in
        Z3.BitVector.mk_bv2int ctx ret_bv false
        (* last arg, "false" indicates that the bitvector will be interpreted as unsigned integer (nat-number) *)
      )
      ~f:(ZExpr.create_dummy ctx (sort ctx))
  end

  let create_cmp : ZCtx.t -> t -> t -> t
  =fun ctx e1 e2 -> begin
    ZExpr.create_ite ctx
      ~cond:(create_eq ctx e1 e2)
      ~t:(zero_ ctx)
      ~f:(ZExpr.create_ite ctx
            ~cond:(create_lt ctx e1 e2)
            ~t:(minus_one_ ctx)
            ~f:(one_ ctx))
  end
  let create_abs : ZCtx.t -> t -> t
  =fun ctx e -> begin
    ZExpr.create_ite ctx
      ~cond:(create_lt ctx e (zero_ ctx))
      ~t:(create_neg ctx e)
      ~f:(e)
  end

  let to_zmutez : t -> ZExpr.t
  = fun e -> e
  (* =fun e -> e |> Z3.Arithmetic.Integer.mk_int2bv ctx (CONST._bit_mutez) *)
end


(*****************************************************************************)
(*****************************************************************************)
(* Natural Number                                                            *)
(*****************************************************************************)
(*****************************************************************************)

module ZNat = ZInt


(*****************************************************************************)
(*****************************************************************************)
(* Mutez                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

module ZMutez = struct
  type t = ZExpr.t

  let sort : ZCtx.t -> ZSort.t
  = fun ctx -> ZInt.sort ctx
  (* =fun ctx -> Z3.BitVector.mk_sort ctx (CONST._bit_mutez) *)

  let of_zarith : ZCtx.t -> Z.t -> t
  = ZInt.of_zarith
  (* =fun n -> Z3.BitVector.mk_numeral ctx (Z.to_string n) (CONST._bit_mutez) *)
  let of_int : ZCtx.t -> int -> t
  = ZInt.of_int
  (* =fun n -> Z3.BitVector.mk_numeral ctx (string_of_int n) (CONST._bit_mutez) *)

  let max_ : ZCtx.t -> t
  = ZInt.mutez_max_
  let zero_ : ZCtx.t -> t
  = ZInt.zero_
  (* =fun ctx -> of_int (0) *)

  let create_add : ZCtx.t -> t -> t -> t
  = fun ctx e1 e2 -> ZInt.create_add ctx [e1; e2]
  (* =fun e1 e2 -> Z3.BitVector.mk_add ctx e1 e2 *)
  let create_sub : ZCtx.t -> t -> t -> t
  = fun ctx e1 e2 -> ZInt.create_sub ctx [e1; e2]
  (* =fun e1 e2 -> Z3.BitVector.mk_sub ctx e1 e2 *)
  let create_mul : ZCtx.t -> t -> t -> t
  = fun ctx e1 e2 -> ZInt.create_mul ctx [e1; e2]
  (* =fun e1 e2 -> Z3.BitVector.mk_mul ctx e1 e2 *)
  let create_div : ZCtx.t -> t -> t -> t
  = ZInt.create_div
  (* =fun e1 e2 -> Z3.BitVector.mk_udiv ctx e1 e2 *)
  let create_mod : ZCtx.t -> t -> t -> t
  = ZInt.create_mod
  (* =fun e1 e2 -> Z3.BitVector.mk_urem ctx e1 e2 *)

  let create_eq : ZCtx.t -> t -> t -> ZBool.t
  = ZBool.create_eq
  let create_neq : ZCtx.t -> t -> t -> ZBool.t
  = ZBool.create_neq
  let create_lt : ZCtx.t -> t -> t -> ZBool.t
  = ZInt.create_lt
  (* =fun e1 e2 -> Z3.BitVector.mk_ult ctx e1 e2 *)
  let create_le : ZCtx.t -> t -> t -> ZBool.t
  = ZInt.create_le
  (* =fun e1 e2 -> Z3.BitVector.mk_ule ctx e1 e2 *)
  let create_gt : ZCtx.t -> t -> t -> ZBool.t
  = ZInt.create_gt
  (* =fun e1 e2 -> Z3.BitVector.mk_ugt ctx e1 e2 *)
  let create_ge : ZCtx.t -> t -> t -> ZBool.t
  = ZInt.create_ge
  (* =fun e1 e2 -> Z3.BitVector.mk_uge ctx e1 e2 *)

  let create_cmp : ZCtx.t -> t -> t -> ZInt.t
  = ZInt.create_cmp
  (* =fun e1 e2 -> begin
    ZExpr.create_ite
      ~cond:(create_eq e1 e2)
      ~t:(ZInt.zero_ ctx)
      ~f:(ZExpr.create_ite
            ~cond:(create_lt e1 e2)
            ~t:(ZInt.minus_one_ ctx)
            ~f:(ZInt.one_ ctx))
  end *)

  let to_zint : t -> ZInt.t
  = fun e1 -> e1
  (* =fun e -> Z3.BitVector.mk_bv2int ctx e false *)

  let create_bound : ZCtx.t -> t -> ZBool.t
  = fun ctx e1 -> begin
    let lower = create_le ctx (zero_ ctx) e1 in
    let upper = create_lt ctx e1 (max_ ctx) in
    ZBool.create_and ctx lower upper
  end (* function create_bound end *)
  let check_add_no_overflow : ZCtx.t -> t -> t -> ZBool.t
  = fun ctx e1 e2 -> begin
    let addition = create_add ctx e1 e2 in
    create_lt ctx addition (max_ ctx)
  end (* function check_add_no_overflow end *)
  (* =fun e1 e2 -> Z3.BitVector.mk_add_no_overflow ctx e1 e2 false *)
  let check_mul_no_overflow : ZCtx.t -> t -> t -> ZBool.t
  = fun ctx e1 e2 -> begin
    let multiplication = create_mul ctx e1 e2 in
    create_le ctx multiplication (max_ ctx)
  end (* function check_mul_no_overflow end *)
  (* =fun e1 e2 -> Z3.BitVector.mk_mul_no_overflow ctx e1 e2 false *)
  let check_sub_no_underflow : ZCtx.t -> t -> t -> ZBool.t
  = fun ctx e1 e2 -> begin
    let subtraction = create_sub ctx e1 e2 in
    create_le ctx (zero_ ctx) subtraction
  end (* function check_sub_no_underflow end *)
  (* =fun e1 e2 -> Z3.BitVector.mk_sub_no_underflow ctx e1 e2 false *)
end


(*****************************************************************************)
(*****************************************************************************)
(* Strings                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

module ZStr = struct
  type t = ZExpr.t

  let sort : ZCtx.t -> ZSort.t
  =fun ctx -> Z3.Seq.mk_string_sort ctx

  let of_string : ZCtx.t -> string -> t
  =fun ctx e -> e |> Z3.Seq.mk_string ctx

  let create_concat : ZCtx.t -> t list -> t
  =fun ctx el -> el |> Z3.Seq.mk_seq_concat ctx
  let create_slice : ZCtx.t -> t -> low:ZInt.t -> high:ZInt.t -> t
  =fun ctx s ~low ~high -> Z3.Seq.mk_seq_extract ctx s low high
  let create_length : ZCtx.t -> t -> ZInt.t
  =fun ctx e -> e |> Z3.Seq.mk_seq_length ctx

  let create_eq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_neq

  let create_cmp : ZCtx.t -> t -> t -> ZInt.t
  =fun ctx e1 e2 -> begin
    ZExpr.create_ite ctx
      ~cond:(create_eq ctx e1 e2)
      ~t:(ZInt.zero_ ctx)
      ~f:(ZExpr.create_ite ctx
            ~cond:(ZInt.create_lt ctx (create_length ctx e1) (create_length ctx e2)) (* Not completely implemented. *)
            ~t:(ZInt.minus_one_ ctx)
            ~f:(ZInt.one_ ctx))
  end
end


(*****************************************************************************)
(*****************************************************************************)
(* Key                                                                       *)
(*****************************************************************************)
(*****************************************************************************)

module ZKey = struct
  type t = ZExpr.t

  let _create_const_of_keystr : ZCtx.t -> ZDatatype.const
  =fun ctx -> ZDatatype.create_const ctx
      ~name:CONST._const_key_keystr
      ~recog_func_name:CONST._recog_key_keystr
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[(Some (ZStr.sort ctx))]
      ~field_sort_refs:[1]

  let sort : ZCtx.t -> ZSort.t
  =fun ctx -> ZDatatype.create_sort ctx
      ~name:CONST._sort_key
      ~const_list:[(_create_const_of_keystr ctx)]

  let of_string : ZCtx.t -> string -> t
  =fun ctx s -> sort ctx |> ZDatatype.create ~const_idx:0 ~expr_list:[ZStr.of_string ctx s]
  let create_keystr : ZCtx.t -> ZExpr.t -> t
  =fun ctx content -> sort ctx |> ZDatatype.create ~const_idx:0 ~expr_list:[content]


  let _read_innerstr : t -> ZStr.t
  =fun t -> ZDatatype.read t ~const_idx:0 ~field_idx:0

  let create_cmp : ZCtx.t -> t -> t -> ZInt.t
  =fun ctx t1 t2 -> ZStr.create_cmp ctx (_read_innerstr t1) (_read_innerstr t2)
  let create_eq : ZCtx.t -> t -> t -> ZBool.t
  =fun ctx t1 t2 -> ZStr.create_eq ctx (_read_innerstr t1) (_read_innerstr t2) 
end


(*****************************************************************************)
(*****************************************************************************)
(* Key Hash                                                                  *)
(*****************************************************************************)
(*****************************************************************************)

module ZKeyHash = struct
  type t = ZExpr.t

  let _create_const_of_str : ZCtx.t -> ZDatatype.const
  =fun ctx -> ZDatatype.create_const ctx
      ~name:CONST._const_keyhash_str
      ~recog_func_name:CONST._recog_keyhash_str
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[(Some (ZStr.sort ctx))]
      ~field_sort_refs:[1]
  let _create_const_of_hashkey : ZCtx.t -> ZDatatype.const
  =fun ctx -> ZDatatype.create_const ctx
      ~name:CONST._const_keyhash_hashkey
      ~recog_func_name:CONST._recog_keyhash_hashkey
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[(Some (ZKey.sort ctx))]
      ~field_sort_refs:[1]
  
  let sort : ZCtx.t -> ZSort.t
  =fun ctx -> ZDatatype.create_sort ctx
      ~name:CONST._sort_keyhash
      ~const_list:[ (_create_const_of_str ctx);
                    (_create_const_of_hashkey ctx);
                  ]

  let of_string : ZCtx.t -> string -> t
  =fun ctx s -> sort ctx |> ZDatatype.create ~const_idx:0 ~expr_list:[ZStr.of_string ctx s]
  let create_hashkey : ZCtx.t -> ZExpr.t -> t
  =fun ctx key -> sort ctx |> ZDatatype.create ~const_idx:1 ~expr_list:[key]

  let _read_innerstr : t -> ZStr.t
  =fun t -> ZDatatype.read t ~const_idx:0 ~field_idx:0
  let _read_innerkey : t -> ZKey.t
  =fun t -> ZDatatype.read t ~const_idx:1 ~field_idx:0

  (* custom order 
    str(s) > hashkey(k)
  *)
  let create_cmp : ZCtx.t -> t -> t -> ZInt.t
  =fun ctx t1 t2 -> begin
    ZExpr.create_ite ctx ~cond:(ZDatatype.is_field t1 ~const_idx:0)
    ~t:(ZExpr.create_ite ctx ~cond:(ZDatatype.is_field t2 ~const_idx:0)
        ~t:(ZStr.create_cmp ctx (_read_innerstr t1) (_read_innerstr t2))
        ~f:(ZExpr.create_dummy ctx (ZInt.sort ctx))
    )
    ~f:(ZExpr.create_ite ctx ~cond:(ZDatatype.is_field t2 ~const_idx:0)
        ~t:(ZExpr.create_dummy ctx (ZInt.sort ctx))
        ~f:(ZKey.create_cmp ctx (_read_innerkey t1) (_read_innerkey t2))
    )
  end

  let create_eq : ZCtx.t -> t -> t -> ZBool.t
  =fun ctx t1 t2 -> ZExpr.create_ite ctx ~cond:(ZInt.create_eq ctx (create_cmp ctx t1 t2) (ZInt.zero_ ctx)) ~t:(ZBool.true_ ctx) ~f:(ZBool.false_ ctx)
end


(*****************************************************************************)
(*****************************************************************************)
(* Options                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

module ZOption = struct
  type t = ZExpr.t

  let _create_const_of_none : ZCtx.t -> ZDatatype.const
  =fun ctx -> ZDatatype.create_const ctx
      ~name:CONST._const_option_none
      ~recog_func_name:CONST._recog_option_none
      ~field_names:[]
      ~field_sorts:[]
      ~field_sort_refs:[]
  let _create_const_of_some : ZCtx.t -> content_sort:ZSort.t -> ZDatatype.const
  =fun ctx ~content_sort -> begin
    ZDatatype.create_const ctx
      ~name:CONST._const_option_some
      ~recog_func_name:CONST._recog_option_some
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[(Some content_sort)]
      ~field_sort_refs:[1]
  end
  let _create_sort_name : content_sort:ZSort.t -> string
  =fun ~content_sort -> (CONST._sort_option) ^ "(" ^ (ZSort.to_string content_sort) ^ ")"

  let create_sort : ZCtx.t -> content_sort:ZSort.t -> ZSort.t
  =fun ctx ~content_sort -> begin
    ZDatatype.create_sort ctx
      ~name:(_create_sort_name ~content_sort:content_sort)
      ~const_list:[(_create_const_of_none ctx); (_create_const_of_some ctx ~content_sort:content_sort)]
  end

  let create_none : ZCtx.t -> content_sort:ZSort.t -> t
  =fun ctx ~content_sort -> create_sort ctx ~content_sort:content_sort |> ZDatatype.create ~const_idx:0 ~expr_list:[]
  let create_some : ZCtx.t -> content:ZExpr.t -> t
  =fun ctx ~content -> create_sort ctx ~content_sort:(content |> ZExpr.read_sort) |> ZDatatype.create ~const_idx:1 ~expr_list:[content]
  let read : t -> ZExpr.t
  =ZDatatype.read ~const_idx:1 ~field_idx:0

  let create_eq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_neq

  let is_none : t -> ZBool.t
  =ZDatatype.is_field ~const_idx:0
  let is_some : t -> ZBool.t
  =ZDatatype.is_field ~const_idx:1
end


(*****************************************************************************)
(*****************************************************************************)
(* Pairs                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

module ZPair = struct
  type t = ZExpr.t

  let _create_const_of_pair : ZCtx.t -> fst_sort:ZSort.t -> snd_sort:ZSort.t -> ZDatatype.const
  =fun ctx ~fst_sort ~snd_sort -> begin
    ZDatatype.create_const ctx
      ~name:CONST._const_pair
      ~recog_func_name:CONST._recog_pair
      ~field_names:[(CONST._field_pair_fst); (CONST._field_pair_snd)]
      ~field_sorts:[(Some fst_sort); (Some snd_sort)]
      ~field_sort_refs:[1; 2]
  end
  let _create_sort_name : fst_sort:ZSort.t -> snd_sort:ZSort.t -> string
  =fun ~fst_sort ~snd_sort -> (CONST._sort_pair) ^ "(" ^ (ZSort.to_string fst_sort) ^ ", " ^ (ZSort.to_string snd_sort) ^ ")"

  let create_sort : ZCtx.t -> fst_sort:ZSort.t -> snd_sort:ZSort.t -> ZSort.t
  =fun ctx ~fst_sort ~snd_sort -> begin
    ZDatatype.create_sort ctx
      ~name:(_create_sort_name ~fst_sort:fst_sort ~snd_sort:snd_sort)
      ~const_list:[(_create_const_of_pair ctx ~fst_sort:fst_sort ~snd_sort:snd_sort)]
  end

  let create : ZCtx.t -> fst:ZExpr.t -> snd:ZExpr.t -> t
  =fun ctx ~fst ~snd -> begin
    (create_sort ctx
      ~fst_sort:(fst |> ZExpr.read_sort)
      ~snd_sort:(snd |> ZExpr.read_sort)) |>
    ZDatatype.create ~const_idx:0 ~expr_list:[fst; snd]
  end
  let read_fst : t -> ZExpr.t
  =ZDatatype.read ~const_idx:0 ~field_idx:0
  let read_snd : t -> ZExpr.t
  =ZDatatype.read ~const_idx:0 ~field_idx:1

  let create_eq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_neq
end


(*****************************************************************************)
(*****************************************************************************)
(* Bytes                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

module ZBytes = struct
  type t = ZExpr.t

  let _create_const_of_bytnil : ZCtx.t -> ZDatatype.const
  =fun ctx -> ZDatatype.create_const ctx
      ~name:"TODO: BYTENIL"
      ~recog_func_name:"TODO: RECOG_BYTENIL"
      ~field_names:[]
      ~field_sorts:[]
      ~field_sort_refs:[]
  let _create_const_of_bytstr : ZCtx.t -> ZDatatype.const
  =fun ctx -> ZDatatype.create_const ctx
      ~name:CONST._const_bytes_bytstr
      ~recog_func_name:CONST._recog_bytes_bytstr
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[(Some (ZStr.sort ctx))]
      ~field_sort_refs:[1]
  let _create_const_of_concatenated : ZCtx.t -> ZDatatype.const
  =fun ctx -> ZDatatype.create_const ctx
      ~name:CONST._const_bytes_concatenated
      ~recog_func_name:CONST._recog_bytes_concatenated
      ~field_names:[(CONST._field_pair_fst); (CONST._field_pair_snd)]
      ~field_sorts:[None; None]
      ~field_sort_refs:[0; 0]
  let _create_const_of_blake2b : ZCtx.t -> ZDatatype.const
  =fun ctx -> ZDatatype.create_const ctx
      ~name:CONST._const_bytes_blake2b
      ~recog_func_name:CONST._recog_bytes_blake2b
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[None]
      ~field_sort_refs:[0]
  let _create_const_of_sha256 : ZCtx.t -> ZDatatype.const
  =fun ctx -> ZDatatype.create_const ctx
      ~name:CONST._const_bytes_sha256
      ~recog_func_name:CONST._recog_bytes_sha256
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[None]
      ~field_sort_refs:[0]
  let _create_const_of_sha512 : ZCtx.t -> ZDatatype.const
  =fun ctx -> ZDatatype.create_const ctx
      ~name:CONST._const_bytes_sha512
      ~recog_func_name:CONST._recog_bytes_sha512
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[None]
      ~field_sort_refs:[0]

  let sort : ZCtx.t -> ZSort.t
  =fun ctx -> ZDatatype.create_sort ctx
    ~name:CONST._sort_bytes
    ~const_list:[ (_create_const_of_bytstr ctx);
                  (_create_const_of_concatenated ctx);
                  (_create_const_of_blake2b ctx);
                  (_create_const_of_sha256 ctx);
                  (_create_const_of_sha512 ctx);
                ]
 
  (* let bytnil : ZCtx.t -> t
  =fun ctx -> sort ctx |> ZDatatype.create ~const_idx:0 ~expr_list:[] *)

  let of_string : ZCtx.t -> string -> t
  =fun ctx s -> sort ctx |> ZDatatype.create ~const_idx:0 ~expr_list:[ZStr.of_string ctx s;]
  let create_bytstr : ZCtx.t -> ZExpr.t -> t
  =fun ctx content -> sort ctx |> ZDatatype.create ~const_idx:0 ~expr_list:[content;]

  let create_pack : ZCtx.t -> t
  =fun ctx -> ZStr.sort ctx |> ZExpr.create_dummy ctx |> create_bytstr ctx

  let create_concatenated : ZCtx.t -> fst_bytes:t -> snd_bytes:t -> t
  =fun ctx ~fst_bytes ~snd_bytes -> sort ctx |> ZDatatype.create ~const_idx:1 ~expr_list:[fst_bytes; snd_bytes]

  (* let create_sliced : ZExpr.t -> t *) (* TODO : after _create_const_of_sliced finished *)

  let create_blake2b : ZCtx.t -> ZExpr.t -> t
  =fun ctx content -> sort ctx |> ZDatatype.create ~const_idx:2 ~expr_list:[content]
  let create_sha256 : ZCtx.t -> ZExpr.t -> t
  =fun ctx content -> sort ctx |> ZDatatype.create ~const_idx:3 ~expr_list:[content]
  let create_sha512 : ZCtx.t -> ZExpr.t -> t
  =fun ctx content -> sort ctx |> ZDatatype.create ~const_idx:4 ~expr_list:[content]

end


(*****************************************************************************)
(*****************************************************************************)
(* Signature                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

module ZSignature = struct
  type t = ZExpr.t

  let _create_const_of_sigstr : ZCtx.t -> ZDatatype.const
  =fun ctx -> ZDatatype.create_const ctx
      ~name:CONST._const_signature_sigstr
      ~recog_func_name:CONST._recog_signature_sigstr
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[(Some (ZStr.sort ctx))]
      ~field_sort_refs:[1]
  let _create_const_of_signed : ZCtx.t -> ZDatatype.const
  =fun ctx -> ZDatatype.create_const ctx
      ~name:CONST._const_signature_signed
      ~recog_func_name:CONST._recog_signature_signed
      ~field_names:[(CONST._field_pair_fst); (CONST._field_pair_snd)]
      ~field_sorts:[(Some (ZKey.sort ctx)); (Some (ZBytes.sort ctx))]
      ~field_sort_refs:[1; 2]

  let sort : ZCtx.t -> ZSort.t
  =fun ctx -> ZDatatype.create_sort ctx
    ~name:CONST._sort_signature
    ~const_list:[ (_create_const_of_sigstr ctx);
                  (_create_const_of_signed ctx);
                ]

  let of_string : ZCtx.t -> string -> t
  =fun ctx s -> sort ctx |> ZDatatype.create ~const_idx:0 ~expr_list:[ZStr.of_string ctx s]
  let create_sigstr : ZCtx.t -> ZExpr.t -> t
  =fun ctx content -> sort ctx |> ZDatatype.create ~const_idx:0 ~expr_list:[content]

  let create_signed : ZCtx.t -> key_data:ZExpr.t -> bytes_data:ZExpr.t -> t
  =fun ctx ~key_data ~bytes_data -> begin
    sort ctx |> ZDatatype.create ~const_idx:1 ~expr_list:[key_data; bytes_data]
  end
end


(*****************************************************************************)
(*****************************************************************************)
(* Timestamp = Int                                                           *)
(*****************************************************************************)
(*****************************************************************************)


(*****************************************************************************)
(*****************************************************************************)
(* Address                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

module ZAddress = struct
  type t = ZExpr.t

  let _create_const_of_addrkh : ZCtx.t -> ZDatatype.const
  =fun ctx -> ZDatatype.create_const ctx
      ~name:CONST._const_address_addrkh
      ~recog_func_name:CONST._recog_address_addrkh
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[(Some (ZKeyHash.sort ctx))]
      ~field_sort_refs:[1]

  let sort : ZCtx.t -> ZSort.t
  =fun ctx -> ZDatatype.create_sort ctx
    ~name:CONST._sort_address
    ~const_list:[ (_create_const_of_addrkh ctx);
                ]

  let of_string : ZCtx.t -> string -> t
  =fun ctx s -> sort ctx |> ZDatatype.create ~const_idx:0 ~expr_list:[ZKeyHash.of_string ctx s]

  let create_addrkh : ZCtx.t -> ZKeyHash.t -> t
  =fun ctx kh -> sort ctx |> ZDatatype.create ~const_idx:0 ~expr_list:[kh]


  let _read_innerkh : t -> ZKeyHash.t
  =fun t -> ZDatatype.read t ~const_idx:0 ~field_idx:0

  let create_cmp : ZCtx.t -> t -> t -> ZInt.t
  =fun ctx t1 t2 -> ZKeyHash.create_cmp ctx (_read_innerkh  t1) (_read_innerkh t2)

  let create_eq : ZCtx.t -> t -> t -> ZBool.t
  =fun ctx t1 t2 -> ZKeyHash.create_eq ctx (_read_innerkh t1) (_read_innerkh t2)
end


(*****************************************************************************)
(*****************************************************************************)
(* Ors                                                                       *)
(*****************************************************************************)
(*****************************************************************************)

module ZOr = struct
  type t = ZExpr.t

  let _create_const_of_left : ZCtx.t -> left_sort:ZSort.t -> ZDatatype.const
  =fun ctx ~left_sort -> begin
    ZDatatype.create_const ctx
      ~name:CONST._const_or_left
      ~recog_func_name:CONST._recog_or_left
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[(Some left_sort)]
      ~field_sort_refs:[1]
  end
  let _create_const_of_right : ZCtx.t -> right_sort:ZSort.t -> ZDatatype.const
  =fun ctx ~right_sort -> begin
    ZDatatype.create_const ctx
      ~name:CONST._const_or_right
      ~recog_func_name:CONST._recog_or_right
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[(Some right_sort)]
      ~field_sort_refs:[1]
  end
  let _create_sort_name : left_sort:ZSort.t -> right_sort:ZSort.t -> string
  =fun ~left_sort ~right_sort -> (CONST._sort_or) ^ "(" ^ (ZSort.to_string left_sort) ^ ", " ^ (ZSort.to_string right_sort) ^ ")"

  let create_sort : ZCtx.t -> left_sort:ZSort.t -> right_sort:ZSort.t -> ZSort.t
  =fun ctx ~left_sort ~right_sort -> begin
    ZDatatype.create_sort ctx
      ~name:(_create_sort_name ~left_sort:left_sort ~right_sort:right_sort)
      ~const_list:[(_create_const_of_left ctx ~left_sort:left_sort); (_create_const_of_right ctx ~right_sort:right_sort)]
  end

  let create_left : ZCtx.t -> left_content:ZExpr.t -> right_sort:ZSort.t -> t
  =fun ctx ~left_content ~right_sort -> begin
    (create_sort ctx
      ~left_sort:(left_content |> ZExpr.read_sort)
      ~right_sort:right_sort) |>
    ZDatatype.create ~const_idx:0 ~expr_list:[left_content]
  end
  let create_right : ZCtx.t -> left_sort:ZSort.t -> right_content:ZExpr.t -> t
  =fun ctx ~left_sort ~right_content -> begin
    (create_sort ctx
      ~left_sort:left_sort
      ~right_sort:(right_content |> ZExpr.read_sort)) |>
    ZDatatype.create ~const_idx:1 ~expr_list:[right_content]
  end
  let read_left : t -> ZExpr.t
  =ZDatatype.read ~const_idx:0 ~field_idx:0
  let read_right : t -> ZExpr.t
  =ZDatatype.read ~const_idx:1 ~field_idx:0

  let create_eq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_neq

  let is_left : t -> ZBool.t
  =ZDatatype.is_field ~const_idx:0
  let is_right : t -> ZBool.t
  =ZDatatype.is_field ~const_idx:1
end


(*****************************************************************************)
(*****************************************************************************)
(* Lists                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

module ZList = struct
  type t = ZExpr.t

  let _create_const_of_nil : ZCtx.t -> ZDatatype.const
  =fun ctx -> ZDatatype.create_const ctx
      ~name:CONST._const_list_nil
      ~recog_func_name:CONST._recog_list_nil
      ~field_names:[]
      ~field_sorts:[]
      ~field_sort_refs:[]
  let _create_const_of_cons : ZCtx.t -> content_sort:ZSort.t -> ZDatatype.const
  =fun ctx ~content_sort -> begin
    ZDatatype.create_const ctx
      ~name:CONST._const_list_cons
      ~recog_func_name:CONST._recog_list_cons
      ~field_names:[(CONST._field_list_head); (CONST._field_list_tail)]
      ~field_sorts:[(Some content_sort); None]
      ~field_sort_refs:[1; 0]
  end
  let _create_sort_name : content_sort:ZSort.t -> string
  =fun ~content_sort -> (CONST._sort_list) ^ "(" ^ (ZSort.to_string content_sort) ^ ")"

  let create_sort : ZCtx.t -> content_sort:ZSort.t -> ZSort.t
  =fun ctx ~content_sort -> begin
    ZDatatype.create_sort ctx
      ~name:(_create_sort_name ~content_sort:content_sort)
      ~const_list:[(_create_const_of_nil ctx); (_create_const_of_cons ctx ~content_sort:content_sort)]
  end

  let create : ZCtx.t -> content_sort:ZSort.t -> t
  =fun ctx ~content_sort -> create_sort ctx ~content_sort:content_sort |> ZDatatype.create ~const_idx:0 ~expr_list:[]
  let read_head : t -> ZExpr.t
  =ZDatatype.read ~const_idx:1 ~field_idx:0
  let read_tail : t -> t
  =ZDatatype.read ~const_idx:1 ~field_idx:1
  let update : ZCtx.t -> t -> content:ZExpr.t -> t
  =fun ctx e ~content -> create_sort ctx ~content_sort:(content |> ZExpr.read_sort) |> ZDatatype.create ~const_idx:1 ~expr_list:[content; e]

  let create_eq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_neq

  let is_nil : t -> ZBool.t
  =ZDatatype.is_field ~const_idx:0
  let is_cons : t -> ZBool.t
  =ZDatatype.is_field ~const_idx:1
end


(*****************************************************************************)
(*****************************************************************************)
(* Maps                                                                      *)
(*****************************************************************************)
(*****************************************************************************)

module ZMap = struct
  type t = ZExpr.t
  
  let _count_map : int ref
  =ref 0
  let _create_name : key_sort:ZSort.t -> value_sort:ZSort.t -> string
  =fun ~key_sort ~value_sort -> begin
    _count_map := !_count_map + 1;
    (CONST._name_map) ^ (!_count_map |> string_of_int) ^ "(" ^ (ZSort.to_string key_sort) ^ " |-> " ^ (ZSort.to_string value_sort) ^ ")"
  end

  let create_sort : ZCtx.t -> key_sort:ZSort.t -> value_sort:ZSort.t -> ZSort.t
  =fun ctx ~key_sort ~value_sort -> begin
    Z3.Z3Array.mk_sort ctx key_sort (ZOption.create_sort ctx ~content_sort:value_sort)
  end

  let read_default_value : ZCtx.t -> t -> ZExpr.t
  =fun ctx e -> begin
    let value_sort = Z3.Z3Array.get_range (e |> ZExpr.read_sort) in
    ZOption.create_none ctx ~content_sort:value_sort
  end

  let create : ZCtx.t -> key_sort:ZSort.t -> value_sort:ZSort.t -> t
  =fun ctx ~key_sort ~value_sort -> begin
    Z3.Z3Array.mk_const_array ctx key_sort (ZOption.create_none ctx ~content_sort:value_sort)
  end
  let read_value : ZCtx.t -> key:ZExpr.t -> map:t -> ZExpr.t
  =fun ctx ~key ~map -> begin
    Z3.Z3Array.mk_select ctx map key
  end
  let read_exist : ZCtx.t -> key:ZExpr.t -> map:t -> ZBool.t
  =fun ctx ~key ~map -> begin
    ZExpr.create_ite ctx
      ~cond:(read_value ctx ~key:key ~map:map |> ZOption.is_some)
      ~t:(ZBool.true_ ctx)
      ~f:(ZBool.false_ ctx)
  end
  let update : ZCtx.t -> key:ZExpr.t -> value:ZExpr.t -> map:t -> t
  =fun ctx ~key ~value ~map -> begin
    Z3.Z3Array.mk_store ctx map key value
  end

  let create_eq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_neq
end


(*****************************************************************************)
(*****************************************************************************)
(* Sets = Maps                                                               *)
(*****************************************************************************)
(*****************************************************************************)

(* key is set-element, value is always "true" *)
module ZSet = ZMap


(*****************************************************************************)
(*****************************************************************************)
(* Operations                                                                *)
(*****************************************************************************)
(*****************************************************************************)

module ZOperation = struct (* Not completely implemented. *)
  type t = ZExpr.t
  
  let sort : ZCtx.t -> ZSort.t
  =fun ctx -> ZSort.create ctx ~name:CONST._sort_operation

  let create_eq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_neq
end


(*****************************************************************************)
(*****************************************************************************)
(* Contracts                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

module ZContract = struct (* Not completely implemented. *)
  type t = ZExpr.t
  
  let sort : ZCtx.t -> ZSort.t
  =fun ctx -> ZSort.create ctx ~name:CONST._sort_contract

  let create_eq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_neq
end


(*****************************************************************************)
(*****************************************************************************)
(* Lambdas                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

module ZLambda = struct (* Not completely implemented. *)
  type t = ZExpr.t
  
  let sort : ZCtx.t -> ZSort.t
  =fun ctx -> ZSort.create ctx ~name:CONST._sort_lambda

  let create_eq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : ZCtx.t -> t -> t -> ZBool.t
  =ZBool.create_neq
end


(*****************************************************************************)
(*****************************************************************************)
(* Model                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

module ZModel = struct
  type t = Z3.Model.model

  let eval : ZExpr.t -> model:t -> ZExpr.t option
  =fun expr ~model -> Z3.Model.eval model expr true

  let to_string : t -> string
  =Z3.Model.to_string
end


(*****************************************************************************)
(*****************************************************************************)
(* Solver                                                                    *)
(*****************************************************************************)
(*****************************************************************************)

module ZSolver = struct
  type t = Z3.Solver.solver
  type validity = VAL | INVAL | UNKNOWN
  type satisfiability = SAT | UNSAT | UNKNOWN

  let create : ZCtx.t -> t
  =fun ctx -> Z3.Solver.mk_solver ctx None
  let _formula_add : t -> ZFormula.t list -> unit
  =Z3.Solver.add

  let check_satisfiability : t -> ZCtx.t -> ZFormula.t list -> (satisfiability * ZModel.t option)
  =fun solver ctx fl -> begin
    let _ = ignore (ctx) in
    match Z3.Solver.check solver fl with
    | UNKNOWN -> (UNKNOWN, None)
    | UNSATISFIABLE -> (UNSAT, None)
    | SATISFIABLE -> (SAT, (solver |> Z3.Solver.get_model))
  end
  let check_validity : t -> ZCtx.t -> ZFormula.t list -> (validity * ZModel.t option)
  =fun solver ctx fl -> begin
    let fmla = fl |> ZFormula.create_and ctx |> ZFormula.create_not ctx in
    match Z3.Solver.check solver [fmla] with
    | UNKNOWN -> (UNKNOWN, None)
    | UNSATISFIABLE -> (VAL, None)
    | SATISFIABLE -> (INVAL, (solver |> Z3.Solver.get_model))
  end

  let is_unknown_sat : satisfiability -> bool
  =fun s -> (s = UNKNOWN)
  let is_sat : satisfiability -> bool
  =fun s -> (s = SAT)
  let is_unsat : satisfiability -> bool
  =fun s -> (s = UNSAT)
  let is_unknown_val : validity -> bool
  =fun v -> (v = UNKNOWN)
  let is_valid : validity -> bool
  =fun v -> (v = VAL)
  let is_invalid : validity -> bool
  =fun v -> (v = INVAL)

  let to_string : t -> string
  =Z3.Solver.to_string
  let string_of_satisfiability : satisfiability -> string
  =fun s -> begin
    match s with
    | UNKNOWN -> "UNKNOWN"
    | SAT -> "SAT"
    | UNSAT -> "UNSAT"
  end
  let string_of_validity : validity -> string
  =fun s -> begin
    match s with
    | UNKNOWN -> "UNKNOWN"
    | VAL -> "VALID"
    | INVAL -> "INVALID"
  end
end