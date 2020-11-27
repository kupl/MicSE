exception ZError = Z3.Error

module CONST = struct
  let _name_dummy : string = "DUMMY"
  let _name_unit : string = "UNIT"
  let _name_map : string = "MAP"

  let _sort_key : string = "Key"
  let _sort_unit : string = "Unit"
  let _sort_operation : string = "Operation"
  let _sort_contract : string = "Contract"
  let _sort_lambda : string = "Lambda"
  let _sort_bytes : string = "Bytes"
  let _sort_option : string = "Option"
  let _sort_pair : string = "Pair"
  let _sort_or : string = "Or"
  let _sort_list : string = "List"

  let _const_key_keystr : string = "KeyStr"
  let _const_bytes_bytstr : string = "BytStr"
  let _const_bytes_pack : string = "Pack"
  let _const_bytes_concatenated : string = "BytConcat"
  let _const_bytes_sliced : string = "BytSlice"
  let _const_bytes_blake2b : string = "Blake2b"
  let _const_bytes_sha256 : string = "Sha256"
  let _const_bytes_sha512 : string = "Sha512"
  let _const_option_none : string = "None"
  let _const_option_some : string = "Some"
  let _const_pair : string = "Pair"
  let _const_or_left : string = "Left"
  let _const_or_right : string = "Right"
  let _const_list_nil : string = "Nil"
  let _const_list_cons : string = "Cons"

  let _recog_key_keystr : string = "is_keystr"
  let _recog_bytes_bytstr : string = "is_bytstr"
  let _recog_bytes_pack : string = "is_pack"
  let _recog_bytes_concatenated : string = "is_bytes_concatenated"
  let _recog_bytes_sliced : string = "is_bytes_sliced"
  let _recog_bytes_blake2b : string = "is_blake2b"
  let _recog_bytes_sha256 : string = "is_sha256"
  let _recog_bytes_sha512 : string = "is_sha512"
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
end

(*****************************************************************************)
(*****************************************************************************)
(* Context                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

module ZCtx = struct
  type body = (string * string)
  type t = Z3.context
  type t_ref = t option ref

  let _obj : t_ref
  =ref None

  let body_timeout : unit -> body
  =fun () -> begin
    let budget = !Utils.Options.z3_time_budget * 1000 in
    ("timeout", (string_of_int (budget)))
  end
  let create : unit -> unit
  =fun () -> begin
    let c = (body_timeout ())::
            [] in
    _obj := c |> Z3.mk_context |> Option.some
  end
  let read : unit -> t
  =fun () -> begin
    let _ = if Option.is_none !_obj then create () in
    !_obj |> Option.get
  end
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

  let create : string -> t
  =Z3.Symbol.mk_string (ZCtx.read ())
  let create_dummy : unit -> t
  =fun () -> begin
    _count_dummy := !_count_dummy + 1;
    (_name_dummy ^ (!_count_dummy |> string_of_int)) |> create
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

  let create_dummy : unit -> t
  =fun () -> () |> ZSym.create_dummy |> Z3.Sort.mk_uninterpreted (ZCtx.read ())
  let create : name:string -> t
  =fun ~name -> name |> ZSym.create |> Z3.Sort.mk_uninterpreted (ZCtx.read ())

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

  let create_dummy : ZSort.t -> t
  =fun sort -> sort |> (() |> ZSym.create_dummy |> Z3.Expr.mk_const (ZCtx.read ()))
  let create_var : ZSort.t -> name:string -> t
  =fun sort ~name -> sort |> (name |> ZSym.create |> Z3.Expr.mk_const (ZCtx.read ()))

  let create_ite : cond:t -> t:t -> f:t -> t
  =fun ~cond ~t ~f -> Z3.Boolean.mk_ite (ZCtx.read ()) cond t f

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

  let create_const : name:string -> recog_func_name:string -> field_names:string list -> field_sorts:ZSort.t option list -> field_sort_refs:int list -> const
  =fun ~name ~recog_func_name ~field_names ~field_sorts ~field_sort_refs -> begin
    field_sort_refs |> (
    field_sorts |> (
    (field_names |> (Core.List.map ~f:ZSym.create)) |> (
    (recog_func_name |> ZSym.create) |> (
    (name |> ZSym.create) |> 
    Z3.Datatype.mk_constructor (ZCtx.read ())))))
  end
  let create_sort : name:string -> const_list:const list -> ZSort.t
  =fun ~name ~const_list -> const_list |> (name |> ZSym.create |> Z3.Datatype.mk_sort (ZCtx.read ()))
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

  let sort : ZSort.t
  =Z3.Boolean.mk_sort (ZCtx.read ())
  
  let true_ : t
  =Z3.Boolean.mk_true (ZCtx.read ())
  let false_ : t
  =Z3.Boolean.mk_false (ZCtx.read ())
  let uninterpreted_ : t
  =Z3.Boolean.mk_const (ZCtx.read ()) (ZSym.create_dummy ())

  let create_not : t -> t
  =Z3.Boolean.mk_not (ZCtx.read ())
  let create_and : t list -> t
  =Z3.Boolean.mk_and (ZCtx.read ())
  let create_or : t list -> t
  =Z3.Boolean.mk_or (ZCtx.read ())
  let create_xor : t -> t -> t
  =Z3.Boolean.mk_xor (ZCtx.read ())
  let create_eq : t -> t -> t
  =Z3.Boolean.mk_eq (ZCtx.read ())
  let create_neq : t -> t -> t
  =fun e1 e2 -> create_eq e1 e2 |> create_not
  let create_imply : t -> t -> t
  =Z3.Boolean.mk_implies (ZCtx.read ())
  let create_iff : t -> t -> t
  =Z3.Boolean.mk_iff (ZCtx.read ())
end


(*****************************************************************************)
(*****************************************************************************)
(* Unit                                                                      *)
(*****************************************************************************)
(*****************************************************************************)

module ZUnit = struct
  type t = ZExpr.t

  let sort : ZSort.t
  =ZSort.create ~name:CONST._name_unit

  let create : t
  =sort |> ZExpr.create_var ~name:CONST._sort_unit
end


(*****************************************************************************)
(*****************************************************************************)
(* Booleans                                                                  *)
(*****************************************************************************)
(*****************************************************************************)

module ZBool = struct
  type t = ZExpr.t

  let sort : ZSort.t
  =Z3.Boolean.mk_sort (ZCtx.read ())

  let of_bool : bool -> t
  =Z3.Boolean.mk_val (ZCtx.read ())
  
  let true_ : t
  =Z3.Boolean.mk_true (ZCtx.read ())
  let false_ : t
  =Z3.Boolean.mk_false (ZCtx.read ())

  let create_not : t -> t
  =Z3.Boolean.mk_not (ZCtx.read ())
  let create_and : t -> t -> t
  =fun e1 e2 -> Z3.Boolean.mk_and (ZCtx.read ()) [e1; e2]
  let create_or : t -> t -> t
  =fun e1 e2 -> Z3.Boolean.mk_or (ZCtx.read ()) [e1; e2]
  let create_xor : t -> t -> t
  =Z3.Boolean.mk_xor (ZCtx.read ())

  let create_eq : t -> t -> t
  =Z3.Boolean.mk_eq (ZCtx.read ())
  let create_neq : t -> t -> t
  =fun e1 e2 -> create_eq e1 e2 |> create_not
end


(*****************************************************************************)
(*****************************************************************************)
(* Integers                                                                  *)
(*****************************************************************************)
(*****************************************************************************)

module ZInt = struct
  type t = ZExpr.t

  let sort : ZSort.t
  =Z3.Arithmetic.Integer.mk_sort (ZCtx.read ())

  let of_zarith : Z.t -> t
  =fun n -> n |> Z.to_string |> Z3.Arithmetic.Integer.mk_numeral_s (ZCtx.read ())
  let of_int : int -> t
  =Z3.Arithmetic.Integer.mk_numeral_i (ZCtx.read ())

  let minus_one_ : t
  =of_int (-1)
  let zero_ : t
  =of_int (0)
  let one_ : t
  =of_int (1)

  let create_neg : t -> t
  =Z3.Arithmetic.mk_unary_minus (ZCtx.read ())
  let create_add : t list -> t
  =Z3.Arithmetic.mk_add (ZCtx.read ())
  let create_sub : t list -> t
  =Z3.Arithmetic.mk_sub (ZCtx.read ())
  let create_mul : t list -> t
  =Z3.Arithmetic.mk_mul (ZCtx.read ())
  let create_div : t -> t -> t
  =Z3.Arithmetic.mk_div (ZCtx.read ())
  let create_mod : t -> t -> t
  =Z3.Arithmetic.Integer.mk_mod (ZCtx.read ())
  let create_power : t -> t -> t
  =Z3.Arithmetic.mk_power (ZCtx.read ())

  let create_eq : t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : t -> t -> ZBool.t
  =ZBool.create_neq
  let create_lt : t -> t -> ZBool.t
  =Z3.Arithmetic.mk_lt (ZCtx.read ())
  let create_le : t -> t -> ZBool.t
  =Z3.Arithmetic.mk_le (ZCtx.read ())
  let create_gt : t -> t -> ZBool.t
  =Z3.Arithmetic.mk_gt (ZCtx.read ())
  let create_ge : t -> t -> ZBool.t
  =Z3.Arithmetic.mk_ge (ZCtx.read ())

  let create_cmp : t -> t -> t
  =fun e1 e2 -> begin
    ZExpr.create_ite
      ~cond:(create_eq e1 e2)
      ~t:(zero_)
      ~f:(ZExpr.create_ite
            ~cond:(create_lt e1 e2)
            ~t:(minus_one_)
            ~f:(one_))
  end
  let create_abs : t -> t
  =fun e -> begin
    ZExpr.create_ite
      ~cond:(create_lt e zero_)
      ~t:(create_neg e)
      ~f:(e)
  end

  let to_zmutez : t -> ZExpr.t
  =Z3.Arithmetic.Integer.mk_int2bv (ZCtx.read ()) (CONST._bit_mutez)
end


(*****************************************************************************)
(*****************************************************************************)
(* Natural Number                                                            *)
(*****************************************************************************)
(*****************************************************************************)

module ZNat = struct
  (* TODO *)
end


(*****************************************************************************)
(*****************************************************************************)
(* Mutez                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

module ZMutez = struct
  type t = ZExpr.t

  let sort : ZSort.t
  =Z3.BitVector.mk_sort (ZCtx.read ()) (CONST._bit_mutez)

  let of_zarith : Z.t -> t
  =fun n -> Z3.BitVector.mk_numeral (ZCtx.read ()) (Z.to_string n) (CONST._bit_mutez)
  let of_int : int -> t
  =fun n -> Z3.BitVector.mk_numeral (ZCtx.read ()) (string_of_int n) (CONST._bit_mutez)

  let zero_ : t
  =of_int (0)

  let create_add : t -> t -> t
  =Z3.BitVector.mk_add (ZCtx.read ())
  let create_sub : t -> t -> t
  =Z3.BitVector.mk_sub (ZCtx.read ())
  let create_mul : t -> t -> t
  =Z3.BitVector.mk_mul (ZCtx.read ())
  let create_div : t -> t -> t
  =Z3.BitVector.mk_udiv (ZCtx.read ())
  let create_mod : t -> t -> t
  =Z3.BitVector.mk_urem (ZCtx.read ())

  let create_eq : t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : t -> t -> ZBool.t
  =ZBool.create_neq
  let create_lt : t -> t -> ZBool.t
  =Z3.BitVector.mk_ult (ZCtx.read ())
  let create_le : t -> t -> ZBool.t
  =Z3.BitVector.mk_ule (ZCtx.read ())
  let create_gt : t -> t -> ZBool.t
  =Z3.BitVector.mk_ugt (ZCtx.read ())
  let create_ge : t -> t -> ZBool.t
  =Z3.BitVector.mk_uge (ZCtx.read ())

  let create_cmp : t -> t -> ZInt.t
  =fun e1 e2 -> begin
    ZExpr.create_ite
      ~cond:(create_eq e1 e2)
      ~t:(ZInt.zero_)
      ~f:(ZExpr.create_ite
            ~cond:(create_lt e1 e2)
            ~t:(ZInt.minus_one_)
            ~f:(ZInt.one_))
  end

  let to_zint : t -> ZInt.t
  =fun e -> Z3.BitVector.mk_bv2int (ZCtx.read ()) e false

  let check_add_no_overflow : t -> t -> ZBool.t
  =fun e1 e2 -> Z3.BitVector.mk_add_no_overflow (ZCtx.read ()) e1 e2 false
  let check_mul_no_overflow : t -> t -> ZBool.t
  =fun e1 e2 -> Z3.BitVector.mk_mul_no_overflow (ZCtx.read ()) e1 e2 false
  let check_sub_no_underflow : t -> t -> ZBool.t
  =fun e1 e2 -> Z3.BitVector.mk_sub_no_underflow (ZCtx.read ()) e1 e2 false
end


(*****************************************************************************)
(*****************************************************************************)
(* Strings                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

module ZStr = struct
  type t = ZExpr.t

  let sort : ZSort.t
  =Z3.Seq.mk_string_sort (ZCtx.read ())

  let of_string : string -> t
  =Z3.Seq.mk_string (ZCtx.read ())

  let create_concat : t list -> t
  =Z3.Seq.mk_seq_concat (ZCtx.read ())
  let create_slice : t -> low:ZInt.t -> high:ZInt.t -> t
  =fun s ~low ~high -> Z3.Seq.mk_seq_extract (ZCtx.read ()) s low high
  let create_length : t -> ZInt.t
  =Z3.Seq.mk_seq_length (ZCtx.read ())

  let create_eq : t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : t -> t -> ZBool.t
  =ZBool.create_neq

  let create_cmp : t -> t -> ZInt.t
  =fun e1 e2 -> begin
    ZExpr.create_ite
      ~cond:(create_eq e1 e2)
      ~t:(ZInt.zero_)
      ~f:(ZExpr.create_ite
            ~cond:(ZInt.create_lt (create_length e1) (create_length e2)) (* Not completely implemented. *)
            ~t:(ZInt.minus_one_)
            ~f:(ZInt.one_))
  end
end


(*****************************************************************************)
(*****************************************************************************)
(* Key                                                                       *)
(*****************************************************************************)
(*****************************************************************************)

module ZKey = struct
  type t = ZExpr.t

  let _create_const_of_keystr : ZDatatype.const
  =ZDatatype.create_const
      ~name:CONST._const_key_keystr
      ~recog_func_name:CONST._recog_key_keystr
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[(Some ZStr.sort)]
      ~field_sort_refs:[1]

  let sort : ZSort.t
  =ZDatatype.create_sort
      ~name:CONST._sort_key
      ~const_list:[_create_const_of_keystr]

  let of_string : string -> t
  =fun s -> sort |> ZDatatype.create ~const_idx:0 ~expr_list:[ZStr.of_string s]
  let create_keystr : ZExpr.t -> t
  =fun content -> sort |> ZDatatype.create ~const_idx:0 ~expr_list:[content]
end


(*****************************************************************************)
(*****************************************************************************)
(* Options                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

module ZOption = struct
  type t = ZExpr.t

  let _create_const_of_none : ZDatatype.const
  =ZDatatype.create_const
      ~name:CONST._const_option_none
      ~recog_func_name:CONST._recog_option_none
      ~field_names:[]
      ~field_sorts:[]
      ~field_sort_refs:[]
  let _create_const_of_some : content_sort:ZSort.t -> ZDatatype.const
  =fun ~content_sort -> begin
    ZDatatype.create_const
      ~name:CONST._const_option_some
      ~recog_func_name:CONST._recog_option_some
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[(Some content_sort)]
      ~field_sort_refs:[1]
  end
  let _create_sort_name : content_sort:ZSort.t -> string
  =fun ~content_sort -> (CONST._sort_option) ^ "(" ^ (ZSort.to_string content_sort) ^ ")"

  let create_sort : content_sort:ZSort.t -> ZSort.t
  =fun ~content_sort -> begin
    ZDatatype.create_sort
      ~name:(_create_sort_name ~content_sort:content_sort)
      ~const_list:[_create_const_of_none; (_create_const_of_some ~content_sort:content_sort)]
  end

  let create_none : content_sort:ZSort.t -> t
  =fun ~content_sort -> create_sort ~content_sort:content_sort |> ZDatatype.create ~const_idx:0 ~expr_list:[]
  let create_some : content:ZExpr.t -> t
  =fun ~content -> create_sort ~content_sort:(content |> ZExpr.read_sort) |> ZDatatype.create ~const_idx:1 ~expr_list:[content]
  let read : t -> ZExpr.t
  =ZDatatype.read ~const_idx:1 ~field_idx:0

  let create_eq : t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : t -> t -> ZBool.t
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

  let _create_const_of_pair : fst_sort:ZSort.t -> snd_sort:ZSort.t -> ZDatatype.const
  =fun ~fst_sort ~snd_sort -> begin
    ZDatatype.create_const
      ~name:CONST._const_pair
      ~recog_func_name:CONST._recog_pair
      ~field_names:[(CONST._field_pair_fst); (CONST._field_pair_snd)]
      ~field_sorts:[(Some fst_sort); (Some snd_sort)]
      ~field_sort_refs:[1; 2]
  end
  let _create_sort_name : fst_sort:ZSort.t -> snd_sort:ZSort.t -> string
  =fun ~fst_sort ~snd_sort -> (CONST._sort_pair) ^ "(" ^ (ZSort.to_string fst_sort) ^ ", " ^ (ZSort.to_string snd_sort) ^ ")"

  let create_sort : fst_sort:ZSort.t -> snd_sort:ZSort.t -> ZSort.t
  =fun ~fst_sort ~snd_sort -> begin
    ZDatatype.create_sort
      ~name:(_create_sort_name ~fst_sort:fst_sort ~snd_sort:snd_sort)
      ~const_list:[(_create_const_of_pair ~fst_sort:fst_sort ~snd_sort:snd_sort)]
  end

  let create : fst:ZExpr.t -> snd:ZExpr.t -> t
  =fun ~fst ~snd -> begin
    (create_sort
      ~fst_sort:(fst |> ZExpr.read_sort)
      ~snd_sort:(snd |> ZExpr.read_sort)) |>
    ZDatatype.create ~const_idx:0 ~expr_list:[fst; snd]
  end
  let read_fst : t -> ZExpr.t
  =ZDatatype.read ~const_idx:0 ~field_idx:0
  let read_snd : t -> ZExpr.t
  =ZDatatype.read ~const_idx:0 ~field_idx:1

  let create_eq : t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : t -> t -> ZBool.t
  =ZBool.create_neq
end


(*****************************************************************************)
(*****************************************************************************)
(* Bytes                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

module ZBytes = struct
  type t = ZExpr.t

  let _create_const_of_bytstr : ZDatatype.const
  =ZDatatype.create_const
      ~name:CONST._const_bytes_bytstr
      ~recog_func_name:CONST._recog_bytes_bytstr
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[(Some ZStr.sort)]
      ~field_sort_refs:[1]
  let _create_const_of_pack : content_sort:ZSort.t -> ZDatatype.const
  =fun ~content_sort -> begin
    ZDatatype.create_const
      ~name:CONST._const_bytes_pack
      ~recog_func_name:CONST._recog_bytes_pack
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[(Some content_sort)]
      ~field_sort_refs:[1]
  end
  let _create_const_of_concatenated : bytes_pair_sort:ZSort.t -> ZDatatype.const
  =fun ~bytes_pair_sort -> begin
    ZDatatype.create_const
      ~name:CONST._const_bytes_concatenated
      ~recog_func_name:CONST._recog_bytes_concatenated
      ~field_names:[CONST._field_content]
      ~field_sorts:[Some bytes_pair_sort]
      ~field_sort_refs:[1]
  end
  (* let _create_const_of_sliced : content_sort:ZSort.t -> ZDatatype.const *) (* TODO : after ZNat completed *)
  let _create_const_of_blake2b : ZDatatype.const
  =ZDatatype.create_const
      ~name:CONST._const_bytes_blake2b
      ~recog_func_name:CONST._recog_bytes_blake2b
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[None]
      ~field_sort_refs:[0]
  let _create_const_of_sha256 : ZDatatype.const
  =ZDatatype.create_const
      ~name:CONST._const_bytes_sha256
      ~recog_func_name:CONST._recog_bytes_sha256
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[None]
      ~field_sort_refs:[0]
  let _create_const_of_sha512 : ZDatatype.const
  =ZDatatype.create_const
      ~name:CONST._const_bytes_sha512
      ~recog_func_name:CONST._recog_bytes_sha512
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[None]
      ~field_sort_refs:[0]

  (* let _create_sort_name : content_sort:ZSort.t -> string
  =fun ~content_sort -> (CONST._sort_bytes) ^ "(" ^ (ZSort.to_string content_sort) ^ ")" *)

  let create_sort : content_sort:ZSort.t -> ZSort.t
  =fun ~content_sort -> begin
    ZDatatype.create_sort
    ~name:CONST._sort_bytes
    ~const_list:[ _create_const_of_bytstr;
                  _create_const_of_pack ~content_sort:content_sort;
                  _create_const_of_concatenated ~bytes_pair_sort:content_sort;
                  _create_const_of_blake2b;
                  _create_const_of_sha256;
                  _create_const_of_sha512;
                ]
  end

  let of_string : string -> t
  =fun s -> create_sort ~content_sort:ZStr.sort |> ZDatatype.create ~const_idx:0 ~expr_list:[ZStr.of_string s]
  let create_bytstr : ZExpr.t -> t
  =fun content -> create_sort ~content_sort:ZStr.sort |> ZDatatype.create ~const_idx:0 ~expr_list:[content]

  let create_pack : ZExpr.t -> t
  =fun content -> create_sort ~content_sort:(ZExpr.read_sort content) |> ZDatatype.create ~const_idx:1 ~expr_list:[content]

  (* it does not check that the ~fst_bytes and ~snd_bytes has real bytes type expression *)
  let create_concatenated : fst_bytes:ZExpr.t -> snd_bytes:ZExpr.t -> t
  =fun ~fst_bytes ~snd_bytes -> begin
    let bpair = ZPair.create ~fst:fst_bytes ~snd:snd_bytes in
    create_sort ~content_sort:(ZExpr.read_sort bpair) |> ZDatatype.create ~const_idx:2 ~expr_list:[bpair]
  end

  (* let create_sliced : ZExpr.t -> t *) (* TODO : after _create_const_of_sliced finished *)

  let create_blake2b : ZExpr.t -> t
  =fun content -> create_sort ~content_sort:(ZExpr.read_sort content) |> ZDatatype.create ~const_idx:3 ~expr_list:[content]
  let create_sha256 : ZExpr.t -> t
  =fun content -> create_sort ~content_sort:(ZExpr.read_sort content) |> ZDatatype.create ~const_idx:4 ~expr_list:[content]
  let create_sha512 : ZExpr.t -> t
  =fun content -> create_sort ~content_sort:(ZExpr.read_sort content) |> ZDatatype.create ~const_idx:5 ~expr_list:[content]

end


(*****************************************************************************)
(*****************************************************************************)
(* Ors                                                                       *)
(*****************************************************************************)
(*****************************************************************************)

module ZOr = struct
  type t = ZExpr.t

  let _create_const_of_left : left_sort:ZSort.t -> ZDatatype.const
  =fun ~left_sort -> begin
    ZDatatype.create_const
      ~name:CONST._const_or_left
      ~recog_func_name:CONST._recog_or_left
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[(Some left_sort)]
      ~field_sort_refs:[1]
  end
  let _create_const_of_right : right_sort:ZSort.t -> ZDatatype.const
  =fun ~right_sort -> begin
    ZDatatype.create_const
      ~name:CONST._const_or_right
      ~recog_func_name:CONST._recog_or_right
      ~field_names:[(CONST._field_content)]
      ~field_sorts:[(Some right_sort)]
      ~field_sort_refs:[1]
  end
  let _create_sort_name : left_sort:ZSort.t -> right_sort:ZSort.t -> string
  =fun ~left_sort ~right_sort -> (CONST._sort_or) ^ "(" ^ (ZSort.to_string left_sort) ^ ", " ^ (ZSort.to_string right_sort) ^ ")"

  let create_sort : left_sort:ZSort.t -> right_sort:ZSort.t -> ZSort.t
  =fun ~left_sort ~right_sort -> begin
    ZDatatype.create_sort
      ~name:(_create_sort_name ~left_sort:left_sort ~right_sort:right_sort)
      ~const_list:[(_create_const_of_left ~left_sort:left_sort); (_create_const_of_right ~right_sort:right_sort)]
  end

  let create_left : left_content:ZExpr.t -> right_sort:ZSort.t -> t
  =fun ~left_content ~right_sort -> begin
    (create_sort
      ~left_sort:(left_content |> ZExpr.read_sort)
      ~right_sort:right_sort) |>
    ZDatatype.create ~const_idx:0 ~expr_list:[left_content]
  end
  let create_right : left_sort:ZSort.t -> right_content:ZExpr.t -> t
  =fun ~left_sort ~right_content -> begin
    (create_sort
      ~left_sort:left_sort
      ~right_sort:(right_content |> ZExpr.read_sort)) |>
    ZDatatype.create ~const_idx:1 ~expr_list:[right_content]
  end
  let read_left : t -> ZExpr.t
  =ZDatatype.read ~const_idx:0 ~field_idx:0
  let read_right : t -> ZExpr.t
  =ZDatatype.read ~const_idx:1 ~field_idx:0

  let create_eq : t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : t -> t -> ZBool.t
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

  let _create_const_of_nil : ZDatatype.const
  =ZDatatype.create_const
      ~name:CONST._const_list_nil
      ~recog_func_name:CONST._recog_list_nil
      ~field_names:[]
      ~field_sorts:[]
      ~field_sort_refs:[]
  let _create_const_of_cons : content_sort:ZSort.t -> ZDatatype.const
  =fun ~content_sort -> begin
    ZDatatype.create_const
      ~name:CONST._const_list_cons
      ~recog_func_name:CONST._recog_list_cons
      ~field_names:[(CONST._field_list_head); (CONST._field_list_tail)]
      ~field_sorts:[(Some content_sort); None]
      ~field_sort_refs:[1; 0]
  end
  let _create_sort_name : content_sort:ZSort.t -> string
  =fun ~content_sort -> (CONST._sort_list) ^ "(" ^ (ZSort.to_string content_sort) ^ ")"

  let create_sort : content_sort:ZSort.t -> ZSort.t
  =fun ~content_sort -> begin
    ZDatatype.create_sort
      ~name:(_create_sort_name ~content_sort:content_sort)
      ~const_list:[_create_const_of_nil; (_create_const_of_cons ~content_sort:content_sort)]
  end

  let create : content_sort:ZSort.t -> t
  =fun ~content_sort -> create_sort ~content_sort:content_sort |> ZDatatype.create ~const_idx:0 ~expr_list:[]
  let read_head : t -> ZExpr.t
  =ZDatatype.read ~const_idx:1 ~field_idx:0
  let read_tail : t -> t
  =ZDatatype.read ~const_idx:1 ~field_idx:1
  let update : t -> content:ZExpr.t -> t
  =fun e ~content -> create_sort ~content_sort:(content |> ZExpr.read_sort) |> ZDatatype.create ~const_idx:1 ~expr_list:[content; e]

  let create_eq : t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : t -> t -> ZBool.t
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

  let create_sort : key_sort:ZSort.t -> value_sort:ZSort.t -> ZSort.t
  =fun ~key_sort ~value_sort -> value_sort |> (key_sort |> Z3.Z3Array.mk_sort (ZCtx.read ()))

  let read_default_value : t -> ZExpr.t
  =Z3.Z3Array.mk_term_array (ZCtx.read ())

  let create : key_sort:ZSort.t -> value_sort:ZSort.t -> t
  =fun ~key_sort ~value_sort -> begin
    value_sort |> (
    key_sort |> (
    (_create_name ~key_sort:key_sort ~value_sort:value_sort) |> ZSym.create |>
    Z3.Z3Array.mk_const (ZCtx.read ())))
  end
  let read_value : key:ZExpr.t -> map:t -> ZExpr.t
  =fun ~key ~map -> begin
    let value = Z3.Z3Array.mk_select (ZCtx.read ()) map key in
    ZExpr.create_ite
      ~cond:(ZBool.create_eq value (map |> read_default_value))
      ~t:(ZOption.create_none ~content_sort:(map |> ZExpr.read_sort |> Z3.Z3Array.get_range))
      ~f:(ZOption.create_some ~content:value)
  end
  let read_exist : key:ZExpr.t -> map:t -> ZBool.t
  =fun ~key ~map -> begin
    ZExpr.create_ite
      ~cond:(read_value ~key:key ~map:map |> ZOption.is_none)
      ~t:(ZBool.true_)
      ~f:(ZBool.false_)
  end
  let update : key:ZExpr.t -> value:ZExpr.t -> map:t -> t
  =fun ~key ~value ~map -> begin
    ZExpr.create_ite
      ~cond:(value |> ZOption.is_none)
      ~t:(map |> read_default_value |> (
          key |> (
          map |>
          Z3.Z3Array.mk_store (ZCtx.read ()))))
      ~f:(value |> ZOption.read |> (
          key |> (
          map |>
          Z3.Z3Array.mk_store (ZCtx.read ()))))
  end

  let create_eq : t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : t -> t -> ZBool.t
  =ZBool.create_neq
end


(*****************************************************************************)
(*****************************************************************************)
(* Sets                                                                *)
(*****************************************************************************)
(*****************************************************************************)


(*****************************************************************************)
(*****************************************************************************)
(* Operations                                                                *)
(*****************************************************************************)
(*****************************************************************************)

module ZOperation = struct (* Not completely implemented. *)
  type t = ZExpr.t
  
  let sort : ZSort.t
  =ZSort.create ~name:CONST._sort_operation

  let create_eq : t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : t -> t -> ZBool.t
  =ZBool.create_neq
end


(*****************************************************************************)
(*****************************************************************************)
(* Contracts                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

module ZContract = struct (* Not completely implemented. *)
  type t = ZExpr.t
  
  let sort : ZSort.t
  =ZSort.create ~name:CONST._sort_contract

  let create_eq : t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : t -> t -> ZBool.t
  =ZBool.create_neq
end


(*****************************************************************************)
(*****************************************************************************)
(* Lambdas                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

module ZLambda = struct (* Not completely implemented. *)
  type t = ZExpr.t
  
  let sort : ZSort.t
  =ZSort.create ~name:CONST._sort_lambda

  let create_eq : t -> t -> ZBool.t
  =ZBool.create_eq
  let create_neq : t -> t -> ZBool.t
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

  let _create : unit -> t
  =fun () -> Z3.Solver.mk_solver (ZCtx.read ()) None
  let _formula_add : t -> ZFormula.t list -> unit
  =Z3.Solver.add

  let check_satisfiability : ZFormula.t list -> (satisfiability * ZModel.t option)
  =fun fl -> begin
    let solver = _create () in
    let _ = _formula_add solver fl in
    match Z3.Solver.check solver [] with
    | UNKNOWN -> (UNKNOWN, None)
    | UNSATISFIABLE -> (UNSAT, None)
    | SATISFIABLE -> (SAT, (solver |> Z3.Solver.get_model))
  end
  let check_validity : ZFormula.t list -> (validity * ZModel.t option)
  =fun fl -> begin
    let solver = _create () in
    let _ = _formula_add solver [(fl |> ZFormula.create_and |> ZFormula.create_not)] in
    match Z3.Solver.check solver [] with
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