exception ZError = Z3.Error

module CONST = struct
  let _name_dummy : string = "DUMMY"
  let _name_unit : string = "UNIT"
  let _name_map : string = "MAP"
  
  let _tmpname_source : string = "SOURCE"
  let _tmpname_sender : string = "SENDER"

  let _sort_unit : string = "Unit"
  let _sort_bool : string = "Bool"
  let _sort_int : string = "Int"
  let _sort_str : string = "String"
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
  let _sort_map : string = "Map"

  let _const_key_keystr : string = "KeyStr"
  let _const_keyhash_str : string = "KeyHashStr"
  let _const_keyhash_hashkey : string = "KeyHashKey"
  let _const_bytes_nil : string = "BytNil"
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
  let _recog_bytes_bytnil : string = "is_bytnil"
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
  = CONST._name_dummy
  let _count_dummy : int ref
  = ref 0

  let create : ZCtx.t -> string -> t
  = fun ctx name -> Z3.Symbol.mk_string ctx name

  let create_dummy : ZCtx.t -> t
  = fun ctx -> begin
    let _ = Stdlib.incr _count_dummy in
    let (name) : string = (_name_dummy ^ (!_count_dummy |> string_of_int)) in
    create ctx name
  end (* function create_dummy end *)

  let to_string : t -> string
  = Z3.Symbol.to_string
end

(******************************************************************************)
(******************************************************************************)
(* Sorts                                                                      *)
(******************************************************************************)
(******************************************************************************)

module ZSort = struct
  type t = Z3.Sort.sort

  let create_dummy : ZCtx.t -> t
  = fun ctx -> Z3.Sort.mk_uninterpreted ctx (ZSym.create_dummy ctx)

  let create : ZCtx.t -> name:string -> t
  = fun ctx ~name -> Z3.Sort.mk_uninterpreted ctx (ZSym.create ctx name)

  let to_string : t -> string
  = Z3.Sort.to_string

  (****************************************************************************)
  (* Sort of Types                                                            *)
  (****************************************************************************)

  module PMap = Core.Map.Poly

  let sort_map : (ZCtx.t, (string, t) PMap.t) PMap.t Stdlib.ref
  = Stdlib.ref PMap.empty

  let _read_x_sort : ZCtx.t -> string -> cst:(ZCtx.t -> t) -> t
  = fun ctx name ~cst -> begin
    match PMap.find !sort_map ctx with
    | None -> (
      let (new_sort) : t = cst ctx in
      let _ = sort_map := (PMap.add_exn !sort_map ~key:ctx ~data:(PMap.singleton name new_sort)) in
      new_sort)
    | Some smap -> (
      match PMap.find smap name with
      | None      -> (
        let (new_sort) : t = cst ctx in
        let _ = sort_map := (PMap.set !sort_map ~key:ctx ~data:(PMap.add_exn smap ~key:name ~data:new_sort)) in
        new_sort)
      | Some sss  -> sss)
  end (* function read_x_sort end *)

  let read_unit_sort : ZCtx.t -> t
  = fun ctx -> _read_x_sort ~cst:(create ~name:CONST._sort_unit) ctx CONST._sort_unit

  let read_bool_sort : ZCtx.t -> t
  = fun ctx -> _read_x_sort ~cst:Z3.Boolean.mk_sort ctx CONST._sort_bool

  let read_int_sort : ZCtx.t -> t
  = fun ctx -> _read_x_sort ~cst:Z3.Arithmetic.Integer.mk_sort ctx CONST._sort_int

  let read_str_sort : ZCtx.t -> t
  = fun ctx -> _read_x_sort ~cst:Z3.Seq.mk_string_sort ctx CONST._sort_str
end


(******************************************************************************)
(******************************************************************************)
(* Expressions                                                                *)
(******************************************************************************)
(******************************************************************************)

module ZExpr = struct
  type t = Z3.Expr.expr

  let create_dummy : ZCtx.t -> ZSort.t -> t
  = fun ctx sort -> Z3.Expr.mk_const ctx (ZSym.create_dummy ctx) sort

  let create_var : ZCtx.t -> ZSort.t -> name:string -> t
  = fun ctx sort ~name -> Z3.Expr.mk_const ctx (ZSym.create ctx name) sort

  let create_ite : ZCtx.t -> cond:t -> t:t -> f:t -> t
  = fun ctx ~cond ~t ~f -> Z3.Boolean.mk_ite ctx cond t f

  let read_sort : t -> ZSort.t
  = Z3.Expr.get_sort

  let to_string : t -> string
  = Z3.Expr.to_string

  (****************************************************************************)
  (* Expression of Literals                                                   *)
  (****************************************************************************)

  module PMap = Core.Map.Poly

  type 'a _lit_map = (ZCtx.t, ('a, t) PMap.t) PMap.t Stdlib.ref

  let _read_x_lit : 'a _lit_map -> ZCtx.t -> 'a -> cst:(ZCtx.t -> t) -> t
  = fun cmap ctx idx ~cst -> begin
    match PMap.find !cmap ctx with
    | None -> (
      let (new_lit) : t = cst ctx in
      let _ = cmap := (PMap.add_exn !cmap ~key:ctx ~data:(PMap.singleton idx new_lit)) in
      new_lit)
    | Some lmap -> (
      match PMap.find lmap idx with
      | None      -> (
        let (new_lit) : t = cst ctx in
        let _ = cmap := (PMap.set !cmap ~key:ctx ~data:(PMap.add_exn lmap ~key:idx ~data:new_lit)) in
        new_lit)
      | Some lll  -> lll)
  end (* function _read_x_lit end *)

  
  (* Unit Literals ************************************************************)
  let _unit_lit_map : unit _lit_map
  = Stdlib.ref PMap.empty
  
  let read_unit : ZCtx.t -> t
  = fun ctx -> _read_x_lit _unit_lit_map ~cst:(fun c -> create_var c (ZSort.read_unit_sort c) ~name:CONST._name_unit) ctx ()

  (* Boolean Literals *********************************************************)
  let _bool_lit_map : bool _lit_map
  = Stdlib.ref PMap.empty

  let read_bool : ZCtx.t -> bool -> t
  = fun ctx e -> _read_x_lit _bool_lit_map ~cst:(fun c -> Z3.Boolean.mk_val c e) ctx e

  (* Integer Literals *********************************************************)
  let _int_lit_map : Z.t _lit_map
  = Stdlib.ref PMap.empty

  let read_int : ZCtx.t -> int -> t
  = fun ctx e -> _read_x_lit _int_lit_map ~cst:(fun c -> Z3.Arithmetic.Integer.mk_numeral_i c e) ctx (Z.of_int e)

  let read_zint : ZCtx.t -> Z.t -> t
  = fun ctx e -> _read_x_lit _int_lit_map ~cst:(fun c -> Z3.Arithmetic.Integer.mk_numeral_s c (Z.to_string e)) ctx e

  (* String Literals **********************************************************)
  let _str_lit_map : string _lit_map
  = Stdlib.ref PMap.empty

  let read_str : ZCtx.t -> string -> t
  = fun ctx e -> _read_x_lit _str_lit_map ~cst:(fun c -> Z3.Seq.mk_string c e) ctx e
end


(******************************************************************************)
(******************************************************************************)
(* FuncDecls                                                                  *)
(******************************************************************************)
(******************************************************************************)

module ZFunc = struct
  type t = Z3.FuncDecl.func_decl

  let get_idx : 'a list -> idx:int -> 'a
  = fun l ~idx -> begin
    try Core.List.nth_exn l idx
    with
    | _ -> Stdlib.raise (ZError (Printf.sprintf "get_idx %d called on list of length %d" idx (Core.List.length l)))
  end (* function get_idx end *)

  let apply : t -> params:ZExpr.t list -> ZExpr.t
  = fun f ~params -> Z3.FuncDecl.apply f params

  let sort_of_domain : t -> idx:int -> ZSort.t
  = fun f ~idx -> get_idx (Z3.FuncDecl.get_domain f) ~idx:idx
end


(******************************************************************************)
(******************************************************************************)
(* Datatypes                                                                  *)
(******************************************************************************)
(******************************************************************************)

module ZDatatype = struct
  type const = Z3.Datatype.Constructor.constructor

  let get_idx : 'a list -> idx:int -> 'a
  = fun l ~idx -> begin
    try Core.List.nth_exn l idx
    with
    | _ -> Stdlib.raise (ZError (Printf.sprintf "get_idx %d called on list of length %d" idx (Core.List.length l)))
  end (* function get_idx end *)

  let create_const : ZCtx.t -> name:string -> recog_func_name:string -> field_names:string list -> field_sorts:Z3.Sort.sort option list -> field_sort_refs:int list -> const
  = fun ctx ~name ~recog_func_name ~field_names ~field_sorts ~field_sort_refs -> begin
    Z3.Datatype.mk_constructor
      ctx
      (ZSym.create ctx name)
      (ZSym.create ctx recog_func_name)
      (Core.List.map field_names ~f:(ZSym.create ctx))
      field_sorts
      field_sort_refs
  end (* function create_const *)

  let create_sort : ZCtx.t -> name:string -> const_list:const list -> ZSort.t
  = fun ctx ~name ~const_list -> Z3.Datatype.mk_sort ctx (ZSym.create ctx name) const_list

  let create_const_func : ZSort.t -> const_idx:int -> ZFunc.t
  = fun sort ~const_idx -> get_idx (Z3.Datatype.get_constructors sort) ~idx:const_idx

  let create_recog_func : ZSort.t -> const_idx:int -> ZFunc.t
  = fun sort ~const_idx -> get_idx (Z3.Datatype.get_recognizers sort) ~idx:const_idx

  let create_access_func : ZSort.t -> const_idx:int -> field_idx:int -> ZFunc.t
  = fun sort ~const_idx ~field_idx -> get_idx (Z3.Datatype.get_accessors sort) ~idx:const_idx |> get_idx ~idx:field_idx

  let read_field_sort : ZSort.t -> const_idx:int -> field_idx:int -> ZSort.t
  = fun sort ~const_idx ~field_idx -> create_const_func sort ~const_idx |> ZFunc.sort_of_domain ~idx:field_idx


  let create : ZSort.t -> const_idx:int -> expr_list:ZExpr.t list -> ZExpr.t
  = fun sort ~const_idx ~expr_list -> create_const_func sort ~const_idx |> ZFunc.apply ~params:expr_list

  let read : ZExpr.t -> const_idx:int -> field_idx:int -> ZExpr.t
  = fun e ~const_idx ~field_idx -> create_access_func (ZExpr.read_sort e) ~const_idx ~field_idx |> ZFunc.apply ~params:[e]


  let is_field : ZExpr.t -> const_idx:int -> ZExpr.t
  = fun e ~const_idx -> create_recog_func (ZExpr.read_sort e) ~const_idx |> ZFunc.apply ~params:[e]

  (****************************************************************************)
  (* Pre-defined Datatypes and Its Sort                                       *)
  (****************************************************************************)

  module PMap = Core.Map.Poly

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

  type _typ_map = (ZCtx.t, (typ, const) PMap.t) PMap.t Stdlib.ref

  let _const_map : _typ_map
  = Stdlib.ref PMap.empty

  let _read_x_const : _typ_map -> ZCtx.t -> typ -> cst:(ZCtx.t -> const) -> const
  = fun cmap ctx typ ~cst -> begin
    match PMap.find !cmap ctx with
    | None -> (
      let (new_const) : const = cst ctx in
      let _ = cmap := (PMap.add_exn !cmap ~key:ctx ~data:(PMap.singleton typ new_const)) in
      new_const)
    | Some lmap -> (
      match PMap.find lmap typ with
      | None      -> (
        let (new_const) : const = cst ctx in
        let _ = cmap := (PMap.set !cmap ~key:ctx ~data:(PMap.add_exn lmap ~key:typ ~data:new_const)) in
        new_const)
      | Some lll  -> lll)
  end (* function _read_x_const end *)

  (* Key Type *****************************************************************)
  let read_key_const : ZCtx.t -> const
  = fun ctx -> begin
    _read_x_const
      _const_map
      ~cst:(fun c -> (
        create_const c
          ~name:CONST._const_key_keystr
          ~recog_func_name:CONST._recog_key_keystr
          ~field_names:[ (CONST._field_content); ]
          ~field_sorts:[ (Some (ZSort.read_str_sort c)); ]
          ~field_sort_refs:[ 1; ]))
      ctx
      Key
  end (* function read_key_const end *)

  let read_key_sort : ZCtx.t -> ZSort.t
  = fun ctx -> begin
    ZSort._read_x_sort
      ~cst:(fun c ->
        create_sort c
          ~name:CONST._sort_key
          ~const_list:[ (read_key_const c); ])
      ctx
      CONST._sort_key
  end (* function read_key_sort end *)

  (* Key Hash Type ************************************************************)
  let read_keyhash_const_of_str : ZCtx.t -> const
  = fun ctx -> begin
    _read_x_const
      _const_map
      ~cst:(fun c -> (
        create_const c
          ~name:CONST._const_keyhash_str
          ~recog_func_name:CONST._recog_keyhash_str
          ~field_names:[ (CONST._field_content); ]
          ~field_sorts:[ (Some (ZSort.read_str_sort c)); ]
          ~field_sort_refs:[ 1; ]))
      ctx
      KeyHash_str
  end (* function read_keyhash_const_of_str end *)

  let read_keyhash_const_of_key : ZCtx.t -> const
  = fun ctx -> begin
    _read_x_const
      _const_map
      ~cst:(fun c -> (
        create_const c
          ~name:CONST._const_keyhash_hashkey
          ~recog_func_name:CONST._recog_keyhash_hashkey
          ~field_names:[ (CONST._field_content); ]
          ~field_sorts:[ (Some (read_key_sort c)); ]
          ~field_sort_refs:[ 1; ]))
      ctx
      KeyHash_key
  end (* function read_keyhash_const_of_key end *)

  let read_keyhash_sort : ZCtx.t -> ZSort.t
  = fun ctx -> begin
    ZSort._read_x_sort
      ~cst:(fun c ->
        create_sort c
          ~name:CONST._sort_keyhash
          ~const_list:[ (read_keyhash_const_of_str c);
                        (read_keyhash_const_of_key c); ])
      ctx
      CONST._sort_keyhash
  end (* function read_keyhash_sort end *)

  (* Option Type **************************************************************)
  let read_option_const_of_none : ZCtx.t -> const
  = fun ctx -> begin
    _read_x_const
      _const_map
      ~cst:(fun c -> (
        create_const c
          ~name:CONST._const_option_none
          ~recog_func_name:CONST._recog_option_none
          ~field_names:[]
          ~field_sorts:[]
          ~field_sort_refs:[]))
      ctx
      Option_none
  end (* function read_option_const_of_none end *)

  let read_option_const_of_some : ZCtx.t -> content_sort:ZSort.t -> const
  = fun ctx ~content_sort -> begin
    _read_x_const
      _const_map
      ~cst:(fun c -> (
        create_const c
          ~name:CONST._const_option_some
          ~recog_func_name:CONST._recog_option_some
          ~field_names:[ (CONST._field_content); ]
          ~field_sorts:[ (Some content_sort); ]
          ~field_sort_refs:[ 1; ]))
      ctx
      (Option_some content_sort)
  end (* function read_option_const_of_some end *)

  let _create_option_sort_name : content_sort:ZSort.t -> string
  = fun ~content_sort -> Printf.sprintf "%s_(%s)" CONST._sort_option (ZSort.to_string content_sort)

  let read_option_sort : ZCtx.t -> content_sort:ZSort.t -> ZSort.t
  = fun ctx ~content_sort -> begin
    let (sort_name) : string = _create_option_sort_name ~content_sort in
    ZSort._read_x_sort
      ~cst:(fun c ->
        create_sort c
          ~name:sort_name
          ~const_list:[ (read_option_const_of_none c);
                        (read_option_const_of_some c ~content_sort); ])
      ctx
      sort_name
  end (* function read_option_sort end *)

  (* Pair Type ****************************************************************)
  let read_pair_const : ZCtx.t -> fst_sort:ZSort.t -> snd_sort:ZSort.t -> const
  = fun ctx ~fst_sort ~snd_sort -> begin
    _read_x_const
      _const_map
      ~cst:(fun c -> (
        create_const c
          ~name:CONST._const_pair
          ~recog_func_name:CONST._recog_pair
          ~field_names:[ (CONST._field_pair_fst); (CONST._field_pair_snd); ]
          ~field_sorts:[ (Some fst_sort); (Some snd_sort); ]
          ~field_sort_refs:[ 1; 2; ]))
      ctx
      (Pair (fst_sort, snd_sort))
  end (* function read_pair_const end *)

  let _create_pair_sort_name : fst_sort:ZSort.t -> snd_sort:ZSort.t -> string
  = fun ~fst_sort ~snd_sort -> Printf.sprintf "%s_(%s,%s)" CONST._sort_pair (ZSort.to_string fst_sort) (ZSort.to_string snd_sort)

  let read_pair_sort : ZCtx.t -> fst_sort:ZSort.t -> snd_sort:ZSort.t -> ZSort.t
  = fun ctx ~fst_sort ~snd_sort -> begin
    let (sort_name) : string = _create_pair_sort_name ~fst_sort ~snd_sort in
    ZSort._read_x_sort
      ~cst:(fun c ->
        create_sort c
          ~name:sort_name
          ~const_list:[ (read_pair_const c ~fst_sort:fst_sort ~snd_sort:snd_sort); ])
      ctx
      sort_name
  end (* function read_pair_sort end *)

  (* Bytes Type ***************************************************************)
  let read_bytes_const_of_nil : ZCtx.t -> const
  = fun ctx -> begin
    _read_x_const
      _const_map
      ~cst:(fun c -> (
        create_const c
          ~name:CONST._const_bytes_nil
          ~recog_func_name:CONST._recog_bytes_bytnil
          ~field_names:[]
          ~field_sorts:[]
          ~field_sort_refs:[]))
      ctx
      Bytes_nil
  end (* function read_bytes_const_of_nil end *)

  let read_bytes_const_of_str : ZCtx.t -> const
  = fun ctx -> begin
    _read_x_const
      _const_map
      ~cst:(fun c -> (
        create_const c
          ~name:CONST._const_bytes_bytstr
          ~recog_func_name:CONST._recog_bytes_bytstr
          ~field_names:[ (CONST._field_content); ]
          ~field_sorts:[ (Some (ZSort.read_str_sort ctx)); ]
          ~field_sort_refs:[ 1; ]))
      ctx
      Bytes_str
  end (* function read_bytes_const_of_str end *)

  let read_bytes_const_of_concat : ZCtx.t -> const
  = fun ctx -> begin
    _read_x_const
      _const_map
      ~cst:(fun c -> (
        create_const c
          ~name:CONST._const_bytes_concatenated
          ~recog_func_name:CONST._recog_bytes_concatenated
          ~field_names:[ (CONST._field_pair_fst); (CONST._field_pair_snd); ]
          ~field_sorts:[ None; None; ]
          ~field_sort_refs:[ 0; 0; ]))
      ctx
      Bytes_concat
  end (* function read_bytes_const_of_concat end *)

  let read_bytes_const_of_blake2b : ZCtx.t -> const
  = fun ctx -> begin
    _read_x_const
      _const_map
      ~cst:(fun c -> (
        create_const c
          ~name:CONST._const_bytes_blake2b
          ~recog_func_name:CONST._recog_bytes_blake2b
          ~field_names:[ (CONST._field_content); ]
          ~field_sorts:[ None; ]
          ~field_sort_refs:[ 0; ]))
      ctx
      Bytes_str
  end (* function read_bytes_const_of_blake2b end *)

  let read_bytes_const_of_sha256 : ZCtx.t -> const
  = fun ctx -> begin
    _read_x_const
      _const_map
      ~cst:(fun c -> (
        create_const c
          ~name:CONST._const_bytes_sha256
          ~recog_func_name:CONST._recog_bytes_sha256
          ~field_names:[ (CONST._field_content); ]
          ~field_sorts:[ None; ]
          ~field_sort_refs:[ 0; ]))
      ctx
      Bytes_sha256
  end (* function read_bytes_const_of_sha256 end *)

  let read_bytes_const_of_sha512 : ZCtx.t -> const
  = fun ctx -> begin
    _read_x_const
      _const_map
      ~cst:(fun c -> (
        create_const c
          ~name:CONST._const_bytes_sha512
          ~recog_func_name:CONST._recog_bytes_sha512
          ~field_names:[ (CONST._field_content); ]
          ~field_sorts:[ None; ]
          ~field_sort_refs:[ 0; ]))
      ctx
      Bytes_sha512
  end (* function read_bytes_const_of_sha512 end *)

  let read_bytes_sort : ZCtx.t -> ZSort.t
  = fun ctx -> begin
    ZSort._read_x_sort
      ~cst:(fun c ->
        create_sort c
          ~name:CONST._sort_bytes
          ~const_list:[ (read_bytes_const_of_str c);
                        (read_bytes_const_of_concat c);
                        (read_bytes_const_of_blake2b c);
                        (read_bytes_const_of_sha256 c);
                        (read_bytes_const_of_sha512 c); ])
      ctx
      CONST._sort_bytes
  end (* function read_bytes_sort end *)

  (* Signature Type ***********************************************************)
  let read_sig_const_of_str : ZCtx.t -> const
  = fun ctx -> begin
    _read_x_const
      _const_map
      ~cst:(fun c -> (
        create_const c
          ~name:CONST._const_signature_sigstr
          ~recog_func_name:CONST._recog_signature_sigstr
          ~field_names:[ (CONST._field_content); ]
          ~field_sorts:[ (Some (ZSort.read_str_sort c)); ]
          ~field_sort_refs:[ 1; ]))
      ctx
      Signature_str
  end (* function read_sig_const_of_str end *)

  let read_sig_const_of_signed : ZCtx.t -> const
  = fun ctx -> begin
    _read_x_const
      _const_map
      ~cst:(fun c -> (
        create_const c
          ~name:CONST._const_signature_signed
          ~recog_func_name:CONST._recog_signature_signed
          ~field_names:[ (CONST._field_pair_fst); (CONST._field_pair_snd); ]
          ~field_sorts:[ (Some (read_key_sort c)); (Some (read_bytes_sort c)); ]
          ~field_sort_refs:[ 1; 2; ]))
      ctx
      Signature_signed
  end (* function read_sig_const_of_signed end *)

  let read_sig_sort : ZCtx.t -> ZSort.t
  = fun ctx -> begin
    ZSort._read_x_sort
      ~cst:(fun c ->
        create_sort c
          ~name:CONST._sort_signature
          ~const_list:[ (read_sig_const_of_str c);
                        (read_sig_const_of_signed c); ])
      ctx
      CONST._sort_signature
  end (* function read_sig_sort end *)

  (* Address Type *************************************************************)
  let read_addr_const : ZCtx.t -> const
  = fun ctx -> begin
    _read_x_const
      _const_map
      ~cst:(fun c -> (
        create_const c
          ~name:CONST._const_address_addrkh
          ~recog_func_name:CONST._recog_address_addrkh
          ~field_names:[ (CONST._field_content); ]
          ~field_sorts:[ (Some (read_keyhash_sort c)); ]
          ~field_sort_refs:[ 1; ]))
      ctx
      Address
  end (* function read_addr_const end *)

  let read_addr_sort : ZCtx.t -> ZSort.t
  = fun ctx -> begin
    ZSort._read_x_sort
      ~cst:(fun c ->
        create_sort c
          ~name:CONST._sort_address
          ~const_list:[ (read_addr_const c); ])
      ctx
      CONST._sort_signature
  end (* function read_addr_sort end *)

  (* Or Type ******************************************************************)
  let read_or_const_of_left : ZCtx.t -> left_sort:ZSort.t -> const
  = fun ctx ~left_sort -> begin
    _read_x_const
      _const_map
      ~cst:(fun c -> (
        create_const c
          ~name:CONST._const_or_left
          ~recog_func_name:CONST._recog_or_left
          ~field_names:[ (CONST._field_content); ]
          ~field_sorts:[ (Some left_sort); ]
          ~field_sort_refs:[ 1; ]))
      ctx
      (Or_left left_sort)
  end (* function read_or_const_of_left end *)

  let read_or_const_of_right : ZCtx.t -> right_sort:ZSort.t -> const
  = fun ctx ~right_sort -> begin
    _read_x_const
      _const_map
      ~cst:(fun c -> (
        create_const c
          ~name:CONST._const_or_right
          ~recog_func_name:CONST._recog_or_right
          ~field_names:[ (CONST._field_content); ]
          ~field_sorts:[ (Some right_sort); ]
          ~field_sort_refs:[ 1; ]))
      ctx
      (Or_right right_sort)
  end (* function read_or_const_of_right end *)

  let _create_or_sort_name : left_sort:ZSort.t -> right_sort:ZSort.t -> string
  = fun ~left_sort ~right_sort -> Printf.sprintf "%s_(%s,%s)" CONST._sort_pair (ZSort.to_string left_sort) (ZSort.to_string right_sort)

  let read_or_sort : ZCtx.t -> left_sort:ZSort.t -> right_sort:ZSort.t -> ZSort.t
  = fun ctx ~left_sort ~right_sort -> begin
    let (sort_name) : string = _create_or_sort_name ~left_sort ~right_sort in
    ZSort._read_x_sort
      ~cst:(fun c ->
        create_sort c
          ~name:sort_name
          ~const_list:[ (read_or_const_of_left c ~left_sort);
                        (read_or_const_of_right c ~right_sort); ])
      ctx
      sort_name
  end (* function read_or_sort end *)

  (* List Type ****************************************************************)
  let _create_list_sort_name : content_sort:ZSort.t -> string
  = fun ~content_sort -> Printf.sprintf "%s_(%s)" CONST._sort_pair (ZSort.to_string content_sort)

  let read_list_sort : ZCtx.t -> content_sort:ZSort.t -> ZSort.t
  = fun ctx ~content_sort -> begin
    let (sort_name) : string = _create_list_sort_name ~content_sort in
    ZSort._read_x_sort
      ~cst:(fun c -> Z3.Z3List.mk_list_s c sort_name content_sort)
      ctx
      sort_name
  end (* function read_list_sort end *)

  (* Map Type *****************************************************************)
  let _create_map_sort_name : key_sort:ZSort.t -> value_sort:ZSort.t -> string
  = fun ~key_sort ~value_sort -> Printf.sprintf "%s_(%s,%s)" CONST._sort_map (ZSort.to_string key_sort) (ZSort.to_string value_sort)

  let read_map_sort : ZCtx.t -> key_sort:ZSort.t -> value_sort:ZSort.t -> ZSort.t
  = fun ctx ~key_sort ~value_sort -> begin
    let (sort_name) : string = _create_map_sort_name ~key_sort ~value_sort in
    ZSort._read_x_sort
      ~cst:(fun c -> Z3.Z3Array.mk_sort c key_sort (read_option_sort c ~content_sort:value_sort))
      ctx
      sort_name
  end

  (* Operation Type ***********************************************************)
  let read_operation_sort : ZCtx.t -> ZSort.t
  = fun ctx -> begin
    ZSort._read_x_sort
      ~cst:(fun c -> ZSort.create c ~name:CONST._sort_operation)
      ctx
      CONST._sort_operation
  end

  (* Contract Type ************************************************************)
  let read_contract_sort : ZCtx.t -> ZSort.t
  = fun ctx -> begin
    ZSort._read_x_sort
      ~cst:(fun c -> ZSort.create c ~name:CONST._sort_contract)
      ctx
      CONST._sort_contract
  end

  (* Lambda Type **************************************************************)
  let read_lambda_sort : ZCtx.t -> ZSort.t
  = fun ctx -> begin
    ZSort._read_x_sort
      ~cst:(fun c -> ZSort.create c ~name:CONST._sort_lambda)
      ctx
      CONST._sort_lambda
  end
end


(*****************************************************************************)
(*****************************************************************************)
(* Formulae                                                                  *)
(*****************************************************************************)
(*****************************************************************************)

module ZFormula = struct
  type t = ZExpr.t

  let sort : ZCtx.t -> ZSort.t
  = ZSort.read_bool_sort

  let true_ : ZCtx.t -> t
  = fun ctx -> ZExpr.read_bool ctx true

  let false_ : ZCtx.t -> t
  = fun ctx -> ZExpr.read_bool ctx false

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
  = ZSort.read_unit_sort

  let create : ZCtx.t -> t
  = ZExpr.read_unit
end


(*****************************************************************************)
(*****************************************************************************)
(* Booleans                                                                  *)
(*****************************************************************************)
(*****************************************************************************)

module ZBool = struct
  type t = ZExpr.t

  let sort : ZCtx.t -> ZSort.t
  = ZSort.read_bool_sort


  let of_bool : ZCtx.t -> bool -> t
  = ZExpr.read_bool


  let _minus_one_ : ZCtx.t -> t
  = fun ctx -> ZExpr.read_int ctx (-1)

  let _zero_ : ZCtx.t -> t
  = fun ctx -> ZExpr.read_int ctx 0

  let _one_ : ZCtx.t -> t
  = fun ctx -> ZExpr.read_int ctx 1
  

  let true_ : ZCtx.t -> t
  = fun ctx -> ZExpr.read_bool ctx true

  let false_ : ZCtx.t -> t
  = fun ctx -> ZExpr.read_bool ctx false


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
      ~t:(_zero_ ctx)
      ~f:(ZExpr.create_ite ctx
            ~cond:(e2)
            ~t:(_minus_one_ ctx)
            ~f:(_one_ ctx))
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
  = ZSort.read_int_sort

  let of_zarith : ZCtx.t -> Z.t -> t
  = ZExpr.read_zint

  let of_int : ZCtx.t -> int -> t
  = ZExpr.read_int

  let minus_one_ : ZCtx.t -> t
  = fun ctx -> of_int ctx (-1)
  let zero_ : ZCtx.t -> t
  = fun ctx -> of_int ctx (0)
  let one_ : ZCtx.t -> t
  = fun ctx -> of_int ctx (1)

  let mutez_max_ : ZCtx.t -> t
  = fun ctx -> of_zarith ctx (Z.shift_left Z.one (CONST._bit_mutez) |> Z.pred)

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
  = ZSort.read_int_sort
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
  = ZSort.read_str_sort

  let of_string : ZCtx.t -> string -> t
  = ZExpr.read_str

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
            ~cond:(Z3.Seq.mk_str_lt ctx e1 e2)
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

  let sort : ZCtx.t -> ZSort.t
  = ZDatatype.read_key_sort

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

  let sort : ZCtx.t -> ZSort.t
  = ZDatatype.read_keyhash_sort

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

  let sort : ZCtx.t -> content_sort:ZSort.t -> ZSort.t
  = ZDatatype.read_option_sort

  let create_none : ZCtx.t -> content_sort:ZSort.t -> t
  = fun ctx ~content_sort -> sort ctx ~content_sort:content_sort |> ZDatatype.create ~const_idx:0 ~expr_list:[]

  let create_some : ZCtx.t -> content:ZExpr.t -> t
  = fun ctx ~content -> sort ctx ~content_sort:(content |> ZExpr.read_sort) |> ZDatatype.create ~const_idx:1 ~expr_list:[content]

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

  let sort : ZCtx.t -> fst_sort:ZSort.t -> snd_sort:ZSort.t -> ZSort.t
  = ZDatatype.read_pair_sort

  let create : ZCtx.t -> fst:ZExpr.t -> snd:ZExpr.t -> t
  =fun ctx ~fst ~snd -> begin
    sort ctx ~fst_sort:(fst |> ZExpr.read_sort) ~snd_sort:(snd |> ZExpr.read_sort)
    |> ZDatatype.create ~const_idx:0 ~expr_list:[fst; snd]
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

  let sort : ZCtx.t -> ZSort.t
  = ZDatatype.read_bytes_sort
 
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

  let sort : ZCtx.t -> ZSort.t
  = ZDatatype.read_sig_sort

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

  let sort : ZCtx.t -> ZSort.t
  = ZDatatype.read_addr_sort

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

  let sort : ZCtx.t -> left_sort:ZSort.t -> right_sort:ZSort.t -> ZSort.t
  = ZDatatype.read_or_sort

  let create_left : ZCtx.t -> left_content:ZExpr.t -> right_sort:ZSort.t -> t
  =fun ctx ~left_content ~right_sort -> begin
    (sort ctx
      ~left_sort:(left_content |> ZExpr.read_sort)
      ~right_sort:right_sort) |>
    ZDatatype.create ~const_idx:0 ~expr_list:[left_content]
  end
  let create_right : ZCtx.t -> left_sort:ZSort.t -> right_content:ZExpr.t -> t
  =fun ctx ~left_sort ~right_content -> begin
    (sort ctx
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

  let create_sort : ZCtx.t -> content_sort:ZSort.t -> ZSort.t
  = ZDatatype.read_list_sort

  let create : ZCtx.t -> content_sort:ZSort.t -> t
  = fun ctx ~content_sort -> Z3.Z3List.nil (create_sort ctx ~content_sort)
  
  let read_head : t -> ZExpr.t
  = fun e -> Z3.Z3List.get_head_decl (ZExpr.read_sort e) |> ZFunc.apply ~params:[ e; ]

  let read_tail : t -> t
  = fun e -> Z3.Z3List.get_tail_decl (ZExpr.read_sort e) |> ZFunc.apply ~params:[ e; ]

  let update : t -> content:ZExpr.t -> t
  = fun e ~content -> Z3.Z3List.get_cons_decl (ZExpr.read_sort e) |> ZFunc.apply ~params:[ content; e; ]

  let create_eq : ZCtx.t -> t -> t -> ZBool.t
  = ZBool.create_eq
  let create_neq : ZCtx.t -> t -> t -> ZBool.t
  = ZBool.create_neq

  let is_nil : t -> ZBool.t
  = fun e -> Z3.Z3List.get_is_nil_decl (ZExpr.read_sort e) |> ZFunc.apply ~params:[ e; ]
  let is_cons : t -> ZBool.t
  = fun e -> Z3.Z3List.get_is_cons_decl (ZExpr.read_sort e) |> ZFunc.apply ~params:[ e; ]
end


(*****************************************************************************)
(*****************************************************************************)
(* Maps                                                                      *)
(*****************************************************************************)
(*****************************************************************************)

module ZMap = struct
  type t = ZExpr.t
  
  let create_sort : ZCtx.t -> key_sort:ZSort.t -> value_sort:ZSort.t -> ZSort.t
  = ZDatatype.read_map_sort

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
  = ZDatatype.read_operation_sort

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
  = ZDatatype.read_contract_sort

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
  = ZDatatype.read_lambda_sort

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