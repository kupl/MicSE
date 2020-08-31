(*****************************************************************************)
(*****************************************************************************)
(* Imperative-Style Michelson                                                *)
(*****************************************************************************)
(*****************************************************************************)

type typ = Mich.typ Mich.t
type data = Mich.data Mich.t
type var = string
type ident = string
type loc = Mich.loc

type operation = 
  | O_create_contract of Mich.program * string * string * string
  | O_transfer_tokens of string * string * string
  | O_set_delegate of string
  | O_create_account of string * string * string * string

type expr =
  | E_push of data * typ
  | E_car of var
  | E_cdr of var
  | E_abs of var
  | E_neg of var
  | E_not of var
  | E_add of var * var
  | E_sub of var * var
  | E_mul of var * var
  | E_ediv of var * var
  | E_shiftL of var * var
  | E_shiftR of var * var
  | E_and of var * var
  | E_or of var * var
  | E_xor of var * var
  | E_eq of var
  | E_neq of var
  | E_lt of var
  | E_gt of var
  | E_leq of var
  | E_geq of var
  | E_compare of var * var
  | E_cons of var * var
  | E_operation of operation
  | E_unit
  | E_pair of var * var
  | E_left of var * typ
  | E_right of var * typ
  | E_some of var
  | E_none of typ
  | E_mem of var * var
  | E_get of var * var
  | E_update of var * var * var
  | E_cast of var
  | E_concat of var * var
  | E_concat_list of var
  | E_slice of var * var * var
  | E_pack of var
  | E_unpack of typ * var
  | E_self
  | E_contract_of_address of var
  | E_implicit_account of var
  | E_now
  | E_amount
  | E_balance
  | E_check_signature of var * var * var
  | E_blake2b of var
  | E_sha256 of var
  | E_sha512 of var
  | E_hash_key of var
  | E_steps_to_quota
  | E_source
  | E_sender
  | E_address_of_contract of var
  | E_unlift_option of var
  | E_unlift_left of var
  | E_unlift_right of var
  | E_hd of var
  | E_tl of var
  | E_size of var
  | E_isnat of var
  | E_int_of_nat of var
  | E_chain_id
  | E_lambda_id of int
  | E_exec of var * var
  | E_dup of var
  | E_nil of typ
  | E_empty_set of typ
  | E_empty_map of typ * typ
  | E_empty_big_map of typ * typ
  | E_append of var * var
  | E_itself of var
  (* DEPRECATED & UNUSED EXPRESSIONS. They can be erased anytime. *)
  | E_div of var * var
  | E_mod of var * var
  | E_create_contract_address of operation
  | E_create_account_address of operation
  | E_lambda of typ * typ * (stmt * var)
  | E_special_nil_list
  | E_phi of var * var
  | E_unlift_or of var

(* Data constructor Prefix "Cfg" in type stmt is originated from Tezla-Cfg's naming. *)
and stmt =
  | Cfg_assign of var * expr
  | Cfg_skip
  | Cfg_drop of var list
  | Cfg_swap
  | Cfg_dig
  | Cfg_dug
  | Cfg_if of var
  | Cfg_if_none of var
  | Cfg_if_left of var
  | Cfg_if_cons of var
  | Cfg_loop of var
  | Cfg_loop_left of var
  | Cfg_map of var
  | Cfg_iter of var
  | Cfg_failwith of var
  | Cfg_micse_check_entry
  | Cfg_micse_check_value of var


(*****************************************************************************)
(* To String                                                                 *)
(*****************************************************************************)

let typ_to_str : typ -> string = Mich.string_of_typt
let data_to_str : data -> string = Mich.string_of_datat_ol

let operation_to_str : operation -> string = begin function
  | O_create_contract (p, v1, v2, v3) -> String.concat " " ["CREATE_CONTRACT"; (Mich.string_of_pgm_ol p); v1; v2; v3]
  | O_transfer_tokens (v1, v2, v3) -> String.concat " " ["TRANSFER_TOKENS"; v1; v2; v3]
  | O_set_delegate v -> String.concat " " ["SET_DELEGATE"; v]
  | O_create_account (v1, v2, v3, v4) -> String.concat " " ["CREATE_ACCOUNT"; v1; v2; v3; v4]
end

let expr_to_str : expr -> string 
=fun e -> begin
  let str_contents : expr -> string list = function
    | E_push (d, t) -> ["PUSH"; data_to_str d; typ_to_str t;]
    | E_car v -> ["CAR"; v] 
    | E_cdr v -> ["CDR"; v]
    | E_abs v -> ["ABS"; v]
    | E_neg v -> ["NEG"; v]
    | E_not v -> ["NOT"; v]
    | E_add  (v1, v2) -> ["ADD";  v1; v2]
    | E_sub  (v1, v2) -> ["SUB";  v1; v2]
    | E_mul  (v1, v2) -> ["MUL";  v1; v2]
    | E_ediv (v1, v2) -> ["EDIV"; v1; v2]
    | E_shiftL (v1, v2) -> ["LSL"; v1; v2]
    | E_shiftR (v1, v2) -> ["LSR"; v1; v2]
    | E_and  (v1, v2) -> ["AND";  v1; v2]
    | E_or   (v1, v2) -> ["OR";   v1; v2]
    | E_xor  (v1, v2) -> ["XOR";  v1; v2]
    | E_eq  v -> ["EQ"; v]
    | E_neq v -> ["NEQ"; v]
    | E_lt  v -> ["LT"; v]
    | E_gt  v -> ["GT"; v]
    | E_leq v -> ["LEQ"; v]
    | E_geq v -> ["GEQ"; v]
    | E_compare (v1, v2) -> ["COMPARE";  v1; v2]
    | E_cons (v1, v2) -> ["CONS";  v1; v2]
    | E_operation op -> [operation_to_str op]
    | E_unit -> ["UNIT"]
    | E_pair (v1, v2) -> ["PAIR";  v1; v2]
    | E_left (v, t) -> ["LEFT"; v; typ_to_str t]
    | E_right (v, t) -> ["RIGHT"; v; typ_to_str t]
    | E_some v -> ["SOME"; v]
    | E_none t -> ["NONE"; typ_to_str t]
    | E_mem (v1, v2) -> ["MEM"; v1; v2]
    | E_get (v1, v2) -> ["GET"; v1; v2]
    | E_update (v1, v2, v3) -> ["UPDATE"; v1; v2; v3]
    | E_cast v -> ["CAST"; v]
    | E_concat (v1, v2) -> ["CONCAT";  v1; v2]
    | E_concat_list v -> ["CONCAT_LIST"; v]
    | E_slice (v1, v2, v3) -> ["SLICE"; v1; v2; v3]
    | E_pack v -> ["PACK"; v]
    | E_unpack (t, v) -> ["UNPACK"; typ_to_str t; v]
    | E_self -> ["SELF"]
    | E_contract_of_address v -> ["CONTRACT"; v]
    | E_implicit_account v -> ["IMPLICIT_ACCOUNT"; v]
    | E_now -> ["NOW"]
    | E_amount -> ["AMOUNT"]
    | E_balance -> ["BALANCE"]
    | E_check_signature (v1, v2, v3) -> ["CHECK_SIGNATURE"; v1; v2; v3]
    | E_blake2b v -> ["BLAKE2B"; v]
    | E_sha256 v -> ["SHA256"; v]
    | E_sha512 v -> ["SHA512"; v]
    | E_hash_key v -> ["HASH_KEY"; v]
    | E_steps_to_quota -> ["STEPS_TO_QUOTA"]
    | E_source -> ["SOURCE"]
    | E_sender -> ["SENDER"]
    | E_address_of_contract v -> ["ADDRESS"; v]
    | E_unlift_option v -> ["unlift_option"; v]
    | E_unlift_left v -> ["unlift_left"; v]
    | E_unlift_right v -> ["unlift_right"; v]
    | E_hd v -> ["hd"; v]
    | E_tl v -> ["tl"; v]
    | E_size v -> ["SIZE"; v]
    | E_isnat v -> ["ISNAT"; v]
    | E_int_of_nat v -> ["INT"; v]
    | E_chain_id -> ["CHAIN_ID"]
    | E_lambda_id n -> ["lambda_id"; string_of_int n]
    | E_exec (v1, v2) -> ["EXEC"; v1; v2]
    | E_dup v -> ["DUP"; v]
    | E_nil t -> ["NIL"; typ_to_str t]
    | E_empty_set t -> ["EMPTY_SET"; typ_to_str t]
    | E_empty_map (t1, t2) -> ["EMPTY_MAP"; typ_to_str t1; typ_to_str t2]
    | E_empty_big_map (t1, t2) -> ["EMPTY_BIG_MAP"; typ_to_str t1; typ_to_str t2]
    | E_append (v1, v2) -> ["APPEND"; v1; v2]
    | E_itself v -> [v]
    (* DEPRECATED & UNUSED EXPRESSIONS. They can be erased anytime. *)
    | E_div (v1, v2) -> ["div"; v1; v2]
    | E_mod (v1, v2) -> ["mod"; v1; v2]
    | E_create_contract_address op -> [operation_to_str op]
    | E_create_account_address op -> [operation_to_str op]
    | E_lambda _ -> ["LAMBDA (_)"]
    | E_special_nil_list -> ["nil-list"]
    | E_phi (v1, v2) -> ["phi"; v1; v2]
    | E_unlift_or v -> ["unlift_or"; v]
  in
  String.concat " " (str_contents e)
end

let stmt_to_str : stmt -> string = begin function
  | Cfg_assign (v, e) -> String.concat " " [v; ":="; expr_to_str e]
  | Cfg_skip -> "skip"
  | Cfg_drop vlist -> "DROP [" ^ (String.concat ";" vlist) ^ "]"
  | Cfg_swap -> "SWAP"
  | Cfg_dig -> "DIG"
  | Cfg_dug -> "DUG"
  | Cfg_if v -> String.concat " " ["IF"; v]
  | Cfg_if_none v -> String.concat " " ["IF_NONE"; v]
  | Cfg_if_left v -> String.concat " " ["IF_LEFT"; v]
  | Cfg_if_cons v -> String.concat " " ["IF_CONS"; v]
  | Cfg_loop v -> String.concat " " ["LOOP"; v]
  | Cfg_loop_left v -> String.concat " " ["LOOP_LEFT"; v]
  | Cfg_map v -> String.concat " " ["MAP"; v]
  | Cfg_iter v -> String.concat " " ["ITER"; v]
  | Cfg_failwith v -> String.concat " " ["FAILWITH"; v]
  | Cfg_micse_check_entry -> "#MICSE_check_entry"
  | Cfg_micse_check_value v -> String.concat " " ["#MICSE_check_value"; v]
end


(*****************************************************************************)
(*****************************************************************************)
(* Exception                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

exception Exn_Cfg of string

let fail s = raise (Exn_Cfg s)


(*****************************************************************************)
(*****************************************************************************)
(* Graph (Flow)                                                              *)
(*****************************************************************************)
(*****************************************************************************)

type vertex = int
type edge_label = | Normal | If_true | If_false | Failed | Check_skip

module V = struct
  type t = int
  let compare = Int.compare
  let hash x = x
  let equal = Int.equal
end

module E = struct
  type t = edge_label
  let default = Normal
  let compare = Stdlib.compare
end

module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (V) (E)

let is_edge_normal : E.t -> bool
=fun edge -> (edge = Normal)

let is_edge_true : E.t -> bool
=fun edge -> (edge = If_true)

let is_edge_false : E.t -> bool
=fun edge -> (edge = If_false)

let is_edge_failed : E.t -> bool
=fun edge -> (edge = Failed)

let is_edge_check_skip : E.t -> bool
=fun edge -> (edge = Check_skip)

let string_of_vertex : vertex -> string
=fun vtx -> (string_of_int vtx)


(*****************************************************************************)
(*****************************************************************************)
(* Context Flow Graph                                                        *)
(*****************************************************************************)
(*****************************************************************************)

type lambda_ident   = int (* identifier for internal functions *)
type lambda_summary = (vertex * vertex * typ * typ) (* (entry-vertex, exit-vertex, param-type, output-type) *)

module CPMap = Core.Map.Poly

type t = {
  flow          : G.t;
  vertex_info   : (int, stmt) CPMap.t;          (* vertex-number -> stmt *)
  type_info     : (string, typ) CPMap.t;        (* variable-name -> typ *)  
  main_entry    : vertex;
  main_exit     : vertex;
  adt           : Adt.t;                        (* original Michelson code (in adt type) *)
  lambda_id_map : (lambda_ident, lambda_summary) CPMap.t;  (* function id -> function summary *)
  fail_vertices : vertex Core.Set.Poly.t;        (* vertex set which has Cfg_failwith instruction *)
  pos_info      : (int, loc) CPMap.t;           (* contains the position of "some" vertices (not "all" vertices) *)
}

let param_storage_name = "param_storage"

let gen_param_name i = "param_" ^ (string_of_int i)

let read_stmt_from_vtx : t -> vertex -> stmt
=fun cfg vtx -> begin
  try
    let stmt = CPMap.find_exn cfg.vertex_info vtx in
    stmt
  with
  | Not_found -> raise (Failure "Cfg.read_stmt_from_vtx: Cannot find node from cfg")
end

let read_succ_from_vtx : t -> vertex -> (E.t * V.t) list
=fun cfg vtx -> begin
  let succ_vtxs = G.succ cfg.flow vtx in
  let succ = Core.List.map succ_vtxs ~f:(fun succ_vtx -> (
    let (_, succ_edge, _) = G.find_edge cfg.flow vtx succ_vtx in
    (succ_edge, succ_vtx)
  )) in
  succ
end

let read_pred_from_vtx : t -> vertex -> (E.t * V.t) list
=fun cfg vtx -> begin
  let pred_vtxs = G.pred cfg.flow vtx in
  let pred = Core.List.map pred_vtxs ~f:(fun pred_vtx -> (
    let (_, pred_edge, _) = G.find_edge cfg.flow pred_vtx vtx in
    (pred_edge, pred_vtx)
  )) in
  pred
end

let is_main_entry : t -> vertex -> bool
=fun cfg vtx -> (vtx = cfg.main_entry)

let is_main_exit : t -> vertex -> bool
=fun cfg vtx -> (vtx = cfg.main_exit)

type lmbd_invmap = (vertex, (lambda_ident * lambda_summary)) CPMap.t
let lmbd_map_to_two_sets : t -> (lmbd_invmap * lmbd_invmap)
=fun cfg -> begin
  let fold_func ~key ~data : (lmbd_invmap * lmbd_invmap) -> (lmbd_invmap * lmbd_invmap)
  =fun (m1, m2) -> begin
    let (entry_v, exit_v, _, _) = data in
    match (CPMap.add m1 ~key:entry_v ~data:(key, data), CPMap.add m2 ~key:exit_v ~data:(key, data)) with
    | `Ok mm1, `Ok mm2 -> (mm1, mm2)
    | _ -> fail ("lmbd_map_to_two_sets : match error : entry_v=" ^ (string_of_int entry_v) ^ ", exit_v=" ^ (string_of_int exit_v))
  end in
  CPMap.fold cfg.lambda_id_map ~init:(CPMap.empty, CPMap.empty) ~f:fold_func
end


(*****************************************************************************)
(*****************************************************************************)
(* Graph Utilities                                                           *)
(*****************************************************************************)
(*****************************************************************************)

type cfgcon_ctr = { (* counter for cfg construction *)
  vertex_counter : vertex ref;
  var_counter : int ref;
  lambda_counter : int ref;
}

let new_vtx : cfgcon_ctr -> Core.Int.t = fun c -> (incr c.vertex_counter; !(c.vertex_counter))
let new_var : cfgcon_ctr -> Core.String.t = fun c -> (incr c.var_counter; "v" ^ (string_of_int (!(c.var_counter))))
let new_lambda_ident : cfgcon_ctr -> Core.Int.t = fun c -> (incr c.lambda_counter; !(c.lambda_counter))

let vtx_add : vertex -> t -> t = begin fun v cfg -> {cfg with flow=(G.add_vertex cfg.flow v);} end
let edg_add : (vertex * vertex) -> t -> t = begin fun (v1, v2) cfg -> {cfg with flow=(G.add_edge cfg.flow v1 v2);} end
let tedg_add : (vertex * vertex) -> t -> t = begin fun (v1, v2) cfg -> let e = G.E.create v1 If_true v2 in {cfg with flow=(G.add_edge_e cfg.flow e);} end
let fedg_add : (vertex * vertex) -> t -> t = begin fun (v1, v2) cfg -> let e = G.E.create v1 If_false v2 in {cfg with flow=(G.add_edge_e cfg.flow e);} end
let fail_edg_add : (vertex * vertex) -> t -> t = begin fun (v1, v2) cfg -> let e = G.E.create v1 Failed v2 in {cfg with flow=(G.add_edge_e cfg.flow e);} end
let cskip_edg_add : (vertex * vertex) -> t -> t = begin fun (v1, v2) cfg -> let e = G.E.create v1 Check_skip v2 in {cfg with flow=(G.add_edge_e cfg.flow e);} end

let t_map_add ?(errtrace = "") m k v = begin
  match Core.Map.Poly.add m ~key:k ~data:v with
  | `Ok m' -> m'
  | `Duplicate -> fail (errtrace ^ " : map_add : duplicated entry.")
end
let t_map_find ?(errtrace = "") m k = begin
  match Core.Map.Poly.find m k with
  | Some v -> v
  | None -> fail (errtrace ^ " : map_find : not found.")
end

let t_add_vtx   c (cfg, _) = begin let v = new_vtx c in (vtx_add v cfg, v) end
let t_add_vtx_2 c (cfg, _) = begin
  let v1 = new_vtx c in
  let v2 = new_vtx c in
  ((cfg |> vtx_add v1 |> vtx_add v2), (v1, v2))
end
let t_add_vtx_3 c (cfg, _) = begin
  let v1 = new_vtx c in
  let v2 = new_vtx c in
  let v3 = new_vtx c in
  ((cfg |> vtx_add v1 |> vtx_add v2 |> vtx_add v3), (v1, v2, v3))
end
let t_add_vtx_4 c (cfg, _) = begin
  let v1 = new_vtx c in
  let v2 = new_vtx c in
  let v3 = new_vtx c in
  let v4 = new_vtx c in
  ((cfg |> vtx_add v1 |> vtx_add v2 |> vtx_add v3 |> vtx_add v4), (v1, v2, v3, v4))
end
let t_add_vtx_5 c (cfg, _) = begin
  let v1 = new_vtx c in
  let v2 = new_vtx c in
  let v3 = new_vtx c in
  let v4 = new_vtx c in
  let v5 = new_vtx c in
  ((cfg |> vtx_add v1 |> vtx_add v2 |> vtx_add v3 |> vtx_add v4 |> vtx_add v5), (v1, v2, v3, v4, v5))
end

let t_add_edg (v1, v2) (cfg, _) = (edg_add (v1, v2) cfg, v2)
let t_add_edgs vvlist (cfg, _) = begin 
  let cf = Core.List.fold vvlist ~init:cfg ~f:(fun acc_cfg (v1, v2) -> edg_add (v1, v2) acc_cfg) in
  let v2list = (Core.List.unzip vvlist |> Stdlib.snd) in
  (cf, v2list)
end
let t_add_tedg (v1, v2) (cfg, _) = (tedg_add (v1, v2) cfg, v2)
let t_add_fedg (v1, v2) (cfg, _) = (fedg_add (v1, v2) cfg, v2)
let t_add_fail_edg (v1, v2) (cfg, _) = (fail_edg_add (v1, v2) cfg, v2)
let t_add_cskip_edg (v1, v2) (cfg, _) = (cskip_edg_add (v1, v2) cfg, v2)

let t_add_vinfo ?(errtrace = "") (v, s) (cfg, _) = begin
  ({cfg with vertex_info=(t_map_add ~errtrace:(errtrace ^ " : t_add_vinfo") cfg.vertex_info v s);}, v)
end
let t_add_vinfos ?(errtrace = "") vslist (cfg, _) = begin
  let et : string = errtrace ^ " : t_add_vinfos" in
  let (cf, _) : t * vertex = Core.List.fold vslist ~init:(cfg, 1) ~f:(fun (acc_cfg, _) (v, s) -> (t_add_vinfo ~errtrace:et (v, s) (acc_cfg, 1))) in
  let vlist = (Core.List.unzip vslist |> Stdlib.fst) in
  (cf, vlist)
end
let t_add_vinfo_now ?(errtrace = "") s (cfg, v) = begin t_add_vinfo ~errtrace:errtrace (v, s) (cfg, ()) end

let t_add_tinfo ?(errtrace = "") (s, t) (cfg, _) = begin
  ({cfg with type_info=(t_map_add ~errtrace:(errtrace ^ " : t_add_tinfo") cfg.type_info s t);}, s)
end
let t_add_tinfos ?(errtrace = "") stlist (cfg, _) = begin
  let et : string = errtrace ^ " : t_add_tinfos" in
  let (cf, _) = Core.List.fold stlist ~init:(cfg, "") ~f:(fun (acc_cfg, _) (s, t) -> (t_add_tinfo ~errtrace:et (s, t) (acc_cfg, ""))) in
  let slist = (Core.List.unzip stlist |> Stdlib.fst) in
  (cf, slist)
end

let t_add_nv_tinfo ?(errtrace = "") counter ty (cfg, _) = begin
  let et : string = errtrace ^ " : t_add_nv_tinfo" in
  let s = new_var counter in
  t_add_tinfo ~errtrace:et (s, ty) (cfg, ())
end

let t_con_vtx_back      v2 (cfg, v1) = (edg_add  (v1, v2) cfg, v2)
let t_con_vtx_front     v1 (cfg, v2) = (edg_add  (v1, v2) cfg, v2)
let t_con_vtx_backr     v2 (cfg, v1) = (edg_add  (v1, v2) cfg, v1)
let t_con_vtx_frontr    v1 (cfg, v2) = (edg_add  (v1, v2) cfg, v1)
let t_con_vtx_back_t    v2 (cfg, v1) = (tedg_add (v1, v2) cfg, v2)
let t_con_vtx_front_t   v1 (cfg, v2) = (tedg_add (v1, v2) cfg, v2)
let t_con_vtx_backr_t   v2 (cfg, v1) = (tedg_add (v1, v2) cfg, v1)
let t_con_vtx_frontr_t  v1 (cfg, v2) = (tedg_add (v1, v2) cfg, v1)
let t_con_vtx_back_f    v2 (cfg, v1) = (fedg_add (v1, v2) cfg, v2)
let t_con_vtx_front_f   v1 (cfg, v2) = (fedg_add (v1, v2) cfg, v2)
let t_con_vtx_backr_f   v2 (cfg, v1) = (fedg_add (v1, v2) cfg, v1)
let t_con_vtx_frontr_f  v1 (cfg, v2) = (fedg_add (v1, v2) cfg, v1)

let t_add_lmbdim ?(errtrace = "") (ident, summ) (cfg, _) = begin
  ({cfg with lambda_id_map=(t_map_add ~errtrace:(errtrace ^ " : t_add_lmbdim") cfg.lambda_id_map ident summ);}, ident)
end

let t_add_failvtx fv (cfg, _) = begin
  ({cfg with fail_vertices=(Core.Set.Poly.add cfg.fail_vertices fv);}, fv)
end

let t_add_posinfo ?(errtrace = "") (vtx, l) (cfg, _) = begin
  ({cfg with pos_info=(t_map_add ~errtrace:(errtrace ^ " : t_add_posinfo") cfg.pos_info vtx l);}, vtx)
end

