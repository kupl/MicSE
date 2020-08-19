(* Control Flow Graph representation of the Michelson program *)
(* Original Michelson program is just functional instruction sequences,
    so in order to make control flow graph, we need to translate the michelson code
    into imperative code.
*)

(*****************************************************************************)
(*****************************************************************************)
(* Imperative-Style Michelson                                                *)
(*****************************************************************************)
(*****************************************************************************)

type typ = Mich.typ Mich.t
type data = Mich.data Mich.t
type var = string
type ident = string

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

val typ_to_str : typ -> string
val data_to_str : data -> string
val operation_to_str : operation -> string
val expr_to_str : expr -> string
val stmt_to_str : stmt -> string


(*****************************************************************************)
(*****************************************************************************)
(* Exception                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

exception Exn_Cfg of string

val fail : string -> 'a


(*****************************************************************************)
(*****************************************************************************)
(* Graph (Flow)                                                              *)
(*****************************************************************************)
(*****************************************************************************)

type vertex = int
type edge_label = | Normal | If_true | If_false | Failed | Check_skip

module V : Graph.Sig.COMPARABLE with type t = vertex         (* VERTEX *)
module E : Graph.Sig.ORDERED_TYPE_DFT with type t = edge_label   (* EDGE LABLE *)
module G : module type of Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (V) (E)

val is_edge_normal : E.t -> bool

val is_edge_true : E.t -> bool

val is_edge_false : E.t -> bool

val is_edge_failed : E.t -> bool

val is_edge_check_skip : E.t -> bool

val string_of_vertex : vertex -> string


(*****************************************************************************)
(*****************************************************************************)
(* Control Flow Graph                                                        *)
(*****************************************************************************)
(*****************************************************************************)

type lambda_ident   = Core.Int.t (* identifier for internal functions *)
type lambda_summary = (vertex * vertex * typ * typ) (* (entry-vertex, exit-vertex, param-type, output-type) *)

module CPMap : module type of Core.Map.Poly     (* module name sugar *)

type t = {
  flow          : G.t;
  vertex_info   : (int, stmt) CPMap.t;          (* vertex-number -> stmt *)
  type_info     : (string, typ) CPMap.t;        (* variable-name -> typ *)  
  main_entry    : vertex;
  main_exit     : vertex;
  adt           : Adt.t;                        (* original Michelson code (in adt type) *)
  lambda_id_map : (lambda_ident, lambda_summary) CPMap.t;  (* function id -> function summary *)
  fail_vertices : vertex Core.Set.Poly.t;        (* vertex set which has Cfg_failwith instruction *)
}

val param_storage_name : string

val gen_param_name : int -> string

val read_stmt_from_vtx : t -> vertex -> stmt

val read_succ_from_vtx : t -> vertex -> (E.t * V.t) list

val read_pred_from_vtx : t -> vertex -> (E.t * V.t) list

val is_main_entry : t -> vertex -> bool

val is_main_exit : t -> vertex -> bool

(* for given cfg, construct two maps, (entry-vertex -> (l-ident * l-summary)) * (exit-vertex -> (l-ident * l-summary)) *)
val lmbd_map_to_two_sets : t -> (((vertex, (lambda_ident * lambda_summary)) CPMap.t) * ((vertex, (lambda_ident * lambda_summary)) CPMap.t))


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

val new_vtx : cfgcon_ctr -> Core.Int.t
val new_var : cfgcon_ctr -> Core.String.t
val new_lambda_ident : cfgcon_ctr -> Core.Int.t

val t_map_add : ?errtrace:string -> ('k, 'v) CPMap.t -> 'k -> 'v -> ('k, 'v) CPMap.t
val t_map_find : ?errtrace:string -> ('k, 'v) CPMap.t -> 'k -> 'v

val vtx_add : vertex -> t-> t
val edg_add : (vertex * vertex) -> t-> t
val tedg_add : (vertex * vertex) -> t -> t
val fedg_add : (vertex * vertex) -> t -> t
val fail_edg_add : (vertex * vertex) -> t -> t
val cskip_edg_add : (vertex * vertex) -> t -> t

val t_add_vtx   : cfgcon_ctr -> (t * 'a) -> (t * vertex)                                        (* t_add_vtx counter (cfg, _) = (cfg-flow-updated, created-vertex) *)
val t_add_vtx_2 : cfgcon_ctr -> (t * 'a) -> (t * (vertex * vertex))                             (* t_add_vtx_2 (cfg, _) = (cfg-flow-updated, created-vertices) *)
val t_add_vtx_3 : cfgcon_ctr -> (t * 'a) -> (t * (vertex * vertex * vertex))
val t_add_vtx_4 : cfgcon_ctr -> (t * 'a) -> (t * (vertex * vertex * vertex * vertex))
val t_add_vtx_5 : cfgcon_ctr -> (t * 'a) -> (t * (vertex * vertex * vertex * vertex * vertex))

val t_add_edg   : (vertex * vertex)       -> (t * 'a) -> (t * vertex)                           (* t_add_edg (v1, v2) (cfg, _) = (cfg-(v1->v2)-updated, v2 *)
val t_add_edgs  : (vertex * vertex) list  -> (t * 'a) -> (t * (vertex list))                    (* t_add_edg (v1, v2)-list (cfg, _) = (cfg-(edges)-updated, v2-list) *)
val t_add_tedg  : (vertex * vertex)       -> (t * 'a) -> (t * vertex)
val t_add_fedg  : (vertex * vertex)       -> (t * 'a) -> (t * vertex)
val t_add_fail_edg : (vertex * vertex)    -> (t * 'a) -> (t * vertex)
val t_add_cskip_edg : (vertex * vertex)    -> (t * 'a) -> (t * vertex)

val t_add_vinfo   : ?errtrace:string  -> (vertex * stmt)      -> (t * 'a) -> (t * vertex)
val t_add_vinfos  : ?errtrace:string -> (vertex * stmt) list  -> (t * 'a) -> (t * (vertex list))
val t_add_vinfo_now : ?errtrace:string -> stmt -> (t * vertex) -> (t * vertex)

val t_add_tinfo   : ?errtrace:string  -> (string * typ)       -> (t * 'a) -> (t * string)
val t_add_tinfos  : ?errtrace:string -> (string * typ) list   -> (t * 'a) -> (t * (string list))

(* add new variable in cfg *)
val t_add_nv_tinfo : ?errtrace:string -> cfgcon_ctr -> typ -> (t * 'a) -> (t * string)          (* t_add_nv_tinfo errtrace type (cfg, _) = (cfg-tinfo-updated, added-new-var-name) *)

(* connect vertex *)
val t_con_vtx_front    : vertex -> (t * vertex) -> (t * vertex)  (* t_con_vtx_front   v1 (cfg, v2) = (cfg-(v1->v2)-updated, v2) *) (* connect vertex *)
val t_con_vtx_back     : vertex -> (t * vertex) -> (t * vertex)  (* t_con_vtx_back    v2 (cfg, v1) = (cfg-(v1->v2)-updated, v2) *)
val t_con_vtx_frontr   : vertex -> (t * vertex) -> (t * vertex)  (* t_con_vtx_frontr  v1 (cfg, v2) = (cfg-(v1->v2)-updated, v1) *)
val t_con_vtx_backr    : vertex -> (t * vertex) -> (t * vertex)  (* t_con_vtx_backr   v2 (cfg, v1) = (cfg-(v1->v2)-updated, v1) *)
val t_con_vtx_front_t  : vertex -> (t * vertex) -> (t * vertex)  (* If_true edge *)
val t_con_vtx_back_t   : vertex -> (t * vertex) -> (t * vertex)
val t_con_vtx_frontr_t : vertex -> (t * vertex) -> (t * vertex)
val t_con_vtx_backr_t  : vertex -> (t * vertex) -> (t * vertex)
val t_con_vtx_front_f  : vertex -> (t * vertex) -> (t * vertex)  (* If_false edge *)
val t_con_vtx_back_f   : vertex -> (t * vertex) -> (t * vertex)
val t_con_vtx_frontr_f : vertex -> (t * vertex) -> (t * vertex)
val t_con_vtx_backr_f  : vertex -> (t * vertex) -> (t * vertex)

(* add new id->lambda mapping info in cfg *)
val t_add_lmbdim  : ?errtrace:string -> (lambda_ident * lambda_summary) -> (t * 'a) -> (t * lambda_ident)

(* add new fail vertex information in cfg *)
val t_add_failvtx : vertex -> (t * 'a) -> (t * vertex)


(*****************************************************************************)
(*****************************************************************************)
(* Optimization                                                              *)
(*****************************************************************************)
(*****************************************************************************)

(* simple optimization - remove meaningless skip node *)
(* meaningless skip node has only one in-degree and one out-degree and both edges are "Normal" *)
(* WARNING : this does not remove any vertex-information in Cfg.t *)
val remove_meaningless_skip_vertices : t -> t
val remove_meaningless_skip_vertices_fixpoint : t -> t

(* simple optimization - remove meaningless fail nodes *)
(* meaningless fail node has one predecessor and one successor, where the predecessor is fail node too. *)
val remove_meaningless_fail_vertices : t -> t
val remove_meaningless_fail_vertices_fixpoint : t -> t


(*****************************************************************************)
(*****************************************************************************)
(* Print                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

val cfg_to_dotformat : t -> string