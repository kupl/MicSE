


(*****************************************************************************)
(*****************************************************************************)
(* Graph (Flow)                                                              *)
(*****************************************************************************)
(*****************************************************************************)

type vertex = int
type edge_label = | Normal | If_true | If_false

module V : Graph.Sig.COMPARABLE with type t = vertex         (* VERTEX *)
module E : Graph.Sig.ORDERED_TYPE_DFT with type t = edge_label   (* EDGE LABLE *)
module G : module type of Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (V) (E)

val is_edge_normal : E.t -> bool

val is_edge_true : E.t -> bool

val is_edge_false : E.t -> bool

val string_of_vertex : vertex -> string

(*****************************************************************************)
(*****************************************************************************)
(* Node Information                                                          *)
(*****************************************************************************)
(*****************************************************************************)

(* loc, ident, decl, typ, expr, stmt will be modified *)
type loc = Unknown | Loc of int * int
type ident = string
type decl = ident
type typ = Tezla.Adt.typ
type expr = Tezla.Adt.expr
type stmt = TezlaCfg.Node.stmt
  (*
  type stmt =
  | Cfg_assign of string * expr
  | Cfg_skip
  | Cfg_drop of string list
  | Cfg_swap
  | Cfg_dig
  | Cfg_dug
  | Cfg_if of string
  | Cfg_if_none of string
  | Cfg_if_left of string
  | Cfg_if_cons of string
  | Cfg_loop of string
  | Cfg_loop_left of string
  | Cfg_map of string
  | Cfg_iter of string
  | Cfg_failwith of string
  *)


(*****************************************************************************)
(*****************************************************************************)
(* Context Flow Graph                                                        *)
(*****************************************************************************)
(*****************************************************************************)

module CPMap : module type of Core.Map.Poly

type t = {
  flow : G.t;
  vertex_info : (int, stmt) CPMap.t;  (* vertex-number -> stmt *)
  type_info : (string, typ) CPMap.t;  (* variable-name -> typ *)  
  main_entry : vertex;
  main_exit : vertex;
}

val param_storage_name : string

val read_stmt_from_vtx : t -> vertex -> stmt

val read_succ_from_vtx : t -> vertex -> (E.t * V.t) list

val is_main_entry : t -> vertex -> bool

val is_main_exit : t -> vertex -> bool

val string_of_ident : ident -> string


(*****************************************************************************)
(*****************************************************************************)
(* Graph Utilities                                                           *)
(*****************************************************************************)
(*****************************************************************************)

type cfgcon_ctr = { (* counter for cfg construction *)
  vertex_counter : vertex ref;
  var_counter : vertex ref;
}

val new_vtx : cfgcon_ctr -> Core.Int.t
val new_var : cfgcon_ctr -> Core.String.t

val t_map_add : ?errtrace:string -> ('k, 'v) CPMap.t -> 'k -> 'v -> ('k, 'v) CPMap.t
val t_map_find : ?errtrace:string -> ('k, 'v) CPMap.t -> 'k -> 'v

val vtx_add : vertex -> t-> t
val edg_add : (vertex * vertex) -> t-> t
val tedg_add : (vertex * vertex) -> t -> t
val fedg_add : (vertex * vertex) -> t -> t

val t_add_vtx   : cfgcon_ctr -> (t * 'a) -> (t * vertex)                                        (* t_add_vtx counter (cfg, _) = (cfg-flow-updated, created-vertex) *)
val t_add_vtx_2 : cfgcon_ctr -> (t * 'a) -> (t * (vertex * vertex))                             (* t_add_vtx_2 (cfg, _) = (cfg-flow-updated, created-vertices) *)
val t_add_vtx_3 : cfgcon_ctr -> (t * 'a) -> (t * (vertex * vertex * vertex))
val t_add_vtx_4 : cfgcon_ctr -> (t * 'a) -> (t * (vertex * vertex * vertex * vertex))
val t_add_vtx_5 : cfgcon_ctr -> (t * 'a) -> (t * (vertex * vertex * vertex * vertex * vertex))

val t_add_edg   : (vertex * vertex)       -> (t * 'a) -> (t * vertex)                           (* t_add_edg (v1, v2) (cfg, _) = (cfg-(v1->v2)-updated, v2 *)
val t_add_edgs  : (vertex * vertex) list  -> (t * 'a) -> (t * (vertex list))                    (* t_add_edg (v1, v2)-list (cfg, _) = (cfg-(edges)-updated, v2-list) *)
val t_add_tedg  : (vertex * vertex)       -> (t * 'a) -> (t * vertex)
val t_add_fedg  : (vertex * vertex)       -> (t * 'a) -> (t * vertex)

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


(*****************************************************************************)
(*****************************************************************************)
(* Print                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

val cfg_to_dotformat : t -> string
