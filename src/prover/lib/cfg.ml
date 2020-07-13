exception Exn_Cfg of string

let fail s = raise (Exn_Cfg s)

(*****************************************************************************)
(*****************************************************************************)
(* Graph (Flow)                                                              *)
(*****************************************************************************)
(*****************************************************************************)

type vertex = int
type edge_label = | Normal | If_true | If_false

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

let string_of_vertex : vertex -> string
=fun vtx -> (string_of_int vtx)


(*****************************************************************************)
(*****************************************************************************)
(* Node Information                                                          *)
(*****************************************************************************)
(*****************************************************************************)

type loc = Unknown | Loc of int * int
type ident = string
type decl = ident
type typ = Tezla.Adt.typ
type expr = Tezla.Adt.expr
type stmt = TezlaCfg.Node.stmt

(*module IntMap = Core.Map.Make (Core.Int)
module StringMap = Core.Map.Make (Core.String)*)
module CPMap = Core.Map.Poly

type t = {
  flow : G.t;
  vertex_info : (int, stmt) CPMap.t;  (* vertex-number -> stmt *)
  type_info : (string, typ) CPMap.t;  (* variable-name -> typ *)
  main_entry : vertex;
  main_exit : vertex;
}

let param_storage_name = "param_storage"

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

let is_main_entry : t -> vertex -> bool
=fun cfg vtx -> (vtx = cfg.main_entry)

let is_main_exit : t -> vertex -> bool
=fun cfg vtx -> (vtx = cfg.main_exit)

let string_of_ident : ident -> string
=fun id -> id


(*****************************************************************************)
(*****************************************************************************)
(* Utilities                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

type cfgcon_ctr = { (* counter for cfg construction *)
  vertex_counter : vertex ref;
  var_counter : vertex ref;
}

let new_vtx : cfgcon_ctr -> Core.Int.t = fun c -> (incr c.vertex_counter; !(c.vertex_counter))
let new_var : cfgcon_ctr -> Core.String.t = fun c -> (incr c.var_counter; "v" ^ (string_of_int (!(c.var_counter))))

let vtx_add : vertex -> t -> t = begin fun v cfg -> {cfg with flow=(G.add_vertex cfg.flow v);} end
let edg_add : (vertex * vertex) -> t -> t = begin fun (v1, v2) cfg -> {cfg with flow=(G.add_edge cfg.flow v1 v2);} end
let tedg_add : (vertex * vertex) -> t -> t = begin fun (v1, v2) cfg -> let e = G.E.create v1 If_true v2 in {cfg with flow=(G.add_edge_e cfg.flow e);} end
let fedg_add : (vertex * vertex) -> t -> t = begin fun (v1, v2) cfg -> let e = G.E.create v1 If_false v2 in {cfg with flow=(G.add_edge_e cfg.flow e);} end

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

(* tip: use it with t_add_nv_tinfo *)
let t_con_edg v1 (cfg, v2) = t_add_edg (v1, v2) (cfg, ())
let t_con_tedg v1 (cfg, v2) = t_add_tedg (v1, v2) (cfg, ())
let t_con_fedg v1 (cfg, v2) = t_add_fedg (v1, v2) (cfg, ())

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
