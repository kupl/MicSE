exception Exn_Cfg of string

let fail s = raise (Exn_Cfg s)

(*****************************************************************************)
(*****************************************************************************)
(* Graph (Flow)                                                              *)
(*****************************************************************************)
(*****************************************************************************)

type vertex = int
type edge_label = | Normal | If_true | If_false | Failed

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

let is_main_entry : t -> vertex -> bool
=fun cfg vtx -> (vtx = cfg.main_entry)

let is_main_exit : t -> vertex -> bool
=fun cfg vtx -> (vtx = cfg.main_exit)

let string_of_ident : ident -> string
=fun id -> id


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


let remove_meaningless_skip_vertices =
  let gen_emsg s : string = ("remove_meaningless_skip_vertices : " ^ s) in
  let is_skip : stmt option -> bool = (function | Some (Tezla_cfg.Cfg_node.Cfg_skip) -> true | _ -> false) in
  (* FUNCTION BEGIN *)
  fun cfg -> begin
    let fold_func : G.V.t -> ((vertex * vertex * vertex) list * (vertex Core.Set.Poly.t)) -> ((vertex * vertex * vertex) list * (vertex Core.Set.Poly.t))
    =fun v (vvvlist, vset) -> begin
      if ((Core.Map.Poly.find cfg.vertex_info v |> is_skip) && (G.out_degree cfg.flow v = 1) && (G.in_degree cfg.flow v = 1))
      then (
        (* Because of if-condition, pred and succ will return singleton list. *)
        let (in_v, in_label, mid_v) = G.pred_e cfg.flow v |> Core.List.hd_exn in
        let (mid_v_2, out_label, out_v) = G.succ_e cfg.flow v |> Core.List.hd_exn in
        if (  (mid_v = mid_v_2)
              && (in_label = Normal)
              && (out_label = Normal)
              && (Core.Set.Poly.for_all vset (fun x -> x <> in_v))
              && (Core.Set.Poly.for_all vset (fun x -> x <> mid_v))
              && ((Core.Set.Poly.for_all vset (fun x -> x <> out_v)))
          )
        then ( (in_v, mid_v, out_v) :: vvvlist, Core.Set.add vset v )
        else ( vvvlist, vset )
      )
      else ( vvvlist, vset )
    end in
    let (vvvl, _) = G.fold_vertex fold_func cfg.flow ([], Core.Set.Poly.empty) in
    let optfunc fl (in_v, mid_v, out_v) = begin
      let fl_1 = G.remove_edge fl in_v mid_v in
      let fl_2 = G.remove_edge fl_1 mid_v out_v in
      let fl_3 = G.remove_vertex fl_2 mid_v in
      (G.add_edge fl_3 in_v out_v)
    end in
    let newflow = Core.List.fold vvvl ~init:(cfg.flow) ~f:optfunc in
    {cfg with flow=newflow;}
  end

let rec remove_meaningless_skip_vertices_fixpoint cfg =
  let sz   = G.nb_vertex cfg.flow in
  let cfg' = remove_meaningless_skip_vertices cfg in
  let sz'  = G.nb_vertex cfg'.flow in
  if sz <> sz' then remove_meaningless_skip_vertices_fixpoint cfg' else cfg'


(*****************************************************************************)
(*****************************************************************************)
(* Print                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

let cfg_to_dotformat : t -> string
= let only_label_str s : string = ( "label=\"" ^ s ^ "\"" ) in
  let get_lhs_varname : stmt -> string = begin function
    | Cfg_assign (x, _) -> x
    | _ -> fail "cfg_to_dotformat : get_lhs_varname : match failed"
  end in
  (* FUNCTION BEGIN *)
  fun cfg -> begin
  (* flow *)
  let flow_fold_func : G.E.t -> string list -> string list
  =fun (in_v, e_label, out_v) acc -> begin
    let body_s = (string_of_int in_v) ^ " -> " ^ (string_of_int out_v) in
    let edge_s = match e_label with | Normal -> "" | If_true -> "[label=\"True\"]" | If_false -> "[label=\"False\"]" | Failed -> "[label=\"Failed\", style=dotted]" in
    (body_s ^ " " ^ edge_s ^ ";") :: acc
  end in
  let flow_s = begin
    G.fold_edges_e flow_fold_func cfg.flow []
    |> List.map (fun x -> "    " ^ x)
    |> String.concat "\n"
  end in
  (* each vertex *)
  let vi_fold_func : G.V.t -> string list -> string list
  =fun v acc -> begin
    let vs = string_of_int v in
    let is_main_entry = v = cfg.main_entry in
    let is_main_exit  = v = cfg.main_exit  in
    let vi : stmt = t_map_find ~errtrace:"cfg_to_dotformat : vi_fold_func : vi" cfg.vertex_info v in
    let lb_str : string = 
      if is_main_entry then (vs ^ " : MAIN-ENTRY")
      else ( 
        if is_main_exit then (let vn = get_lhs_varname vi in (vs ^ " : " ^ vn ^ " : MAIN-EXIT"))
        else (
          let vi_wrapped : TezlaCfg.Node.t = TezlaCfg.Node.create_node ~id:(-1) vi in
          let vis : string = TezlaCfg.Node.to_string vi_wrapped in
          (vs ^ " : " ^ vis)
        )
      )
    in
    if (is_main_entry || is_main_exit) then (vs ^ " [shape=doubleoctagon, " ^ (only_label_str lb_str) ^ "];") :: acc
    else (
      match vi with
      | Cfg_if _ | Cfg_if_none _ | Cfg_if_left _ | Cfg_if_cons _
      | Cfg_loop _ | Cfg_loop_left _ | Cfg_map _ | Cfg_iter _
        -> (vs ^ " [shape=diamond, " ^ (only_label_str lb_str) ^ "];") :: acc
      | Cfg_assign _
        -> (vs ^ " [shape=box, " ^ (only_label_str lb_str) ^ "];") :: acc
      | Cfg_failwith _
        -> (vs ^ " [shape=cds, " ^ (only_label_str lb_str) ^ "];") :: acc
      | _ -> (vs ^ " [" ^ (only_label_str lb_str) ^ "];") :: acc
    )
  end in
  let vi_s = begin
    G.fold_vertex vi_fold_func cfg.flow []
    |> List.map (fun x -> "    " ^ x)
    |> String.concat "\n"
  end in
  (* entire graph *)
  let main_s = begin
    "digraph G {\n"
    ^ flow_s
    ^ "\n"
    ^ vi_s
    ^ "\n}"
  end in
  main_s
end
