open Cfg

(*****************************************************************************)
(*****************************************************************************)
(* Optimization                                                              *)
(*****************************************************************************)
(*****************************************************************************)

let remove_meaningless_skip_vertices =
  (*let gen_emsg s : string = ("remove_meaningless_skip_vertices : " ^ s) in*)
  let is_skip : stmt option -> bool = (function | Some Cfg_skip -> true | _ -> false) in
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
              && (in_label = Normal || in_label = If_true || in_label = If_false)
              && (out_label = Normal)
              && (Core.Set.Poly.for_all vset ~f:(fun x -> x <> in_v))
              && (Core.Set.Poly.for_all vset ~f:(fun x -> x <> mid_v))
              && (Core.Set.Poly.for_all vset ~f:(fun x -> x <> out_v))
          )
        then ( (in_v, mid_v, out_v) :: vvvlist, Core.Set.add vset v )
        else ( vvvlist, vset )
      )
      else ( vvvlist, vset )
    end in
    let (vvvl, _) = G.fold_vertex fold_func cfg.flow ([], Core.Set.Poly.empty) in
    let optfunc fl (in_v, mid_v, out_v) = begin
      let (_, in_label, _) = G.pred_e fl mid_v |> Core.List.hd_exn in
      let fl_1 = G.remove_edge fl in_v mid_v in
      let fl_2 = G.remove_edge fl_1 mid_v out_v in
      let fl_3 = G.remove_vertex fl_2 mid_v in
      (G.add_edge_e fl_3 (in_v, in_label, out_v))
    end in
    let newflow = Core.List.fold vvvl ~init:(cfg.flow) ~f:optfunc in
    {cfg with flow=newflow;}
  end

let rec remove_meaningless_skip_vertices_fixpoint cfg =
  let sz   = G.nb_vertex cfg.flow in
  let cfg' = remove_meaningless_skip_vertices cfg in
  let sz'  = G.nb_vertex cfg'.flow in
  if sz <> sz' then remove_meaningless_skip_vertices_fixpoint cfg' else cfg'

let remove_meaningless_fail_vertices =
  (*let gen_emsg s : string = ("remove_meaningless_fail_vertices : " ^ s) in*)
  let is_fail : stmt option -> bool = (function | Some (Cfg_failwith _) -> true | _ -> false) in
  (* FUNCTION BEGIN *)
  fun cfg -> begin
    let fold_func : G.V.t -> ((vertex * vertex * vertex) list * (vertex Core.Set.Poly.t)) -> ((vertex * vertex * vertex) list * (vertex Core.Set.Poly.t))
    =fun v (vvvlist, vset) -> begin
      if ((Core.Map.Poly.find cfg.vertex_info v |> is_fail) && (G.out_degree cfg.flow v = 1) && (G.in_degree cfg.flow v = 1))
      then (
        (* Because of if-condition, pred will return singleton list. *)
        let (in_v, in_label, mid_v) = G.pred_e cfg.flow v |> Core.List.hd_exn in
        let (mid_v_2, out_label, out_v) = G.succ_e cfg.flow v |> Core.List.hd_exn in
        if (  (mid_v = mid_v_2)
              && (in_label = Failed)
              && (out_label = Failed)
              && (Core.Set.Poly.for_all vset ~f:(fun x -> x <> in_v))
              && (Core.Set.Poly.for_all vset ~f:(fun x -> x <> mid_v))
              && (Core.Set.Poly.for_all vset ~f:(fun x -> x <> out_v))
        )
        then ( (in_v, mid_v, out_v) :: vvvlist, Core.Set.add vset v)
        else ( vvvlist, vset )
      )
      else ( vvvlist, vset )
    end in
    let (vvvl, _) = G.fold_vertex fold_func cfg.flow ([], Core.Set.Poly.empty) in
    let optfunc fl (in_v, mid_v, out_v) = begin
      let fl_1 = G.remove_edge fl in_v mid_v in
      let fl_2 = G.remove_edge fl_1 mid_v out_v in
      let fl_3 = G.remove_vertex fl_2 mid_v in
      let failedge = G.E.create in_v Failed out_v in
      (G.add_edge_e fl_3 failedge)
    end in
    let newflow = Core.List.fold vvvl ~init:(cfg.flow) ~f:optfunc in
    {cfg with flow=newflow;}
  end

let rec remove_meaningless_fail_vertices_fixpoint cfg = 
  let sz    = G.nb_vertex cfg.flow in
  let cfg'  = remove_meaningless_fail_vertices cfg in
  let sz'   = G.nb_vertex cfg'.flow in
  if sz <> sz' then remove_meaningless_fail_vertices_fixpoint cfg' else cfg'


let remove_simple_stack_operation_vertices : t -> t
= let naive_merge_edge_label : (edge_label * edge_label) -> (bool * edge_label) =
  ( function
    | Normal, Normal -> (true, Normal)
    | If_true, Normal -> (true, If_true)
    | If_false, Normal -> (true, If_false)
    | Failed, Normal -> (true, Failed)
    | _ -> (false, Normal)
  )
  in
  (* FUNCTION BEGIN *)
  fun cfg ->
  let is_ssop : stmt -> bool = (function | Cfg_drop _ | Cfg_swap | Cfg_dig | Cfg_dug -> true | _ -> false) in
  G.fold_vertex 
    (fun v acc_cfg ->
      let nvi : (vertex, stmt) Core.Map.Poly.t = Core.Map.Poly.update acc_cfg.vertex_info v ~f:(function | _ -> Cfg_skip) in
      let cfg_elsebr : t = {acc_cfg with vertex_info=nvi} in
      let is_ssop_v : bool = t_map_find ~errtrace:("cfgUtil : remove_simple_stack_operation_vertices : if") cfg.vertex_info v |> is_ssop in
      if is_ssop_v
      then (
        let pel : G.edge list = G.pred_e acc_cfg.flow v in
        let sel : G.edge list = G.succ_e acc_cfg.flow v in
        if (List.length pel = 1 && List.length sel = 1)
        then (
          let ((pv, pl, _), (_, sl, sv)) : (G.edge * G.edge) = List.hd pel, List.hd sel in
          let (flag, lbl) = naive_merge_edge_label (pl, sl) in
          if flag
          then ({acc_cfg with flow=(G.remove_vertex (G.add_edge_e acc_cfg.flow (pv, lbl, sv)) v);})
          else (cfg_elsebr)
        )
        else (cfg_elsebr)
      )
      else (acc_cfg)
    )
    cfg.flow
    cfg


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
    (* edge label and style *)
    let edge_s = match e_label with | Normal -> "" | If_true -> "[label=\"True\"]" | If_false -> "[label=\"False\"]" | Failed -> "[label=\"Failed\", style=dotted]" | If_skip -> "[label=\"If_skip\", style=dotted]" | Loop_skip -> "[label=\"Loop_skip\", style=dotted]" | Check_skip -> "[label=\"Check_skip\", style=dotted]" in
    (body_s ^ " " ^ edge_s ^ ";") :: acc
  end in
  let flow_s = begin
    G.fold_edges_e flow_fold_func cfg.flow []
    |> List.map (fun x -> "    " ^ x)
    |> String.concat "\n"
  end in
  (* before enter "each vertex", convert lambda_id_map into two maps to search easily *)
  let (lmbd_entry_map, lmbd_exit_map) = lmbd_map_to_two_sets cfg in
  (* each vertex *)
  let vi_fold_func : G.V.t -> string list -> string list
  =fun v acc -> begin
    let vs = string_of_int v in
    let is_main_entry = v = cfg.main_entry in
    let is_main_exit  = v = cfg.main_exit  in
    let vi : stmt = t_map_find ~errtrace:"cfg_to_dotformat : vi_fold_func : vi" cfg.vertex_info v in
    (* vertex label-string *)
    let lb_str : string = 
      if is_main_entry then (vs ^ " : MAIN-ENTRY")  (* The contents of the MAIN_ENTRY are expected to be Cfg_skip *)
      else ( 
        if is_main_exit then (let vn = get_lhs_varname vi in (vs ^ " : MAIN-EXIT : " ^ vn))  (* The contents of the MAIN_EXIT are expected to be Cfg_assign (v_i, (E_itself v_i)) *)
        else (
          match (CPMap.find lmbd_entry_map v, CPMap.find lmbd_exit_map v) with
          | Some _, Some _ -> fail "cfg_to_dotformat : vi_fold_func : lb_str : cpmap.find : both found : cannot be reached in normal situation."
          (* lambda entry/exit vertices *)
          | Some (id, _), None -> (vs ^ " : LAMBDA-" ^ (string_of_int id) ^ "-ENTRY")   (* The contents of the MAIN_ENTRY are expected to be Cfg_skip *)
          | None, Some (id, _) -> let vn = get_lhs_varname vi in (vs ^ " : LAMBDA-" ^ (string_of_int id) ^ "-EXIT : " ^ vn) (* The contents of the MAIN_EXIT are expected to be Cfg_assign (v_i, (E_itself v_i)) *)
          (* otherwise - default vertex label *)
          | None, None ->
            let vis : string = stmt_to_str vi in
            (vs ^ " : " ^ vis)
        )
      )
    in
    (* vertex shape *)
    if (is_main_entry || is_main_exit) then (vs ^ " [shape=doubleoctagon, " ^ (only_label_str lb_str) ^ "];") :: acc
    else (
      match (CPMap.find lmbd_entry_map v, CPMap.find lmbd_exit_map v) with
      | Some _, Some _ -> fail "cfg_to_dotformat : vi_fold_func : cpmap.find : both found : cannot be reached in normal situation."
      (* lambda entry/exit vertices *)
      | Some _, None
      | None, Some _ -> (vs ^ " [shape=octagon, " ^ (only_label_str lb_str) ^ "];") :: acc
      (* otherwise - default vertex shape *)
      | None, None -> (
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


(*****************************************************************************)
(*****************************************************************************)
(* Loop Unrolling                                                            *)
(*****************************************************************************)
(*****************************************************************************)

module LoopUnrolling = struct
  (* sugar *)
  module CPSet = Core.Set.Poly
  module CPMap = Core.Map.Poly

  let _null_vertex : vertex = (-1)

  (* "loopnest_info" : make loop-nesting information *)
  (* input : UNOPTIMIZED cfg. *)
  (* return : (vtx -> vtxlist) map. It means that the loop-vertex(key) contains other loop-vertices(value). *)
  type lni_dfs_param = {
    lni_worklist :  G.edge list;
    lni_acc_map : (vertex, vertex list) CPMap.t;  
    lni_visited : vertex CPSet.t;
  }
  let loopnest_info : t -> (vertex, vertex CPSet.t) CPMap.t
  =fun cfg -> begin
    let errmsg_gen s : string = ("cfgutil.ml : LoopUnrolling : loopnest_info : " ^ s) in
    (* 1. DFS - It collects that the given vertex-n is in the loops [v1; v2; ...] *)
    let rec dfs : lni_dfs_param -> lni_dfs_param
    =fun p -> begin
      match p.lni_worklist with
      | [] -> p
      | (src, label, dst) :: wl_t ->
        if CPSet.mem p.lni_visited dst then dfs {p with lni_worklist=wl_t} else
        let src_stmt = t_map_find ~errtrace:(errmsg_gen "dfs : src_stmt") cfg.vertex_info src in
        (*let dst_stmt = t_map_find ~errtrace:(errmsg_gen "dfs : dst_stmt") cfg.vertex_info dst in*)
        let src_lst = CPMap.find p.lni_acc_map src |> (function None -> [] | Some l -> l) in
        let looplist : vertex list = 
          ( match src_stmt with
            | Cfg_loop _ | Cfg_loop_left _ | Cfg_iter _ | Cfg_map _ -> (
                match label with
                | If_true -> src :: src_lst
                | _ -> src_lst
              )
            | _ -> src_lst
          )
        in
        let updated_map : (vertex, vertex list) CPMap.t = CPMap.update p.lni_acc_map dst ~f:(fun _ -> looplist) in
        let dst_succ_e : G.edge list = G.succ_e cfg.flow dst in
        dfs {lni_worklist=(dst_succ_e @ wl_t); lni_acc_map=updated_map; lni_visited=(CPSet.add p.lni_visited dst)}
      (*
      (match p.lni_worklist with
      | [] -> p
      | (src, label, dst) :: wl_t -> if CPMap.mem p.lni_acc_map dst then dfs {p with lni_worklist=wl_t} else
        if CPSet.mem p.lni_visited dst then dfs {p with lni_worklist=wl_t} else
        let src_stmt = t_map_find ~errtrace:(errmsg_gen "dfs : src_stmt") cfg.vertex_info src in
        let dst_stmt = t_map_find ~errtrace:(errmsg_gen "dfs : dst_stmt") cfg.vertex_info dst in
        let baselist : vertex list = (
          let _ : unit = print_endline ("DEBUG : loopnest_info : dfs : edge = (" ^ (string_of_int src) ^ ", " ^ (string_of_int dst) ^ ")") in
          (*let b_aselist = t_map_find ~errtrace:(errmsg_gen "dfs : b_aselist") p.lni_acc_map src in*)
          let b_aselist = CPMap.find p.lni_acc_map src |> (function None -> [] | Some l -> l) in
          match src_stmt with
          | Cfg_loop _ | Cfg_loop_left _ | Cfg_iter _ | Cfg_map _ -> (match label with | If_false -> List.tl b_aselist | _ -> b_aselist)
          | _ -> b_aselist)
        in
        let looplist : vertex list = (
          match dst_stmt with
          | Cfg_loop _ | Cfg_loop_left _ | Cfg_iter _ | Cfg_map _ -> (dst :: baselist)
          | _ -> baselist)
        in
        let updated_map : (vertex, vertex list) CPMap.t = CPMap.update p.lni_acc_map dst ~f:(fun _ -> looplist) in (* update p.lni_acc_map using (dst -> looplist) *)
        let dst_succ_e : G.edge list = G.succ_e cfg.flow dst in
        dfs {lni_worklist=(dst_succ_e @ wl_t); lni_acc_map=updated_map; lni_visited=(CPSet.add p.lni_visited dst)} 
      )
      *)
    end in
    let entry_edges : G.edge list =
      let lambda_entry_e : G.edge list = CPMap.fold cfg.lambda_id_map ~init:[] ~f:(fun ~key:_ ~data:(lambda_entry, _, _, _) vlst -> (G.succ_e cfg.flow lambda_entry) @ vlst) in
      (G.succ_e cfg.flow cfg.main_entry) @ lambda_entry_e
    in
    let {lni_acc_map=loopmap; _} : lni_dfs_param = dfs {lni_worklist=entry_edges; lni_acc_map=CPMap.empty; lni_visited=CPSet.empty} in
    (*let _ = print_endline ("DEBUG : loopnest_info : loopmap = {" ^ (CPMap.fold loopmap ~init:"" ~f:(fun ~key:k ~data:d acc_str -> acc_str ^ "; " ^ (string_of_int k) ^ ": (" ^ (List.fold_left (fun acc x -> acc ^ ", " ^ (string_of_int x)) "" d) ^ ")" )) ^ "}") in*)
    (* 2. Collects loop-list *)
    (* these vertex lists are represented backward, e.g. [vtx-1; vtx-2] means that the vtx-1-loop located insided vtx-2-loop. *)
    let loop_dependencies : (vertex list) CPSet.t = CPMap.fold loopmap ~init:(CPSet.empty) ~f:(fun ~key:_ ~data:vl acc_set -> CPSet.add acc_set vl) in
    (*let _ : unit = print_endline ("DEBUG : loopnest_info : loop_dependencies size = " ^ (CPSet.length loop_dependencies |> string_of_int)) in *)
    (*let _ : unit = print_endline ("DEBUG : loopnest_info : loop_dependencies = " ^ (CPSet.fold loop_dependencies ~init:"" ~f:(fun accstr vl -> accstr ^ (List.fold_left (fun accstr v -> accstr ^ "," ^ (string_of_int v)) "} :: {" vl ) ) ^ "}" )) in*)
    (* 3. Construct loopnest-info *)
    (* "dependencies_fold" : collect all loop-nested information from the given vertex list, for example,
        "dependencies_fold ([vtx-1; vtx-2; vtx-3], EMPTY-MAP)" makes (vtx-3 -> {vtx-1; vtx-2}) and (vtx-2 -> {vtx-1})
    *)
    let rec dependencies_fold : vertex list * (vertex, vertex CPSet.t) CPMap.t -> (vertex, vertex CPSet.t) CPMap.t
    =fun (vl, acc_map) -> begin
      match vl with
      | [] -> acc_map
      | h :: t -> 
        let acc_map_listapplied = 
          let setupdate = function | Some s -> CPSet.add s h | None -> CPSet.singleton h in
          let new_map : (vertex, vertex CPSet.t) CPMap.t = List.fold_left (fun acc upper -> CPMap.update acc upper ~f:setupdate) acc_map t in
          CPMap.update new_map h ~f:(function | None -> CPSet.singleton _null_vertex | Some s -> s)
        in
        dependencies_fold (t, acc_map_listapplied)
    end in
    (* run "dependencies_fold" for every vertex-list in loop_dependencies, collect the result *)
    CPSet.fold loop_dependencies ~init:(CPMap.empty) ~f:(fun acc_map dep_lst -> dependencies_fold (dep_lst, acc_map)  )
  end (* function loopnest_info ends *)

  (* "tplg_sort" : topology sort*)
  module DepG = Graph.Persistent.Digraph.ConcreteBidirectional (V) (* loop-nested dependency simple graph. each node hash a vertex-id (refer module Cfg.V) *)
  type ts_dfs_param = {
    ts_graph : DepG.t;   (* loop-vertices dependency graph. "vtx1 -> vtx2" means that the loop vtx2 is in loop vtx1 *)
    ts_result : vertex list;   (* topology-sorted list. most-nested loop vertex will be located at the head of this list. *)
    ts_unvisited : vertex CPSet.t; (* the set of unvisited vertices, it helps ignoring visited vertices. *)
    ts_unvisited_top : vertex CPSet.t; (* the set of unvisited vertices, which has no pred-vertices. *)
    ts_stack : vertex list;    (* stack for Depth first search *)
  }
  let tplg_sort : (vertex, vertex CPSet.t) CPMap.t -> vertex list
  =fun p_map -> begin
    (* generate dependency graph *)
    let dependency_graph : DepG.t =
      let map_foldf : key:vertex -> data:(vertex CPSet.t) -> DepG.t -> DepG.t =
        fun ~key ~data dep_g -> CPSet.fold data ~init:dep_g ~f:(fun graph set_elem -> DepG.add_vertex (DepG.add_edge graph key set_elem) key)
      in
      CPMap.fold p_map ~init:DepG.empty ~f: map_foldf
    in
    (* dfs at the graph *)
    let rec dfs : ts_dfs_param -> ts_dfs_param =
      fun p -> 
      (match (CPSet.is_empty p.ts_unvisited_top), p.ts_stack with
      | _, st_hd :: st_tl ->
        if CPSet.mem p.ts_unvisited st_hd
        then dfs {p with ts_result=(st_hd :: p.ts_result); ts_unvisited=(CPSet.remove p.ts_unvisited st_hd); ts_stack=((DepG.succ p.ts_graph st_hd) @ st_tl)}
        else dfs {p with ts_stack=st_tl}
      | false, [] -> let newv : vertex = CPSet.choose_exn p.ts_unvisited_top in dfs {p with ts_unvisited_top=(CPSet.remove p.ts_unvisited_top newv); ts_stack=[newv]}
      | true, [] -> p
      )
    in
    let v_in_dgraph : vertex CPSet.t = DepG.fold_vertex (fun v accset -> CPSet.add accset v) dependency_graph CPSet.empty in
    let topv_in_dgraph : vertex CPSet.t = CPSet.filter v_in_dgraph ~f:(fun v -> DepG.pred dependency_graph v = []) in
    let dfs_return_val = dfs {ts_graph=dependency_graph; ts_result=[]; ts_unvisited=v_in_dgraph; ts_unvisited_top=topv_in_dgraph; ts_stack=[]} in
    (dfs_return_val.ts_result |> List.filter (fun x -> x <> _null_vertex))
  end (* function tplg_sort end *)


  module UnrollParam = struct
    (* environment *)
    type unroll_env = {
      env_entry_vtx : vertex;  (* unrolled loop seq start from here. *)
      env_exit_vtx : vertex;   (* unrolled loop seq ends here. *)
      env_vset : vertex CPSet.t;  (* vertices in unrolled-graph *)
      env_eset : G.edge CPSet.t;  (* edges in unrolled-graph *)
    }
    (* param-type *)
    type pt = {
      (* cfg : Cfg.t
        it is persistent data type, however, it will be updated for every new vertex&variable information created.
        For the aspect of clean data structure, it is useful to contain every unrolled loops and counters individually contained at unroll_body type,
        but it is convenient to update information at one place.
        In consequence, "counter" pulled out from "unroll_env" too, which supports monolithic cfg-construction.
      *)
      cfg : t;   (* cfg-workspace. it is persistent data type, and it will be updated a lot at the end of "unroll" function evaluation. *)
      counter : cfgcon_ctr;    (* counter for cfg-construction, IT SHOULD BE "Cfg.hardcopy_counter"-ED CFGCON_CTR FROM THE RETURN VALUE OF "Translator.adt_to_cfg_counter_included". *)
      unroll_num : int;    (* how many times to unroll every loop *)
      vtxrel : (vertex, vertex CPSet.t) CPMap.t;   (* relation between newly-created vertex and the vertices from original cfg. *)
      env : (vertex, unroll_env) CPMap.t;    (* pre-unrolled loop-vertex's information *)
    }
    (* paramter for folding *)
    type unroll_fold_f_param = {
      ptval : pt;    (* unroll_param from "unroll" function. it should be updated *)
      target_vtx : vertex;  (* target loop-vertex *)
      target_stmt : stmt;   (* "uff_target_vtx"'s stmt. *)
      unroll_count : int;   (* how many unrolling performed for now, it should be start with 0, and increasing to (ptval.unroll_num + 1) *)
                            (* for example, N="ptval.unroll_num", C="unroll_count", 
                                if C > N, stop unrolling,
                                if C = 0, Don't append body, but the only entry and exit things (e.g. DROP instruction at the end) and make C <- 1.
                                if C <= N, append body once AT THE FRONT OF UFF_CUR_ENTRY, and make C <- C+1 .
                            *)
      acc_unroll_env : unroll_env;  (* current graph accumulator. but other cfg-related information are not belongs to this value.
                                        final acc_unroll_env will be added at ptval.env
                                    *)
    }

    type copy_unrolled_cfg_param = {
      cuc_ptval : pt;
      cuc_loopvtx : vertex; (* loop to copy. it is the key value of pt.env *)
      cuc_entry : vertex; (* graph starts here. i.e. copied vertices will be appended from here. *)
      cuc_exit : vertex;  (* graph ends here. i.e. copied graph ends here. *)
    }

    type dfs_copy_cfg_param = {
      dcc_ptval : pt;
      (* every trace go through dcc_o_entry must go through dcc_o_exit too. *)
      dcc_o_entry : vertex; (* original graph start-vertex *)
      dcc_o_exit : vertex;  (* original graph exit-vertex *)
    }
    type dfs_copy_cfg_inner_param = {
      dcci_visited : vertex CPSet.t;
      dcci_stack : vertex list;
      (* entry, exit vertices and cfg are already given at the dfs_copy_cfg_param. *)
    }
    
  end (* module UnrollParam ends *)


  (* "copy_unrolled_cfg_param"
    [precondition]
    - cuc_entry and cuc_exit should be enrolled at the graph before.
  *)
  let copy_unrolled_cfg : UnrollParam.copy_unrolled_cfg_param -> (UnrollParam.pt * (vertex CPSet.t) * (G.edge CPSet.t))
  = let open UnrollParam in
    let emsg_gen s : string = ("cfgUtil.LoopUnrolling.copy_unrolled_cfg : " ^ s) in
    (* FUNCTION BEGIN *)
    fun p -> begin
    let ptval = p.cuc_ptval in
    (*let _ = print_endline ("DEBUG : copy_unrolled_cfg : p.cuc_loopvtx = " ^ (string_of_int p.cuc_loopvtx)) in*)
    let uenv : unroll_env = t_map_find ~errtrace:(emsg_gen "uenv") ptval.env p.cuc_loopvtx in
    (* add vertices *)
    (* new_ptval : updated pt. cfg(flow, vertex-info) and vtxrel will be changed. counter will be updated autmoatically.
      new_vertices : newly created vertices. need it to construct the return value.
      vtxrel_i : association list of (vertex in uenv, newly created vertex). need it for copying edges in cfg flow later.
      new_entry_opt : newly created entry vertex
      new_exit_opt : newly created exit vertex
    *)
    let (new_ptval, new_vertices, vtxrel_i, new_entry_opt, new_exit_opt) : pt * (vertex CPSet.t) * ((vertex * vertex) list) * (vertex option) * (vertex option) = 
      CPSet.fold 
        uenv.env_vset 
        ~init:(ptval, CPSet.empty, [], None, None) 
        ~f:(fun (acc_ptval, acc_vset, acc_vrel, acc_entry_opt, acc_exit_opt) v ->
          let vstmt : stmt = t_map_find ~errtrace:(emsg_gen "vstmt") acc_ptval.cfg.vertex_info v in
          let nv = new_vtx ptval.counter in
          let ncfg : t = {acc_ptval.cfg with
            flow = G.add_vertex acc_ptval.cfg.flow nv;
            vertex_info = t_map_add ~errtrace:(emsg_gen "ncfg : vertex_info") acc_ptval.cfg.vertex_info nv vstmt;
          } in
          let nptval : pt = {acc_ptval with
            cfg=ncfg;
            vtxrel=(t_map_add ~errtrace:(emsg_gen "nptval : vtxrel") acc_ptval.vtxrel nv (t_map_find ~errtrace:(emsg_gen "nptval : vtxrel") acc_ptval.vtxrel v));
          } in
          let nentry : vertex option = (function | None -> if (v = uenv.env_entry_vtx) then Some nv else None | Some q -> Some q) acc_entry_opt in
          let nexit : vertex option = (function | None -> if (v = uenv.env_exit_vtx) then Some nv else None | Some q -> Some q) acc_exit_opt in
          (nptval, CPSet.add acc_vset nv, ((v, nv) :: acc_vrel), nentry, nexit)
        )
    in
    (* add edges in flow *)
    let (new_ptval_edges_added, new_edges) : pt * (G.edge CPSet.t) =
      CPSet.fold
        uenv.env_eset
        ~init:(new_ptval, CPSet.empty)
        ~f:(fun (acc_ptval, acc_eset) (src, el, dst) ->
          let src_n : vertex = List.assoc src vtxrel_i in
          let dst_n : vertex = List.assoc dst vtxrel_i in
          let nedg : G.edge = (src_n, el, dst_n) in
          ({new_ptval with cfg={acc_ptval.cfg with flow=(G.add_edge_e acc_ptval.cfg.flow nedg)}}, CPSet.add acc_eset nedg)
        )
    in
    (* add starting, ending edges in flow *)
    let (new_ptval_terminal_edges_added, new_edges_terminal_added) : pt * (G.edge CPSet.t) = 
      let entry_edg : G.edge = (p.cuc_entry, Normal, new_entry_opt |> Option.get) in
      let exit_edg : G.edge = (new_exit_opt |> Option.get, Normal, p.cuc_exit) in
      let edgs : G.edge CPSet.t = CPSet.add (CPSet.add new_edges entry_edg) exit_edg in
      let edgs_in_cfg : G.t = G.add_edge_e (G.add_edge_e new_ptval_edges_added.cfg.flow entry_edg) exit_edg in
      ({new_ptval_edges_added with cfg={new_ptval_edges_added.cfg with flow=edgs_in_cfg}}, edgs)
    in
    (new_ptval_terminal_edges_added, new_vertices, new_edges_terminal_added)
  end (* function copy_unrolled_cfg ends *)

  (* it copy partial-graph (between dcc_o_entry and dcc_o_exit, excluding dcc_o_exit) and updates pt's cfg, counter, vtxrel *)
  (* it considers that the loops between dcc_o_entry and dcc_o_exit were already unrolled before, so it will copy unrolled_cfg using the function "copy_unrolled_cfg" *)
  (* if you want to copy loop body, dcc_o_entry should be the first vertex of loop body, and dcc_o_exit should be the loop vertex *)
  (* similarly, be aware of that the dcc_o_exit WILL NOT BE COPIED in the new graph. *)
  (* return value explanation:
    - UnrollParam.pt : updated pt
    - vertex CPSet.t : newly created vertices
    - G.edge CPSet.t : newly created edges
    - vertex * ((vertex * edge_label) CPSet.t) : (copied dcc_o_entry) * (vertices and edge-labels which is clone of exit-vertex-pointing edges)
  *)
  let dfs_copy_cfg : UnrollParam.dfs_copy_cfg_param -> (UnrollParam.pt * (vertex CPSet.t) * (G.edge CPSet.t) * (vertex * ((vertex * edge_label) CPSet.t)))
  = let open UnrollParam in
    let emsg_gen s : string = ("cfgUtil.LoopUnrolling.dfs_copy_cfg : " ^ s) in
    (* FUNCTION BEGIN *)
    fun p -> begin
      (*let _ : unit = print_endline  ("DEBUG : dfs_copy_cfg : p.o_entry = " ^ (string_of_int p.dcc_o_entry) ^ ", p.o_exit = " ^ (string_of_int p.dcc_o_exit)) in*)
      (* 1. DFS - collects original-cfg's vertices between dcc_o_entry and dcc_o_exit *)
      (* Loop body are not collected *)
      let cfg_0 : t = p.dcc_ptval.cfg in
      let rec dfs : dfs_copy_cfg_inner_param -> dfs_copy_cfg_inner_param 
      =fun ip -> begin
        match ip.dcci_stack with
        | [] -> ip
        | hd_s :: tl_s ->
          if (CPSet.mem ip.dcci_visited hd_s) || (hd_s = p.dcc_o_exit) then dfs {ip with dcci_stack=tl_s} else
          (match (t_map_find ~errtrace:(emsg_gen "dfs : find hd_s stmt") cfg_0.vertex_info hd_s) with
            | Cfg_loop _ | Cfg_loop_left _ | Cfg_iter _ | Cfg_map _ -> 
              (* Insert only Non-True branch. *)
              let n_vset : vertex CPSet.t = CPSet.add ip.dcci_visited hd_s in
              let n_stack : vertex list = List.fold_left (fun acc (_, lbl, dst) -> if (lbl <> If_true) then (dst :: acc) else acc) tl_s (G.succ_e cfg_0.flow hd_s) in
              dfs {dcci_visited=n_vset; dcci_stack=n_stack}
            | _ ->
              let n_vset : vertex CPSet.t = CPSet.add ip.dcci_visited hd_s in
              let n_stack : vertex list = List.fold_left (fun acc (_, _, dst) -> dst :: acc) tl_s (G.succ_e cfg_0.flow hd_s) in
              dfs {dcci_visited=n_vset; dcci_stack=n_stack}
          )
      end in
      let {dcci_visited=o_vertices; _} = dfs {dcci_visited=CPSet.empty; dcci_stack=[p.dcc_o_entry]} in
      (* 2. copy vertices (update p.dcc_ptval) *)
      let add_vtx : (pt * vertex CPSet.t * (vertex * (vertex * vertex)) list * (G.edge CPSet.t)) -> vertex -> (pt * vertex CPSet.t * (vertex * (vertex * vertex)) list * (G.edge CPSet.t))
      =fun (acc_pt, acc_vset, acc_v3list, acc_eset) v -> begin
        (*  acc_pt : ptvalue to updates 
          acc_vset : the set of newly created vertices
          acc_v3list : association list of (original-vertex (v) -> (new vertex-entry, new vertex-exit))
                      new vertex-entry and new vertex-exit can be the same vertex,
                      however, if the original-vertex is loop vertex, this function will call the "copy_unrolled_cfg" function,
                      so new vertex-entry and new vertex-exit will not be same.
          acc_eset : the set of newly created edges (It will be updated only when loop vertices appears. Other edges which are not in loop will be introduced after add_vtx called.)
        *)
        let v_stmt : stmt = t_map_find ~errtrace:(emsg_gen "add_vtx : v_stmt") cfg_0.vertex_info v in
        match v_stmt with
        | Cfg_loop _ | Cfg_loop_left _ | Cfg_iter _ | Cfg_map _ -> 
          let (ncfg_0, (nv_entry, nv_exit)) = (acc_pt.cfg, ()) |> t_add_vtx_2 acc_pt.counter in
          let (ncfg_1, _) = (ncfg_0, ()) |> t_add_vinfos ~errtrace:(emsg_gen "add_vtx : (ncfg_1, _)") [nv_entry, Cfg_skip; nv_exit, Cfg_skip] in
          let n_vtxrel_0 : (vertex, vertex CPSet.t) CPMap.t = 
            let sv : vertex CPSet.t = CPSet.singleton v in
            let n0 = t_map_add ~errtrace:(emsg_gen "add_vtx : n_vtxrel_0 : n0") acc_pt.vtxrel nv_entry sv in
            let n1 = t_map_add ~errtrace:(emsg_gen "add_vtx : n_vtxrel_0 : n1") n0 nv_exit sv in
            n1
          in
          let n_vset : vertex CPSet.t = CPSet.add (CPSet.add acc_vset nv_entry) nv_exit in
          let n_v3list : (vertex * (vertex * vertex)) list = (v, (nv_entry, nv_exit)) :: acc_v3list in
          let cucp : copy_unrolled_cfg_param = {
            cuc_ptval = {acc_pt with cfg=ncfg_1; vtxrel=n_vtxrel_0};
            cuc_loopvtx = v;
            cuc_entry = nv_entry;
            cuc_exit = nv_exit;
          } in
          let (npt_0, nvset_0, neset_0) = copy_unrolled_cfg cucp in
          (npt_0, CPSet.union nvset_0 n_vset, n_v3list, CPSet.union neset_0 acc_eset)
        | _ -> 
          (* just create a new vertex *)
          (*let nv : vertex = new_vtx p.dcc_ptval.counter in*)
          let (ncfg, nv) : t * vertex = (acc_pt.cfg, ()) |> t_add_vtx acc_pt.counter |> t_add_vinfo_now ~errtrace:(emsg_gen "add_vtx : (ncfg, nv)") v_stmt in
          let n_vtxrel : (vertex, vertex CPSet.t) CPMap.t = t_map_add ~errtrace:(emsg_gen "add_vtx : n_vtxrel") acc_pt.vtxrel nv (CPSet.singleton v) in
          let n_vset : vertex CPSet.t = CPSet.add acc_vset nv in
          let n_v3list : (vertex * (vertex * vertex)) list = (v, (nv, nv)) :: acc_v3list in
          ({acc_pt with cfg=ncfg; vtxrel=n_vtxrel}, n_vset, n_v3list, acc_eset)
      end in
      let (new_pt_1, nvset_1, nv3list_1, neset_1) = CPSet.fold o_vertices ~init:(p.dcc_ptval, CPSet.empty, [], CPSet.empty) ~f:add_vtx in
      (* 3. copy edges *)
      (* 3.1. collect edges 
        - edges from dcc_o_exit are only collected when the edge points to dcc_o_entry only.
      *)
      let c_edges_0 : G.edge CPSet.t =
        CPSet.fold 
          o_vertices 
          ~init:(CPSet.empty) 
          ~f:(fun acc_set v -> 
            let succ_es : G.edge list = G.succ_e cfg_0.flow v in
            let succ_es_f1 : G.edge list = (* succ_es_f1 : remove loop-true-branches *)
              List.filter (
                fun (src, lbl, _) ->
                let src_stmt = t_map_find ~errtrace:(emsg_gen "c_edges_0 : succ_es_f1") cfg_0.vertex_info src in
                match (src_stmt, lbl) with
                | Cfg_loop _, If_true | Cfg_loop_left _, If_true | Cfg_iter _, If_true | Cfg_map _, If_true -> false
                | _ -> true
              )
              succ_es
            in
            (* succ_es_f2 : kill (dcc_o_exit -> ) edges except (dcc_o_exit -> dcc_o_entry) thing *)
            let succ_es_f2 : G.edge list = if v = p.dcc_o_exit then List.filter (fun (_, _, d) -> d = p.dcc_o_entry) succ_es_f1 else succ_es_f1 in
            (* succ_es_m1 : convert loop-false edges into normal edge (becuase every loops are copied in "add_vtx" procedure) *)
            let succ_es_m1 : G.edge list =
              List.map (
                fun (src, lbl, dst) ->
                let src_stmt = t_map_find ~errtrace:(emsg_gen "c_edges_0 : succ_es_f1") cfg_0.vertex_info src in
                (match (src_stmt, lbl) with
                | Cfg_loop _, If_false | Cfg_loop_left _, If_false | Cfg_iter _, If_false | Cfg_map _, If_false -> (src, Normal, dst)
                | _ -> (src, lbl, dst))
              )
              succ_es_f2
            in
            List.fold_left (fun aset x -> CPSet.add aset x) acc_set succ_es_m1
          )
      in
      let add_edg : (pt * G.edge CPSet.t) -> G.edge -> (pt * G.edge CPSet.t)
      =fun (acc_pt, acc_eset) (src, lbl, dst) -> begin
        if dst = p.dcc_o_exit then (acc_pt, acc_eset) else
        (*let _ : unit = print_endline ("DEBUG : dfs_copy_cfg : add_edg : src=" ^ (string_of_int src) ^ ", dst=" ^ (string_of_int dst)) in*)
        let (_, src_exit) : vertex * vertex = List.assoc src nv3list_1 in
        let (dst_entry, _) : vertex * vertex = List.assoc dst nv3list_1 in
        (*let _ : unit = print_endline ("DEBUG : dfs_copy_cfg : add_edg : src_exit=" ^ (string_of_int src_exit) ^ ", dst_entry=" ^ (string_of_int dst_entry)) in*)
        let new_edg : G.edge = (src_exit, lbl, dst_entry) in
        let ncfg = {acc_pt.cfg with flow=(G.add_edge_e acc_pt.cfg.flow new_edg)} in
        let new_eset = CPSet.add acc_eset new_edg in
        ({acc_pt with cfg=ncfg}, new_eset)
      end in
      let (new_pt_2, neset_2) : pt * (G.edge CPSet.t) = CPSet.fold c_edges_0 ~init:(new_pt_1, neset_1) ~f:add_edg in
      (* 4. get entry, exit information from nv3list_1 *)
      let entry_v : vertex = List.assoc p.dcc_o_entry nv3list_1 |> Stdlib.fst in 
      let exit_set : (vertex * edge_label) CPSet.t =
        let foldf : (vertex * edge_label) CPSet.t -> (vertex * (vertex * vertex)) -> (vertex * edge_label) CPSet.t
        =fun accset (ov, (_, nv_1)) -> begin
          let ee : G.edge list = G.succ_e cfg_0.flow ov in
          List.fold_left (fun aset (_, lbl, dst) -> if dst = p.dcc_o_exit then CPSet.add aset (nv_1, lbl) else aset) accset ee
        end in
        List.fold_left foldf CPSet.empty nv3list_1
      in
      (* 5. return *)
      (new_pt_2, nvset_1, neset_2, (entry_v, exit_set))
  end (* function dfs_copy_cfg ends *)


  (* unroll_fold_f unrolls the given loop(target_vtx) for n-times(unroll_count)
      - ptval.cfg will be updated
      - ptval.counter will be automatically updated
      - ptval.vtxrel will be updated
      - ptval.env will be updated
      - consider that the every nested loop were already unrolled before.
  *)
  let rec unroll_fold_f : UnrollParam.unroll_fold_f_param -> UnrollParam.unroll_fold_f_param
  = let open UnrollParam in
    let emsg_gen s : string = ("CfgUtil.LoopUnrolling.unroll_fold_f : " ^ s) in
    let add_edges : G.t -> G.edge list -> G.t = List.fold_left (fun acc_g e -> G.add_edge_e acc_g e) in
    let add_edges_cfg : t -> G.edge list -> t = fun cfg el -> {cfg with flow=(add_edges cfg.flow el)} in
    (* FUNCTION BEGIN *)
    fun p -> begin
      (*let _ = print_endline ("DEBUG : unroll_fold_f : target_vtx = " ^ (string_of_int p.target_vtx) ^ ", unroll_count = " ^ (string_of_int p.unroll_count)) in*)
      if p.unroll_count > p.ptval.unroll_num 
      then (
        let newenv : (vertex, unroll_env) CPMap.t = t_map_add ~errtrace:(emsg_gen "before recursion escape") p.ptval.env p.target_vtx p.acc_unroll_env in 
        {p with ptval={p.ptval with env=newenv}} (* the only escape condition of "unroll_fold_f" *) 
      )
      else
      match p.target_stmt with
      | Cfg_loop var_1 ->
        let emsg_gen s : string = ("CfgUtil.LoopUnrolling.unroll_fold_f : Cfg_loop : " ^ s) in
        if (p.unroll_count = 0)
        then (
          (* insert initial exit vertex and the Cfg_drop(=entry vertex) stmt *)
          let (cfg_0, (v_exit, v_drop)) = t_add_vtx_2 p.ptval.counter (p.ptval.cfg, ()) in
          let drop_exit_edg : G.edge = (v_drop, Normal, v_exit) in
          let (cfg_1, _) : t * 'a = 
            t_add_vinfos 
              ~errtrace:(emsg_gen "cfg_1") 
              [ v_exit, Cfg_skip;
                v_drop, Cfg_drop [var_1];
              ] 
              (cfg_0, ())
          in
          let cfg_2 = {cfg_1 with flow=(G.add_edge_e cfg_1.flow drop_exit_edg)} in
          let new_vtxrel : (vertex, vertex CPSet.t) CPMap.t = 
            let tvtx_set : vertex CPSet.t = CPSet.singleton p.target_vtx in
            let vtxrel_1 = t_map_add ~errtrace:(emsg_gen "new_vtxrel : vtxrel_1") p.ptval.vtxrel v_exit tvtx_set in
            let vtxrel_2 = t_map_add ~errtrace:(emsg_gen "new_vtxrel : vtxrel_2") vtxrel_1 v_drop tvtx_set in
            vtxrel_2
          in
          let newpt : pt = {p.ptval with cfg=cfg_2; vtxrel=new_vtxrel;} in
          let new_vset : vertex CPSet.t = CPSet.of_list [v_exit; v_drop] in
          let new_eset : G.edge CPSet.t = CPSet.singleton drop_exit_edg in
          let new_acc_uenv : unroll_env = {env_entry_vtx=v_drop; env_exit_vtx=v_exit; env_vset=new_vset; env_eset=new_eset;} in
          unroll_fold_f {p with ptval=newpt; unroll_count=(p.unroll_count + 1); acc_unroll_env=new_acc_uenv}
        )
        else (
          (* insert if-stmt *)
          let (cfg_0, (v_if, v_exit)) = t_add_vtx_2 p.ptval.counter (p.ptval.cfg, ()) in
          let (cfg_1, _) = t_add_vinfos ~errtrace:(emsg_gen "else : cfg_1") [v_if, Cfg_if var_1; v_exit, Cfg_skip] (cfg_0, ()) in
          let else_edge : G.edge = (v_if, If_false, v_exit) in
          let new_exit_edge : G.edge = (p.acc_unroll_env.env_exit_vtx, Normal, v_exit) in   (* env_exit_vtx -> v_exit. It preserves If-stmt graph shape. *)
          let cfg_2 = add_edges_cfg cfg_1 [else_edge; new_exit_edge] in
          let new_vtxrel_0 : (vertex, vertex CPSet.t) CPMap.t = 
            let tvtx_set : vertex CPSet.t = CPSet.singleton p.target_vtx in
            let vtxrel_1 = t_map_add ~errtrace:(emsg_gen "else : new-vtxrel : vtxrel_1") p.ptval.vtxrel v_if tvtx_set in
            let vtxrel_2 = t_map_add ~errtrace:(emsg_gen "else : new-vtxrel : vtxrel_2") vtxrel_1 v_exit tvtx_set in
            vtxrel_2
          in
          let newpt_0 : pt = {p.ptval with cfg=cfg_2; vtxrel=new_vtxrel_0;} in
          (* copy loop body *)
          (* before copy loop body, we should know the loop body entry vertex. PRECONDITION: there are only one loop body entry vertex, which has If_true edge comes from the target loop vertex. *)
          let loop_body_entry_vtx : vertex = List.find (fun (_, lbl, _) -> lbl = If_true) (G.succ_e p.ptval.cfg.flow p.target_vtx) |> (fun (_, _, dst) -> dst) in
          (*let _ : unit = print_endline "DEBUG : unroll_fold_f : above dfs_copy_cfg" in*)
          let (newpt_1, added_vtx_1, added_edg_1, (new_entry_v_1, new_exit_set)) = dfs_copy_cfg {dcc_ptval=newpt_0; dcc_o_entry=loop_body_entry_vtx; dcc_o_exit=p.target_vtx} in
          (*let _ : unit = print_endline "DEBUG : unroll_fold_f : below dfs_copy_cfg" in*)
          (* add edge from v_if to new_entry_v_1 *)
          let then_edge : G.edge = (v_if, If_true, new_entry_v_1) in
          (* add edges from new_exit_set to p.acc_unroll_env.env_entry_vtx *)
          let new_exit_edg_list : G.edge list = 
            CPSet.fold new_exit_set ~init:[] ~f: (fun acclist (ev, lbl) -> (ev, lbl, p.acc_unroll_env.env_entry_vtx) :: acclist)
          in
          (* add edges to pt's cfg flow. *)
          let cfg_3 : t = add_edges_cfg newpt_1.cfg (then_edge :: new_exit_edg_list) in
          let newpt_2 = {newpt_1 with cfg=cfg_3} in
          let new_vset_0 : vertex CPSet.t = CPSet.add (CPSet.add p.acc_unroll_env.env_vset v_if) v_exit in
          let new_vset_1 : vertex CPSet.t = CPSet.union new_vset_0 added_vtx_1  in
          let new_eset : G.edge CPSet.t = List.fold_left CPSet.add p.acc_unroll_env.env_eset ([then_edge; else_edge; new_exit_edge] @ new_exit_edg_list @ (CPSet.to_list added_edg_1)) in
          let new_acc_uenv : unroll_env = {env_entry_vtx=v_if; env_exit_vtx=v_exit; env_vset=new_vset_1; env_eset=new_eset} in
          unroll_fold_f {p with ptval=newpt_2; unroll_count=(p.unroll_count + 1); acc_unroll_env=new_acc_uenv}
        ) 
      | Cfg_loop_left var_1 ->
        let emsg_gen s : string = ("CfgUtil.LoopUnrolling.unroll_fold_f : Cfg_loop_left : " ^ s) in
        if (p.unroll_count = 0) 
        then(
          (* insert initial exit vertex only, no edge needed (and no eset update). *)
          (* unwrap-r is out of this function's scope *)
          let (cfg_0, v_exit) = t_add_vtx p.ptval.counter (p.ptval.cfg, ())
                                |> t_add_vinfo_now ~errtrace:(emsg_gen "cfg_0") Cfg_skip
          in
          let new_vtxrel : (vertex, vertex CPSet.t) CPMap.t = t_map_add ~errtrace:(emsg_gen "new_vtxrel") p.ptval.vtxrel v_exit (CPSet.singleton p.target_vtx) in
          let newpt : pt = {p.ptval with cfg=cfg_0; vtxrel=new_vtxrel} in
          let new_vset : vertex CPSet.t = CPSet.singleton v_exit in
          let new_acc_uenv : unroll_env = {env_entry_vtx=v_exit; env_exit_vtx=v_exit; env_vset=new_vset; env_eset=CPSet.empty} in
          unroll_fold_f {p with ptval=newpt; unroll_count=(p.unroll_count+1); acc_unroll_env=new_acc_uenv}
        )
        else (
          (* wrap previous cfg from Cfg_if_left to new-exit (insert if-stmt) *)
          let (cfg_0, (v_ifleft, v_exit)) = t_add_vtx_2 p.ptval.counter (p.ptval.cfg, ()) in
          let (cfg_1, _) = t_add_vinfos ~errtrace:(emsg_gen "else : cfg_1") [v_ifleft, Cfg_if_left var_1; v_exit, Cfg_skip] (cfg_0, ()) in
          let else_edge : G.edge = (v_ifleft, If_false, v_exit) in
          let new_exit_edge : G.edge = (p.acc_unroll_env.env_exit_vtx, Normal, v_exit) in
          let cfg_2 = add_edges_cfg cfg_1 [else_edge; new_exit_edge] in
          let new_vtxrel_0 : (vertex, vertex CPSet.t) CPMap.t = 
            let tvtx_set : vertex CPSet.t = CPSet.singleton p.target_vtx in
            let vtxrel_1 = t_map_add ~errtrace:(emsg_gen "else : new-vtxrel : vtxrel_1") p.ptval.vtxrel v_ifleft tvtx_set in
            let vtxrel_2 = t_map_add ~errtrace:(emsg_gen "else : new-vtxrel : vtxrel_2") vtxrel_1 v_exit tvtx_set in
            vtxrel_2
          in
          let newpt_0 : pt = {p.ptval with cfg=cfg_2; vtxrel=new_vtxrel_0;} in
          (* copy loop body *)
          (* before copy loop body, we should know the loop body entry vertex. PRECONDITION: there are only one loop body entry vertex, which has If_true edge comes from the target loop vertex. *)
          let loop_body_entry_vtx : vertex = List.find (fun (_, lbl, _) -> lbl = If_true) (G.succ_e p.ptval.cfg.flow p.target_vtx) |> (fun (_, _, dst) -> dst) in
          let (newpt_1, added_vtx_1, added_edg_1, (new_entry_v_1, new_exit_set)) = dfs_copy_cfg {dcc_ptval=newpt_0; dcc_o_entry=loop_body_entry_vtx; dcc_o_exit=p.target_vtx} in
          (* add edge between then-body and if-stmt *)
          let then_edge : G.edge = (v_ifleft, If_true, new_entry_v_1) in
          let new_exit_edg_list : G.edge list = 
            CPSet.fold new_exit_set ~init:[] ~f:(fun acclist (ev, lbl) -> (ev, lbl, p.acc_unroll_env.env_entry_vtx) :: acclist) 
          in
          (* add edges to pt's cfg flow *)
          let cfg_3 : t = add_edges_cfg newpt_1.cfg (then_edge :: new_exit_edg_list) in
          let newpt_2 = {newpt_1 with cfg=cfg_3} in
          let new_vset_0 : vertex CPSet.t = CPSet.add (CPSet.add p.acc_unroll_env.env_vset v_ifleft) v_exit in
          let new_vset_1 : vertex CPSet.t = CPSet.union new_vset_0 added_vtx_1 in
          let new_eset : G.edge CPSet.t = List.fold_left CPSet.add p.acc_unroll_env.env_eset ([then_edge; else_edge; new_exit_edge] @ new_exit_edg_list @ (CPSet.to_list added_edg_1)) in
          let new_acc_uenv : unroll_env = {env_entry_vtx=v_ifleft; env_exit_vtx=v_exit; env_vset=new_vset_1; env_eset=new_eset} in
          unroll_fold_f {p with ptval=newpt_2; unroll_count=(p.unroll_count+1); acc_unroll_env=new_acc_uenv}
        )
        (* unrolling procedure produces same result for ITER and MAP *)
      | Cfg_iter var_1
      | Cfg_map var_1 ->
        let emsg_gen s : string = ("CfgUtil.LoopUnrolling.unroll_fold_f : Cfg_loop_left : " ^ s) in
        if (p.unroll_count = 0)
        then (
          (* insert initial exit vertex only, no edge needed (and no eset update) *)
          let (cfg_0, v_exit) = t_add_vtx p.ptval.counter (p.ptval.cfg, ())
                                |> t_add_vinfo_now ~errtrace:(emsg_gen "cfg_0") Cfg_skip
          in
          let new_vtxrel : (vertex, vertex CPSet.t) CPMap.t = t_map_add ~errtrace:(emsg_gen "new_vtxrel") p.ptval.vtxrel v_exit (CPSet.singleton p.target_vtx) in
          let newpt : pt = {p.ptval with cfg=cfg_0; vtxrel=new_vtxrel} in
          let new_vset : vertex CPSet.t = CPSet.singleton v_exit in
          let new_acc_uenv : unroll_env = {env_entry_vtx=v_exit; env_exit_vtx=v_exit; env_vset=new_vset; env_eset=CPSet.empty} in
          unroll_fold_f {p with ptval=newpt; unroll_count=(p.unroll_count+1); acc_unroll_env=new_acc_uenv}
        )
        else (
          (* Put [Cfg_assign (nvar-1, E_size var-1); Cfg_assign (nvar-2, E_int nvar-1); Cfg_assign (nvar-3, E_neq nvar-2); Cfg_if (nvar-3)] *)
          let (cfg_nv_1, nvar_1) = t_add_nv_tinfo ~errtrace:(emsg_gen "else : cfg_nv_1") p.ptval.counter (Mich.gen_t Mich.T_nat) (p.ptval.cfg, ()) in
          let (cfg_nv_2, nvar_2) = t_add_nv_tinfo ~errtrace:(emsg_gen "else : cfg_nv_2") p.ptval.counter (Mich.gen_t Mich.T_int) (cfg_nv_1, ()) in
          let (cfg_nv_3, nvar_3) = t_add_nv_tinfo ~errtrace:(emsg_gen "else : cfg_nv_3") p.ptval.counter (Mich.gen_t Mich.T_bool) (cfg_nv_2, ()) in
          let (cfg_0, (v_size, v_int, v_neq, v_if, v_exit)) = t_add_vtx_5 p.ptval.counter (cfg_nv_3, ()) in
          let (cfg_1, _) = 
            t_add_vinfos 
              ~errtrace:(emsg_gen "else : cfg_1")
              [ v_size, Cfg_assign (nvar_1, E_size var_1);
                v_int, Cfg_assign (nvar_2, E_int_of_nat nvar_1);
                v_neq, Cfg_assign (nvar_3, E_neq nvar_2);
                v_if, Cfg_if (nvar_3);
                v_exit, Cfg_skip;
              ]
              (cfg_0, ())
          in
          let (edg_size_int, edg_int_neq, edg_neq_if, edg_else, new_exit_edge) : (G.edge * G.edge * G.edge * G.edge * G.edge) = 
            ((v_size, Normal, v_int), (v_int, Normal, v_neq), (v_neq, Normal, v_if), (v_if, If_false, v_exit), (p.acc_unroll_env.env_exit_vtx, Normal, v_exit))
          in
          let cfg_2 = add_edges_cfg cfg_1 [edg_size_int; edg_int_neq; edg_neq_if; edg_else; new_exit_edge] in
          let new_vtxrel_0 : (vertex, vertex CPSet.t) CPMap.t =
            let tvtx_set : vertex CPSet.t = CPSet.singleton p.target_vtx in
            let vtxrel_1 = t_map_add ~errtrace:(emsg_gen "else : new-vtxrel : vtxrel_1") p.ptval.vtxrel v_size tvtx_set in
            let vtxrel_2 = t_map_add ~errtrace:(emsg_gen "else : new-vtxrel : vtxrel_2") vtxrel_1 v_int tvtx_set in
            let vtxrel_3 = t_map_add ~errtrace:(emsg_gen "else : new-vtxrel : vtxrel_3") vtxrel_2 v_neq tvtx_set in
            let vtxrel_4 = t_map_add ~errtrace:(emsg_gen "else : new-vtxrel : vtxrel_4") vtxrel_3 v_if tvtx_set in
            let vtxrel_5 = t_map_add ~errtrace:(emsg_gen "else : new_vtxrel : vtxrel_5") vtxrel_4 v_exit tvtx_set in
            vtxrel_5
          in
          let newpt_0 : pt = {p.ptval with cfg=cfg_2; vtxrel=new_vtxrel_0;} in
          (* copy loop body *)
          (* before copy loop body, we should know the loop body entry vertex. PRECONDITION: there are only one loop body entry vertex, which has If_true edge comes from the target loop vertex. *)
          let loop_body_entry_vtx : vertex = List.find (fun (_, lbl, _) -> lbl = If_true) (G.succ_e p.ptval.cfg.flow p.target_vtx) |> (fun (_, _, dst) -> dst) in
          let (newpt_1, added_vtx_1, added_edg_1, (new_entry_v_1, new_exit_set)) = dfs_copy_cfg {dcc_ptval=newpt_0; dcc_o_entry=loop_body_entry_vtx; dcc_o_exit=p.target_vtx} in
          (* add edge between then-body and if-stmt *)
          let then_edge : G.edge = (v_if, If_true, new_entry_v_1) in
          let new_exit_edg_list : G.edge list = CPSet.fold new_exit_set ~init:[] ~f:(fun acclist (ev, lbl) -> (ev, lbl, p.acc_unroll_env.env_entry_vtx) :: acclist) in
          (* add edges to pt's cfg flow *)
          let cfg_3 : t = add_edges_cfg newpt_1.cfg (then_edge :: new_exit_edg_list) in
          let newpt_2 = {newpt_1 with cfg=cfg_3} in
          let new_vset_0 : vertex CPSet.t = CPSet.union p.acc_unroll_env.env_vset (CPSet.union added_vtx_1 (CPSet.of_list [v_size; v_int; v_neq; v_if; v_exit])) in
          let new_eset : G.edge CPSet.t = List.fold_left CPSet.add p.acc_unroll_env.env_eset ([edg_size_int; edg_int_neq; edg_neq_if; then_edge; edg_else; new_exit_edge] @ new_exit_edg_list @ (CPSet.to_list added_edg_1)) in
          let new_acc_uenv : unroll_env = {env_entry_vtx=v_size; env_exit_vtx=v_exit; env_vset=new_vset_0; env_eset=new_eset} in
          unroll_fold_f {p with ptval=newpt_2; unroll_count=(p.unroll_count+1); acc_unroll_env=new_acc_uenv}
        )
      | _ -> Stdlib.failwith (emsg_gen "targt_stmt match failed")
  end   (* function unroll_fold_f ends *)

  let construct_pt : t * cfgcon_ctr * int -> UnrollParam.pt
  =fun (cfg, counter, unroll_num) -> {cfg=cfg; counter=counter; unroll_num=unroll_num; vtxrel=CPMap.empty; env=CPMap.empty;}

  (* unroll : unroll every loops, generate unrolled main-function & lambda-functions, and erase original & unrelated debris *)
  let unroll : UnrollParam.pt -> UnrollParam.pt
  =fun p -> begin
    let open UnrollParam in
    let gen_emsg s : string = ("CfgUtil : LoopUnrolling : unroll : " ^ s) in
    (* 1. collect topology sorted loop dependencies *)
    let lni : (vertex, vertex CPSet.t) CPMap.t = loopnest_info p.cfg in
    (*let _ = print_endline ("DEBUG : unroll : lni = {" ^ (CPMap.fold lni ~init:"" ~f:(fun ~key:k ~data:d acc_str -> acc_str ^ "; " ^ (string_of_int k) ^ ": " ^ (CPSet.fold d ~init:"" ~f:(fun acc x -> acc ^ ", " ^ (string_of_int x))) )) ^ "}") in*)
    let tplg_sorted_loops : vertex list = tplg_sort lni in
    (*let _ = print_endline ("DEBUG : unroll : tplg_sorted_loops = [" ^ (String.concat ", " (List.map string_of_int tplg_sorted_loops)) ^ "]" ) in*)
    (* 2. unroll every loop vertices and store it at UnrollParam.pt value *)
    let urpt : pt = 
      let arbitrary_unroll_env : unroll_env = {env_entry_vtx=0; env_exit_vtx=0; env_vset=CPSet.empty; env_eset=CPSet.empty;} in
      let fold_func : unroll_fold_f_param -> vertex -> unroll_fold_f_param
      =fun acc_uff loop_vtx -> begin
        let loop_vtx_stmt : stmt = t_map_find ~errtrace:(gen_emsg "urpt : loop_vtx_stmt") p.cfg.vertex_info loop_vtx in
        unroll_fold_f {acc_uff with target_vtx=loop_vtx; target_stmt=loop_vtx_stmt; unroll_count=0; acc_unroll_env=arbitrary_unroll_env}
      end in
      (* "first_uff" is just a placeholder. *)
      let first_uff : unroll_fold_f_param = {ptval=p; target_vtx=0; target_stmt=Cfg_skip; unroll_count=0; acc_unroll_env=arbitrary_unroll_env} in
      let uff_finished : unroll_fold_f_param = List.fold_left fold_func first_uff tplg_sorted_loops in
      uff_finished.ptval
    in
    (* 3. copy main function and lambda functions (remove loop vertices with unroll-environment) *)
    (* 3.1. copy main function / update main_entry and main_exit too *)
    let (new_pt_1, n_main_ue) : pt * unroll_env  =
      let (_npt, _vset, _eset, (_entry_v, _end_rel)) : pt * (vertex CPSet.t) * (G.edge CPSet.t) * (vertex * ((vertex * edge_label) CPSet.t)) = 
        dfs_copy_cfg {dcc_ptval=urpt; dcc_o_entry=p.cfg.main_entry; dcc_o_exit=p.cfg.main_exit} 
      in
      (* 3.1.1. use "_end_rel"'s information to new main-exit vertex *)
      (* 3.1.1.1. create new main-exit-vertex and update cfg(flow,vertex-info), vertex-relationship *)
      let (new_cfg, new_main_exit_v) : (t * vertex) = 
        (_npt.cfg, ())
        |> t_add_vtx _npt.counter
        |> t_add_vinfo_now 
            ~errtrace:(gen_emsg "new_pt_1 : new_cfg") 
            (t_map_find ~errtrace:(gen_emsg "new_pt_1 : new_cfg : find-vinfo") p.cfg.vertex_info p.cfg.main_exit)
      in
      let new_vrel : (vertex, vertex CPSet.t) CPMap.t = 
        t_map_add ~errtrace:(gen_emsg "new_pt_1 : new_vrel") _npt.vtxrel new_main_exit_v (CPSet.singleton p.cfg.main_exit)
      in
      (* 3.1.2. add edges *)
      let (new_flow, exit_edgs) : G.t * (G.edge list) = 
        CPSet.fold 
        _end_rel 
        ~init:(new_cfg.flow, []) 
        ~f:(fun (acc_flw, acc_edgs) (ev, el) -> 
            let e = (ev, el, new_main_exit_v) in 
            (G.add_edge_e acc_flw (ev, el, new_main_exit_v), (e :: acc_edgs))
        ) 
      in
      let new_uenv : unroll_env = 
        { env_entry_vtx = _entry_v;
          env_exit_vtx = new_main_exit_v;
          env_vset = CPSet.add _vset new_main_exit_v;
          env_eset = CPSet.union _eset (CPSet.of_list exit_edgs);
        }
      in
      (* 3.1.2. update main_entry and main_exit in cfg too *)
      ({_npt with cfg={new_cfg with flow=new_flow; main_entry=_entry_v; main_exit=new_main_exit_v;}; vtxrel=new_vrel}, new_uenv)
    in
    (* 3.2. copy lambda functions / update lambda_id_map too *)
    let (new_pt_2, n_lambda_ues) : pt * (unroll_env list) =
      let lm : (lambda_ident, lambda_summary) CPMap.t = new_pt_1.cfg.lambda_id_map in
      CPMap.fold
        lm
        ~init:(new_pt_1, [])
        ~f:(fun ~key:k ~data:(lmb_entry, lmb_exit, paramtyp, outtyp) (acc_pt, acc_uel) ->
          (* 3.2.1. for each lambda, copy graph *)
          let (_l_pt, _l_vset, _l_eset, (_l_entry_v, _l_end_rel)) : pt * (vertex CPSet.t) * (G.edge CPSet.t) * (vertex * ((vertex * edge_label) CPSet.t)) =
            dfs_copy_cfg {dcc_ptval=acc_pt; dcc_o_entry=lmb_entry; dcc_o_exit=lmb_exit}
          in
          (* 3.2.2. create new lambda-exit-vertex and update cfg(flow,vertex-info), vertex-relationship, vset, eset *)
          let (cfg_with_lmbd_exit, lmbd_exit_vertex) : (t * vertex) =
            (_l_pt.cfg, ())
            |> t_add_vtx _l_pt.counter
            |> t_add_vinfo_now
                ~errtrace:(gen_emsg "new_pt_2 : cfg_with_lmbd_exit")
                (t_map_find ~errtrace:(gen_emsg "new_pt_2 : cfg_with_lmbd_exit : find stmt") _l_pt.cfg.vertex_info lmb_exit)
          in
          let (cfg_with_lmbd_exit_connected, new_eset) : t * (G.edge CPSet.t) =
            CPSet.fold
              _l_end_rel
              ~init:(cfg_with_lmbd_exit, _l_eset)
              ~f:(fun (acfg, aeset) (ev, el) -> let e : G.edge = (ev, el, lmbd_exit_vertex) in ({acfg with flow=(G.add_edge_e acfg.flow e)}, CPSet.add aeset e))
          in
          let new_vtxrel : (vertex, vertex CPSet.t) CPMap.t = t_map_add ~errtrace:(gen_emsg "new_pt_2 : new_vtxrel") _l_pt.vtxrel lmbd_exit_vertex (CPSet.singleton lmb_exit) in
          let new_vset : vertex CPSet.t = CPSet.add _l_vset lmbd_exit_vertex in
          (* 3.2.3. create new unroll_env *)
          let new_ue : unroll_env = {
            env_entry_vtx=_l_entry_v;
            env_exit_vtx=lmbd_exit_vertex;
            env_vset=new_vset;
            env_eset=new_eset;
          }
          in
          (* 3.2.4. update lambda_id_map in cfg(cfg_with_lmbd_exit_connected) *)
          let new_ls : lambda_summary = (_l_entry_v, lmbd_exit_vertex, paramtyp, outtyp) in
          let new_lim : (lambda_ident, lambda_summary) CPMap.t = CPMap.change cfg_with_lmbd_exit_connected.lambda_id_map k ~f:(fun _ -> Some new_ls) in
          let new_cfg_f : t = {cfg_with_lmbd_exit_connected with lambda_id_map=new_lim} in
          (* 3.2.5. accumulate *)
          ({_l_pt with cfg=new_cfg_f; vtxrel=new_vtxrel;}, new_ue :: acc_uel)
        )
    in
    (* 4. remove (original or unrelated) subgraphs from new_pt.cfg *)
    (* 4.1. collect vertices which should be remained (use n_main_ue & n_main_ues) *)
    let all_new_vertices : vertex CPSet.t = List.fold_left (fun accset x_ue -> CPSet.union accset x_ue.env_vset ) n_main_ue.env_vset n_lambda_ues in
    (* 4.2. use n_main_ue & n_main_ues, remove vertices in flow *)
    let trimmed_flow : G.t = 
      let o_f : G.t = new_pt_2.cfg.flow in 
      G.fold_vertex (fun v accflow -> if CPSet.mem all_new_vertices v then accflow else (G.remove_vertex accflow v)) o_f o_f 
    in
    let new_pt_3 : pt = {new_pt_2 with cfg={new_pt_2.cfg with flow=trimmed_flow}} in
    (* 5. update fail_vertices, pos_info in cfg *)
      (* TODO *)
    (* 6. return *)
    new_pt_3
  end (* function unroll ends *)

  let run : (t * cfgcon_ctr * int) -> (t * cfgcon_ctr)
  =fun p -> let npt : UnrollParam.pt = unroll (construct_pt p) in (npt.cfg, npt.counter)

end (* module LoopUnrolling ends *)
