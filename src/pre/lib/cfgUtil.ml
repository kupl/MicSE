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
              && (in_label = Normal)
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


(*****************************************************************************)
(*****************************************************************************)
(* Print                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

let cfg_to_dotformat : t -> string
= let only_label_str s : string = ( "label=\"" ^ (Core.String.substr_replace_all s ~pattern:"\"" ~with_:"\\\"") ^ "\"" ) in
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
    let edge_s = match e_label with | Normal -> "" | If_true -> "[label=\"True\"]" | If_false -> "[label=\"False\"]" | Failed -> "[label=\"Failed\", style=dotted]" | Check_skip -> "[label=\"Check_skip\", style=dotted]" in
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
