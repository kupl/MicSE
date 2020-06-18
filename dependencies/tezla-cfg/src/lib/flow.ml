open Batteries

type vertex = Cfg_node.t

type t = {
  nodes : vertex Set.t;
  initial : vertex Set.t;
  final : vertex Set.t;
  flow : (vertex * vertex) Set.t;
  init_ht : (int, int) Hashtbl.t;
  final_ht : (int, int) Hashtbl.t;
}

let rec init ht n =
  let open Tezla.Adt in
  match n.stm with
  | S_seq (s, _) -> init ht s
  | S_assign _ | S_skip | S_map _ | S_failwith _ | S_swap | S_dig
  | S_dug | S_drop _ ->
      Hashtbl.find_default ht n.id n.id
  | S_iter _ | S_loop _ | S_loop_left _ ->  Hashtbl.find_default ht n.id n.id
  | S_if _ | S_if_cons _ | S_if_left _ | S_if_none _ -> Hashtbl.find_default ht n.id n.id

let final ht =
  let open Tezla.Adt in
  let rec final_rec acc n =
    match n.stm with
    | S_if (_, x, y)
    | S_if_cons (_, x, y)
    | S_if_left (_, x, y)
    | S_if_none (_, x, y) ->
        final_rec (final_rec acc x) y
    | S_seq (_, x) -> final_rec acc x
    | S_assign _ | S_skip | S_failwith _ | S_swap | S_dig | S_dug
    | S_drop _ | S_loop _ | S_loop_left _ | S_iter _ | S_map _ ->
        Set.add (Hashtbl.find_default ht n.id n.id) acc
  in
  final_rec Set.empty

let flow counter s =
  let next_counter () =
    let () = counter := !counter + 1 in
    !counter in
  let rev_pair x y = (y, x) in
  let nodes_ht = Hashtbl.create 10 in
  let init_ht = Hashtbl.create 10 in
  let final_ht = Hashtbl.create 10 in
  let open Tezla.Adt in
  let rec flow_rec (nodes, flow) s =
    let new_node ?id nodes s =
      let id = match id with
          None -> next_counter ()
        | Some id -> id in
      let n = Cfg_node.create_node ~id s in
      let () = Hashtbl.add nodes_ht n.id n in
      n.id, Set.add n.id nodes
    in
    let if_p cfg_s x y =
      let id, nodes = new_node ~id:(s.id) nodes cfg_s in
      let nodes, flow = flow_rec (flow_rec (nodes, flow) x) y in
      let init_x = init init_ht x in
      let init_y = init init_ht y in
      let flow = Set.add (id, init_x) flow |> Set.add (id, init_y) in
      nodes, flow
    in
    let loop_p n_s c v_1 v_2 b =
      let phi_id, nodes = new_node nodes (Cfg_assign (c, E_phi (v_1, v_2))) in
      let c_node_id, nodes = new_node ~id:s.id nodes n_s in
      let () = Hashtbl.add init_ht s.id phi_id in
      let () = Hashtbl.add final_ht s.id c_node_id in
      let nodes, flow = flow_rec (nodes, flow) b in
      let flow =
        Set.map (rev_pair phi_id) (final final_ht b)
        |> Set.add (c_node_id, (init init_ht b))
        |> Set.add (phi_id, c_node_id)
        |> Set.union flow
      in
      nodes, flow
    in
    let open Cfg_node in
    match s.stm with
    | S_if (c, x, y) ->
        if_p (Cfg_if c) x y
    | S_if_cons (c, x, y) ->
        if_p (Cfg_if_cons c) x y
    | S_if_left (c, x, y) ->
        if_p (Cfg_if_left c) x y
    | S_if_none (c, x, y) ->
        if_p (Cfg_if_none c) x y
    | S_loop (c, (v_1, v_2), b) ->
        loop_p (Cfg_loop c) c v_1 v_2 b
    | S_loop_left (c, (v_1, v_2), b) ->
        loop_p (Cfg_loop_left c) c v_1 v_2 b
    | S_iter (c, (v_1, v_2), b) ->
        loop_p (Cfg_iter c) c v_1 v_2 b
    | S_map ((c, (c_1, c_2)), (r, (r_1, r_2)), b) ->
      let phi_1_id, nodes = new_node nodes (Cfg_assign (c, E_phi (c_1, c_2))) in
      let phi_2_id, nodes = new_node nodes (Cfg_assign (r, E_phi (r_1, r_2))) in
      let c_node_id, nodes = new_node ~id:s.id nodes (Cfg_map c) in
      let () = Hashtbl.add init_ht s.id phi_1_id in
      let () = Hashtbl.add final_ht s.id c_node_id in
      let nodes, flow = flow_rec (nodes, flow) b in
      let flow =
        Set.map (rev_pair phi_1_id) (final final_ht b)
        |> Set.add (c_node_id, (init init_ht b))
        |> Set.add (phi_2_id, c_node_id)
        |> Set.union flow
      in
      nodes, flow
    | S_seq (s_1, s_2) ->
        let nodes, flow = flow_rec (flow_rec (nodes, flow) s_1) s_2 in
        let init_s2 = (init init_ht s_2) in
        let final_s1 = (final final_ht s_1) in
        let flow = Set.map (rev_pair init_s2) final_s1 |> Set.union flow in
        nodes, flow
    | S_skip ->
        let n, nodes = new_node ~id:s.id nodes Cfg_skip in
        let () = Hashtbl.add init_ht s.id n in
        let () = Hashtbl.add final_ht s.id n in
        (nodes, flow)
    | S_swap ->
        let n, nodes = new_node ~id:s.id nodes Cfg_swap in
        let () = Hashtbl.add init_ht s.id n in
        let () = Hashtbl.add final_ht s.id n in
        (nodes, flow)
    | S_dig ->
        let n, nodes = new_node ~id:s.id nodes Cfg_dig in
        let () = Hashtbl.add init_ht s.id n in
        let () = Hashtbl.add final_ht s.id n in
        (nodes, flow)
    | S_dug ->
        let n, nodes = new_node ~id:s.id nodes Cfg_dug in
        let () = Hashtbl.add init_ht s.id n in
        let () = Hashtbl.add final_ht s.id n in
        (nodes, flow)
    | S_assign (v, e) ->
        let n, nodes = new_node ~id:s.id nodes (Cfg_assign (v, e)) in
        let () = Hashtbl.add init_ht s.id n in
        let () = Hashtbl.add final_ht s.id n in
        (nodes, flow)
    | S_drop v ->
        let n, nodes = new_node ~id:s.id nodes (Cfg_drop v) in
        let () = Hashtbl.add init_ht s.id n in
        let () = Hashtbl.add final_ht s.id n in
        (nodes, flow)
    | S_failwith e ->
        let n, nodes = new_node ~id:s.id nodes (Cfg_failwith e) in
        let () = Hashtbl.add init_ht s.id n in
        let () = Hashtbl.add final_ht s.id n in
        (nodes, flow)
  in
  let nodes', flow = flow_rec (Set.empty, Set.empty) s in
  let initial = Hashtbl.find nodes_ht (init init_ht s) |> Set.singleton in
  let final = Set.map (Hashtbl.find nodes_ht) (final final_ht s) in
  let nodes = Set.map (Hashtbl.find nodes_ht) nodes' in
  let flow =
    Set.map (fun (a, b) -> (Hashtbl.find nodes_ht a, Hashtbl.find nodes_ht b)) flow
  in
  { initial; final; nodes; flow; init_ht; final_ht }

let flowR counter n =
  let f = flow counter n in
  let flow = Set.map (fun (a, b) -> (b, a)) f.flow in
  { f with initial = f.final; final = f.initial; flow }
