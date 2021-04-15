(* Loop Unrolling *)

exception Error of string


(******************************************************************************
*******************************************************************************

Design of Loop Unrolling Process

<< 0. Input : Se.state_set >>
Example Input: 
- Entry : 0
- Exit  : (-1)
- Blocked-States :  1. 0 -> (-1)  (then-branch),
                    2. 0 -> 2     (else-branch),
                    3. 2 -> (-1),
                    (In Loop-2 (2->2),
                      4. 2 -> 2   (then-branch),
                      5. 2 -> 2   (else-branch)
                    )
                    
- Query-States   :  6. 0 -> (Q1) -> (-1),
                    (In Loop-2 (2->2),
                      7. 2 -> (Q2) -> 2 (then-branch)
                    )

<< 1. Bake Routes (Preprocessing) >>
- Trx-starting Route = { (1); (2,(Loop-2),3) }
- Trx-starting Query = { (6) }
- Loop-starting Route = { (Loop-2) -> {4,5} }
- Loop-starting Query = { (Loop-2) -> {7} }

<< 2. Generate Paths >>
- Example Input  : "Unroll up to 2 times for every loop"
- Example Output (Paths) = { (1); (2,(),3); (2,(4),3); (2,(5),3); (2,(4,4),3); (2,(4,5),3); (2,(5,5),3) }
(* - Example Output (Query) = { (6-Q1); (2,(7-Q2)); (2,(4,7-Q2)); (2,(5,7-Q2)) } *) (* TODO: it should be replaced *)

<< 3. Convert Path to Tz.sym_state >>
(Convert Example Output Queries to Tz.sym_state - Remain it for future work)


******************************************************************************
******************************************************************************)


(*****************************************************************************)
(*****************************************************************************)
(* Route                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

type route =
  | R_list  of route list
  | R_state of Tz.sym_state
  | R_loop  of Tz.mich_cut_info

type route_q = {
  rq_r    : route;
  rq_mci  : Tz.mich_cut_info;
  rq_qc   : Se.query_category;
}

val concat_route : route -> route -> route

val loop_in_route : route -> bool
val loop_in_route_q : route_q -> bool

type bake_routes_param = {
  brp_trx_entry_mci : Tz.mich_cut_info;
  brp_trx_exit_mci : Tz.mich_cut_info;
  brp_sset : Se.state_set;
}

type bake_routes_output = {
  bro_visited : Tz.mich_cut_info Tz.PSet.t;
  bro_r : (Tz.mich_cut_info, route Tz.PSet.t) Tz.PMap.t;  (* (Entry to Exit); (Loop-k to Loop-k) *)
  bro_q : (Tz.mich_cut_info, route_q Tz.PSet.t) Tz.PMap.t;  (* (Entry to Query); (Loop-k to Query) *)
}

type bake_routes_dfs_param = {
  brdp_is_init : bool;
  brdp_cur_mci : Tz.mich_cut_info;
  brdp_dest_mci : Tz.mich_cut_info;
}

val bake_routes : bake_routes_param -> bake_routes_output


(*****************************************************************************)
(*****************************************************************************)
(* Path                                                                      *)
(*****************************************************************************)
(*****************************************************************************)

type path = 
  | P_list of path list
  | P_state of Tz.sym_state
  | P_loop of Tz.mich_cut_info * (path list)

type path_q = {
  pq_p    : path;
  pq_mci  : Tz.mich_cut_info;
  pq_qc   : Se.query_category;
}

val path_of_route : route -> path   (* WARNING: Loops in routes will be transformed to empty loop-path. *)
val pathq_of_routeq : route_q -> path_q (* Same warning as "path_of_route" *)


(*****************************************************************************)
(* Utility - Path to Json - Only MCI                                         *)
(*****************************************************************************)

module P2Jomci : sig
  type js = Yojson.Safe.t
  val cv_path : path -> js
  val cv_path_q : path_q -> js
end (* module P2Jomci end *)


(*****************************************************************************)
(* Path Construction using Loop Unrolling                                    *)
(*****************************************************************************)

type unroll_n_naive_param = {
  unnp_num : int;
  unnp_trx_entry_mci : Tz.mich_cut_info;
  unnp_r : (Tz.mich_cut_info, route Tz.PSet.t) Tz.PMap.t;
  unnp_q : (Tz.mich_cut_info, route_q Tz.PSet.t) Tz.PMap.t;
}

type unroll_n_naive_output = {
  unno_visited : Tz.mich_cut_info Tz.PSet.t;
  unno_p : (Tz.mich_cut_info, path Tz.PSet.t) Tz.PMap.t;  (* (Entry to Exit); (Loop-k to Loop-k) *)
  unno_q : (Tz.mich_cut_info, path_q Tz.PSet.t) Tz.PMap.t;  (* (Entry to EVERY-QUERY); (Loop-k to EVERY-QUERY between (Loop-k to Loop-k)) *)
}

val unroll_n_naive : unroll_n_naive_param -> unroll_n_naive_output


(*****************************************************************************)
(*****************************************************************************)
(* Utility - Route to Json - Only MCI                                        *)
(*****************************************************************************)
(*****************************************************************************)

module R2Jomci : sig
  type js = Yojson.Safe.t
  val cv_route : route -> js
  val cv_route_q : route_q -> js
end (* module R2Jomci end *)

