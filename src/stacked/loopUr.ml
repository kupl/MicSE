(* Loop Unrolling *)

exception Error of string (* Use Stdlib.__LOC__ only *)


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

let concat_route : route -> route -> route
= fun r1 r2 -> begin
  match r1, r2 with
  | R_list rr1, R_list rr2 -> R_list (rr1 @ rr2)
  | R_list rr1, _ -> R_list (rr1 @ [r2])
  | _, R_list rr2 -> R_list (r1 :: rr2)
  | _ -> R_list [r1; r2]
end (* function concat_route end *)

type bake_routes_param = {
  brp_trx_entry_mci : Tz.mich_cut_info;
  brp_trx_exit_mci : Tz.mich_cut_info;
  brp_sset : Se.state_set;
}

type bake_routes_output = {
  bro_visited : Tz.mich_cut_info Tz.PSet.t;
  bro_r : (Tz.mich_cut_info, route Tz.PSet.t) Tz.PMap.t;
  bro_q : (Tz.mich_cut_info, route_q Tz.PSet.t) Tz.PMap.t;
}

type bake_routes_dfs_param = {
  brdp_is_init : bool;
  brdp_cur_mci : Tz.mich_cut_info;
  brdp_dest_mci : Tz.mich_cut_info;
}

let bake_routes : bake_routes_param -> bake_routes_output
= let open Tz in
  let brdp_cons b cmci dmci : bake_routes_dfs_param = {brdp_is_init=b; brdp_cur_mci=cmci; brdp_dest_mci=dmci;} in
  (* FUNCTION BEGIN *)
  fun {brp_trx_entry_mci; brp_trx_exit_mci; brp_sset;} -> begin
  let pmap_bl : (mich_cut_info, sym_state PSet.t) PMap.t = 
    pmap_of_pset brp_sset.blocked ~key_f:(fun ss -> ss.ss_entry_mci) ~data_f:(fun x -> x)
  and pmap_q : (mich_cut_info, (sym_state * Se.query_category) PSet.t) PMap.t =
    pmap_of_pset brp_sset.queries ~key_f:(fun (ss,_) -> ss.ss_entry_mci) ~data_f:(fun x -> x)
  in
  let rec dfs : bake_routes_dfs_param -> bake_routes_output -> bake_routes_output
  = fun brdp bro -> begin
    (* sugars *)
    let {brdp_is_init; brdp_cur_mci; brdp_dest_mci} = brdp in
    let {bro_visited; bro_r=_; bro_q=_} = bro in
    (* If already visited or (not "is_init" and (cur_mci = dest_mci), escape. *)
    if (PSet.mem bro_visited brdp_cur_mci || ((brdp_cur_mci = brdp_dest_mci) && Stdlib.not brdp_is_init)) then bro else
    (* (bro_1, r_1) : If cur_mci is loop-nonbody, then dfs loop-body first *)
    let (bro_1, r_1) = 
      if is_ln_mcc brdp_cur_mci.mci_cutcat
      then 
        let loop_mci = lb_of_ln_exn brdp_cur_mci ~debug:(__LOC__) in 
        (dfs (brdp_cons true loop_mci loop_mci) bro, R_loop loop_mci)
      else (bro, R_list [])
    in
    (* bro_2 : for every blocked-states start with cur_mci, concat r_1 to collected routes & queries, and save the results at bro_1 *)
    let bro_2 =
      let bset = pmap_find_dft pmap_bl brdp_cur_mci ~default:(PSet.empty) in 
      PSet.fold bset ~init:(bro_1)
        ~f:(fun broacc ss ->
            (* broacc_1 : result of dfs recursive call *)
            let rh = concat_route r_1 (R_state ss) in
            let broacc_1 = dfs (brdp_cons false ss.ss_block_mci brdp_dest_mci) broacc in
            (* broacc_2 : add routes (to end) *)
            let ss_r = pmap_find_exn broacc_1.bro_r ss.ss_block_mci ~debug:(Stdlib.__LOC__) |> PSet.map ~f:(fun ssrs -> concat_route rh ssrs) in
            let broacc_2 = {broacc_1 with bro_r=(PMap.update broacc_1.bro_r brdp_cur_mci ~f:(function | None -> ss_r | Some s -> PSet.union ss_r s))} in
            (* broacc_3 : add routes (to queries) *)
            let ss_q = pmap_find_exn broacc_2.bro_q ss.ss_block_mci ~debug:(Stdlib.__LOC__) |> PSet.map ~f:(fun ssrqs -> {ssrqs with rq_r=(concat_route rh ssrqs.rq_r)}) in
            let broacc_3 = {broacc_2 with bro_q=(PMap.update broacc_2.bro_q brdp_cur_mci ~f:(function | None -> ss_q | Some s -> PSet.union ss_q s))} in
            (* return *)
            broacc_3
        )
    in
    (* bro_3 : for every query-states start with cur_mci, concat r_1 to collected routes, and save the results at bro_2 *)
    let bro_3 = 
      pmap_find_dft pmap_q brdp_cur_mci ~default:(PSet.empty)
      |> PSet.map ~f:(fun (ss, qc) -> {rq_r=(concat_route r_1 (R_state ss)); rq_mci=ss.ss_block_mci; rq_qc=qc;})
      |> (fun rqset -> {bro_2 with bro_q=(PMap.update bro_2.bro_q brdp_cur_mci ~f:(function | None -> rqset | Some s -> PSet.union rqset s))})
    in
    (* bro_4 : Update bro_visited of bro_3 *)
    let bro_4 = {bro_3 with bro_visited=(PSet.add bro_3.bro_visited brdp_cur_mci)} in
    (* internal function dfs return value *)
    bro_4
  end in (* internal function dfs end *)
  let init_bro : bake_routes_output = {bro_visited = PSet.empty; bro_r = PMap.empty; bro_q = PMap.empty} in
  let init_brdp : bake_routes_dfs_param = brdp_cons true brp_trx_entry_mci brp_trx_exit_mci in
  dfs init_brdp init_bro
end (* function bake_routes end *)


(*****************************************************************************)
(*****************************************************************************)
(* Path                                                                      *)
(*****************************************************************************)
(*****************************************************************************)

type path = 
  | P_pair  of path * path
  | P_state of Tz.sym_state
  | P_loop  of Tz.mich_cut_info * int * (path list)
  | P_null  of Tz.mich_cut_info

let rec first_mci_of_path : path -> Tz.mich_cut_info
= (function
  | P_pair (p1, _) -> first_mci_of_path p1
  | P_state ss -> ss.ss_entry_mci
  | P_loop (mci, _, _) -> mci
  | P_null mci -> mci
) (* function first_mci_of_path end *)

let rec last_mci_of_path : path -> Tz.mich_cut_info
= (function
  | P_pair (_, p2) -> last_mci_of_path p2
  | P_state ss -> ss.ss_block_mci
  | P_loop (mci, _, _) -> mci
  | P_null mci -> mci
) (* function last_mci_of_path end *)


(*
(*****************************************************************************)
(*****************************************************************************)
(* Unroll-N-Naive                                                            *)
(*****************************************************************************)
(*****************************************************************************)

type unni_param = {
  unnip_num       : int;
  unnip_goal_mci  : Tz.mich_cut_info;
  unnip_p         : (Tz.mich_cut_info, Tz.sym_state Tz.PSet.t) Tz.PMap.t;
  unnip_q         : (Tz.mich_cut_info, (Tz.sym_state * Se.query_category) Tz.PSet.t) Tz.PMap.t;
}
type unni_output = {
  unnio_pathset_work  : path Tz.PSet.t;
  unnio_pathset_acc   : path Tz.PSet.t;
  unnio_queryset_acc  : (path * Se.query_category) Tz.PSet.t;
}

(* "unroll_n_naive_i" runs in tail-recursive manner. *)
let rec unroll_n_naive_i : unni_param -> unni_output -> unni_output
= fun 
    {unnip_num; unnip_goal_mci; unnip_p; unnip_q;}
    {unnio_pathset_work; unnio_pathset_acc; unnio_queryset_acc;}
  -> begin
end (* function unroll_n_naive_i end *)



type unroll_n_naive_param = {
  unnp_num        : int;
  unnp_sset       : Se.state_set;
  unnp_entry_mci  : Tz.mich_cut_info;
  unnp_exit_mci   : Tz.mich_cut_info;
}
type unroll_n_naive_output = {
  unno_pathset  : path Tz.PSet.t;
  unno_queryset : (path * Se.query_category) Tz.PSet.t;
  unno_looppaths    : 
}

let unroll_n_naive : unroll_n_naive_param -> unroll_n_naive_output
= fun {unnp_num; unnp_sset; unnp_entry_mci; unnp_exit_mci} -> begin
  (* PREPROCESSING *)
  let blocked_pmap = Tz.pmap_of_pset unnp_sset.blocked ~key_f:(fun ss -> ss.Tz.ss_entry_mci) ~data_f:(fun x -> x) in
  let query_pmap = Tz.pmap_of_pset unnp_sset.queries ~key_f:(fun (ss, _) -> ss.Tz.ss_entry_mci) ~data_f:(fun x -> x) in
  (* MAIN FUNCTION *)
  let {unnio_pathset_work=_; unnio_pathset_acc; unnio_queryset_acc;} = 
    unroll_n_naive_i
      { unnip_num       = unnp_num;
        unnip_goal_mci  = unnp_exit_mci;
        unnip_p         = blocked_pmap;
        unnip_q         = query_pmap;
      }
      { unnio_pathset_work = Tz.PSet.singleton (P_null unnp_entry_mci);
        unnio_pathset_acc  = Tz.PSet.empty;
        unnio_queryset_acc = Tz.PSet.empty;
      }
  in
  {unno_pathset=unnio_pathset_acc; unno_queryset=unnio_queryset_acc;}
end (* funtion unroll_n_naive end *)
*)


(*
(*****************************************************************************)
(*****************************************************************************)
(* Utility - Pair-Flattened Path                                             *)
(*****************************************************************************)
(*****************************************************************************)

(* flattened-path introduced to serve a better printing experience to me. *)
type fpath =
  | FP_list of fpath list
  | FP_state of Tz.sym_state
  | FP_loop of Tz.mich_cut_info * fpath
  | FP_null of Tz.mich_cut_info

let rec fpath_of_path : path -> fpath
= (function
  | P_pair (p1, p2) -> (
      match fpath_of_path p1, fpath_of_path p2 with
      | FP_list l1, FP_list l2 -> FP_list (l1 @ l2)
      | FP_list l1, fp2 -> FP_list (l1 @ [fp2])
      | fp1, FP_list l2 -> FP_list (fp1 :: l2)
      | fp1, fp2 -> FP_list [fp1; fp2]
    )
  | P_state ss -> FP_state ss
  | P_loop (mci, p) -> FP_loop (mci, fpath_of_path p)
  | P_null mci -> FP_null mci
) (* function fpath_of_path end *)
*)
