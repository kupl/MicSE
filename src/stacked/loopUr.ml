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

let rec loop_in_route : route -> bool
= (function
  | R_list rl -> List.exists loop_in_route rl
  | R_state _ -> false
  | R_loop _ -> true
)
let loop_in_route_q : route_q -> bool = fun rq -> loop_in_route rq.rq_r

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
  (* If you're careful smart reader, you might notice that the "dfs" function does not perform 
      any route accumulation using its parameter. Just guess that the backtracking-accumulation
      below will work well.
  *)
  let rec dfs : bake_routes_dfs_param -> bake_routes_output -> bake_routes_output
  = fun brdp bro -> begin
    (* sugars *)
    let {brdp_is_init=_; brdp_cur_mci; brdp_dest_mci} = brdp in
    let {bro_visited; bro_r=_; bro_q=_} = bro in
    (* If already visited, escape. *)
    if (PSet.mem bro_visited brdp_cur_mci) then bro else
    (* 
      (* Deprecated Code: 
          The condition (not "is_init" and (cur_mci = dest_mci)) will not be occured, since newly introduced "broacc_0" in "bro_2" will cover that case.
      *)
      (* If already visited or (not "is_init" and (cur_mci = dest_mci), escape. *)
      if (PSet.mem bro_visited brdp_cur_mci || ((brdp_cur_mci = brdp_dest_mci) && Stdlib.not brdp_is_init)) then bro else
    *)
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
            if (ss.ss_block_mci = brdp_dest_mci)
            then (
              (* broacc_0 : if "ss" ends with "brdp_dest_mci", put "ss" and return *)
              let rs = (concat_route r_1 (R_state ss)) in
              {broacc with bro_r=(PMap.update broacc.bro_r brdp_cur_mci ~f:(function | None -> PSet.singleton rs | Some s -> PSet.add s rs))}
            )
            else (
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
  let init_bro : bake_routes_output = {bro_visited = PSet.empty; bro_r = (*(PMap.singleton brp_trx_entry_mci (PSet.singleton (R_list [])))*) PMap.empty; bro_q = PMap.empty} in
  let init_brdp : bake_routes_dfs_param = brdp_cons true brp_trx_entry_mci brp_trx_exit_mci in
  dfs init_brdp init_bro
end (* function bake_routes end *)


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

let rec path_of_route : route -> path
= (function
  | R_list rl -> P_list (List.map path_of_route rl)
  | R_state ss -> P_state ss
  | R_loop mci -> P_loop (mci, [])
)
let pathq_of_routeq : route_q -> path_q 
= fun {rq_r; rq_mci; rq_qc} -> {pq_p=(path_of_route rq_r); pq_mci=rq_mci; pq_qc=rq_qc;}


(*****************************************************************************)
(* Utility - Path to Json - Only MCI                                         *)
(*****************************************************************************)

module P2Jomci = struct
  type js = Yojson.Safe.t
  open Jc
  let cv_mci = TzCvt.T2J.cv_mich_cut_info
  let rec cv_path : path -> js
  = (function
    | P_list pl -> `List (List.map cv_path pl)
    | P_state ss -> `Tuple [cv_mci ss.ss_entry_mci; cv_mci ss.ss_block_mci]
    | P_loop (mci, pl) -> `Tuple [`String lur_p_loop; cv_mci mci; `List (List.map cv_path pl)]
  ) (* function cv_path end *)
  let cv_path_q : path_q -> js
  = fun {pq_p; pq_mci; pq_qc;} -> begin
    `Assoc [
      lur_jc_pq_p, cv_path pq_p;
      lur_jc_pq_mci, cv_mci pq_mci;
      lur_jc_pq_qc, TzCvt.S2J.cv_qc pq_qc;
    ]
  end (* function cv_path_q end *)
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
  unno_p : (Tz.mich_cut_info, path Tz.PSet.t) Tz.PMap.t;
  unno_q : (Tz.mich_cut_info, path_q Tz.PSet.t) Tz.PMap.t;
}

type _unroll_n_naive_fp_acc = {
  _unnfpa_unno : unroll_n_naive_output; (* accumulated value *)
  _unnfpa_p : path list Tz.PSet.t; (* reversed paths *)
  _unnfpa_q : path_q Tz.PSet.t; (* filled-NON-reversed paths *)
}

type _unroll_n_naive_fq_acc = {
  _unnfqa_unno : unroll_n_naive_output; (* accumulated value *)
  _unnfqa_p : path list Tz.PSet.t; (* reversed paths *)
}

let _loop_in_route_not : route -> bool = fun r -> Stdlib.not (loop_in_route r)
let _loop_in_route_q_not : route_q -> bool = fun rq -> Stdlib.not (loop_in_route_q rq)

(* example. "_cons_combine {1, 2, 3} {[1;2], [3;4]}" === "{[1;1;2], [1;3;4], [2;1;2], [2;3;4], [3;1;2], [3;3;4]}" *)
let _cons_combine : 'a Tz.PSet.t -> 'a list Tz.PSet.t -> 'a list Tz.PSet.t
= fun elemset lset -> begin
  let open Tz in
  PSet.fold lset ~init:PSet.empty
    ~f:(fun accs l -> PSet.fold elemset ~init:accs ~f:(fun accaccs e -> PSet.add accaccs (e :: l)))
end (* function _cons_combine end *)

(* example. "_cons_combine_acc {1, 2, 3} {[1;2], [3;4]}" === "{[1;2], [3,4], [1;1;2], [1;3;4], [2;1;2], [2;3;4], [3;1;2], [3;3;4]}" *)
let _cons_combine_acc : 'a Tz.PSet.t -> 'a list Tz.PSet.t -> 'a list Tz.PSet.t
= fun elemset lset -> begin
  _cons_combine elemset lset |> Tz.PSet.union lset
end (* function _cons_combine_acc end *)

(* WARNING: "_cons_combine_n" MIGHT EXPLODE YOUR MEMORY *)
let rec _cons_combine_n : int -> 'a Tz.PSet.t -> 'a list Tz.PSet.t -> 'a list Tz.PSet.t
= fun n elemset lset -> begin
  if n <= 0 then lset else
  _cons_combine_n (n-1) elemset (_cons_combine elemset lset)
end (* function _cons_combine_n end *)

(* WARNING: "_cons_combine_n_acc" MIGHT EXPLODE YOUR MEMORY *)
(* the difference between "_cons_combnie_n" and "_cons_combine_n_acc" is whether it accumulates intermediate (=initial) lset or not. *)
let rec _cons_combine_n_acc : int -> 'a Tz.PSet.t -> 'a list Tz.PSet.t -> 'a list Tz.PSet.t
= fun n elemset lset -> begin
  if n <= 0 then lset else
  let longer_lset = _cons_combine_n_acc (n-1) elemset (_cons_combine elemset lset) in
  Tz.PSet.union lset longer_lset
  (* _cons_combine_n (n-1) elemset (_cons_combine elemset lset)
  |> Tz.PSet.union lset *)
end (* funtion _cons_combine_n_acc end *)


let unroll_n_naive : unroll_n_naive_param -> unroll_n_naive_output
= let open Tz in 
  fun {unnp_num; unnp_trx_entry_mci; unnp_r; unnp_q} -> begin
  (* "acuumulate" accumulates paths & query-paths *)
  let rec accumulate : mich_cut_info -> unroll_n_naive_output -> unroll_n_naive_output
  = fun cur_mci acc_unno -> begin
    (* "fill_p" fill loops in route. update paths and query-paths *)
    let fill_p : unroll_n_naive_output -> route -> unroll_n_naive_output
    = fun fp_unno r -> begin
      (* "foldf" is the function that really fills loops & collect queries in route, but paths are accumulated reversed for convenience. *)
      let rec foldf : _unroll_n_naive_fp_acc -> route -> _unroll_n_naive_fp_acc
      = fun unnfpa r -> begin
        let {_unnfpa_unno; _unnfpa_p; _unnfpa_q} = unnfpa in
        (match r with
        | R_list [] -> unnfpa
        | R_list (hd :: tl) -> foldf (foldf unnfpa hd) (R_list tl) (* note: be aware of execution order *)
        | R_state ss -> {unnfpa with _unnfpa_p=(PSet.map _unnfpa_p ~f:(List.cons (P_state ss)));}
        | R_loop rl_mci -> 
          let new_unno : unroll_n_naive_output = accumulate rl_mci _unnfpa_unno in
          if (unnp_num <= 0)
          then ( {unnfpa with _unnfpa_p=(PSet.map _unnfpa_p ~f:(List.cons (P_loop (rl_mci, []))))} )
          else (
            let _cons_combine_pq : path_q PSet.t -> path list PSet.t -> (path list * mich_cut_info * Se.query_category) PSet.t
            = fun qset plset -> begin
              PSet.fold plset ~init:PSet.empty
                ~f:(fun accs pl -> PSet.fold qset ~init:accs ~f:(fun accaccs q -> PSet.add accaccs (q, pl)))
              |> PSet.map ~f:(fun ({pq_p; pq_mci; pq_qc}, pl) -> ((pq_p :: pl), pq_mci, pq_qc))
            end in (* internal function _cons_combine_q end *)
            let ff_paths      : path PSet.t           = pmap_find_dft new_unno.unno_p rl_mci ~default:PSet.empty in
            let ff_queries    : path_q PSet.t         = pmap_find_dft new_unno.unno_q rl_mci ~default:PSet.empty in
            let ff_nm1_paths  : path list PSet.t      = _cons_combine_n_acc (unnp_num - 1) ff_paths (PSet.singleton []) in
            let inloop_n_paths    : path list PSet.t  = _cons_combine_acc ff_paths ff_nm1_paths |> PSet.map ~f:(List.rev) in
            let inloop_n_queries  : (path list * mich_cut_info * Se.query_category) PSet.t = _cons_combine_pq ff_queries ff_nm1_paths |> PSet.map ~f:(fun (pl,mci,qc) -> (List.rev pl, mci, qc)) in
            let acc_rev_paths : path list PSet.t      = PSet.map inloop_n_paths ~f:(fun pl -> P_loop (rl_mci, pl)) |> (fun ps -> _cons_combine ps _unnfpa_p) in
            let acc_queries_s : path_q PSet.t PSet.t  = PSet.map inloop_n_queries ~f:(fun (pl, pqm, pqq) -> PSet.map _unnfpa_p ~f:(fun ex_p -> P_list (List.rev (P_loop (rl_mci, pl) :: ex_p)) |> (fun pqp -> {pq_p=pqp; pq_mci=pqm; pq_qc=pqq;}) )) in
            let acc_queries   : path_q PSet.t         = PSet.fold acc_queries_s ~init:PSet.empty ~f:(fun accs qs -> PSet.union qs accs) in
            {_unnfpa_unno=new_unno; _unnfpa_p=acc_rev_paths; _unnfpa_q=acc_queries;}
          )
        )
      end in
      let {_unnfpa_unno; _unnfpa_p; _unnfpa_q;} = foldf {_unnfpa_unno=fp_unno; _unnfpa_p=(PSet.singleton []); _unnfpa_q=PSet.empty;} r in
      let paths = PSet.map _unnfpa_p ~f:(fun x -> P_list (List.rev x)) in
      { _unnfpa_unno with
        unno_p=(psetmap_update_union _unnfpa_unno.unno_p cur_mci paths);
        unno_q=(psetmap_update_union _unnfpa_unno.unno_q cur_mci _unnfpa_q);
      }
    end in (* internal function fill_p end *)
    (* "fill_q" fill loops in route_q. update query-paths only (which ends with route_q's query) *)
    let fill_q : unroll_n_naive_output -> route_q -> unroll_n_naive_output
    = fun fq_unno q -> begin
      (* "foldf" is the function that really fills loops in route, but paths are accumulated reversed for conveniece *)
      let rec foldf : _unroll_n_naive_fq_acc -> route -> _unroll_n_naive_fq_acc
      = fun unnfqa r -> begin
        let {_unnfqa_unno; _unnfqa_p} = unnfqa in
        (match r with
        | R_list [] -> unnfqa
        | R_list (hd :: tl) -> foldf (foldf unnfqa hd) (R_list tl) (* note : be aware of execution order *)
        | R_state ss -> {unnfqa with _unnfqa_p=(PSet.map _unnfqa_p ~f:(List.cons (P_state ss)))}
        | R_loop rl_mci -> 
          let new_unno        : unroll_n_naive_output = accumulate rl_mci _unnfqa_unno in
          let ff_paths        : path PSet.t           = pmap_find_dft new_unno.unno_p rl_mci ~default:PSet.empty in
          let inloop_n_paths  : path list PSet.t      = _cons_combine_n_acc unnp_num ff_paths (PSet.singleton []) |> PSet.map ~f:(List.rev) in
          let acc_rev_paths   : path list PSet.t      = PSet.map inloop_n_paths ~f:(fun pl -> P_loop (rl_mci, pl)) |> (fun ps -> _cons_combine ps _unnfqa_p) in
          {_unnfqa_unno=new_unno; _unnfqa_p=acc_rev_paths;}
        )
      end in (* internal function foldf end *)
      let {_unnfqa_unno; _unnfqa_p;} = foldf {_unnfqa_unno=fq_unno; _unnfqa_p=(PSet.singleton []);} q.rq_r in
      let queries = PSet.map _unnfqa_p ~f:(fun x -> {pq_p=(P_list (List.rev x)); pq_mci=q.rq_mci; pq_qc=q.rq_qc;}) in
      { _unnfqa_unno with
        unno_q=(psetmap_update_union _unnfqa_unno.unno_q cur_mci queries);
      }
    end in (* internal function fill_q end *)
    (* if already visited, escape *)
    if PSet.mem acc_unno.unno_visited cur_mci then acc_unno
    else ( (* main procedure *)
      let routes_r = pmap_find_dft unnp_r cur_mci ~default:PSet.empty in
      let routes_q = pmap_find_dft unnp_q cur_mci ~default:PSet.empty in
      acc_unno 
      |> (fun unno -> PSet.fold routes_r ~init:unno ~f:fill_p)
      |> (fun unno -> PSet.fold routes_q ~init:unno ~f:fill_q)
      |> (fun unno -> {unno with unno_visited=(PSet.add unno.unno_visited cur_mci)})
    )
  end in (* internal function accumulate end *)
  let init_unno = {unno_visited=PSet.empty; unno_p=PMap.empty; unno_q=PMap.empty;} in
  accumulate unnp_trx_entry_mci init_unno
end (* function unroll_n_naive end *)


(*****************************************************************************)
(*****************************************************************************)
(* Utility - Route to Json - Only MCI                                        *)
(*****************************************************************************)
(*****************************************************************************)

module R2Jomci = struct
  type js = Yojson.Safe.t
  open Jc
  let cv_mci = TzCvt.T2J.cv_mich_cut_info
  let rec cv_route : route -> js
  = (function
    | R_list rl -> `List (List.map cv_route rl)
    | R_state ss -> `Tuple [cv_mci ss.ss_entry_mci; cv_mci ss.ss_block_mci]
    | R_loop mci -> `Variant (lur_r_loop, Some (cv_mci mci))
  ) (* function cv_route end *)
  let cv_route_q : route_q -> js
  = fun {rq_r; rq_mci; rq_qc;} -> begin
    `Assoc [
      lur_jc_rq_r, cv_route rq_r;
      lur_jc_rq_mci, cv_mci rq_mci;
      lur_jc_rq_qc, TzCvt.S2J.cv_qc rq_qc;
    ]
  end (* function cv_route_q end *)
end (* module R2Jomci end *)



