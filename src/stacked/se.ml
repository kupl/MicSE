(* Symbolic Executer *)

open Tz

exception Error of string
exception DebugInstSS of (Tz.mich_i Tz.cc * Tz.sym_state)
exception DebugTT of (Tz.mich_t * Tz.mich_t)

type query_category =
  (* Each of them are indicator of "State -> Formula" function *)
  | Q_mutez_add_no_overflow
  | Q_mutez_sub_no_underflow
  | Q_mutez_mul_mnm_no_overflow
  | Q_mutez_mul_nmm_no_overflow
  | Q_shiftleft_safe
  | Q_shiftright_safe
  | Q_assertion

type state_set = {
  running : Tz.sym_state Tz.PSet.t;
  blocked : Tz.sym_state Tz.PSet.t;
  queries : (Tz.sym_state * query_category) Tz.PSet.t;
  terminated : Tz.sym_state Tz.PSet.t;
}

type cache = {
  ch_entered_loop : Tz.mich_cut_info Tz.PSet.t;
  ch_entered_lmbd : Tz.mich_cut_info Tz.PSet.t;
}

type invmap = (Tz.mich_cut_info, (Tz.mich_f Tz.PSet.t)) Tz.PMap.t


(*****************************************************************************)
(*****************************************************************************)
(* Se to Json                                                                *)
(*****************************************************************************)
(*****************************************************************************)

module S2J = struct
  type js = Yojson.Safe.t
  open Jc
  open TzCvt
  let cv_qc : query_category -> js
  = (function
    | Q_mutez_add_no_overflow -> `Variant (q_mutez_add_no_overflow, None)
    | Q_mutez_sub_no_underflow -> `Variant (q_mutez_sub_no_underflow, None)
    | Q_mutez_mul_mnm_no_overflow -> `Variant (q_mutez_mul_mnm_no_overflow, None)
    | Q_mutez_mul_nmm_no_overflow -> `Variant (q_mutez_mul_nmm_no_overflow, None)
    | Q_shiftleft_safe -> `Variant (q_shiftleft_safe, None)
    | Q_shiftright_safe -> `Variant (q_shiftright_safe, None)
    | Q_assertion -> `Variant (q_assertion, None)
    ) (* function cv_qc end *)
  let cv_sset : state_set -> js
  = let s2l s = Tz.PSet.map s ~f:(T2J.cv_ss) |> Tz.PSet.to_list in
    let sqs2l s = Tz.PSet.map s ~f:(fun (ss, qc) -> `Tuple [T2J.cv_ss ss; cv_qc qc;]) |> Tz.PSet.to_list in
    fun sset -> begin
    `Assoc [
      jc_running,     `List (s2l sset.running);
      jc_blocked,     `List (s2l sset.blocked);
      jc_queries,     `List (sqs2l sset.queries);
      jc_terminated,  `List (s2l sset.terminated);
    ]
  end (* function cv_sset end *)
  let cv_cache : (cache ref) -> js
  = let s2l s = Tz.PSet.map s ~f:(T2J.cv_mich_cut_info) |> Tz.PSet.to_list in
    fun c -> begin
    `Assoc [
      jc_ch_entered_loop, `List (s2l !c.ch_entered_loop);
      jc_ch_entered_lmbd, `List (s2l !c.ch_entered_lmbd);
    ]
  end (* function cv_cache end *)
end (* module S2J end *)


(*****************************************************************************)
(*****************************************************************************)
(* Utilities - Cache                                                         *)
(*****************************************************************************)
(*****************************************************************************)

let init_cache : unit -> cache ref
= fun () ->
  ref {
    ch_entered_loop = PSet.empty;
    ch_entered_lmbd = PSet.empty;
  }

let add_entered_loop : cache ref -> Tz.mich_cut_info -> unit
= fun cache mci ->
  let newset = PSet.add !cache.ch_entered_loop mci in
  (cache := {!cache with ch_entered_loop=newset})
let is_entered_loop : cache ref -> Tz.mich_cut_info -> bool
= fun cache mci -> PSet.mem !cache.ch_entered_loop mci

let add_entered_lmbd : cache ref -> Tz.mich_cut_info -> unit
= fun cache mci ->
  let newset = PSet.add !cache.ch_entered_lmbd mci in
  (cache := {!cache with ch_entered_lmbd=newset})
let is_entered_lmbd : cache ref -> Tz.mich_cut_info -> bool
= fun cache mci -> PSet.mem !cache.ch_entered_lmbd mci


(*****************************************************************************)
(*****************************************************************************)
(* Utilities - Query                                                         *)
(*****************************************************************************)
(*****************************************************************************)

let state_query_reduce : Tz.sym_state * query_category -> Tz.mich_f
= fun (ss, qc) -> begin
  let (h,h2) = (Core.List.hd_exn ss.ss_symstack, Core.List.nth_exn ss.ss_symstack 1) in
  match qc with
  | Q_mutez_add_no_overflow -> MF_add_mmm_no_overflow (h,h2)
  | Q_mutez_sub_no_underflow -> MF_sub_mmm_no_underflow (h,h2)
  | Q_mutez_mul_mnm_no_overflow -> MF_mul_mnm_no_overflow (h,h2)
  | Q_mutez_mul_nmm_no_overflow -> MF_mul_nmm_no_overflow (h,h2)
  | Q_shiftleft_safe -> MF_shiftL_nnn_rhs_in_256 (h,h2)
  | Q_shiftright_safe -> MF_shiftR_nnn_rhs_in_256 (h,h2)
  | Q_assertion -> MF_is_true (Core.List.hd_exn ss.ss_symstack)
end (* function state_query_reduce end *)


(*****************************************************************************)
(*****************************************************************************)
(* Utilities - State Set Mapping                                             *)
(*****************************************************************************)
(*****************************************************************************)

let map_ss_running : (Tz.sym_state -> Tz.sym_state) -> state_set -> state_set
= fun map_f {running; blocked; queries; terminated} -> {running=(Tz.PSet.map running ~f:map_f); blocked; queries; terminated;}


(*****************************************************************************)
(*****************************************************************************)
(* Run Instruction                                                           *)
(*****************************************************************************)
(*****************************************************************************)

let rec run_inst : cache ref -> (mich_i cc) -> state_set -> state_set
= fun cache inst ss_set -> begin 
  PSet.fold ss_set.running 
    ~init:{running=PSet.empty; blocked=ss_set.blocked; queries=ss_set.queries; terminated=ss_set.terminated}
    ~f:(
      fun {running; blocked; queries; terminated} ss ->
      let {running=ps; blocked=bs; queries=qs; terminated=ts} = run_inst_i cache inst ss in
      {running=(PSet.union ps running); blocked=(PSet.union bs blocked); queries=(PSet.union qs queries); terminated=(PSet.union ts terminated)}
    )
end (* function run_inst end *)

(* It might raise unhandled exceptions if the given michelson program cannot pass Michelson type system. *)
and run_inst_i : cache ref -> (mich_i cc) -> sym_state -> state_set
= (* SUGAR - module name *)
  let module CList = Core.List in
  (* SUGAR - push to stack *)
  let sstack_push : (mich_v cc list) -> (mich_v cc) -> (mich_v cc list) = fun sstack v -> (v :: sstack) in
  let cons_tl_n : (mich_v cc list) -> int -> (mich_v cc) -> (mich_v cc list)
    (* cons_tl_n sstack 1 v => (v :: sstack-tail) *)
    (* cons_tl_n sstack 2 v => (v :: sstack-tailtail) *)
  = fun sstack n v -> let (_, tl_n) = CList.split_n sstack n in (v :: tl_n)
  in
  let cons2_tl_n : (mich_v cc list) -> int -> (mich_v cc * mich_v cc) -> (mich_v cc list)
    (* cons2_tl_n sstack 1 v => (v1 :: v2 :: sstack-tail) *)
    (* cons2_tl_n sstack 2 v => (v1 :: v2 :: sstack-tailtail) *)
  = fun sstack n (v1,v2) -> let (_, tl_n) = CList.split_n sstack n in (v1 :: v2 :: tl_n)
  in
  (* SUGAR - record update *)
  let sstack_to_ss : sym_state -> (mich_v cc list) -> sym_state
  = fun ss sstack -> {ss with ss_symstack=sstack}
  in
  let ss_to_srset : sym_state -> state_set
  = fun ss -> {running=PSet.singleton(ss); blocked=PSet.empty; queries=PSet.empty; terminated=PSet.empty}
  in
  let sstack_to_srset : sym_state -> (mich_v cc list) -> state_set
  = fun ss sstack -> sstack |> sstack_to_ss ss |> ss_to_srset 
  in
  let new_ss_for_loopinst : sym_state -> mich_cut_info -> (mich_v cc list) -> sym_state
  = (* function new_ss_for_loopinst start *)
    fun ss mci sstack -> begin
    let mutez_constraints : mich_f list = (
      sstack
      |> CList.filter ~f:(fun vvv -> (typ_of_val vvv).cc_v = MT_mutez)
      |> CList.map ~f:(fun vvv -> (MF_and [ (* MUTEZ BOUND *)
            MF_add_mmm_no_overflow (vvv, ((MV_lit_mutez Z.zero) |> gen_dummy_cc));
            MF_sub_mmm_no_underflow (vvv, ((MV_lit_mutez Z.zero) |> gen_dummy_cc));
          ]))) in
    {ss with ss_entry_mci=mci; ss_entry_symstack=sstack; ss_block_mci=mci; ss_symstack=sstack; ss_constraints=mutez_constraints;} 
  end in (* function new_ss_for_loopinst end *)
  let update_block_mci : mich_cut_info -> sym_state -> sym_state = fun mci ss -> {ss with ss_block_mci=mci} 
  in
  let update_queryblock_mci : Tz.ccp_loc -> sym_state -> sym_state = fun loc ss -> {ss with ss_block_mci={mci_loc=loc; mci_cutcat=Tz.MCC_query}}
  in
  (* SUGAR - constraint update *)
  let ss_add_constraint : sym_state -> mich_f -> sym_state
  = fun ss fmla -> {ss with ss_constraints=(fmla :: ss.ss_constraints)}
  in
  let ss_add_mutez_bound_constraint_if_v_is_mutez : sym_state -> mich_v cc -> sym_state
  = fun ss vvv -> begin
    if (typ_of_val vvv).cc_v = MT_mutez then 
      ss_add_constraint
        ss 
        (MF_and [ (* MUTEZ BOUND *)
          MF_add_mmm_no_overflow (vvv, ((MV_lit_mutez Z.zero) |> gen_dummy_cc));
          MF_sub_mmm_no_underflow (vvv, ((MV_lit_mutez Z.zero) |> gen_dummy_cc));
        ])
    else ss
  end in (* function ss_add_mutez_bound_constraint_if_v_is_mutez end *)
  let ss_add_list_sigma_constraint : sym_state -> mich_v cc -> sym_state
  = (* function ss_add_list_sigma_constraint start *)
    fun ss vvv -> begin
    (* 0. Make same expression of list with using cons *)
    let hd_vvv : mich_v cc = MV_hd_l vvv |> gen_custom_cc vvv in
    let tl_vvv : mich_v cc = MV_tl_l vvv |> gen_custom_cc vvv in
    let cons_vvv : mich_v cc = MV_cons (hd_vvv, tl_vvv) |> gen_custom_cc vvv in
    (* 1. Get components from each element *)
    (match (typ_of_val vvv).cc_v with
    | MT_list (t1) -> (
      MV_symbol (
        t1, 
        ( { Jc.Fsvn.typ=`Elem;
            Jc.Fsvn.c_vn="elem";
            Jc.Fsvn.c_acc_l=[];
            Jc.Fsvn.e_acc_l=[]; }  |> Jc.Fsvn.to_string))
      |> gen_dummy_cc
      |> Comp.base_comp_from_v
      |> (fun e -> Comp.collect e Comp.CTMap.empty))
    | _ -> Error ("run_inst : ss_add_list_sigma_constraint : _") |> Stdlib.raise)
    (* 2. Add constraints from each type of element *)
    |> Comp.CTMap.fold
      ~init:ss
      ~f:(fun ~key ~data ss' -> (
        match key.cc_v with
        | MT_mutez -> (
          PSet.fold
            data
            ~init:ss'
            ~f:(fun acc c -> (
              ss_add_constraint
                acc
                (MF_eq (
                  (MV_sigma_lm (vvv, c.Comp.cp_value) |> gen_dummy_cc),
                  (MV_sigma_lm (cons_vvv, c.Comp.cp_value) |> gen_dummy_cc)))
              |> (fun ss -> ss_add_mutez_bound_constraint_if_v_is_mutez
                ss
                (MV_sigma_lm (tl_vvv, c.Comp.cp_value) |> gen_dummy_cc)))))
        | _ -> ss'))
  end in (* function ss_add_list_sigma_constraint end *)
  (* SUGAR - state set *)
  let sset_union_pointwise : state_set -> state_set -> state_set
  = fun sset1 sset2 -> {
    running = PSet.union sset1.running sset2.running;
    blocked = PSet.union sset1.blocked sset2.blocked;
    queries = PSet.union sset1.queries sset2.queries;
    terminated = PSet.union sset1.terminated sset2.terminated;
  } in
  let empty_sset : state_set = {running=PSet.empty; blocked=PSet.empty; queries=PSet.empty; terminated=PSet.empty} in
  let move_running_to_blocked : mich_cut_info -> state_set -> state_set
  = fun mci sset -> {sset with running=PSet.empty; blocked=(PSet.union (PSet.map sset.running ~f:(update_block_mci mci)) sset.blocked)} 
  in
  (* SUGAR - michelson value *)
  let mich_int_0 : mich_v cc = MV_lit_int Z.zero |> gen_dummy_cc in
  (* FUNCTION BEGIN *)
  fun cache inst ss -> begin
  (* DEBUG START *) 
  (* let _ = 
    let cv_pos : Tz.ccp_pos -> Yojson.Safe.t
    = fun {col; lin} -> `Tuple [`Int lin; `Int col] (* function cv_pos end *) in
    List.length ss.ss_symstack |> print_int;
    print_string "  ";
    (function CCLOC_Unknown -> `Variant (Jc.cc_l_unk, None) | CCLOC_Pos (p1, p2) -> `Variant (Jc.cc_l_pos, Some (`Tuple [cv_pos p1; cv_pos p2]))) inst.cc_loc
    |> Yojson.Safe.pretty_to_string |> print_endline
  in *)
  (* DEBUG END *)
  let gen_inst_cc : 'a -> 'a cc = gen_custom_cc inst in
  let 
    { ss_fixchain=_;
      ss_exop=_;
      ss_dynchain=_;
      ss_exec_addrs=_;
      ss_oper_queue=_;
      ss_optt=_;
      ss_entry_mci=_;
      ss_entry_symstack=_;
      ss_block_mci=_;
      ss_symstack;
      ss_constraints=_;
    } = ss in
  (*
  (* resolve ss_symstack optimization issue *)
  let ss_symstack : mich_v cc list = (
    match ss_symstack with
    | [] -> []
    | hd :: tl -> (Tz.optimize_v hd) :: tl
  ) in
  *)
  match inst.cc_v with
  | MI_seq (i1,i2) -> ss |> ss_to_srset |> run_inst cache i1 |> run_inst cache i2
  (* | MI_seq (i1, i2) ->
    let result_i1 = run_inst_i cache i1 ss in
    let {running=r1; blocked=b1; queries=q1; terminated=t1;} = result_i1 in
    (match (PSet.length r1, PSet.length b1, PSet.length q1, PSet.length t1) with
    | 1, 0, 0, 0 -> print_endline (Stdlib.__LOC__ ^ " : DEBUG : 1"); run_inst_i cache i2 (PSet.choose_exn r1)
    | _ -> print_endline (Stdlib.__LOC__ ^ " : DEBUG : NONOOOs"); run_inst cache i2 result_i1
    ) *)
  | MI_drop zn -> (CList.split_n ss_symstack (Z.to_int zn) |> Stdlib.snd) |> sstack_to_srset ss
  | MI_dup zn -> (CList.nth_exn ss_symstack (Z.to_int zn - 1)) :: ss_symstack |> sstack_to_srset ss
  | MI_swap ->
    (match ss_symstack with
      | h1 :: h2 :: tl -> (h2 :: h1 :: tl)
      | _ -> Error "run_inst_i : MI_swap" |> raise)
    |> sstack_to_srset ss
  | MI_dig zn ->
    (match CList.split_n ss_symstack (Z.to_int zn) with
      | (hdlst, (tlhd :: tltl)) -> (tlhd :: hdlst @ tltl)
      | _ -> Error ("run_inst_i : MI_dig" ^ (zn |> Z.to_string)) |> raise)
    |> sstack_to_srset ss
  | MI_dug zn ->
    (match CList.split_n ss_symstack (Z.to_int zn + 1) with
      | ((hdhd :: hdtl), tl) -> (hdtl @ (hdhd :: tl))
      | _ -> Error ("run_inst_i : MI_dug" ^ (zn |> Z.to_string)) |> raise)
    |> sstack_to_srset ss
  | MI_push (_, v) -> 
    (v :: ss_symstack)
    |> sstack_to_ss (ss_add_mutez_bound_constraint_if_v_is_mutez ss v)
    |> ss_to_srset
  | MI_some -> (MV_some (CList.hd_exn ss_symstack) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_none t -> (MV_none t |> gen_inst_cc) |> sstack_push ss_symstack |> sstack_to_srset ss 
  | MI_unit -> (MV_unit |> gen_inst_cc) |> sstack_push ss_symstack |> sstack_to_srset ss
  | MI_if_none (i1,i2) ->
    let cond_constraint : mich_f = MF_is_none (CList.hd_exn ss_symstack) in
    let then_br_sset : state_set =
      (CList.tl_exn ss_symstack)
      |> sstack_to_ss (ss_add_constraint ss cond_constraint)
      (* |> ss_to_srset
      |> run_inst cache i1 *)
      |> run_inst_i cache i1
    in
    let else_br_sset : state_set = 
      let some_vvv : mich_v cc = MV_unlift_option (CList.hd_exn ss_symstack) |> gen_inst_cc in
      some_vvv
      |> cons_tl_n ss_symstack 1
      |> sstack_to_ss (ss_add_mutez_bound_constraint_if_v_is_mutez (ss_add_constraint ss (MF_not cond_constraint)) some_vvv)
      (* |> ss_to_srset
      |> run_inst cache i2 *)
      |> run_inst_i cache i2
    in
    sset_union_pointwise then_br_sset else_br_sset
  | MI_pair ->
    (* (MV_pair (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1) |> gen_inst_cc)
    |> cons_tl_n ss_symstack 2
    |> sstack_to_srset ss *)
    (try (MV_pair (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1) |> gen_inst_cc)
    |> cons_tl_n ss_symstack 2
    |> sstack_to_srset ss 
  with | _ ->DebugInstSS (inst, ss) |> raise
    )
  | MI_car ->
    let vvv : mich_v cc = (
      MV_car (CList.hd_exn ss_symstack)
      |> gen_inst_cc) in
    vvv
    |> cons_tl_n ss_symstack 1
    |> sstack_to_ss (ss_add_mutez_bound_constraint_if_v_is_mutez ss vvv)
    |> ss_to_srset
  | MI_cdr -> 
    let vvv : mich_v cc = (
      MV_cdr (CList.hd_exn ss_symstack)
      |> gen_inst_cc) in
    vvv
    |> cons_tl_n ss_symstack 1
    |> sstack_to_ss (ss_add_mutez_bound_constraint_if_v_is_mutez ss vvv)
    |> ss_to_srset
  | MI_left t -> (MV_left (t,(CList.hd_exn ss_symstack)) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_right t -> (MV_right (t,(CList.hd_exn ss_symstack)) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_if_left (i1,i2) ->
    let cond_constraint : mich_f = MF_is_left (CList.hd_exn ss_symstack) in
    let then_br_sset : state_set =
      let left_vvv : mich_v cc = MV_unlift_left (CList.hd_exn ss_symstack) |> gen_inst_cc in
      left_vvv
      |> cons_tl_n ss_symstack 1
      |> sstack_to_ss (ss_add_mutez_bound_constraint_if_v_is_mutez (ss_add_constraint ss cond_constraint) left_vvv)
      (* |> ss_to_srset
      |> run_inst cache i1 *)
      |> run_inst_i cache i1
    in
    let else_br_sset : state_set =
      let right_vvv : mich_v cc = MV_unlift_right (CList.hd_exn ss_symstack) |> gen_inst_cc in
      right_vvv
      |> cons_tl_n ss_symstack 1
      |> sstack_to_ss (ss_add_mutez_bound_constraint_if_v_is_mutez (ss_add_constraint ss (MF_not cond_constraint)) right_vvv)
      (* |> ss_to_srset
      |> run_inst cache i2 *)
      |> run_inst_i cache i2
    in
    sset_union_pointwise then_br_sset else_br_sset
  | MI_nil t -> (MV_nil t |> gen_inst_cc) |> sstack_push ss_symstack |> sstack_to_srset ss
  | MI_cons -> 
    (MV_cons (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1) |> gen_inst_cc)
    |> cons_tl_n ss_symstack 2
    |> sstack_to_srset ss
  | MI_if_cons (i1,i2) ->
    let cond_constraint : mich_f = MF_is_cons (CList.hd_exn ss_symstack) in
    let then_br_sset : state_set =
      let listv : mich_v cc = CList.hd_exn ss_symstack in
      let hd_vvv : mich_v cc = MV_hd_l listv |> gen_inst_cc in
      (hd_vvv, gen_inst_cc (MV_tl_l listv))
      |> cons2_tl_n ss_symstack 1
      |> sstack_to_ss (
        ss_add_list_sigma_constraint (
          ss_add_mutez_bound_constraint_if_v_is_mutez 
            (ss_add_constraint ss cond_constraint) 
            hd_vvv) 
          listv)
      (* |> ss_to_srset
      |> run_inst cache i1 *)
      |> run_inst_i cache i1
    in
    let else_br_sset : state_set =
      (CList.tl_exn ss_symstack)
      |> sstack_to_ss (ss_add_constraint ss (MF_not cond_constraint))
      (* |> ss_to_srset
      |> run_inst cache i2 *)
      |> run_inst_i cache i2
    in
    sset_union_pointwise then_br_sset else_br_sset
  | MI_size ->
    let h = CList.hd_exn ss_symstack in
    (match (typ_of_val h).cc_v with
      | MT_set _ -> MV_size_s h
      | MT_map _ -> MV_size_m h
      | MT_list _ -> MV_size_l h
      | MT_string -> MV_size_str h
      | MT_bytes -> MV_size_b h
      | _ -> Error "run_inst_i : MI_size" |> raise)
    |> gen_inst_cc
    |> cons_tl_n ss_symstack 1
    |> sstack_to_srset ss
  | MI_empty_set t -> (MV_empty_set t |> gen_inst_cc) |> sstack_push ss_symstack |> sstack_to_srset ss
  | MI_empty_map (t1,t2) -> (MV_empty_map (t1,t2) |> gen_inst_cc) |> sstack_push ss_symstack |> sstack_to_srset ss
  | MI_empty_big_map (t1,t2) -> (MV_empty_big_map (t1,t2) |> gen_inst_cc) |> sstack_push ss_symstack |> sstack_to_srset ss
  | MI_map (i) ->
    let (outer_cutcat, inner_cutcat) : (mich_cut_category * mich_cut_category) = (MCC_ln_map, MCC_lb_map) in
    let blocked_mci : mich_cut_info = {mci_loc=inst.cc_loc; mci_cutcat=outer_cutcat} in
    let thenbr_mci : mich_cut_info = {mci_loc=inst.cc_loc; mci_cutcat=inner_cutcat} in
    let elsebr_mci : mich_cut_info = blocked_mci in
    let container_t : mich_t cc = CList.hd_exn ss_symstack |> typ_of_val in
    let elem_t : mich_t cc = (
      match container_t.cc_v with
      | MT_list e -> e
      | MT_map (kt,vt) -> MT_pair (kt,vt) |> gen_custom_cc container_t
      | _ -> Error "run_inst_i : MI_map : elem_t" |> raise
    ) in
    (* 1. Make current state to blocked state *)
    let blocked_state : sym_state = update_block_mci blocked_mci ss in
    (* 2. check if this loop instruction was entered already. if not, add it to the cache too. *)
    let entered_flag : bool = is_entered_loop cache blocked_mci in
    let _ = if entered_flag then () else (List.iter (add_entered_loop cache) [blocked_mci; thenbr_mci; elsebr_mci]) in
    (* 3. make then-branch (loop-body) running state and run *)
    let thenbr_sset_raw : state_set =  
      (* if this map instruction is already in cache's entered_loop, then skip it. *)
      if entered_flag then empty_sset else 
      (* from "symstack for then-branch" to "state-set" *)
      ((gen_new_symval_t elem_t) :: (gen_newvar_symstack_vs (CList.tl_exn ss_symstack)))
      |> new_ss_for_loopinst ss thenbr_mci
      |> run_inst_i cache i
    in
    let res_container_t : mich_t cc =
      (* get type of container in top of stack to propagate to else branch *)
      thenbr_sset_raw.running
      |> PSet.fold ~init:None ~f:(fun acc sset -> (
        let cur_t = sset.ss_symstack |> CList.hd_exn |> typ_of_val in
        if Option.is_none acc then Some cur_t
        else begin
          let acc_t = Option.get acc in
          if cur_t = acc_t then acc
          else Error "run_inst_i : MI_map : result_container_type : fold" |> Stdlib.raise
        end))
      |> (function Some t -> t | None -> Error "run_inst_i : MI_map : result_container_type" |> Stdlib.raise)
      in
    let thenbr_sset : state_set = 
      (* convert every running states in inner_sset into blocked cases *)
      thenbr_sset_raw
      |> move_running_to_blocked thenbr_mci in
    (* 4. make else-branch (loop-exit) running state *)
    let elsebr_sset : state_set =
      (* if this map instruction is already in cache's entered_loop, then skip it. *)
      if entered_flag then empty_sset else
      (* from "symstack for else-branch" to "state-set" *)
      ((gen_new_symval_t res_container_t) :: (gen_newvar_symstack_vs (CList.tl_exn ss_symstack)))
      |> new_ss_for_loopinst ss elsebr_mci
      |> ss_to_srset
    in
    sset_union_pointwise {thenbr_sset with blocked=(PSet.add thenbr_sset.blocked blocked_state)} elsebr_sset
  | MI_iter (i) ->
    let (outer_cutcat, inner_cutcat) : (mich_cut_category * mich_cut_category) = (MCC_ln_iter, MCC_lb_iter) in
    let blocked_mci : mich_cut_info = {mci_loc=inst.cc_loc; mci_cutcat=outer_cutcat} in
    let thenbr_mci : mich_cut_info = {mci_loc=inst.cc_loc; mci_cutcat=inner_cutcat} in
    let elsebr_mci : mich_cut_info = blocked_mci in
    let container_t : mich_t cc = CList.hd_exn ss_symstack |> typ_of_val in
    let elem_t : mich_t cc = (
      match container_t.cc_v with
      | MT_list e -> e
      | MT_set e -> e
      | MT_map (kt,vt) -> MT_pair (kt,vt) |> gen_custom_cc container_t
      | _ -> Error "run_inst_i : MI_iter : elem_t" |> raise
    ) in
    (* refer MI_map case for detailed explanation *)
    let blocked_state : sym_state = update_block_mci blocked_mci ss in
    let entered_flag : bool = is_entered_loop cache blocked_mci in
    let _ = if entered_flag then () else (List.iter (add_entered_loop cache) [blocked_mci; thenbr_mci; elsebr_mci]) in
    let thenbr_sset : state_set = 
      if entered_flag then empty_sset else 
      ((gen_new_symval_t elem_t) :: (gen_newvar_symstack_vs (CList.tl_exn ss_symstack)))
      |> new_ss_for_loopinst ss thenbr_mci
      |> run_inst_i cache i
      |> move_running_to_blocked thenbr_mci
    in
    (* symstack for MI_iter's elsebr_ss is different from MI_map's symstack for else-branch *)
    let elsebr_sset : state_set =
      if entered_flag then empty_sset else
      (gen_newvar_symstack_vs (CList.tl_exn ss_symstack))
      |> new_ss_for_loopinst ss elsebr_mci
      |> ss_to_srset
    in
    sset_union_pointwise {thenbr_sset with blocked=(PSet.add thenbr_sset.blocked blocked_state)} elsebr_sset
  | MI_mem ->
    let (h, h2) = (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1) in
    (match (typ_of_val h2).cc_v with
      | MT_set _ -> MV_mem_xsb (h,h2)
      | MT_map _ -> MV_mem_xmb (h,h2)
      | MT_big_map _ -> MV_mem_xbmb (h,h2)
      | _ -> Error "run_inst_i : MI_mem" |> raise)
    |> gen_inst_cc
    |> cons_tl_n ss_symstack 2
    |> sstack_to_srset ss
  | MI_get ->
    let (h, h2) = (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1) in
    let get_vvv : mich_v cc = (
      match (typ_of_val h2).cc_v with
      | MT_map _ -> MV_get_xmoy (h,h2)
      | MT_big_map _ -> MV_get_xbmo (h,h2)
      | _ -> Error "run_inst_i : MI_get" |> raise)
      (* | _ -> DebugSS ss |> raise) *)
      |> gen_inst_cc in
    get_vvv
    |> cons_tl_n ss_symstack 2
    |> sstack_to_ss (ss_add_mutez_bound_constraint_if_v_is_mutez ss get_vvv)
    |> ss_to_srset
  | MI_update ->
    let (h, h2, h3) = (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1, CList.nth_exn ss_symstack 2) in
    (match (typ_of_val h3).cc_v with
      | MT_set _ -> MV_update_xbss (h,h2,h3)
      | MT_map _ -> MV_update_xomm (h,h2,h3)
      | MT_big_map _ -> MV_update_xobmbm (h,h2,h3)
      | _ -> Error "run_inst_i : MI_update" |> raise)
    |> gen_inst_cc
    |> cons_tl_n ss_symstack 3
    |> sstack_to_srset ss
  | MI_if (i1,i2) ->
    let cond_constraint : mich_f = MF_is_true (CList.hd_exn ss_symstack) in
    let then_br_sset : state_set =
      (CList.tl_exn ss_symstack)
      |> sstack_to_ss (ss_add_constraint ss cond_constraint)
      (* |> ss_to_srset
      |> run_inst cache i1 *)
      |> run_inst_i cache i1
    in
    let else_br_sset : state_set =
      (CList.tl_exn ss_symstack)
      |> sstack_to_ss (ss_add_constraint ss (MF_not cond_constraint))
      (* |> ss_to_srset
      |> run_inst cache i2 *)
      |> run_inst_i cache i2
    in
    sset_union_pointwise then_br_sset else_br_sset
  | MI_loop (i) ->
    let (outer_cutcat, inner_cutcat) : (mich_cut_category * mich_cut_category) = (MCC_ln_loop, MCC_lb_loop) in
    let blocked_mci : mich_cut_info = {mci_loc=inst.cc_loc; mci_cutcat=outer_cutcat} in
    let thenbr_mci : mich_cut_info = {mci_loc=inst.cc_loc; mci_cutcat=inner_cutcat} in
    let elsebr_mci : mich_cut_info = blocked_mci in
    (* refer MI_map case for detailed explanation *)
    let blocked_state : sym_state = update_block_mci blocked_mci ss in
    let entered_flag : bool = is_entered_loop cache blocked_mci in
    let _ = if entered_flag then () else (List.iter (add_entered_loop cache) [blocked_mci; thenbr_mci; elsebr_mci]) in
    let thenbr_sset : state_set = 
      if entered_flag then empty_sset else 
      (gen_newvar_symstack_vs (CList.tl_exn ss_symstack))
      |> new_ss_for_loopinst ss thenbr_mci
      |> run_inst_i cache i
      |> move_running_to_blocked thenbr_mci
    in
    (* symstack for MI_iter's elsebr_ss is different from MI_map's symstack for else-branch *)
    let elsebr_sset : state_set =
      if entered_flag then empty_sset else
      (gen_newvar_symstack_vs (CList.tl_exn ss_symstack))
      |> new_ss_for_loopinst ss elsebr_mci
      |> ss_to_srset
    in
    sset_union_pointwise {thenbr_sset with blocked=(PSet.add thenbr_sset.blocked blocked_state)} elsebr_sset
  | MI_loop_left (i) ->
    let (outer_cutcat, inner_cutcat) : (mich_cut_category * mich_cut_category) = (MCC_ln_loopleft, MCC_lb_loopleft) in
    let blocked_mci : mich_cut_info = {mci_loc=inst.cc_loc; mci_cutcat=outer_cutcat} in
    let thenbr_mci : mich_cut_info = {mci_loc=inst.cc_loc; mci_cutcat=inner_cutcat} in
    let elsebr_mci : mich_cut_info = blocked_mci in
    (* refer MI_map case for detailed explanation *)
    let container_t : mich_t cc = CList.hd_exn ss_symstack |> typ_of_val in
    let (left_elem_t, right_elem_t) : mich_t cc * mich_t cc = (
      match container_t.cc_v with
      | MT_or (t1,t2) -> (t1,t2)
      | _ -> Error "run_inst_i : MI_loop_left" |> raise
    ) in
    let blocked_state : sym_state = update_block_mci blocked_mci ss in
    let entered_flag : bool = is_entered_loop cache blocked_mci in
    let _ = if entered_flag then () else (List.iter (add_entered_loop cache) [blocked_mci; thenbr_mci; elsebr_mci]) in
    let thenbr_sset : state_set =  
      if entered_flag then empty_sset else 
      ((gen_new_symval_t left_elem_t) :: (gen_newvar_symstack_vs (CList.tl_exn ss_symstack)))
      |> new_ss_for_loopinst ss thenbr_mci
      |> run_inst_i cache i
      |> move_running_to_blocked thenbr_mci
    in
    (* 3. make else-branch (loop-exit) running state *)
    let elsebr_sset : state_set = 
      if entered_flag then empty_sset else
      ((gen_new_symval_t right_elem_t) :: (gen_newvar_symstack_vs (CList.tl_exn ss_symstack)))
      |> new_ss_for_loopinst ss elsebr_mci
      |> ss_to_srset
    in
    sset_union_pointwise {thenbr_sset with blocked=(PSet.add thenbr_sset.blocked blocked_state)} elsebr_sset
  | MI_lambda (t1,t2,i) ->
    (*
    (* perform symbolic execution for instruction-i *)
    let funcbody_mci : mich_cut_info = {mci_loc=i.cc_loc; mci_cutcat=MCC_lb_lmbd;} in
    let entered_flag : bool = is_entered_lmbd cache funcbody_mci in
    let _ = if entered_flag then () else (add_entered_lmbd cache funcbody_mci) in
    let funcbody_sset : state_set = 
      if entered_flag then empty_sset else
      [gen_new_symval_t t1]
      |> new_ss_for_loopinst ss funcbody_mci
      |> run_inst_i cache i
      |> move_running_to_blocked funcbody_mci
    in
      ...
    *)
    (MV_lit_lambda (t1,t2,i) |> gen_inst_cc) |> sstack_push ss_symstack |> sstack_to_srset ss
  | MI_exec ->
    (* MicSE does not perform any symbolic execution for the body of the function in MI_exec case.
      Thus it does not create any blocked state, just emit a running state that has one new variable on the top of the stack. *)
    let lmbd_ty : mich_t cc = CList.nth_exn ss_symstack 1 |> typ_of_val in
    let (_, ret_ty) : mich_t cc * mich_t cc = (
      match lmbd_ty.cc_v with
      | MT_lambda (t1,t2) -> (t1,t2)
      | _ -> Error "run_inst_i : MI_exec" |> raise
    ) in
    let ret_vvv : mich_v cc = (
      (gen_new_symval_t ret_ty).cc_v
      |> gen_inst_cc) in
    ret_vvv
    |> cons_tl_n ss_symstack 2
    |> sstack_to_ss (ss_add_mutez_bound_constraint_if_v_is_mutez ss ret_vvv)
    |> ss_to_srset
  | MI_dip_n (zn, i) ->
    let (hd, tl) = CList.split_n ss_symstack (Z.to_int zn) in
    let hd_mutez_constraints : mich_f list = (
      hd
      |> CList.filter ~f:(fun vvv -> (typ_of_val vvv).cc_v = MT_mutez)
      |> CList.map ~f:(fun vvv -> (MF_and [ (* MUTEZ BOUND *)
            MF_add_mmm_no_overflow (vvv, ((MV_lit_mutez Z.zero) |> gen_dummy_cc));
            MF_sub_mmm_no_underflow (vvv, ((MV_lit_mutez Z.zero) |> gen_dummy_cc));
          ]))) in
    let i_sset : state_set = tl |> sstack_to_ss ss |> run_inst_i cache i in
    let restored_running : sym_state PSet.t = 
      PSet.map 
        i_sset.running 
        ~f:(fun x -> {x with ss_symstack=(hd @ x.ss_symstack); ss_constraints=(hd_mutez_constraints @ x.ss_constraints)}) in
    {i_sset with running=restored_running}
  | MI_failwith -> 
    let blocked_mci : mich_cut_info = {mci_loc=inst.cc_loc; mci_cutcat=MCC_trx_exit} in
    update_block_mci blocked_mci ss
    |> (fun x -> {running=PSet.empty; blocked=PSet.empty; queries=PSet.empty; terminated=PSet.singleton(x)})
  | MI_cast _ -> 
    (* Currently, it is enough to make "MI_cast"'s symbolic execution as identity function. *)
    ss_to_srset ss
  | MI_rename ->
    let renamed_hd : mich_v cc = {(CList.hd_exn ss_symstack) with cc_anl=inst.cc_anl} in
    (renamed_hd :: CList.tl_exn ss_symstack) |> sstack_to_srset ss
  | MI_concat ->
    let hd = CList.hd_exn ss_symstack in
    (match (typ_of_val hd).cc_v with
      | MT_string ->
        (MV_concat_sss (hd, CList.nth_exn ss_symstack 1) |> gen_inst_cc)
        |> cons_tl_n ss_symstack 2
        |> sstack_to_srset ss
      | MT_bytes ->
        (MV_concat_bbb (hd, CList.nth_exn ss_symstack 1) |> gen_inst_cc)
        |> cons_tl_n ss_symstack 2
        |> sstack_to_srset ss
      | MT_list et when et.cc_v = MT_string ->
        (MV_concat_list_s hd |> gen_inst_cc)
        |> cons_tl_n ss_symstack 1
        |> sstack_to_srset ss
      | MT_list et when et.cc_v = MT_bytes ->
        (MV_concat_list_b hd |> gen_inst_cc)
        |> cons_tl_n ss_symstack 1
        |> sstack_to_srset ss
      | _ -> Error "run_inst_i : MI_concat" |> raise
    )
  | MI_slice ->
    let (h,h2,h3) = (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1, CList.nth_exn ss_symstack 2) in
    (match (typ_of_val h3).cc_v with
      | MT_string -> (MV_slice_nnso (h,h2,h3) |> gen_inst_cc) |> cons_tl_n ss_symstack 3 |> sstack_to_srset ss
      | MT_bytes -> (MV_slice_nnbo (h,h2,h3) |> gen_inst_cc) |> cons_tl_n ss_symstack 3 |> sstack_to_srset ss
      | _ -> Error "run_inst_i : MI_slice" |> raise
    )
  | MI_pack ->
    (MV_pack (CList.hd_exn ss_symstack) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_unpack t ->
    let unpack_vvv : mich_v cc = (
      MV_unpack (t, CList.hd_exn ss_symstack) 
      |> gen_inst_cc) in
    unpack_vvv
    |> cons_tl_n ss_symstack 1
    |> sstack_to_ss (ss_add_mutez_bound_constraint_if_v_is_mutez ss unpack_vvv)
    |> ss_to_srset
  | MI_add ->
    let (h,h2) = (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1) in
    let gen_srset : mich_v -> state_set
    = fun mv -> (mv |> gen_inst_cc) |> cons_tl_n ss_symstack 2 |> sstack_to_srset ss
    in
    (match (typ_of_val h).cc_v, (typ_of_val h2).cc_v with
    | MT_nat, MT_int -> MV_add_nii (h,h2) |> gen_srset
    | MT_int, MT_nat -> MV_add_ini (h,h2) |> gen_srset
    | MT_int, MT_int -> MV_add_iii (h,h2) |> gen_srset
    | MT_nat, MT_nat -> MV_add_nnn (h,h2) |> gen_srset
    | MT_timestamp, MT_int -> MV_add_tit (h,h2) |> gen_srset
    | MT_int, MT_timestamp -> MV_add_itt (h,h2) |> gen_srset
    | MT_mutez, MT_mutez ->
      (* for mutez addition, 
          - query added
          - non-overflow constraint should be added to running state
      *)
      let added_state : sym_state = (MV_add_mmm (h,h2) |> gen_inst_cc) |> cons_tl_n ss_symstack 2 |> sstack_to_ss ss in
      let runstate : sym_state = ss_add_constraint added_state (MF_add_mmm_no_overflow (h,h2)) in
      let query_ss : sym_state = ss |> update_queryblock_mci inst.cc_loc in
      let query : sym_state * query_category = (query_ss, Q_mutez_add_no_overflow) in
      {empty_sset with running=(PSet.singleton runstate); queries=(PSet.singleton query);}
    | _ -> Error "run_inst_i : MI_add" |> raise
    )
  | MI_sub ->
    let (h,h2) = (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1) in
    let gen_srset : mich_v -> state_set
    = fun mv -> (mv |> gen_inst_cc) |> cons_tl_n ss_symstack 2 |> sstack_to_srset ss
    in
    (match (typ_of_val h).cc_v, (typ_of_val h2).cc_v with
    | MT_nat, MT_nat -> MV_sub_nni (h,h2) |> gen_srset
    | MT_nat, MT_int -> MV_sub_nii (h,h2) |> gen_srset
    | MT_int, MT_nat -> MV_sub_ini (h,h2) |> gen_srset
    | MT_int, MT_int -> MV_sub_iii (h,h2) |> gen_srset
    | MT_timestamp, MT_timestamp -> MV_sub_tti (h,h2) |> gen_srset
    | MT_timestamp, MT_int -> MV_sub_tit (h,h2) |> gen_srset
    | MT_mutez, MT_mutez ->
      (* for mutez subtraction, 
          - query added
          - non-underflow constraint should be added to running state
      *)
      let subed_state : sym_state = (MV_sub_mmm (h,h2) |> gen_inst_cc) |> cons_tl_n ss_symstack 2 |> sstack_to_ss ss in
      let runstate : sym_state = ss_add_constraint subed_state (MF_sub_mmm_no_underflow (h,h2)) in
      let query_ss : sym_state = ss |> update_queryblock_mci inst.cc_loc in
      let query : sym_state * query_category = (query_ss, Q_mutez_sub_no_underflow) in
      {empty_sset with running=(PSet.singleton runstate); queries=(PSet.singleton query);}
    (* | _ -> Error "run_inst_i : MI_sub" |> raise *)
    | t1, t2 -> DebugTT (t1, t2) |> raise
    )
  | MI_mul ->
    let (h,h2) = (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1) in
    let gen_srset : mich_v -> state_set
    = fun mv -> (mv |> gen_inst_cc) |> cons_tl_n ss_symstack 2 |> sstack_to_srset ss
    in
    (match (typ_of_val h).cc_v, (typ_of_val h2).cc_v with
    | MT_nat, MT_nat -> MV_mul_nnn (h,h2) |> gen_srset
    | MT_nat, MT_int -> MV_mul_nii (h,h2) |> gen_srset
    | MT_int, MT_nat -> MV_mul_ini (h,h2) |> gen_srset
    | MT_int, MT_int -> MV_mul_iii (h,h2) |> gen_srset
    | MT_mutez, MT_nat ->
      (* for mutez addition, 
          - query added
          - non-overflow constraint should be added to running state
      *)
      let muled_state : sym_state = (MV_mul_mnm (h,h2) |> gen_inst_cc) |> cons_tl_n ss_symstack 2 |> sstack_to_ss ss in
      let runstate : sym_state = ss_add_constraint muled_state (MF_mul_mnm_no_overflow (h,h2)) in
      let query_ss : sym_state = ss |> update_queryblock_mci inst.cc_loc in
      let query : sym_state * query_category = (query_ss, Q_mutez_mul_mnm_no_overflow) in
      {empty_sset with running=(PSet.singleton runstate); queries=(PSet.singleton query);}
    | MT_nat, MT_mutez ->
      (* for mutez addition, 
          - query added
          - non-overflow constraint should be added to running state
      *)
      let muled_state : sym_state = (MV_mul_nmm (h,h2) |> gen_inst_cc) |> cons_tl_n ss_symstack 2 |> sstack_to_ss ss in
      let runstate : sym_state = ss_add_constraint muled_state (MF_mul_nmm_no_overflow (h,h2)) in
      let query_ss : sym_state = ss |> update_queryblock_mci inst.cc_loc in
      let query : sym_state * query_category = (query_ss, Q_mutez_mul_nmm_no_overflow) in
      {empty_sset with running=(PSet.singleton runstate); queries=(PSet.singleton query);}
    | _ -> Error "run_inst_i : MI_mul" |> raise
    )
  | MI_ediv -> 
    let (h,h2) = (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1) in
    let gen_srset : mich_v -> state_set
    = fun mv -> (mv |> gen_inst_cc) |> cons_tl_n ss_symstack 2 |> sstack_to_srset ss
    in
    (match (typ_of_val h).cc_v, (typ_of_val h2).cc_v with
    | MT_nat, MT_nat -> MV_ediv_nnnn (h,h2) |> gen_srset
    | MT_nat, MT_int -> MV_ediv_niin (h,h2) |> gen_srset
    | MT_int, MT_nat -> MV_ediv_inin (h,h2) |> gen_srset
    | MT_int, MT_int -> MV_ediv_iiin (h,h2) |> gen_srset
    | MT_mutez, MT_nat -> MV_ediv_mnmm (h,h2) |> gen_srset
    | MT_mutez, MT_mutez -> MV_ediv_mmnm (h,h2) |> gen_srset
    | _ -> Error "run_inst_i : MI_ediv" |> raise
    )
  | MI_abs -> (MV_abs_in (CList.hd_exn ss_symstack) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_isnat -> (MV_isnat (CList.hd_exn ss_symstack) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_int -> (MV_int_of_nat (CList.hd_exn ss_symstack) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_neg ->
    let h = CList.hd_exn ss_symstack in 
    let gen_srset : mich_v -> state_set
    = fun mv -> (mv |> gen_inst_cc) |> cons_tl_n ss_symstack 2 |> sstack_to_srset ss
    in
    (match (typ_of_val h).cc_v with
    | MT_nat -> MV_neg_ni (h) |> gen_srset
    | MT_int -> MV_neg_ii (h) |> gen_srset
    | _ -> Error "run_inst_i : MI_neg" |> raise
    )
  | MI_lsl ->
    (* for left logical shift, 
        - query added
        - in-bound constraint should be added to running state
    *)
    let (h,h2) = (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1) in
    let shifted_state : sym_state = (MV_shiftL_nnn (h,h2) |> gen_inst_cc) |> cons_tl_n ss_symstack 2 |> sstack_to_ss ss in
    let runstate : sym_state = ss_add_constraint shifted_state (MF_shiftL_nnn_rhs_in_256 (h,h2)) in
    let query_ss : sym_state = ss |> update_queryblock_mci inst.cc_loc in
    let query : sym_state * query_category = (query_ss, Q_shiftleft_safe) in
    {empty_sset with running=(PSet.singleton runstate); queries=(PSet.singleton query);}
  | MI_lsr ->
    (* for right logical shift, 
        - query added
        - in-bound constraint should be added to running state
    *)
    let (h,h2) = (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1) in
    let shifted_state : sym_state = (MV_shiftR_nnn (h,h2) |> gen_inst_cc) |> cons_tl_n ss_symstack 2 |> sstack_to_ss ss in
    let runstate : sym_state = ss_add_constraint shifted_state (MF_shiftR_nnn_rhs_in_256 (h,h2)) in
    let query_ss : sym_state = ss |> update_queryblock_mci inst.cc_loc in
    let query : sym_state * query_category = (query_ss, Q_shiftright_safe) in
    {empty_sset with running=(PSet.singleton runstate); queries=(PSet.singleton query);}
  | MI_or ->
    let (h,h2) = (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1) in
    let gen_srset : mich_v -> state_set
    = fun mv -> (mv |> gen_inst_cc) |> cons_tl_n ss_symstack 2 |> sstack_to_srset ss
    in
    (match (typ_of_val h).cc_v, (typ_of_val h2).cc_v with
    | MT_nat, MT_nat -> MV_or_nnn (h,h2) |> gen_srset
    | MT_bool, MT_bool -> MV_or_bbb (h,h2) |> gen_srset
    | _ -> Error "run_inst_i : MI_or" |> raise
    )
  | MI_and ->
    let (h,h2) = (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1) in
    let gen_srset : mich_v -> state_set
    = fun mv -> (mv |> gen_inst_cc) |> cons_tl_n ss_symstack 2 |> sstack_to_srset ss
    in
    (match (typ_of_val h).cc_v, (typ_of_val h2).cc_v with
    | MT_nat, MT_nat -> MV_and_nnn (h,h2) |> gen_srset
    | MT_int, MT_nat -> MV_and_inn (h,h2) |> gen_srset
    | MT_bool, MT_bool -> MV_and_bbb (h,h2) |> gen_srset
    | _ -> Error "run_inst_i : MI_and" |> raise
    )
  | MI_xor ->
    let (h,h2) = (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1) in
    let gen_srset : mich_v -> state_set
    = fun mv -> (mv |> gen_inst_cc) |> cons_tl_n ss_symstack 2 |> sstack_to_srset ss
    in
    (match (typ_of_val h).cc_v, (typ_of_val h2).cc_v with
    | MT_nat, MT_nat -> MV_xor_nnn (h,h2) |> gen_srset
    | MT_bool, MT_bool -> MV_xor_bbb (h,h2) |> gen_srset
    | _ -> Error "run_inst_i : MI_xor" |> raise
    )
  | MI_not -> 
    let h = CList.hd_exn ss_symstack in
    let gen_srset : mich_v -> state_set
    = fun mv -> (mv |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
    in
    (match (typ_of_val h).cc_v with
    | MT_nat -> MV_not_ni (h) |> gen_srset
    | MT_int -> MV_not_ii (h) |> gen_srset
    | MT_bool -> MV_not_bb (h) |> gen_srset
    | _ -> Error "run_inst_i : MI_not" |> raise
    )
  | MI_compare ->
    (MV_compare (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1) |> gen_inst_cc)
    |> cons_tl_n ss_symstack 2
    |> sstack_to_srset ss
  | MI_eq -> (MV_eq_ib (CList.hd_exn ss_symstack, mich_int_0) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_neq -> (MV_neq_ib (CList.hd_exn ss_symstack, mich_int_0) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_lt -> (MV_lt_ib (CList.hd_exn ss_symstack, mich_int_0) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_gt -> (MV_gt_ib (CList.hd_exn ss_symstack, mich_int_0) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_le -> (MV_leq_ib (CList.hd_exn ss_symstack, mich_int_0) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_ge -> (MV_geq_ib (CList.hd_exn ss_symstack, mich_int_0) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_self -> 
    (MV_lit_contract ((typ_of_val ss.ss_optt.optt_param), ss.ss_optt.optt_addr) |> gen_inst_cc) 
    |> sstack_push ss_symstack
    |> sstack_to_srset ss
  (*
  | MI_contract t ->
    (* In the concrete execution, Michelson-VM will seek fixed-blockchain if there are any contract exists.
      However, it is impossible to encode such information in Tz.bc_code.
      So it is the best to branch current running-state, one for None and the other for Some (new-contract).
      Tz.sym_state should be modified to contain a new kind of constraint, such as 
      "we already say that the fixed-chain contains a contract of the address 'kt1abcdef'." (* TODO !!!! *)
    *)
    let found_br_sset : state_set =
      (MV_some (MV_lit_contract (t, CList.hd_exn ss_symstack) |> gen_inst_cc) |> gen_inst_cc)
      |> cons_tl_n ss_symstack 1
      |> sstack_to_srset ss
    in
    let not_found_br_sset : state_set =
      (MV_none (MT_contract t |> gen_custom_cc inst) |> gen_inst_cc)
      |> cons_tl_n ss_symstack 1
      |> sstack_to_srset ss
    in
    sset_union_pointwise found_br_sset not_found_br_sset
  *)
  | MI_contract t ->
    (* As current MicSE's verification does not require any blockchain value, 
      previous approach which generates two running states will be deprecated. *)
    (MV_contract_of_address (t, CList.hd_exn ss_symstack) |> gen_inst_cc)
    |> cons_tl_n ss_symstack 1
    |> sstack_to_srset ss
  | MI_transfer_tokens ->
    let (h,h2,h3) = (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1, CList.nth_exn ss_symstack 2) in
    (MV_transfer_tokens (h,h2,h3) |> gen_inst_cc)
    |> cons_tl_n ss_symstack 3
    |> sstack_to_srset ss
  | MI_set_delegate ->
    (MV_set_delegate (CList.hd_exn ss_symstack) |> gen_inst_cc)
    |> cons_tl_n ss_symstack 1
    |> sstack_to_srset ss
  | MI_create_account -> Error "run_inst_i : MI_create_account : deprecated instruction" |> raise
  | MI_create_contract (t1,t2,i) -> 
    let ctrt_input_typ : mich_t cc = MT_pair (t1,t2) |> gen_custom_cc inst in
    let operlist_typ : mich_t cc = MT_list (gen_custom_cc inst MT_operation) |> gen_custom_cc inst in
    let ctrt_output_typ : mich_t cc = MT_pair (operlist_typ, t2) |> gen_custom_cc inst in
    let lambda : mich_v cc = MV_lit_lambda (ctrt_input_typ,ctrt_output_typ,i) |> gen_inst_cc in
    let new_addr : mich_v cc = gen_new_symval_t (gen_custom_cc inst MT_address) in
    let (h,h2,h3) = (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1, CList.nth_exn ss_symstack 2) in
    ((MV_create_contract (ctrt_input_typ, ctrt_output_typ, lambda, h, h2, h3, new_addr) |> gen_inst_cc), new_addr)
    |> cons2_tl_n ss_symstack 3
    |> sstack_to_srset ss
  | MI_implicit_account ->
    (MV_implicit_account (CList.hd_exn ss_symstack) |> gen_inst_cc)
    |> cons_tl_n ss_symstack 1
    |> sstack_to_srset ss
  | MI_now -> (ss.ss_optt.optt_now) |> sstack_push ss_symstack |> sstack_to_srset ss
  | MI_amount -> 
    let amount_vvv : mich_v cc = ss.ss_optt.optt_amount in
    amount_vvv
    |> sstack_push ss_symstack
    |> sstack_to_ss (ss_add_mutez_bound_constraint_if_v_is_mutez ss amount_vvv)
    |> ss_to_srset
  | MI_balance -> 
    let blce_opt : mich_v cc = MV_get_xmoy (ss.ss_optt.optt_addr, ss.ss_dynchain.bc_balance) |> gen_inst_cc in
    let blce_vvv : mich_v cc = MV_unlift_option blce_opt |> gen_inst_cc in
    blce_vvv
    |> sstack_push ss_symstack
    |> sstack_to_ss (ss_add_mutez_bound_constraint_if_v_is_mutez (ss_add_constraint ss (MF_not (MF_is_none blce_opt))) blce_vvv)
    |> ss_to_srset
  | MI_check_signature ->
    let (h,h2,h3) = (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1, CList.nth_exn ss_symstack 2) in
    (MV_check_signature (h,h2,h3) |> gen_inst_cc)
    |> cons_tl_n ss_symstack 3
    |> sstack_to_srset ss
  | MI_blake2b -> (MV_blake2b (CList.hd_exn ss_symstack) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_sha256 -> (MV_sha256 (CList.hd_exn ss_symstack) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_sha512 -> (MV_sha512 (CList.hd_exn ss_symstack) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_hash_key -> (MV_hash_key (CList.hd_exn ss_symstack) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_steps_to_quota -> Error "run_inst_i : MI_steps_to_quota : deprecated instruction" |> raise
  | MI_source -> (ss.ss_optt.optt_source) |> sstack_push ss_symstack |> sstack_to_srset ss
  | MI_sender -> (ss.ss_optt.optt_sender) |> sstack_push ss_symstack |> sstack_to_srset ss
  | MI_address -> (MV_address_of_contract (CList.hd_exn ss_symstack) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_chain_id -> (ss.ss_dynchain.bc_chain_id) |> sstack_push ss_symstack |> sstack_to_srset ss
  | MI_unpair -> 
    let h = CList.hd_exn ss_symstack in
    let a_vvv : mich_v cc = MV_car h |> gen_inst_cc in
    let d_vvv : mich_v cc = MV_cdr h |> gen_inst_cc in
    (a_vvv, d_vvv)
    |> cons2_tl_n ss_symstack 1
    |> sstack_to_ss (ss_add_mutez_bound_constraint_if_v_is_mutez (ss_add_mutez_bound_constraint_if_v_is_mutez ss a_vvv) d_vvv)
    |> ss_to_srset
  | MI_micse_check (i) ->
    (* dealing with micse-check
        - bring running states from the result of "i", and convert them to queries.
    *)
    let qset : (sym_state * query_category) PSet.t = PSet.map (run_inst_i cache i ss).running ~f:(fun x -> ((update_queryblock_mci inst.cc_loc x), Q_assertion)) in
    {(ss_to_srset ss) with queries=qset;}
end (* function run_inst_i end *)


(*****************************************************************************)
(*****************************************************************************)
(* Utilities - Run Instruction                                               *)
(*****************************************************************************)
(*****************************************************************************)

(* "run_inst_in_fog" symbolic executes the given contract with no other information provided. *)
let run_contract_in_fog : (Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc) -> (Tz.sym_state * (cache ref) * state_set)
= (* SUGARS *)
  let gdcc = gen_dummy_cc in
  let addr_typ = gdcc MT_address
    and chainid_typ = gdcc MT_chain_id
    and mutez_typ = gdcc MT_mutez
    and operation_typ = gdcc MT_operation
    and opt_typ t = gdcc (MT_option t)
    and timestamp_typ = gdcc MT_timestamp
  in
  let int_one = gdcc (MV_lit_int Z.one)
    and empty_addr_set = gdcc (MV_empty_set addr_typ)
    and empty_operation_list = gdcc (MV_nil operation_typ) 
  in

  fun (param_typ, storage_typ, inst) -> begin
  
  (* 0. address *)
  let ctrt_addr : mich_v cc = gen_new_symval_ts addr_typ Jc.Rcfv.address in

  (* 1. blockchain *)
  let ctrt_storage : mich_v cc = gen_new_symval_ts storage_typ Jc.Rcfv.storage in
  let ctrt_balance : mich_v cc = gen_new_symval_ts mutez_typ Jc.Rcfv.balance in
  let ctrt_delegate : mich_v cc = gen_new_symval_ts (opt_typ addr_typ) Jc.Rcfv.delegate in
  let balancemap : (mich_v cc, mich_v cc) PMap.t = PMap.singleton ctrt_addr ctrt_balance in
  let delegatemap : (mich_v cc, mich_v cc) PMap.t = PMap.singleton ctrt_addr ctrt_delegate in
  let blockchain_fix : blockchain = {
    bc_storage = PMap.singleton ctrt_addr ctrt_storage;
    bc_code = PMap.singleton ctrt_addr inst;
    bc_balance = pmap_to_mtmap (addr_typ, mutez_typ, balancemap);
    bc_delegate = pmap_to_mtmap (addr_typ, (opt_typ addr_typ), delegatemap);
    bc_chain_id = gen_new_symval_ts chainid_typ Jc.Rcfv.chainid;
    bc_last_blocktime = gen_new_symval_ts timestamp_typ Jc.Rcfv.lastblocktime;
  } in

  (* 2. transfer-token operation *)
  let tt_amount : mich_v cc = gen_new_symval_ts mutez_typ Jc.Rcfv.tt_amount in
  let tt_source : mich_v cc = gen_new_symval_ts addr_typ Jc.Rcfv.tt_source in
  let tt_param : mich_v cc = gen_new_symval_ts param_typ Jc.Rcfv.tt_param in
  let exop : explicit_operation = EXOP_transfer_token (ctrt_addr, tt_source, tt_amount, tt_param) in
  let transfertoken_op : oper_transfertoken = {
    optt_addr = ctrt_addr;
    optt_source = tt_source;
    optt_sender = gen_new_symval_ts addr_typ Jc.Rcfv.optt_sender;
    optt_amount = tt_amount;
    optt_param = tt_param;
    optt_now = MV_add_tit (blockchain_fix.bc_last_blocktime, int_one) |> gdcc;
  } in

  (* 3. set dynamic-chain *)
  let updated_balance : mich_v cc = MV_add_mmm (ctrt_balance, tt_amount) |> gdcc in
  let updated_balancemap : (mich_v cc, mich_v cc) PMap.t = PMap.singleton ctrt_addr updated_balance in
  let updated_balance_no_overflow : mich_f = MF_add_mmm_no_overflow (ctrt_balance, tt_amount) in  (* DON'T FORGET TO ADD THIS CONSTRAINT *)
  let blockchain_dyn : blockchain = {blockchain_fix with bc_balance=(pmap_to_mtmap (addr_typ, mutez_typ, updated_balancemap));} in
  
  (* 4. symbolic state *)
  let init_mci : mich_cut_info = {
    mci_loc=inst.cc_loc;
    mci_cutcat=MCC_trx_entry;
  } in
  let end_mci : mich_cut_info = {
    mci_loc=inst.cc_loc;
    mci_cutcat=MCC_trx_exit;
  } in
  let init_symstack : mich_v cc list = [MV_pair (tt_param, ctrt_storage) |> gdcc] in
  let init_ss : sym_state = {
    ss_fixchain=blockchain_fix;
    ss_exop=exop;
    ss_dynchain=blockchain_dyn;
    ss_exec_addrs=empty_addr_set;
    ss_oper_queue=empty_operation_list;
    ss_optt=transfertoken_op;
    ss_entry_mci=init_mci;
    ss_entry_symstack=init_symstack;
    ss_block_mci=end_mci; (* for convenience to get the information of the code's end, set this to "end_mci" *)
    ss_symstack=init_symstack;
    ss_constraints=[updated_balance_no_overflow];
  } in

  (* run & block *)
  let cache = init_cache () in
  let run_result_0 : state_set = run_inst_i cache inst init_ss in
  let run_result : state_set = {
    run_result_0 with
    running=PSet.empty;
    blocked=(PSet.union (PSet.map run_result_0.running ~f:(fun ss -> {ss with ss_block_mci=end_mci})) run_result_0.blocked);
  } in

  (* DEBUG START *)
  (* 0. blocked state *)
  (* PSet.iter
    run_result.blocked
    ~f:(fun bs -> Utils.Log.debug (fun m -> m "\n> Entry-MCI: %s\n> Entry-Stack: \n%s\n> Exit-MCI: %s\n> Exit-Stack:\n%s\n"
          (bs.ss_entry_mci |> TzCvt.T2Jnocc.cv_mich_cut_info |> Yojson.Safe.to_string)
          (bs.ss_entry_symstack |> Core.List.map ~f:(fun x -> x |> TzCvt.T2Jnocc.cv_mvcc |> Yojson.Safe.to_string) |> Core.String.concat ~sep:"\n")
          (bs.ss_block_mci |> TzCvt.T2Jnocc.cv_mich_cut_info |> Yojson.Safe.to_string)
          (bs.ss_symstack |> Core.List.map ~f:(fun x -> x |> TzCvt.T2Jnocc.cv_mvcc |> Yojson.Safe.to_string) |> Core.String.concat ~sep:"\n"))); *)
  (* DEBUG END *)

  (* return *)
  (init_ss, cache, run_result)

end (* function run_inst_in_fog end *)


(*****************************************************************************)
(*****************************************************************************)
(* Generate Inductiveness & Query Formula                                    *)
(*****************************************************************************)
(*****************************************************************************)

(*****************************************************************************)
(* Get Coupled mich_cut_info for indicating invariant                        *)
(*****************************************************************************)

let get_normal_mci : invmap -> Tz.mich_cut_info -> Tz.mich_cut_info
= let module CPMap = Core.Map.Poly in
  let module CList = Core.List in
  (* function get_normal_mcc start *)
  fun invm mci -> begin
  match mci.mci_cutcat with
  | MCC_trx_entry
  | MCC_ln_loop
  | MCC_ln_loopleft
  | MCC_ln_map
  | MCC_ln_iter     -> mci
  | MCC_trx_exit    -> (
    CPMap.keys invm
    |> CList.find ~f:(fun m -> Tz.is_trx_entry_mcc m.mci_cutcat)
    |> (function Some mmm -> mmm | None -> Error "get_normal_mci : MCC_trx_exit" |> Stdlib.raise))
  | MCC_lb_loop
  | MCC_lb_loopleft
  | MCC_lb_map
  | MCC_lb_iter     -> mci |> Tz.ln_of_lb_exn ~debug:("get_normal_mci : " ^ (mci |> TzCvt.T2Jnocc.cv_mich_cut_info |> Yojson.Safe.to_string))
  | MCC_query       -> Error "get_normal_mci : MCC_query" |> Stdlib.raise
end (* function get_normal_mci end *)


(*****************************************************************************)
(* Initial invariant map                                                     *)
(*****************************************************************************)

let true_invmap_of_blocked_sset : Tz.sym_state Tz.PSet.t -> invmap
= let pm_add_if_possible k v pmap = (
    if Tz.is_normal_mcc k.mci_cutcat then
      PMap.add pmap ~key:k ~data:v |> (function | `Ok m -> m | `Duplicate -> pmap)
    else pmap) in
  fun blocked_set -> begin
  PSet.fold blocked_set ~init:PMap.empty 
    ~f:(fun accm bl_ss -> 
      accm 
      |> pm_add_if_possible bl_ss.ss_entry_mci (MF_true |> Tz.PSet.singleton)
      |> pm_add_if_possible bl_ss.ss_block_mci (MF_true |> Tz.PSet.singleton)
    )
end (* function true_invmap_from_blocked end *)


(*****************************************************************************)
(* Invariant Application form for each mich_cut_category                     *)
(*****************************************************************************)

let extract_typ_stack : Tz.mich_v Tz.cc list -> Tz.mich_t Tz.cc list
= let module CList = Core.List in
  (* function get_type_stack start *)
  fun vs -> begin
  CList.fold_right
    vs
    ~f:(fun v ts -> 
      (* (Utils.Log.debug (fun m -> m "Se : extract_typ_stack : v : %s" (v |> TzCvt.T2J.cv_mvcc |> Yojson.Safe.to_string))); *)
      let t = (v |> Tz.typ_of_val |> Tz.get_dummy_cc_of_typ) in
      (* (Utils.Log.debug (fun m -> m "Se : extract_typ_stack : t : %s" (t |> TzCvt.T2J.cv_mtcc |> Yojson.Safe.to_string))); *)
      t::ts)
    ~init:[]
end (* function get_type stack end *)

let inv_app_guide_vstack : mich_f Core.Set.Poly.t -> ([`Entry | `Block] * mich_cut_info option * mich_v cc list) -> mich_f
= let module CList = Core.List in
  let module CPSet = Core.Set.Poly in
  (* function inv_app_guide_vstack start *)
  fun inv_fs (mci_loc, mci_opt, vs) -> begin
  (* 0. set value stack following the mci *)
  let vs' : mich_v cc list = (
    if Option.is_none mci_opt then vs
    else (
      match (Option.get mci_opt).mci_cutcat, mci_loc with
      | MCC_trx_entry   , `Entry
      | MCC_trx_exit    , `Block -> (
        if CList.length vs <> 1 then (Error "inv_app_guide_vstack : vs' : MCC_trx_entry | MCC_trx_exit" |> Stdlib.raise)
        else (vs |> CList.hd_exn |> (fun v -> MV_cdr v |> gen_dummy_cc))::[])
      | MCC_ln_loop     , `Entry
      | MCC_lb_loop     , `Entry
      | MCC_ln_iter     , `Entry
      | MCC_lb_iter     , `Block -> vs
      | MCC_ln_loop     , `Block
      | MCC_lb_loop     , `Block
      | MCC_ln_loopleft , `Entry
      | MCC_lb_loopleft , `Entry
      | MCC_ln_loopleft , `Block
      | MCC_lb_loopleft , `Block
      | MCC_lb_iter     , `Entry
      | MCC_ln_iter     , `Block
      | MCC_ln_map      , `Entry
      | MCC_lb_map      , `Entry
      | MCC_ln_map      , `Block
      | MCC_lb_map      , `Block -> vs |> CList.tl |> (function Some lll -> lll | None -> Error "inv_app_guide_vstack : vs' : CList.tl" |> Stdlib.raise)
      | _ -> Error ("inv_app_guide_vstack : vs' : " ^ (mci_opt |> Option.get |> TzCvt.T2Jnocc.cv_mich_cut_info |> Yojson.Safe.to_string)) |> Stdlib.raise)) in
  (* 1. make base variable and value pair *)
  let vsp : (mich_v cc * mich_v cc) list = (
    CList.mapi vs'
      ~f:(fun loc v -> (
        if (Option.is_some mci_opt) && ((Option.get mci_opt).mci_cutcat = MCC_trx_exit || (Option.get mci_opt).mci_cutcat = MCC_trx_entry) && loc = 0 then (
          let t : mich_t cc = v |> typ_of_val in
          (gen_dummy_cc (MV_symbol ((t |> get_dummy_cc_of_typ), Jc.Locvn.for_strg))), v)
        else
          let t : mich_t cc = v |> typ_of_val in
          (gen_dummy_cc (MV_symbol ((t |> get_dummy_cc_of_typ), (Jc.Locvn.to_string {loc=loc; acc_l=[]})))), v))) in
  (* 2. substitute each base variable to proper value *)
  CList.fold vsp
    ~init:(MF_and (inv_fs |> CPSet.to_list))
    ~f:(fun accinv (bv, v) -> 
      (map_f_v2v_outer accinv ~v2v:(fun e -> if e.cc_v = bv.cc_v then Some v else None))) 
end (* function inv_app_guide_vstack end *)

let inv_app_guide_entry : Tz.mich_f Core.Set.Poly.t -> Tz.sym_state -> Tz.mich_f
= (* function inv_app_guide_entry start *)
  fun inv_fs ss -> begin
  inv_app_guide_vstack inv_fs (`Entry, Some ss.ss_entry_mci, ss.ss_entry_symstack)
end (* function inv_app_guide_entry end *)

let inv_app_guide_block : Tz.mich_f Core.Set.Poly.t -> Tz.sym_state -> Tz.mich_f
= (* function inv_app_guide_block start *)
  fun inv_fs ss -> begin
  inv_app_guide_vstack inv_fs (`Block, Some ss.ss_block_mci, ss.ss_symstack)
end (* function inv_app_guide_block end *)


(*****************************************************************************)
(* Invariant Map to Michelson Formula                                        *)
(*****************************************************************************)

let find_inv_fmla : invmap -> Tz.mich_cut_info -> Tz.mich_f PSet.t
= (* function find_inv_fmla start *)
  fun invm mci -> begin
  PMap.find invm (get_normal_mci invm mci)
  |> (function Some sss -> sss | None -> PSet.empty)
end (* function find_inv_fmla end *)

let inv_induct_fmla_i : Tz.sym_state -> invmap -> Tz.mich_f
= fun blocked_ss invm -> begin
  let inv_entry : Tz.mich_f PSet.t = find_inv_fmla invm blocked_ss.ss_entry_mci in
  let inv_block : Tz.mich_f PSet.t = find_inv_fmla invm blocked_ss.ss_block_mci in
  match (PSet.length inv_entry, PSet.length inv_block) with
  | 0, _ | _, 0 -> Error "inv_induct_fmla : cannot find invariant" |> raise
  | _ -> MF_imply (MF_and ((inv_app_guide_entry inv_entry blocked_ss) :: blocked_ss.ss_constraints), inv_app_guide_block inv_block blocked_ss)
end (* function inv_induct_fmla_i end *) 
let inv_induct_fmla : (Tz.sym_state Tz.PSet.t) -> invmap -> (Tz.mich_f Tz.PSet.t)
= fun blocked_sset invm -> begin
  PSet.map blocked_sset ~f:(fun bl_ss -> inv_induct_fmla_i bl_ss invm)
end (* function inv_induct_fmla end *)
let inv_query_fmla : (Tz.sym_state * query_category) -> invmap -> Tz.mich_f
= fun query_ssp invm -> begin
  let query_ss = Stdlib.fst query_ssp in
  let inv_entry = find_inv_fmla invm query_ss.ss_entry_mci 
    |> (fun s -> if PSet.length s > 0 then s else  Error "inv_query_fmla : inv_entry : not-found" |> raise)
  in
  let entry_fmla = inv_app_guide_entry inv_entry query_ss in
  let query = state_query_reduce query_ssp in
  MF_imply (MF_and (entry_fmla :: query_ss.ss_constraints), query)
end (* function inv_query_fmla end *)
let inv_query_fmla_with_precond : (Tz.sym_state * query_category) -> invmap -> Tz.mich_f -> Tz.mich_f
= fun query_ssp invm precond -> begin
  let query_ss = Stdlib.fst query_ssp in
  let inv_entry = find_inv_fmla invm query_ss.ss_entry_mci 
    |> (fun s -> if PSet.length s > 0 then s else  Error "inv_query_fmla : inv_entry : not-found" |> raise)
  in
  let entry_fmla = inv_app_guide_entry inv_entry query_ss in
  let query = state_query_reduce query_ssp in
  MF_imply (MF_and (precond :: entry_fmla :: query_ss.ss_constraints), query)
end (* function inv_query_fmla_with_precond end *)


(*****************************************************************************)
(*****************************************************************************)
(* State Merging Utilities                                                   *)
(*****************************************************************************)
(*****************************************************************************)


(*****************************************************************************)
(* Set Structured Variable Names to mich_v and mich_f                        *)
(*****************************************************************************)

let set_stvn_sym_tmpl : (int option * int option) -> (Tz.mich_v Tz.cc -> Tz.mich_v Tz.cc option)
= fun (tnopt, lnopt) -> begin
  (fun mvcc -> 
    match mvcc.cc_v with 
    | MV_symbol (tcc, s) -> Some {mvcc with cc_v=(MV_symbol (tcc, Jc.Stvn.set_both_n (tnopt, lnopt) s));}
    | _ -> None
  )
end (* function set_stvn_sym_tmpl end *)

let set_stvn_mv : (int option * int option) -> Tz.mich_v Tz.cc -> Tz.mich_v Tz.cc
= fun (tnopt, lnopt) vvvv -> begin
  map_v_v2v_outer vvvv ~v2v:(set_stvn_sym_tmpl (tnopt, lnopt))
end (* function set_stvn_mv end *)
let set_stvn_mf : (int option * int option) -> Tz.mich_f -> Tz.mich_f
= fun (tnopt, lnopt) ffff -> begin
  map_f_v2v_outer ffff ~v2v:(set_stvn_sym_tmpl (tnopt, lnopt))
end (* function set_stvn_mf end *)

