(* Symbolic Executer *)

open Tz

exception Error of string

type query_category =
  (* Each of them are indicator of "State -> Formula" function *)
  | Q_mutez_add_overflow
  | Q_mutez_mul_overflow
  | Q_mutez_sub_underflow
  | Q_shiftleft_prohibited
  | Q_shiftright_prohibited
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
  = fun ss mci sstack -> {ss with ss_entry_mci=mci; ss_entry_symstack=sstack; ss_block_mci=mci; ss_symstack=sstack; ss_constraints=[];} 
  in
  let update_block_mci : mich_cut_info -> sym_state -> sym_state = fun mci ss -> {ss with ss_block_mci=mci} 
  in
  (* SUGAR - constraint update *)
  let ss_add_constraint : sym_state -> mich_f -> sym_state
  = fun ss fmla -> {ss with ss_constraints=(fmla :: ss.ss_constraints)}
  in
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
  = fun mci sset -> {sset with running=PSet.empty; blocked=(PSet.map sset.running ~f:(update_block_mci mci))} 
  in
  (* FUNCTION BEGIN *)
  fun cache inst ss -> begin
  let gen_inst_cc : 'a -> 'a cc = gen_custom_cc inst in
  let 
    { ss_fixchain;
      ss_exop;
      ss_dynchain;
      ss_exec_addrs;
      ss_oper_queue;
      ss_optt;
      ss_entry_mci;
      ss_entry_symstack;
      ss_block_mci;
      ss_symstack;
      ss_constraints;
    } = ss in
  match inst.cc_v with
  | MI_seq (i1,i2) -> ss |> ss_to_srset |> run_inst cache i1 |> run_inst cache i2
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
  | MI_push (_,v) -> (v :: ss_symstack) |> sstack_to_srset ss
  | MI_some -> (MV_some (CList.hd_exn ss_symstack) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_none t -> (MV_none t |> gen_inst_cc) |> sstack_push ss_symstack |> sstack_to_srset ss 
  | MI_unit -> (MV_unit |> gen_inst_cc) |> sstack_push ss_symstack |> sstack_to_srset ss
  | MI_if_none (i1,i2) ->
    let cond_constraint : mich_f = MF_is_none (CList.hd_exn ss_symstack) in
    let then_br_sset : state_set =
      (CList.tl_exn ss_symstack)
      |> sstack_to_ss (ss_add_constraint ss cond_constraint)
      |> ss_to_srset
      |> run_inst cache i1
    in
    let else_br_sset : state_set = 
      (MV_unlift_option (CList.hd_exn ss_symstack) |> gen_inst_cc)
      |> cons_tl_n ss_symstack 1
      |> sstack_to_ss (ss_add_constraint ss (MF_not cond_constraint))
      |> ss_to_srset
      |> run_inst cache i2
    in
    sset_union_pointwise then_br_sset else_br_sset
  | MI_pair ->
    (MV_pair (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1) |> gen_inst_cc)
    |> cons_tl_n ss_symstack 2
    |> sstack_to_srset ss
  | MI_car -> (MV_car (CList.hd_exn ss_symstack) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_cdr -> (MV_cdr (CList.hd_exn ss_symstack) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_left t -> (MV_left (t,(CList.hd_exn ss_symstack)) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_right t -> (MV_right (t,(CList.hd_exn ss_symstack)) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_if_left (i1,i2) ->
    let cond_constraint : mich_f = MF_is_left (CList.hd_exn ss_symstack) in
    let then_br_sset : state_set =
      (MV_unlift_left (CList.hd_exn ss_symstack) |> gen_inst_cc)
      |> cons_tl_n ss_symstack 1
      |> sstack_to_ss (ss_add_constraint ss cond_constraint)
      |> ss_to_srset
      |> run_inst cache i1
    in
    let else_br_sset : state_set =
      (MV_unlift_right (CList.hd_exn ss_symstack) |> gen_inst_cc)
      |> cons_tl_n ss_symstack 1
      |> sstack_to_ss (ss_add_constraint ss (MF_not cond_constraint))
      |> ss_to_srset
      |> run_inst cache i2
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
      (gen_inst_cc (MV_hd_l listv), gen_inst_cc (MV_tl_l listv))
      |> cons2_tl_n ss_symstack 1
      |> sstack_to_ss (ss_add_constraint ss cond_constraint)
      |> ss_to_srset
      |> run_inst cache i1
    in
    let else_br_sset : state_set =
      (CList.tl_exn ss_symstack)
      |> sstack_to_ss (ss_add_constraint ss (MF_not cond_constraint))
      |> ss_to_srset
      |> run_inst cache i2
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
    let thenbr_sset : state_set =  
      (* if this map instruction is already in cache's entered_loop, then skip it. *)
      if entered_flag then empty_sset else 
      (* from "symstack for then-branch" to "state-set" *)
      ((gen_new_symval_t elem_t) :: (gen_newvar_symstack_vs (CList.tl_exn ss_symstack)))
      |> new_ss_for_loopinst ss thenbr_mci
      |> run_inst_i cache i
      (* convert every running states in inner_sset into blocked cases *)
      |> move_running_to_blocked thenbr_mci
    in
    (* 3. make else-branch (loop-exit) running state *)
    let elsebr_ss : sym_state = 
      (gen_newvar_symstack_vs ss_symstack)
      |> new_ss_for_loopinst ss elsebr_mci
    in
    {thenbr_sset with running=(PSet.add thenbr_sset.running elsebr_ss); blocked=(PSet.add thenbr_sset.blocked blocked_state);}
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
      | _ -> Error "run_inst_i : MI_map : elem_t" |> raise
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
    let elsebr_ss : sym_state =
      (gen_newvar_symstack_vs (CList.tl_exn ss_symstack))
      |> new_ss_for_loopinst ss elsebr_mci
    in
    {thenbr_sset with running=(PSet.add thenbr_sset.running elsebr_ss); blocked=(PSet.add thenbr_sset.blocked blocked_state;)}
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
    (match (typ_of_val h2).cc_v with
      | MT_map _ -> MV_get_xmoy (h,h2)
      | MT_big_map _ -> MV_get_xbmo (h,h2)
      | _ -> Error "run_inst_i : MI_get" |> raise)
    |> gen_inst_cc
    |> cons_tl_n ss_symstack 2
    |> sstack_to_srset ss
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
      |> ss_to_srset
      |> run_inst cache i1
    in
    let else_br_sset : state_set =
      (CList.tl_exn ss_symstack)
      |> sstack_to_ss (ss_add_constraint ss (MF_not cond_constraint))
      |> ss_to_srset
      |> run_inst cache i2
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
    let elsebr_ss : sym_state =
      (gen_newvar_symstack_vs (CList.tl_exn ss_symstack))
      |> new_ss_for_loopinst ss elsebr_mci
    in
    {thenbr_sset with running=(PSet.add thenbr_sset.running elsebr_ss); blocked=(PSet.add thenbr_sset.blocked blocked_state);}
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
    let elsebr_ss : sym_state = 
      ((gen_new_symval_t right_elem_t) :: (gen_newvar_symstack_vs (CList.tl_exn ss_symstack)))
      |> new_ss_for_loopinst ss elsebr_mci
    in
    {thenbr_sset with running=(PSet.add thenbr_sset.running elsebr_ss); blocked=(PSet.add thenbr_sset.blocked blocked_state);}
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
    (gen_new_symval_t ret_ty).cc_v
    |> gen_inst_cc
    |> cons_tl_n ss_symstack 1
    |> sstack_to_srset ss
  | MI_dip_n (zn, i) ->
    let (hd, tl) = CList.split_n ss_symstack (Z.to_int zn) in
    let i_sset : state_set = tl |> sstack_to_ss ss |> run_inst_i cache i in
    let restored_running : sym_state PSet.t = PSet.map i_sset.running ~f:(fun x -> {x with ss_symstack=(hd @ x.ss_symstack)}) in
    {i_sset with running=restored_running}
  | MI_failwith -> {running=PSet.empty; blocked=PSet.empty; queries=PSet.empty; terminated=PSet.singleton(ss)}
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
    (MV_unpack (t, CList.hd_exn ss_symstack) |> gen_inst_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
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
      let query : sym_state * query_category = (added_state, Q_mutez_add_overflow) in
      let runstate : sym_state = ss_add_constraint ss (MF_add_mmm_no_overflow (h,h2)) in
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
      let query : sym_state * query_category = (subed_state, Q_mutez_sub_underflow) in
      let runstate : sym_state = ss_add_constraint ss (MF_sub_mmm_no_underflow (h,h2)) in
      {empty_sset with running=(PSet.singleton runstate); queries=(PSet.singleton query);}
    | _ -> Error "run_inst_i : MI_sub" |> raise
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
      let query : sym_state * query_category = (muled_state, Q_mutez_mul_overflow) in
      let runstate : sym_state = ss_add_constraint ss (MF_mul_mnm_no_overflow (h,h2)) in
      {empty_sset with running=(PSet.singleton runstate); queries=(PSet.singleton query);}
    | MT_nat, MT_mutez ->
      (* for mutez addition, 
          - query added
          - non-overflow constraint should be added to running state
      *)
      let muled_state : sym_state = (MV_mul_nmm (h,h2) |> gen_inst_cc) |> cons_tl_n ss_symstack 2 |> sstack_to_ss ss in
      let query : sym_state * query_category = (muled_state, Q_mutez_mul_overflow) in
      let runstate : sym_state = ss_add_constraint ss (MF_mul_nmm_no_overflow (h,h2)) in
      {empty_sset with running=(PSet.singleton runstate); queries=(PSet.singleton query);}
    | _ -> Error "run_inst_i : MI_mul" |> raise
    )
  | _ -> 
    (ignore (
    { ss_fixchain;
      ss_exop;
      ss_dynchain;
      ss_exec_addrs;
      ss_oper_queue;
      ss_optt;
      ss_entry_mci;
      ss_entry_symstack;
      ss_block_mci;
      ss_symstack;
      ss_constraints;
    }));
    (ignore (ss, inst)); 
    Error "run_inst_i : match failed" |> raise
end (* function run_inst_i end *)

(*
(*****************************************************************************)
(*****************************************************************************)
(* Run Operation                                                             *)
(*****************************************************************************)
(*****************************************************************************)

let rec run_operation : operation -> state_set -> state_set
= fun op ss_set -> begin
  PSet.fold ss_set.running
    ~init:{running=PSet.empty; queries=ss_set.queries; terminated=ss_set.terminated}
    ~f:(
      fun {running; queries; terminated} ss ->
        let {running=ps; queries=qs; terminated=ts} = run_operation_i op ss in
        {running=(PSet.union ps running); queries=(PSet.union qs queries); terminated=(PSet.union ts terminated)}
    )
end (* function run_operation end *)

(* NOTE: the "ss_cur_sender" should be well-updated BEFORE run implicit operations. *)
and run_operation_i : operation -> Tz.sym_state -> state_set
= fun op ss -> begin
  match op with
  (*************************************************************************)
  (* Explicit                                                              *)
  (*************************************************************************)
  | TO_explicit_create_contract (source_addr, (delegate_opt, balance_init, storage_v), (param_ty, storage_ty, code)) ->
    (ignore (source_addr, delegate_opt, balance_init, storage_v, param_ty, storage_ty, code)); Error "run_operation_i : TO_explicit_create_contract : not implemented" |> raise
  | TO_explicit_transfer_tokens (source_addr, param_val, amount, target_contract) ->
    (ignore (source_addr, param_val, amount, target_contract)); Error "run_operation_i : TO_explicit_transer_tokens : not implemented" |> raise
  | TO_explicit_set_delegate (_, delegate_opt) ->
    let new_fixchain : blockchain = {ss.ss_fixchain with bc_delegate=(MV_update_xomm(ss.ss_cur_source, delegate_opt, ss.ss_fixchain.bc_delegate) |> gen_dummy_cc)} in
    {running=PSet.singleton {ss with ss_fixchain=new_fixchain}; queries=PSet.empty; terminated=PSet.empty;}
  (*************************************************************************)
  (* Implicit                                                              *)
  (*************************************************************************)
  | TO_implicit_create_contract (sender_addr, (delegate_opt, balance_init, storage_v), (param_ty, storage_ty, code)) ->
    (ignore (sender_addr, delegate_opt, balance_init, storage_v, param_ty, storage_ty, code)); Error "run_operation_i : TO_create_contract : not implemented" |> raise
  | TO_implicit_transfer_tokens (sender_addr, param_val, amount, target_contract) ->
    (*  1. set current sender
        2. dynchain.balance[sender] -= amount
        3. dynchain.balance[target] += amount
        4. set current contract address
        5. initialize symstack
        6. run contract code
        7. update dynchain's storage
        8. put this contract into executed address set
        (9. update operation-queue)
        (10. (resolve remain operation-queue : set cur-sender, call "Se.run_operation", ...))
    *)
    (* 0. generates common components *)
    let target_contract_addr : mich_v cc = MV_address_of_contract target_contract |> gen_dummy_cc in
    {running=(PSet.singleton ss); queries=PSet.empty; terminated=PSet.empty}
    (* 1. set current sender *)
    |> map_ss_running (fun ss -> {ss with ss_cur_sender=sender_addr;})
    (* 2. balance[sender] -= amount *)
    |> run_operation (TO_internal_balance_sub (sender_addr, amount))
    (* 3. balance[target] += amount *)
    |> run_operation (TO_internal_balance_add (target_contract_addr, amount))
    (* 4. set current contract address *)
    |> map_ss_running (fun ss -> {ss with ss_cur_ctaddr=target_contract_addr})
    (* 5. initialize symstack *)
    |> run_operation (TO_internal_symstack_prepare (param_val))
    (* 6. run contract code *)
    (* |> run_operation (TO_internal_run_code (target_contract_addr)) *)
    |> ignore;(ignore (param_val, amount, target_contract)); Error "run_operation_i : TO_transer_tokens : not implemented" |> raise
  | TO_implicit_set_delegate (_, delegate_opt) ->
    let new_dynchain : blockchain = {ss.ss_dynchain with bc_delegate=(MV_update_xomm(ss.ss_cur_sender, delegate_opt, ss.ss_dynchain.bc_delegate) |> gen_dummy_cc)} in
    {running=PSet.singleton {ss with ss_dynchain=new_dynchain}; queries=PSet.empty; terminated=PSet.empty;}
  (*************************************************************************)
  (* Internal                                                              *)
  (*************************************************************************)
  | TO_internal_balance_sub (target_addr, amount) ->
    (* It generates 1 running state and 1 terminated state.
      - running: "dynchain.balance[target] -= amount" applied && no-underflow constraint added.
      - terminated: underflow constraint added.
    *)
    let target_balance_before : mich_v cc = MV_get_xmoy (target_addr, ss.ss_dynchain.bc_balance) |> gen_dummy_cc in
    let target_balance_after : mich_v cc = MV_sub_mmm (target_balance_before, amount) |> gen_dummy_cc in
    let target_balance_no_underflow : mich_f = MF_sub_mmm_no_underflow (target_balance_before, amount) in
    (* make terminated-state *)
    let terminated_condition : mich_f list = (MF_not target_balance_no_underflow) :: ss.ss_constraints in
    let terminated_state : sym_state = {ss with ss_constraints=terminated_condition;} in
    (* make running-state *)
    let target_balance_after_some : mich_v cc = MV_some target_balance_after |> gen_dummy_cc in
    let updated_balance : mich_v cc = MV_update_xomm(target_addr, target_balance_after_some, ss.ss_dynchain.bc_balance) |> gen_dummy_cc in
    let running_dynchain : blockchain = {ss.ss_dynchain with bc_balance=updated_balance} in
    let running_condition : mich_f list = target_balance_no_underflow :: ss.ss_constraints in
    let running_state : sym_state = {ss with ss_dynchain=running_dynchain; ss_constraints=running_condition;} in
    (* return *)
    {running=(PSet.singleton running_state); queries=PSet.empty; terminated=(PSet.singleton terminated_state);}
  | TO_internal_balance_add (target_addr, amount) ->
    (* It generates 1 running state.
      - running: "dynchain.balance[target] += amount" applied && no-overflow constraint added.
    *)
    let target_balance_before : mich_v cc = MV_get_xmoy (target_addr, ss.ss_dynchain.bc_balance) |> gen_dummy_cc in
    let target_balance_after : mich_v cc = MV_add_mmm (target_balance_before, amount) |> gen_dummy_cc in
    let target_balance_no_overflow : mich_f = MF_add_mmm_no_overflow (target_balance_before, amount) in
    (* make running-state *)
    let target_balance_after_some : mich_v cc = MV_some target_balance_after |> gen_dummy_cc in
    let updated_balance : mich_v cc = MV_update_xomm(target_addr, target_balance_after_some, ss.ss_dynchain.bc_balance) |> gen_dummy_cc in
    let running_dynchain : blockchain = {ss.ss_dynchain with bc_balance=updated_balance} in
    let running_condition : mich_f list = target_balance_no_overflow :: ss.ss_constraints in
    let running_state : sym_state = {ss with ss_dynchain=running_dynchain; ss_constraints=running_condition;} in
    (* return *)
    {running=(PSet.singleton running_state); queries=PSet.empty; terminated=PSet.empty;}
  | TO_internal_symstack_prepare (param_val) ->
    (* It generates 1 running state OR 1 terminated state, since "bc_storage" is managed with PMap, not mich_v.
      - running: length-1 symstack "[<param-val, storage-val in dynamic-chain>]" applied.
      - terminated: if the storage value of the target-contract not found.
    *)
    let target_contract_addr : mich_v cc = ss.ss_cur_ctaddr in
    let storage_val_opt : mich_v cc option = PMap.find ss.ss_dynchain.bc_storage target_contract_addr in
    (match storage_val_opt with
    | None -> {running=PSet.empty; queries=PSet.empty; terminated=(PSet.singleton ss);}
    | Some sval ->
      let param_storage_pair : mich_v cc = MV_pair (param_val, sval) |> gen_dummy_cc in
      let running_state : sym_state = {ss with ss_symstack=[param_storage_pair]} in
      {running=(PSet.singleton running_state); queries=PSet.empty; terminated=PSet.empty}
    )
  (* | TO_internal_run_code (target_addr) -> *)
    (* It is the interface to call "Se.run_inst", since it might fails to find  *)
end (* function run_operation_i end *)


(*****************************************************************************)
(*****************************************************************************)
(* Main                                                                      *)
(*****************************************************************************)
(*****************************************************************************)

let main : blockchain -> string -> operation list -> state_set
= fun bc source alist -> begin
  let initial_sym_stack = {
    ss_fixchain = bc;
    ss_dynchain = bc;
    ss_exec_addrs = (MV_lit_set (gen_dummy_cc MT_address, PSet.empty)) |> gen_dummy_cc;
    ss_oper_queue = (MV_lit_list (gen_dummy_cc MT_operation, [])) |> gen_dummy_cc;
    ss_cur_source = (MV_lit_address (gen_dummy_cc (MV_lit_key_hash source))) |> gen_dummy_cc;
    ss_cur_sender = (MV_lit_address (gen_dummy_cc (MV_lit_key_hash source))) |> gen_dummy_cc;
    ss_cur_ctaddr = (MV_lit_address (gen_dummy_cc (MV_lit_key_hash source))) |> gen_dummy_cc; (* dummy value *)
    ss_curct_startloc = CCLOC_Unknown;
    ss_curct_blockloc = CCLOC_Unknown;
    ss_curct_startstack = [];
    ss_symstack = [];
    ss_constraints = [];
  } in
  List.fold_left
    (fun state_set op ->
      let {running=r; queries=q; terminated=t} = run_operation op state_set in
      {running=(PSet.union r state_set.running); queries=(PSet.union q state_set.queries); terminated=(PSet.union t state_set.terminated)}
    )
    {running=(PSet.singleton initial_sym_stack); queries=PSet.empty; terminated=PSet.empty}
    alist
end (* function main end *)
*)