(* Symbolic Executer *)

open Tz

exception Error of string

type query_category =
  | Q_mutez_add_overflow
  | Q_mutez_mul_overflow
  | Q_mutez_sub_underflow
  | Q_shiftleft_prohibited
  | Q_shiftright_prohibited
  | Q_assertion

type state_set = {
  running : Tz.sym_state Tz.PSet.t;
  queries : (Tz.sym_state * query_category) Tz.PSet.t;
  terminated : Tz.sym_state Tz.PSet.t;
}


(*****************************************************************************)
(*****************************************************************************)
(* Utilities - State Set Mapping                                             *)
(*****************************************************************************)
(*****************************************************************************)

let map_ss_running : (Tz.sym_state -> Tz.sym_state) -> state_set -> state_set
= fun map_f {running; queries; terminated} -> {running=(Tz.PSet.map running ~f:map_f); queries; terminated;}


(*****************************************************************************)
(*****************************************************************************)
(* Run Instruction                                                           *)
(*****************************************************************************)
(*****************************************************************************)

let rec run_inst : (mich_i cc) -> state_set -> state_set
= fun inst ss_set -> begin 
  PSet.fold ss_set.running 
    ~init:{running=PSet.empty; queries=ss_set.queries; terminated=ss_set.terminated}
    ~f:(
      fun {running; queries; terminated} ss ->
      let {running=ps; queries=qs; terminated=ts} = run_inst_i inst ss in
      {running=(PSet.union ps running); queries=(PSet.union qs queries); terminated=(PSet.union ts terminated)}
    )
end (* function run_inst end *)

(* It might raise unhandled exceptions if the given michelson program cannot pass Michelson type system. *)
and run_inst_i : (mich_i cc) -> sym_state -> state_set
= (* SUGAR - module name *)
  let module CList = Core.List in
  (* SUGAR - push to stack *)
  let sstack_push : (mich_v cc list) -> (mich_v cc) -> (mich_v cc list) = fun sstack v -> (v :: sstack) in
  let cons_tl_n : (mich_v cc list) -> int -> (mich_v cc) -> (mich_v cc list)
  = fun sstack n v -> let (_, tl_n) = CList.split_n sstack n in (v :: tl_n)
  in
  (* let cons2_tl_n : (mich_v cc list) -> int -> (mich_v cc * mich_v cc) -> (mich_v cc list)
  = fun sstack n (v1,v2) -> let (_, tl_n) = CList.split_n sstack n in (v1 :: v2 :: tl_n)
  in *)
  (* SUGAR - record update *)
  let sstack_to_ss : sym_state -> (mich_v cc list) -> sym_state
  = fun ss sstack -> {ss with ss_symstack=sstack}
  in
  let ss_to_srset : sym_state -> state_set
  = fun ss -> {running=PSet.singleton(ss); queries=PSet.empty; terminated=PSet.empty}
  in
  let sstack_to_srset : sym_state -> (mich_v cc list) -> state_set
  = fun ss sstack -> sstack |> sstack_to_ss ss |> ss_to_srset 
  in
  (* SUGAR - constraint update *)
  let ss_add_constraint : sym_state -> mich_f -> sym_state
  = fun ss fmla -> {ss with ss_constraints=(fmla :: ss.ss_constraints)}
  in
  (* SUGAR - state set *)
  let sset_union_pointwise : state_set -> state_set -> state_set
  = fun sset1 sset2 -> {
    running = PSet.union sset1.running sset2.running;
    queries = PSet.union sset2.queries sset2.queries;
    terminated = PSet.union sset1.terminated sset2.terminated;
  } in
  (* FUNCTION BEGIN *)
  fun inst ss -> begin
  let gen_cc : 'a -> 'a cc
  = fun x -> {
    cc_loc = inst.cc_loc;
    cc_anl = inst.cc_anl;
    cc_v = x;
  } in
  let {ss_fixchain; ss_dynchain; ss_exec_addrs; ss_oper_queue; ss_cur_source; ss_cur_sender; ss_cur_ctaddr; ss_symstack; ss_constraints;} = ss in
  match inst.cc_v with
  | MI_seq (i1,i2) -> ss |> ss_to_srset |> run_inst i1 |> run_inst i2
  | MI_drop zn ->
    (CList.split_n ss_symstack (Z.to_int zn) |> Stdlib.snd)
    |> sstack_to_srset ss
  | MI_dup zn -> 
    (CList.nth_exn ss_symstack (Z.to_int zn - 1)) :: ss_symstack
    |> sstack_to_srset ss
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
  | MI_some -> (MV_some (CList.hd_exn ss_symstack) |> gen_cc) |> cons_tl_n ss_symstack 1 |> sstack_to_srset ss
  | MI_none t -> (MV_none t |> gen_cc) |> sstack_push ss_symstack |> sstack_to_srset ss 
  | MI_unit -> (MV_unit |> gen_cc) |> sstack_push ss_symstack |> sstack_to_srset ss
  | MI_if_none (i1,i2) ->
    let cond_constraint : mich_f = MF_is_none (CList.hd_exn ss_symstack) in
    let then_br_sset : state_set = cond_constraint |> ss_add_constraint ss |> ss_to_srset |> run_inst i1 in
    let else_br_sset : state_set = (MF_not cond_constraint) |> ss_add_constraint ss |> ss_to_srset |> run_inst i2 in
    sset_union_pointwise then_br_sset else_br_sset
  | MI_pair ->
    (MV_pair (CList.hd_exn ss_symstack, CList.nth_exn ss_symstack 1) |> gen_cc)
    |> cons_tl_n ss_symstack 2
    |> sstack_to_srset ss
  | _ -> 
    (ignore (ss_fixchain, ss_dynchain, ss_exec_addrs, ss_oper_queue, ss_cur_source, ss_cur_sender, ss_cur_ctaddr, ss_symstack, ss_constraints)); 
    (ignore (ss, inst)); 
    Error "run_inst_i : match failed" |> raise
end (* function run_inst_i end *)


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
