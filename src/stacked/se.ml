(* Symbolic Executer *)

open Tz

exception Error of string

type query_category =
  | Q_mutez_overflow
  | Q_mutez_underflow
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
(* Run Instruction                                                           *)
(*****************************************************************************)
(*****************************************************************************)

let rec run_inst : state_set -> (mich_i cc) -> state_set
= fun ss_set inst -> begin 
  PSet.fold ss_set.running 
    ~init:{running=PSet.empty; queries=ss_set.queries; terminated=ss_set.terminated}
    ~f:(
      fun {running; queries; terminated} ss ->
      let {running=ps; queries=qs; terminated=ts} = run_inst_i ss inst in
      {running=(PSet.union ps running); queries=(PSet.union qs queries); terminated=(PSet.union ts terminated)}
    )
end (* function run_inst end *)

and run_inst_i : sym_state -> (mich_i cc) -> state_set
= fun ss inst -> begin
  match inst with
  | _ -> (ignore (ss, inst)); Error "run_inst_i : match failed" |> raise
end (* function run_inst_i end *)


(*****************************************************************************)
(*****************************************************************************)
(* Run Operation                                                             *)
(*****************************************************************************)
(*****************************************************************************)

let rec run_operation : state_set -> operation -> state_set
= fun ss_set op -> begin
  PSet.fold ss_set.running
    ~init:{running=PSet.empty; queries=ss_set.queries; terminated=ss_set.terminated}
    ~f:(
      fun {running; queries; terminated} ss ->
        let {running=ps; queries=qs; terminated=ts} = run_operation_i ss op in
        {running=(PSet.union ps running); queries=(PSet.union qs queries); terminated=(PSet.union ts terminated)}
    )
end (* function run_operation end *)

(* NOTE: the "ss_cur_sender" should be well-updated BEFORE run implicit operations. *)
and run_operation_i : Tz.sym_state -> operation -> state_set
= fun ss op -> begin
  let {ss_fixchain; ss_dynchain; ss_exec_addrs; ss_oper_queue; ss_cur_source; ss_cur_sender; ss_symstack; ss_constraints;} = ss in
  match op with
  | TO_explicit_create_contract ((delegate_opt, balance_init, storage_v), (param_ty, storage_ty, code)) ->
    (ignore (delegate_opt, balance_init, storage_v, param_ty, storage_ty, code, ss_exec_addrs, ss_oper_queue, ss_symstack, ss_constraints)); Error "run_operation_i : TO_explicit_create_contract : not implemented" |> raise
  | TO_explicit_transfer_tokens (param_val, amount, target_contract) ->
    (ignore (param_val, amount, target_contract)); Error "run_operation_i : TO_explicit_transer_tokens : not implemented" |> raise
  | TO_explicit_set_delegate (delegate_opt) ->
    let new_fixchain : blockchain = {ss_fixchain with bc_delegate=(MV_update_xomm(ss_cur_source, delegate_opt, ss_fixchain.bc_delegate) |> gen_dummy_cc)} in
    {running=PSet.singleton {ss with ss_fixchain=new_fixchain}; queries=PSet.empty; terminated=PSet.empty;}
  | TO_implicit_create_contract ((delegate_opt, balance_init, storage_v), (param_ty, storage_ty, code)) ->
    (ignore (delegate_opt, balance_init, storage_v, param_ty, storage_ty, code)); Error "run_operation_i : TO_create_contract : not implemented" |> raise
  | TO_implicit_transfer_tokens (param_val, amount, target_contract) ->
    (*  1. balance[sender] -= amount
          - if balance[sender] < amount, terminated.
        2. balance[target] += amount
          - put a constraint that balance[target] + amount will not mutez-overflows.
          - Mutez Overflow for real tezos token can occur after 156 years,
            Condition: (current-total-supply = 10^9 XTZ = 10^15 mutez), (inflation rate = 1.06/year), (mutez overflows from (2^63) mutez), (no XTZ burns for gas)
        3. run contract code
        4. update dynchain's storage
        5. put this contract into executed address set
        (6. update operation-queue)
        (7. (resolve remain operation-queue : set cur-sender, call "Se.run_operation", ...))
    *)
    (ignore (param_val, amount, target_contract)); Error "run_operation_i : TO_transer_tokens : not implemented" |> raise
  | TO_implicit_set_delegate (delegate_opt) ->
    let new_dynchain : blockchain = {ss_dynchain with bc_delegate=(MV_update_xomm(ss_cur_sender, delegate_opt, ss_dynchain.bc_delegate) |> gen_dummy_cc)} in
    {running=PSet.singleton {ss with ss_dynchain=new_dynchain}; queries=PSet.empty; terminated=PSet.empty;}
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
    ss_symstack = [];
    ss_constraints = [];
  } in
  List.fold_left
    (fun state_set op ->
      let {running=r; queries=q; terminated=t} = run_operation state_set op in
      {running=(PSet.union r state_set.running); queries=(PSet.union q state_set.queries); terminated=(PSet.union t state_set.terminated)}
    )
    {running=(PSet.singleton initial_sym_stack); queries=PSet.empty; terminated=PSet.empty}
    alist
end (* function main end *)
