(* Symbolic Executer *)

open Tz

exception Error of string

type state_set = {
  running : Tz.sym_state Tz.PSet.t;
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
    ~init:{running=PSet.empty; terminated=ss_set.terminated}
    ~f:(
      fun {running; terminated} ss ->
      let {running=ps; terminated=qs} = run_inst_i ss inst in
      {running=(PSet.union ps running); terminated=(PSet.union qs terminated)}
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
    ~init:{running=PSet.empty; terminated=ss_set.terminated}
    ~f:(
      fun {running; terminated} ss ->
        let {running=ps; terminated=qs} = run_operation_i ss op in
        {running=(PSet.union ps running); terminated=(PSet.union qs terminated)}
    )
end (* function run_operation end *)

and run_operation_i : Tz.sym_state -> operation -> state_set
= fun ss op -> begin
  match op with
  | _ -> (ignore (ss)); Error "run_operation_i : match failed" |> raise
end (* function run_operation_i end *)


(*****************************************************************************)
(*****************************************************************************)
(* Main                                                                      *)
(*****************************************************************************)
(*****************************************************************************)

let main : blockchain -> operation list -> state_set
= fun bc alist -> begin
  let initial_sym_stack = {
    ss_fixstack = bc;
    ss_dynstack = bc;
    ss_exec_addrs = gen_dummy_cc (MV_lit_set (gen_dummy_cc MT_address, PSet.empty));
    ss_oper_queue = gen_dummy_cc (MV_lit_list (gen_dummy_cc MT_operation, []));
    ss_symstack = [];
    ss_contraints = [];
  } in
  List.fold_left
    (fun state_set op ->
      let {running=r; terminated=t} = run_operation state_set op in
      {running=(PSet.union r state_set.running); terminated=(PSet.union t state_set.terminated)}
    )
    {running=(PSet.singleton initial_sym_stack); terminated=PSet.empty}
    alist
end (* function main end *)