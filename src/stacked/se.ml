open Tz

exception Error of string


(*****************************************************************************)
(*****************************************************************************)
(* Run Instruction                                                           *)
(*****************************************************************************)
(*****************************************************************************)

type run_inst_ret = {
  path_vcs : sym_stack PSet.t;
  query_vcs : sym_stack PSet.t;
}

let rec run_inst : sym_stack PSet.t -> (Tz.mich_i cc) -> run_inst_ret
= fun ss_set inst -> begin 
  PSet.fold ss_set 
    ~init:{path_vcs=PSet.empty; query_vcs=PSet.empty;} 
    ~f:(
      fun {path_vcs; query_vcs} ss ->
      let {path_vcs=ps; query_vcs=qs} = run_inst_i ss inst in
      {path_vcs=(PSet.union ps path_vcs); query_vcs=(PSet.union qs query_vcs)}
    )
end (* function run_inst end *)

and run_inst_i : sym_stack -> (Tz.mich_i cc) -> run_inst_ret
= fun ss inst -> begin
  match inst with
  | _ -> (ignore (ss, inst)); Error "run_inst_i : match failed" |> raise
end (* function run_inst_i end *)


(*****************************************************************************)
(*****************************************************************************)
(* Run Operation                                                             *)
(*****************************************************************************)
(*****************************************************************************)

and run_operation : Bc.t -> Tz.operation -> Bc.t
= fun {contracts; chain_id; last_time} op -> begin
  match op with
  | _ -> (ignore (contracts, chain_id, last_time)); Error "run_operation : match failed" |> raise
end (* function run_operation end *)
