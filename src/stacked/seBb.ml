(* Symbolic Executer - Extract One Symbolic State per Basic Block (Execution Path Sequence) *)

open Tz

exception Error of string

type state_set = Se.state_set

(*****************************************************************************)
(*****************************************************************************)
(* Run Instruction                                                           *)
(*****************************************************************************)
(*****************************************************************************)

let _mich_v_equal_naive : mich_v cc -> mich_v cc -> bool
= fun x y -> x = y

let is_newvar_required : mich_v cc list PSet.t -> bool list
= fun stacks -> begin
  if PSet.length stacks = 0 then (Error "is_newvar_required" |> raise) else
  let a_stack = PSet.choose_exn stacks in
  let remain_stacks = PSet.remove stacks a_stack in
  PSet.fold remain_stacks ~init:(List.init (List.length a_stack) (fun _ -> false))
    ~f:(fun accl n_stack ->
      List.map2 (fun q w -> Stdlib.not (_mich_v_equal_naive q w)) a_stack n_stack
      |> List.map2 Stdlib.(||) accl
    )
end (* function is_newvar_required end *)

let merge_ss : sym_state PSet.t -> (mich_v cc list * mich_f)
= fun s_set -> begin
  if PSet.length s_set = 0 then (Error "merge_ss" |> raise) else
  let nvr : bool list = is_newvar_required (PSet.map s_set ~f:(fun x -> x.ss_symstack)) in
  let n_stack : mich_v cc list = List.map2 (fun e b -> if b then gen_new_symval_v e else e) (PSet.choose_exn s_set).ss_symstack nvr in
  let or_constraint_elems : mich_f list =
    PSet.fold s_set ~init:[]
      ~f:(fun accl ss ->
        let one_state_constraint : mich_f = 
          List.map2 (fun e n -> if (_mich_v_equal_naive e n) then MF_true else MF_eq (e,n)) ss.ss_symstack n_stack
          |> List.filter (fun x -> x = MF_true)
          |> (fun x -> MF_and (x @ ss.ss_constraints))
        in
        one_state_constraint :: accl
      )
  in
  (n_stack, MF_or or_constraint_elems)
end (* function merge_ss end*)

let merge_value : ((mich_v cc) * (mich_v cc PSet.t) * bool) -> mich_v cc -> ((mich_v cc) * (mich_v cc PSet.t) * bool)
= fun (mv, alt_set, is_not_activated) new_v -> begin
  match is_not_activated, (new_v = mv) with
  | true, true -> (mv, alt_set, true)
  | true, false -> (gen_new_symval_v mv, (PSet.of_list [new_v; mv]), false)
  | false, _ -> (mv, (PSet.add alt_set new_v), false)
end (* function merge_value end *)

let merge_stack_i : ((mich_v cc) * (mich_v cc PSet.t) * bool) list -> (mich_v cc list) -> ((mich_v cc) * (mich_v cc PSet.t) * bool) list
= fun acc_stack new_stack -> begin
  (* List.map2 might raise Invalid_argument exception *)
  List.map2 (fun x y -> merge_value x y) acc_stack new_stack
end (* function merge_stack_i end *)

let merge_stack : sym_state PSet.t -> (mich_v cc list * mich_f)
= let cstr_or_fmla : (mich_v cc) -> (mich_v cc PSet.t) -> mich_f
  = fun mv vals -> MF_or (PSet.map vals ~f:(fun v -> MF_eq (mv, v)) |> PSet.to_list) in
  fun runningset -> begin
  if (PSet.length runningset = 0) then (Error "running_merge : length-0" |> raise) else
  let init_ss = PSet.choose_exn runningset in
  let remaining_stacks = PSet.remove runningset init_ss |> PSet.map ~f:(fun x -> x.ss_symstack) in
  let init_stack : (mich_v cc * mich_v cc PSet.t * bool) list = List.map (fun x -> (x, PSet.empty, true)) init_ss.ss_symstack in
  PSet.fold remaining_stacks ~init:init_stack ~f:merge_stack_i
  |> List.map (fun (mv, vals, flag) -> (mv, if flag then None else Some (cstr_or_fmla mv vals)))
  |> List.split
  |> (fun (x,y) -> x, MF_and (List.filter Option.is_some y |> List.map Option.get))
end (* function running_merge end *)

(* It is recommended to put running states which has the same starting point only in "run_inst" function *)
let rec run_inst : (mich_i cc) -> state_set -> state_set
= let rii = run_inst_i in (* syntax sugar to deal with duplicated function names *)
  let open Se in
  fun inst ss_set -> begin
  if PSet.length ss_set.running = 0 then ss_set else
  let chosen_rs = PSet.choose_exn ss_set.running in
  (* PLACEHOLDER *) let _ = Stdlib.ignore chosen_rs in
  (* let integrated_rs = merge_stack ss_set.running |> (fun (x,y) -> {chosen_rs with }) *) (* TODO *)
  PSet.fold ss_set.running 
    ~init:{running=PSet.empty; blocked=ss_set.blocked; queries=ss_set.queries; terminated=ss_set.terminated}
    ~f:(
      fun {running; blocked; queries; terminated} ss ->
      let {running=ps; blocked=bs; queries=qs; terminated=ts} = rii inst ss in
      {running=(PSet.union ps running); blocked=(PSet.union bs blocked); queries=(PSet.union qs queries); terminated=(PSet.union ts terminated)}
    )
end (* function run_inst end *)

and run_inst_i : (mich_i cc) -> sym_state -> state_set
= let module CList = Core.List in
  let se_run_inst : (mich_i cc) -> state_set -> state_set
  = fun inst sset -> Se.run_inst (Se.init_cache ()) inst sset in
  fun inst ss -> begin
  (* let gen_inst_cc : 'a -> 'a cc = gen_custom_cc inst in *)
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
  (* match inst.cc_v with
  | MI_seq (i1,i2) -> ss |>  *) (* TODO *)
  (* PLACEHOLDER *) let _ = Stdlib.ignore (se_run_inst, inst, ss_symstack) in
  (* PLACEHOLDER *) {running=PSet.empty; blocked=PSet.empty; queries=PSet.empty; terminated=PSet.empty;}
end (* function run_inst end *)
