(* Invariant Synthesizer *)

exception Error of string


(******************************************************************************)
(******************************************************************************)
(* Set Combination                                                            *)
(******************************************************************************)
(******************************************************************************)

(* bind 1 {1; 2; 3} === {(1, 1); (1, 2); (1, 3)} *)
let bind : 'a -> 'b Core.Set.Poly.t -> ('a * 'b) Core.Set.Poly.t
= let module CPSet = Core.Set.Poly in
  (* function bind start *)
  fun a bset -> begin
  CPSet.fold
    bset
    ~init:CPSet.empty
    ~f:(fun acc b -> CPSet.add acc (a, b))
end (* function bind end *)

(* combination {1; 2} {a; b} === {(1, a); (1, b); (2, a); (2, b)} *)
let combination : 'a Core.Set.Poly.t -> 'b Core.Set.Poly.t -> ('a * 'b) Core.Set.Poly.t
= let module CPSet = Core.Set.Poly in
  (* function combination start *)
  fun aset bset -> begin
  CPSet.fold
    aset
    ~init:CPSet.empty
    ~f:(fun acc a -> CPSet.union acc (bind a bset))
end (* function combination end *)

(* combination_rfl {1; 2} === {(1, 1); (1, 2); (2, 2)} *)
let combination_rfl : 'a Core.Set.Poly.t -> ('a * 'a) Core.Set.Poly.t
= (* function combination_rfl start *)
  fun s -> begin
  combination s s
end (* function combination_rfl end *)

(* combination_self_two_diff {1; 2; 3} === {(1, 2); (1, 3); (2, 3)} *)
let combination_self_two_diff : 'a Core.Set.Poly.t -> ('a * 'a) Core.Set.Poly.t
= let module CPSet = Core.Set.Poly in
  (* function combination_self_two_diff start *)
  fun s -> begin
  let (comb, _) : ('a * 'a) CPSet.t * 'a CPSet.t = 
    CPSet.fold
      s
      ~init:(CPSet.empty, s)
      ~f:(fun (acc, rs) x -> (
            let rs' : 'a CPSet.t = (
              CPSet.remove rs x) in
            let comb : ('a * 'a) CPSet.t = (
              bind x rs') in
            ((CPSet.union acc comb), rs'))) in
  comb
end (* function combination_self_two_diff end *)

(* combination_self_two_diff_rf {1; 2; 3} === {(1, 2); (1, 3); (2, 1); (2, 3); (3, 1); (3, 2)} *)
let combination_self_two_diff_rf : 'a Core.Set.Poly.t -> ('a * 'a) Core.Set.Poly.t
= let module CPSet = Core.Set.Poly in
  (* function combination_self_two_diff_rf start *)
  fun s -> begin
  let comb = combination_self_two_diff s in
  CPSet.fold
    comb
    ~init:comb
    ~f:(fun acc (x, y) -> CPSet.add acc (y, x))
end (* function combination_self_two_diff_rf end *)


(******************************************************************************)
(******************************************************************************)
(* Component Collector                                                        *)
(******************************************************************************)
(******************************************************************************)

type vstack = Tz.mich_v Tz.cc list (* syntax sugar *)
type tstack = Tz.mich_t Tz.cc list (* syntax sugar *)


  (*****************************************************************************
    The type comp_map is a pre-baked component map.
    Function bake_comp_map makes a set of components from the type stack of each MCI.
    The component set which is the value of comp_map is used to make a set of new invariants by recipe.
    type comp_map = MCI |-> (mich_t |-> component set)
  *****************************************************************************)
type comp_map = (Tz.mich_cut_info, (Comp.t Core.Set.Poly.t) Comp.CTMap.t) Core.Map.Poly.t

let bake_comp_map : Se.state_set * ((Tz.mich_v Tz.cc) option * Tz.sym_state) -> comp_map
= let module CList = Core.List in
  let module CPSet = Core.Set.Poly in
  let module CPMap = Core.Map.Poly in
  let module CTMap = Comp.CTMap in
  let get_type_stack : vstack CPSet.t -> tstack option
  = let tstack_equal : tstack -> tstack -> bool
    = (* function tstack_equal start *)
      fun ts1 ts2 -> begin
      CList.fold2 ts1 ts2 ~init:true ~f:(fun c t1 t2 -> if t1.cc_v <> t2.cc_v then false else c)
      |> (function | Ok cc -> cc | Unequal_lengths -> false)
    end in (* function tstack_equal end *)
    (* function get_type_stack start *)
    fun vsset -> begin
    let vs : vstack = 
      (CPSet.choose vsset
      |> (function | Some ss -> ss| None -> Error "bake_comp_map : get_type_stack : vs" |> Stdlib.raise)) in
    let ts : tstack = vs |> Se.extract_typ_stack in
    vs
    |> CPSet.remove vsset
    |> CPSet.fold ~init:(Some ts) ~f:(fun ts_opt vs -> (if Option.is_none ts_opt then None else 
        let ts = Option.get ts_opt in
        if tstack_equal ts (vs |> Se.extract_typ_stack) then ts_opt else None))
  end in (* function get_type_stack end *)
  (* function bake_comp_map start *)
  fun (sset, (init_strg_opt, _)) -> begin
  let mci_vstack_set : (Tz.mich_cut_info, vstack CPSet.t) CPMap.t =
    CPSet.fold sset.blocked
      ~init:CPMap.empty
      ~f:(fun acc s -> (CPMap.update acc s.ss_entry_mci ~f:(function | None -> CPSet.singleton s.ss_entry_symstack | Some ss -> CPSet.add ss s.ss_entry_symstack))) in
  let mci_tstack : (Tz.mich_cut_info, tstack) CPMap.t =
    CPMap.map mci_vstack_set
      ~f:(fun vsset -> (vsset
        |> get_type_stack
        |> (function | Some ts -> ts | None -> Error "bake_comp_map : mci_tstack" |> Stdlib.raise))) in
  CPMap.mapi mci_tstack
    ~f:(fun ~key ~data -> (
      CList.foldi data
        ~init:CTMap.empty
        ~f:(fun i ctmap t -> (
          let base_comp_opt : Comp.t option = Comp.base_comp_from_mci (key, i, t) in
          match base_comp_opt with
          | None -> ctmap
          | Some base_comp -> (
            (if key.mci_cutcat = MCC_trx_entry && Option.is_some init_strg_opt then (
              Comp.collect (Comp.base_comp_from_v (Option.get init_strg_opt) ~loc:base_comp.cp_loc) ctmap)
            else ctmap)
            |> Comp.collect base_comp)))))
end (* function bake_comp_map end *)

(*****************************************************************************)
(*****************************************************************************)
(* Recipe                                                                    *)
(*****************************************************************************)
(*****************************************************************************)

let all_equal : (Comp.t Core.Set.Poly.t) Comp.CTMap.t -> Tz.mich_f Core.Set.Poly.t
= let open Tz in
  let open Comp in
  let module CList = Core.List in
  let module CPSet = Core.Set.Poly in
  (* function all_equal start *)
  fun ctmap -> begin
  CTMap.fold
    ctmap
    ~init:CPSet.empty
    ~f:(fun ~key ~data acc -> (
          let _ = key in
          data
          |> combination_self_two_diff
          |> CPSet.map
              ~f:(fun (c1, c2) -> MF_imply ((fold_precond [c1; c2;]), MF_eq (c1.cp_value, c2.cp_value)))
          |> CPSet.union acc))
end (* function all_equal end *)

let all_ge : (Comp.t Core.Set.Poly.t) Comp.CTMap.t -> Tz.mich_t list -> Tz.mich_f Core.Set.Poly.t
= let open Tz in
  let open Comp in
  let module CList = Core.List in
  let module CPSet = Core.Set.Poly in
  (* function all_ge start *)
  fun ctmap tl -> begin
  let ts : (Tz.mich_t Tz.cc) CPSet.t = (* target types *)
    tl
    |> CList.map ~f:Tz.gen_dummy_cc
    |> CPSet.of_list in
  CTMap.fold
    ctmap
    ~init:CPSet.empty
    ~f:(fun ~key ~data acc -> (
          if CPSet.exists ts ~f:(fun t -> t = key) then (
            data
            |> combination_self_two_diff_rf
            |> CPSet.map
              ~f:(fun (c1, c2) ->
                    let cmp : Tz.mich_v Tz.cc = MV_compare (c1.cp_value, c2.cp_value) |> gen_dummy_cc in
                    let zero : Tz.mich_v Tz.cc = MV_lit_int (Z.zero) |> gen_dummy_cc in
                    MF_imply ((fold_precond [c1; c2;]), MF_is_true (gen_dummy_cc (MV_geq_ib (cmp, zero)))))
            |> CPSet.union acc)
          else acc))
end (* function all_ge end *)

let all_gt : (Comp.t Core.Set.Poly.t) Comp.CTMap.t -> Tz.mich_t list -> Tz.mich_f Core.Set.Poly.t
= let open Tz in
  let open Comp in
  let module CList = Core.List in
  let module CPSet = Core.Set.Poly in
  (* function all_gt start *)
  fun ctmap tl  -> begin
  let ts : (Tz.mich_t Tz.cc) CPSet.t = (* target types *)
    tl
    |> CList.map ~f:Tz.gen_dummy_cc
    |> CPSet.of_list in
  CTMap.fold
    ctmap
    ~init:CPSet.empty
    ~f:(fun ~key ~data acc -> (
          if CPSet.exists ts ~f:(fun t -> t = key) then (
            data
            |> combination_self_two_diff_rf
            |> CPSet.map
                ~f:(fun (c1, c2) -> 
                  let cmp : Tz.mich_v Tz.cc = MV_compare (c1.cp_value, c2.cp_value) |> gen_dummy_cc in
                  let zero : Tz.mich_v Tz.cc = MV_lit_int (Z.zero) |> gen_dummy_cc in
                  MF_imply ((fold_precond [c1; c2;]), MF_is_true (gen_dummy_cc (MV_gt_ib (cmp, zero)))))
            |> CPSet.union acc)
          else acc))
end (* function all_gt end *)

(*****************************************************************************)
(*****************************************************************************)
(* Synthesizer                                                               *)
(*****************************************************************************)
(*****************************************************************************)

type generate_param = 
  (* igi_failed_set *)    ((Tz.sym_state * Se.query_category) * (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option) * Tz.mich_f * Utils.Timer.time) Core.Set.Poly.t *
  (* igi_cur_inv *)       Se.invmap *
  (* igi_comp_map *)      comp_map *
  (* igi_collected *)     Se.invmap Core.Set.Poly.t

type ingredients = {
  igdt_query_category : Se.query_category;
  igdt_model_opt      : ProverLib.Smt.ZModel.t option;
  igdt_vc             : Tz.mich_f;
  igdt_sym_state      : Tz.sym_state;
  igdt_comp_type_map  : (Comp.t Core.Set.Poly.t) Comp.CTMap.t
}

let collect_set : ('a Core.Set.Poly.t) list -> 'a Core.Set.Poly.t
= let module CList = Core.List in
  let module CPSet = Core.Set.Poly in
  (* function collect_set start *)
  fun slist -> begin
  CList.fold
    slist
    ~init:CPSet.empty
    ~f:(fun acc s -> CPSet.union acc s)
  (* function collect_set end *)
end

let refine_t : Se.invmap -> ingredients -> Se.invmap Core.Set.Poly.t
= let open Tz in
  let module CPSet = Core.Set.Poly in
  let module CPMap = Core.Map.Poly in
  (* function refine_t start *)
  fun cur_inv igdt -> begin
  (* 0-1. extract component on entrance of transaction *)
  let ctmap = igdt.igdt_comp_type_map in
  (* 1. generate recipe *)
  let all_eq_fmlas : Tz.mich_f CPSet.t = all_equal ctmap in
  let all_ge_fmlas : Tz.mich_f CPSet.t = all_ge ctmap [MT_int; MT_nat; MT_mutez] in
  let all_gt_fmlas : Tz.mich_f CPSet.t = all_gt ctmap [MT_int; MT_nat; MT_mutez] in
  (* 2. generate invariant map *)
  let fmlas : (Tz.mich_f * Tz.mich_f) CPSet.t = 
    [ all_eq_fmlas;
      all_ge_fmlas;
      all_gt_fmlas; ]
    |> collect_set
    |> CPSet.map ~f:(fun entry_f -> ((entry_f, entry_f))) in
  CPSet.map
    fmlas
    ~f:(fun (entry_f, exit_f) -> (
          CPMap.mapi
            cur_inv
            ~f:(fun ~key ~data ->
                  if key.mci_cutcat = MCC_trx_entry then CPSet.add data entry_f
                  else if key.mci_cutcat = MCC_trx_exit then CPSet.add data exit_f
                  else data)))
end (* function refine_t end *)

let refine_l : Se.invmap -> ingredients -> Se.invmap Core.Set.Poly.t
= let module CPSet = Core.Set.Poly in
  let module CPMap = Core.Map.Poly in
  fun cur_inv igdt -> begin
  (* refine_l function start *)
  let _ = cur_inv, igdt in
  (* 0. extract component on loop entrance *)
  let ctmap = igdt.igdt_comp_type_map in
  (* 1. generate recipe *)
  let all_eq_fmlas : Tz.mich_f CPSet.t = all_equal ctmap in
  let all_ge_fmlas : Tz.mich_f CPSet.t = all_ge ctmap [MT_int; MT_nat; MT_mutez] in
  let all_gt_fmlas : Tz.mich_f CPSet.t = all_gt ctmap [MT_int; MT_nat; MT_mutez] in
  (* 2. generate invariant map *)
  let fmlas : Tz.mich_f CPSet.t =
    [ all_eq_fmlas;
      all_ge_fmlas;
      all_gt_fmlas; ]
    |> collect_set in
  CPSet.map
    fmlas
    ~f:(fun f -> (
          CPMap.mapi
            cur_inv
            ~f:(fun ~key ~data -> (
                  if key = igdt.igdt_sym_state.ss_entry_mci then CPSet.add data f
                  else data))))
  (* refine_l function end *)
end

let generate : generate_param -> Se.invmap Core.Set.Poly.t
= let open Tz in
  let module CPSet = Core.Set.Poly in
  let module CPMap = Core.Map.Poly in
  fun (igi_failed_set, igi_cur_inv, igi_comp_map, igi_collected) -> begin
  (* generate function start *)
  (* 1. collect refine targets *)
  let refine_targets : (Tz.mich_cut_info, (ingredients CPSet.t)) CPMap.t =
    CPSet.fold
      igi_failed_set
      ~init:CPMap.empty
      ~f:(fun acc ((fs, qctg), (_, mopt), vc, _) ->
          (* 1-1. make new ingredients *)
          let ctmap : (Comp.t CPSet.t) Comp.CTMap.t =
            CPMap.find igi_comp_map fs.ss_entry_mci
            |> function
                | Some ccss -> ccss
                | None -> Error "generate : refine_targets : clst" |> Stdlib.raise in
          let new_igdt : ingredients =
            { igdt_query_category=qctg;
              igdt_model_opt=mopt;
              igdt_vc=vc;
              igdt_sym_state=fs;
              igdt_comp_type_map=ctmap} in
          (* 1-2. accumulate new ingredients *)
          CPMap.update
            acc
            fs.ss_entry_mci
            ~f:(fun igdts_opt -> 
                  if Option.is_none igdts_opt then (CPSet.singleton new_igdt) 
                  else (CPSet.add 
                          (igdts_opt |> Option.get)
                          new_igdt))) in
  (* 2. generate invariants from current invariant *)
  let newly_generated_inv : Se.invmap CPSet.t =
    CPMap.fold
      refine_targets
      ~init:CPSet.empty
      ~f:(fun ~key ~data acc -> 
          (* select refine function whether it is entry or not *)
          let rf = if (key.mci_cutcat = MCC_trx_entry) then (refine_t igi_cur_inv) else (refine_l igi_cur_inv) in
          (* generate new invariants and accumulate it *)
          CPSet.fold
            data
            ~init:acc
            ~f:(fun acc igdt -> CPSet.union acc (rf igdt))) in
  CPSet.diff newly_generated_inv igi_collected
  (* generate function end *)
end
