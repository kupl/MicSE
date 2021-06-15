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
      ~f:(fun acc s -> (
        if Tz.is_normal_mcc s.ss_entry_mci.mci_cutcat then (
          CPMap.update acc s.ss_entry_mci ~f:(function | None -> CPSet.singleton s.ss_entry_symstack | Some ss -> CPSet.add ss s.ss_entry_symstack))
        else acc)) in
  let mci_tstack : (Tz.mich_cut_info, tstack) CPMap.t =
    CPMap.map mci_vstack_set
      ~f:(fun vsset -> (vsset
        |> get_type_stack
        |> (function | Some ts -> ts | None -> Error "bake_comp_map : mci_tstack" |> Stdlib.raise))) in
  CPMap.mapi mci_tstack
    ~f:(fun ~key ~data -> (
      Comp.base_comp_from_mci key data
      |> CList.fold
        ~init:CTMap.empty
        ~f:(fun ctmap base_comp_opt -> (
          match base_comp_opt with
          | None -> ctmap
          | Some base_comp -> (
            (if Option.is_some init_strg_opt then (
              Comp.collect (Comp.base_comp_from_v (Option.get init_strg_opt) ~loc:0) ctmap)
            else ctmap)
            |> Comp.collect base_comp)))))
end (* function bake_comp_map end *)

(* 
let init_invmap : comp_map -> Tz.sym_state -> Se.invmap -> Se.invmap
= let module CPSet = Core.Set.Poly in
  let module CPMap = Core.Map.Poly in
  let module CTMap = Comp.CTMap in
  (* function init_invmap start *)
  fun cpmap init_ss true_invmap -> begin
  true_invmap
  |> CPMap.mapi
    ~f:(fun ~key ~data -> (
      (* 0. set the type map for the components *)
      let tmap : (Comp.t CPSet.t) CTMap.t = 
        CPMap.find cpmap key
        |> (function Some tmap -> tmap | None -> CTMap.empty)
        |> (fun tmap -> (
          if key = init_ss.ss_entry_mci then (
          init_ss.ss_entry_symstack
          |> Core.List.hd_exn
          |> (fun vvv -> Tz.MV_car vvv |> Tz.gen_custom_cc vvv)
          |> Comp.base_comp_from_v ~loc:0
          |> (fun bc -> Comp.collect bc tmap))
          else if key = init_ss.ss_block_mci then (
            CPMap.find cpmap (init_ss.ss_entry_mci)
            |> (function Some tmap -> tmap | None -> CTMap.empty))
          else tmap)) in
      (* 1. Update initial invariant with each type *)
      tmap
      |> CTMap.fold
        ~init:data
        ~f:(fun ~key ~data acc -> (
          if key.cc_v = MT_mutez then (
            data
            |> CPSet.filter
              ~f:(fun c -> 
                if Option.is_some c.Comp.cp_base_var && (Option.get c.Comp.cp_base_var) = c.Comp.cp_value then true
                else false)
            |> CPSet.map
              ~f:(fun c -> (
                Tz.MF_and [ (* MUTEZ BOUND *)
                  MF_add_mmm_no_overflow (c.Comp.cp_value, ((Tz.MV_lit_mutez Z.zero) |> Tz.gen_dummy_cc));
                  MF_sub_mmm_no_underflow (c.Comp.cp_value, ((Tz.MV_lit_mutez Z.zero) |> Tz.gen_dummy_cc));]))
            |> CPSet.union acc)
          else acc))))
end (* function init_invmap end *)
 *)


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
  CTMap.fold ctmap ~init:CPSet.empty
    ~f:(fun ~key ~data acc -> (
      combination_self_two_diff data
      |> CPSet.map ~f:(fun (c1, c2) -> (
        MF_imply ((fold_precond [c1; c2;]), (
          match key.cc_v with
          | MT_mutez -> (MF_and (CList.fold [c1; c2;] ~init:[(MF_eq (c1.cp_value, c2.cp_value));]
            ~f:(fun acc cin -> (Tz.mutez_bound_f cin.cp_value)::acc)))
          | _ -> MF_eq (c1.cp_value, c2.cp_value)))))
          |> CPSet.union acc))
end (* function all_equal end *)

let all_ge : (Comp.t Core.Set.Poly.t) Comp.CTMap.t -> Tz.mich_t list -> Tz.mich_f Core.Set.Poly.t
= let open Tz in
  let open Comp in
  let module CList = Core.List in
  let module CPSet = Core.Set.Poly in
  (* function all_ge start *)
  fun ctmap tl -> begin
  let ts : Tz.mich_t CPSet.t = tl |> CPSet.of_list in (* target types *)
  CTMap.fold ctmap ~init:CPSet.empty ~f:(fun ~key ~data acc -> (
          if CPSet.exists ts ~f:(fun t -> t = key.cc_v) then (
      combination_self_two_diff_rf data
      |> CPSet.map ~f:(fun (c1, c2) -> (
                    let cmp : Tz.mich_v Tz.cc = MV_compare (c1.cp_value, c2.cp_value) |> gen_dummy_cc in
                    let zero : Tz.mich_v Tz.cc = MV_lit_int (Z.zero) |> gen_dummy_cc in
        MF_imply ((fold_precond [c1; c2;]), (
          match key.cc_v with
          | MT_mutez -> (MF_and (CList.fold [c1; c2;] ~init:[(MF_is_true (gen_dummy_cc (MV_geq_ib (cmp, zero))));]
            ~f:(fun acc cin -> (Tz.mutez_bound_f cin.cp_value)::acc)))
          | _ -> MF_is_true (gen_dummy_cc (MV_geq_ib (cmp, zero)))))))
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
  let ts : Tz.mich_t CPSet.t = tl |> CPSet.of_list in (* target types *)
  CTMap.fold ctmap ~init:CPSet.empty ~f:(fun ~key ~data acc -> (
          if CPSet.exists ts ~f:(fun t -> t = key.cc_v) then (
      combination_self_two_diff_rf data
      |> CPSet.map ~f:(fun (c1, c2) -> (
                  let cmp : Tz.mich_v Tz.cc = MV_compare (c1.cp_value, c2.cp_value) |> gen_dummy_cc in
                  let zero : Tz.mich_v Tz.cc = MV_lit_int (Z.zero) |> gen_dummy_cc in
        MF_imply ((fold_precond [c1; c2;]), (
          match key.cc_v with
          | MT_mutez -> (MF_and (CList.fold [c1; c2;] ~init:[(MF_is_true (gen_dummy_cc (MV_gt_ib (cmp, zero))));]
            ~f:(fun acc cin -> (Tz.mutez_bound_f cin.cp_value)::acc)))
          | _ -> MF_is_true (gen_dummy_cc (MV_gt_ib (cmp, zero)))))))
            |> CPSet.union acc)
          else acc))
end (* function all_gt end *)

let add_2_eq : (Comp.t Core.Set.Poly.t) Comp.CTMap.t -> Tz.mich_t list -> Tz.mich_f Core.Set.Poly.t
= let open Tz in
  let open Comp in
  let module CList = Core.List in
  let module CPSet = Core.Set.Poly in
  (* function add_2_eq start *)
  fun ctmap tl  -> begin
  let ts : Tz.mich_t CPSet.t = tl |> CPSet.of_list in (* target types *)
  CTMap.fold ctmap ~init:CPSet.empty ~f:(fun ~key ~data acc -> (
          if CPSet.exists ts ~f:(fun t -> t = key.cc_v) then (
            match key.cc_v with
            | MT_mutez -> (
        (* mutez + mutez = mutez *)
        let mmm : Tz.mich_f CPSet.t = (
          CPSet.filter data ~f:(fun c -> Option.is_some c.cp_base_var)
              |> combination_self_two_diff
              |> CPSet.map 
            ~f:(fun (c1, c2) -> ((MV_add_mmm (c1.cp_value, c2.cp_value) |> gen_dummy_cc), [c1; c2;]))
          |> CPSet.fold ~init:acc
            ~f:(fun accs (add, clst) -> (
              CPSet.map data ~f:(fun c -> (
                MF_imply (
                  (fold_precond (c::clst)),
                  (MF_and (CList.fold (c::clst) ~init:[(MF_eq (add, c.cp_value));]
                    ~f:(fun acc cin -> (Tz.mutez_bound_f cin.cp_value)::acc))))))
              |> CPSet.union accs))) in
        mmm)
            | _ -> acc)
          else acc))
end (* function add_2_eq end *)


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
  igdt_mci            : Tz.mich_cut_info;
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

let refine_t : Se.invmap -> ingredients -> Tz.mich_f Core.Set.Poly.t
= let open Tz in
  let module CPSet = Core.Set.Poly in
  let module CPMap = Core.Map.Poly in
  (* function refine_t start *)
  fun cur_inv igdt -> begin
  (* 0-1. extract component on entrance of transaction *)
  let ctmap = igdt.igdt_comp_type_map in
  (* 1. generate recipe *)
  let all_eq_fmlas : Tz.mich_f CPSet.t = all_equal ctmap in
  (* let all_ge_fmlas : Tz.mich_f CPSet.t = all_ge ctmap [MT_int; MT_nat; MT_mutez] in *)
  (* let all_gt_fmlas : Tz.mich_f CPSet.t = all_gt ctmap [MT_int; MT_nat; MT_mutez] in *)
  let add_2_eq_fmlas : Tz.mich_f CPSet.t = add_2_eq ctmap [MT_mutez] in
  (* 2. generate invariant map *)
  [ all_eq_fmlas;
    (* all_ge_fmlas; *)
    (* all_gt_fmlas; *)
    add_2_eq_fmlas; ]
  |> collect_set
  |> CPSet.filter
  ~f:(fun c -> (
    CPMap.find
      cur_inv
      igdt.igdt_mci
    |> (function None -> CPSet.empty | Some ss -> ss)
    |> (fun ss -> Stdlib.not (CPSet.mem ss c))))
end (* function refine_t end *)

let refine_l : Se.invmap -> ingredients -> Tz.mich_f Core.Set.Poly.t
= let module CPSet = Core.Set.Poly in
  let module CPMap = Core.Map.Poly in
  fun cur_inv igdt -> begin
  (* refine_l function start *)
  (* 0. extract component on loop entrance *)
  let ctmap = igdt.igdt_comp_type_map in
  (* 1. generate recipe *)
  let all_eq_fmlas : Tz.mich_f CPSet.t = all_equal ctmap in
  (* let all_ge_fmlas : Tz.mich_f CPSet.t = all_ge ctmap [MT_int; MT_nat; MT_mutez] in *)
  (* let all_gt_fmlas : Tz.mich_f CPSet.t = all_gt ctmap [MT_int; MT_nat; MT_mutez] in *)
  let add_2_eq_fmlas : Tz.mich_f CPSet.t = add_2_eq ctmap [MT_mutez] in
  (* 2. generate invariant map *)
  [ all_eq_fmlas;
    (* all_ge_fmlas; *)
    (* all_gt_fmlas; *)
    add_2_eq_fmlas; ]
  |> collect_set
  |> CPSet.filter
    ~f:(fun c -> (
      CPMap.find
        cur_inv
        igdt.igdt_mci
      |> (function None -> CPSet.empty | Some ss -> ss)
      |> (fun ss -> Stdlib.not (CPSet.mem ss c))))
  (* refine_l function end *)
end

let gen_igdt : comp_map -> Tz.mich_cut_info -> ingredients
= let module CPMap = Core.Map.Poly in
  let module CPSet = Core.Set.Poly in
  (* function gen_igdt start *)
  fun comp_map mci -> begin
  let (normalized_mci) : Tz.mich_cut_info = Tz.get_normal_exn mci ~debug:"gen_igdt : normalized_mci : get_normal_mci" in
  let (ctmap) : (Comp.t CPSet.t) Comp.CTMap.t = (
    CPMap.find comp_map normalized_mci
    |> (function Some ccss -> ccss | None -> Error "gen_igdt : ctmap" |> Stdlib.raise)) in
  { igdt_mci=normalized_mci;
    igdt_comp_type_map=ctmap; }
end (* function gen_igdt end *)

let gen_cand : Se.invmap -> ingredients -> Tz.mich_f Core.Set.Poly.t
= let module CPSet = Core.Set.Poly in
  (* function gen_cand start *)
  fun cur_inv igdt -> begin
  (* 1. get cand without in current invariant map *)
  let cur_inv_set : Tz.mich_f CPSet.t = Se.find_inv_fmla cur_inv igdt.igdt_mci in
  let new_fmlas : Tz.mich_f CPSet.t = (
    if Tz.is_trx_entry_mcc igdt.igdt_mci.mci_cutcat then (refine_t cur_inv igdt) else (refine_l cur_inv igdt)) in
  CPSet.diff new_fmlas cur_inv_set
end (* function gen_cand end *)

let update_cand : (Tz.mich_cut_info, Tz.mich_f Core.Set.Poly.t) Core.Map.Poly.t -> 
  Tz.mich_cut_info -> 
  Tz.mich_f Core.Set.Poly.t -> 
  (Tz.mich_cut_info, Tz.mich_f Core.Set.Poly.t) Core.Map.Poly.t
= let module CPSet = Core.Set.Poly in
  let module CPMap = Core.Map.Poly in
  (* function cand_update start *)
  fun accm key cand -> begin
  CPMap.update accm key
    ~f:(fun cset_opt -> (
      if Option.is_none cset_opt then (CPSet.add cand Tz.MF_true)
      else (CPSet.union (Option.get cset_opt) cand)))
end (* function cand_update end *)

let combinate_invmap : 
  Se.invmap ->
  (Tz.mich_cut_info, Tz.mich_f Core.Set.Poly.t) Core.Map.Poly.t ->
  Tz.mich_cut_info Core.Set.Poly.t ->
  Se.invmap Core.Set.Poly.t
= let module CPSet = Core.Set.Poly in
  let module CPMap = Core.Map.Poly in
  let rec combinate_invmap_i :
    Se.invmap ->
    (Tz.mich_cut_info, Tz.mich_f CPSet.t) CPMap.t ->
    Tz.mich_cut_info CPSet.t ->
    Se.invmap CPSet.t -> 
    Se.invmap CPSet.t
  = (* function combinate_invmap_i start *)
    fun cur_invmap cand_map mci_set acc -> begin
    if CPSet.is_empty mci_set then acc
    else (
      (* 1. get MCI target *)
      let target : Tz.mich_cut_info = (
        CPSet.choose mci_set
        |> (function Some mci -> mci | None -> Error "combinate_invmap : combinate_invmap_i : target" |> Stdlib.raise)) in
      let mci_set' : Tz.mich_cut_info CPSet.t = CPSet.remove mci_set target in
      (* 2. get set of formulas in target MCI *)
      let target_cand : Tz.mich_f CPSet.t = (
        CPMap.find cand_map target
        |> (function Some sss -> sss | None -> CPSet.empty)) in
      (* 3. make combinations *)
      let acc' : Se.invmap CPSet.t = (
        if CPSet.is_empty acc then (
          (* input candidates as a singleton set *)
          CPSet.fold target_cand
            ~init:CPSet.empty
            ~f:(fun tacc cand -> (CPSet.add tacc (CPMap.singleton target (CPSet.singleton cand)))))
        else (
          (* attach the candidates to the acc inv candidates *)
          CPSet.fold target_cand
            ~init:CPSet.empty
            ~f:(fun tacc cand -> (
              CPSet.union tacc
                (CPSet.map acc
                  ~f:(fun invmap -> (
                    let update_f : Tz.mich_f CPSet.t option -> Tz.mich_f CPSet.t = (
                      function None -> CPSet.singleton cand | Some sss -> CPSet.add sss cand) in
                    CPMap.update invmap target ~f:update_f))))))) in
      (* 4. Fold *)
      combinate_invmap_i cur_invmap cand_map mci_set' acc')
  end in (* function combinate_invmap_i end *)
  (* function combinate_invmap start *)
  fun cur_invmap cand_map target_mci -> begin
  (* 1. get combinations *)
  combinate_invmap_i cur_invmap cand_map target_mci CPSet.empty
  (* 2. merge with current invmap *)
  |> CPSet.map
    ~f:(fun new_invmap -> (
      CPMap.merge
        cur_invmap
        new_invmap
        ~f:(fun ~key indicator -> (
          let _ = key in
          match indicator with
          | `Both (s1, s2) -> Some (CPSet.union s1 s2)
          | `Left s1 -> Some s1
          | `Right s2 -> Some s2))))
end (* function combinate_invmap end *)

let generate : generate_param -> Se.invmap Core.Set.Poly.t
= let module CPSet = Core.Set.Poly in
  let module CPMap = Core.Map.Poly in
  fun (igi_failed_set, igi_cur_inv, igi_comp_map, igi_collected) -> begin
  (* generate function start *)
  let _ = igi_failed_set in
  (* 1. collect candidates *)
  let cand_map : (Tz.mich_cut_info, Tz.mich_f CPSet.t) CPMap.t = (
    CPMap.fold
      igi_comp_map
      ~init:CPMap.empty
      ~f:(fun ~key ~data accm -> (
        let _ = data in
        if not (Tz.is_normal_mcc key.mci_cutcat) then Error "generate : refine_targets : Tz.is_normalized_mcc" |> Stdlib.raise
        else (gen_igdt igi_comp_map key |> gen_cand igi_cur_inv |> update_cand accm key)))) in
  let target_mci : Tz.mich_cut_info CPSet.t = cand_map |> CPMap.keys |> CPSet.of_list in
  combinate_invmap igi_cur_inv cand_map target_mci
  |> (fun x -> (Utils.Log.debug (fun m -> m "CAND: cand_length = %d" (CPSet.length x))); x)
  |> (fun new_gen_inv -> CPSet.diff new_gen_inv igi_collected)
end
