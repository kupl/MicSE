(*****************************************************************************)
(*****************************************************************************)
(* Available Variable Analysis                                               *)
(*****************************************************************************)
(*****************************************************************************)

(* For each vertex, get the information of which variables are defined "before" and "after" execute that vertex. *)
(* Because it does consider whether the variable comes from *)

module AvailVar = struct
  open PreLib
  
  type vertex = Cfg.vertex

  module CPSet = Core.Set.Poly
  module CPMap = Core.Map.Poly

  (* type t = (vertex |-> (available variable "before" execute the stmt) & (available variables "after" execute the stmt)) mapping *)
  type abs_set = Top | S of string CPSet.t  (* variable set - abstract domain *)
  let abs_set_join : abs_set -> abs_set -> abs_set
  =fun s1 s2 -> begin
    match s1, s2 with
    | Top, _ -> Top
    | _, Top -> Top
    | S set1, S set2 -> S (CPSet.union set1 set2)
  end
  let abs_set_meet : abs_set -> abs_set -> abs_set
  =fun s1 s2 -> begin
    match s1, s2 with
    | Top, _ -> s2
    | _, Top -> s1
    | S set1, S set2 -> S (CPSet.inter set1 set2) 
  end
  let abs_set_eq : abs_set -> abs_set -> bool
  =fun s1 s2 -> begin
    match s1, s2 with
    | Top, Top -> true
    | S set1, S set2 -> CPSet.equal set1 set2
    | _ -> false
  end
  let abs_set_concr : abs_set -> string CPSet.t
  = (function | Top -> CPSet.empty | S sset -> sset)

  type t = (int, abs_set * abs_set) CPMap.t
  type worklist = vertex CPSet.t

  (* ANALYSIS PROCESS 
    1. Problem Def: Calculate pre & post available-variable set for each vertex
    2. Main-Entry vertex and Lambda-Entry vertices' pre-set are always bottom (bottom = empty set)
    3. Other pre & post sets are all Top (artificial, abstract domain set) value
    4. pre-set of the vertex-v from worklist = (forall pv in (v's predecessor): Meet-All pv.postset)
    5. Strong update vertex-v's postset, postset = join((v's preset from 4),(v's assign-stmt LHS))S
    6. initial worklist = {main entry} + lambda_entries
    7. if vertex-v's preset or postset updated, (worklist := worklist + v's successors)
  *)
  let run : Cfg.t -> t
  =fun cfg -> begin
    let emsg_gen s : string = "Pre.Analyzer.AvailVar.run : " ^ s in
    let entry_vtxset : vertex CPSet.t = CPMap.fold cfg.lambda_id_map ~init:(CPSet.singleton cfg.main_entry) ~f:(fun ~key:_ ~data:(ev,_,_,_) accset -> CPSet.add accset ev) in
    (* let is_entry_vtx : vertex -> bool = fun v -> CPSet.mem entry_vtxset v in *)
    let initial_t : t = CPSet.fold entry_vtxset ~init:(CPMap.empty) ~f:(fun accmap ev -> CPMap.update accmap ev ~f:(fun _ -> (S CPSet.empty, Top))) in
    let rec work : (t * worklist) -> (t * worklist)
    =fun (acct, wl) -> begin
      match CPSet.choose wl with
      | None -> (acct, wl)
      | Some wvtx -> (
          let wvtx_emitted_worklist : vertex CPSet.t = CPSet.remove wl wvtx in
          let find_prepost : vertex -> (abs_set * abs_set) = fun v -> CPMap.find acct v |> (function None -> (Top, Top) | Some r -> r) in
          (*let find_pre : vertex -> abs_set = fun v -> find_prepost v |> Stdlib.fst in*)
          let find_post : vertex -> abs_set = fun v -> find_prepost v |> Stdlib.snd in
          let (before_pre, before_post) : abs_set * abs_set = find_prepost wvtx in
          let calculated_pre : abs_set = List.fold_left (fun acc_aset (_, predv) -> abs_set_meet acc_aset (find_post predv)) before_pre (Cfg.read_pred_from_vtx cfg wvtx) in
          let calculated_post : abs_set =
            (* strong update *)
            match calculated_pre, (Cfg.t_map_find ~errtrace:(emsg_gen "work : calculated_post") cfg.vertex_info wvtx) with
            | S preset, Cfg_assign (rhs_v, _) -> S (CPSet.add preset rhs_v)
            | _ -> calculated_pre
          in
          let is_updated : bool = Stdlib.not ((abs_set_eq before_pre calculated_pre) && (abs_set_eq before_post calculated_post)) in
          if is_updated
          then (
            (* update acct *)
            let updated_acct = CPMap.update acct wvtx ~f:(fun _ -> (calculated_pre, calculated_post)) in
            (* update wl *)
            let updated_wl = List.fold_left (fun acc_wlset (_, succv) -> CPSet.add acc_wlset succv) wvtx_emitted_worklist (Cfg.read_succ_from_vtx cfg wvtx) in
            (* return *)
            work (updated_acct, updated_wl)
          ) else (
            work (acct, wvtx_emitted_worklist)
          )
        )
    end in (* function work end *)
    let worklist_result = work (initial_t, entry_vtxset) in
    worklist_result |> Stdlib.fst
  end (* function run end *)

end (* module AvailVar end *)
