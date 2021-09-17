(* Inv : Invariant manager for verification *)

exception InvError of string

open! Core

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(* Map of Tz.mich_t Tz.cc *)
module MTMap = Map.Make (Tz.MichTCC_cmp)

(* Set of Tz.mich_v Tz.cc *)
module MVSet = Set.Make (Tz.MichVCC_cmp)

(* Set of Tz.mich_f *)
module MFSet = Set.Make (Tz.MichF_cmp)

(* Map of set of Tz.mich_f *)
module MFSMap = Map.Make (Tz.MFSet)

(* Map of Tz.mich_cut_info *)
module MCIMap = Map.Make (Tz.MichCutInfo_cmp)

(* Set of Tz.r_mich_cut_info *)
module RMCISet = Set.Make (Tz.RMichCutInfo_cmp)

(* Map of Tz.r_mich_cut_info *)
module RMCIMap = Map.Make (Tz.RMichCutInfo_cmp)

(* Set of Tz.sym_state *)
module SSet = Set.Make (Tz.SymState_cmp)

(* Set of Igdt.igdt *)
module ISet = Set.Make (Igdt.IGDT_cmp)

(******************************************************************************)
(******************************************************************************)
(* Invariants                                                                 *)
(******************************************************************************)
(******************************************************************************)

type inv_map = MFSet.t RMCIMap.t [@@deriving sexp, compare, equal]

module InvMap_cmp = struct
  type t = inv_map [@@deriving sexp, compare]
end

(* Set of inv_map *)
module InvSet = Set.Make (InvMap_cmp)

(******************************************************************************)
(******************************************************************************)
(* Invariant Candidates                                                       *)
(******************************************************************************)
(******************************************************************************)

type cands = (bool * int) MFSMap.t [@@deriving sexp, compare, equal]

type cand_map = cands RMCIMap.t [@@deriving sexp, compare, equal]

(******************************************************************************)
(******************************************************************************)
(* Failed Candidate Pair                                                      *)
(******************************************************************************)
(******************************************************************************)

type mci_pair = {
  mp_start : Tz.r_mich_cut_info;
  mp_block : Tz.r_mich_cut_info;
}
[@@deriving sexp, compare, equal]

type cand_pair = {
  cp_start : MFSet.t;
  cp_block : MFSet.t;
}
[@@deriving sexp, compare, equal]

module MciPair_cmp = struct
  type t = mci_pair [@@deriving compare, sexp]
end

module CandPair_cmp = struct
  type t = cand_pair [@@deriving compare, sexp]
end

module MPMap = Map.Make (MciPair_cmp)
module CPSet = Set.Make (CandPair_cmp)

type failed_cp = CPSet.t MPMap.t [@@deriving sexp, compare, equal]

(******************************************************************************)
(******************************************************************************)
(* Utility Functions                                                          *)
(******************************************************************************)
(******************************************************************************)

(******************************************************************************)
(* Helper Functions                                                           *)
(******************************************************************************)

module IGDTL_cmp = struct
  type t = Igdt.igdt list [@@deriving sexp, compare]
end

module ILSet = Set.Make (IGDTL_cmp)

(* combination [{1; 2;}; {a; b;}; ...] === {[1; a; ...]; [1; b; ...]; [2; a; ...]; [2; b; ...];} *)
let combination : ISet.t list -> ILSet.t =
  fun set_lst ->
  if List.for_all set_lst ~f:(fun i_set -> not (ISet.is_empty i_set))
  then
    List.fold_right set_lst
      ~f:(fun i_set acc_l_set ->
        if ISet.is_empty i_set
        then ILSet.empty
        else
          ISet.to_list i_set
          |> List.map ~f:(fun i ->
                 if ILSet.is_empty acc_l_set
                 then ILSet.singleton [ i ]
                 else ILSet.map acc_l_set ~f:(List.cons i)
             )
          |> ILSet.union_list)
      ~init:ILSet.empty
  else ILSet.empty
(* function combination end *)

(* combination_self {a; b;} 2 === {[a; a;]; [a; b;]; [b; a;]; [b; b;];} *)
let combination_self : ISet.t -> size:int -> ILSet.t =
  fun iset ~size ->
  let (set_lst : ISet.t list) = List.init size ~f:(fun _ -> iset) in
  combination set_lst
(* function combination_self end *)

module MFOPT_cmp = struct
  type t = Tz.mich_f option [@@deriving sexp, compare]
end

module MFOSet = Set.Make (MFOPT_cmp)

(* filter_symmetry {[a; a;]; [a; b;]; [a; c;] [b; a;]; [b; b;]; [b; c;]; [c; a;]; [c; b;]; [c; c;];} === {[a; b;]; [a; c;]; [b; c;];}  *)
(* This function is only working at ingredient list size 2 *)
let filter_symmetry : ILSet.t -> ILSet.t =
  fun ilset ->
  if ILSet.is_empty ilset
  then ilset
  else if List.length (ILSet.choose_exn ilset) > 2
  then ilset
  else
    ILSet.fold ilset ~init:ILSet.empty ~f:(fun acc il ->
        if ILSet.mem acc (List.rev il) then acc else ILSet.add acc il
    )
(* function filter_symmetry end *)

(* filter_equal {[a; a;]; [a; b;]; [a; c;] [b; a;]; [b; b;]; [b; c;]; [c; a;]; [c; b;]; [c; c;];} === {[a; b;]; [a; c;] [b; a;]; [b; c;]; [c; a;]; [c; b;];}  *)
let filter_equal : ILSet.t -> ILSet.t =
  fun ilset ->
  if ILSet.is_empty ilset
  then ilset
  else
    ILSet.fold ilset ~init:ILSet.empty ~f:(fun acc il ->
        let (il_hd : Igdt.igdt option) = List.hd il in
        if Option.is_none il_hd
        then acc
        else if List.for_all il ~f:(Igdt.equal_igdt (Option.value_exn il_hd))
        then acc
        else ILSet.add acc il
    )
(* function filter_equal end *)

(******************************************************************************)
(* Invariant Candidate Templates                                              *)
(******************************************************************************)

let dummy_ctx : Tz.mich_sym_ctxt = []

let gen_template :
    ?except_lit_only:bool ->
    ?target_mode:[ `Normal | `Asymm | `Asymm_rfl ] ->
    f:((Tz.mich_t * Tz.mich_v Tz.cc) list -> Tz.mich_f option) ->
    Igdt.igdt_sets ->
    Tz.mich_t Tz.cc list list ->
    MFSet.t =
   let open Tz in
   let open Igdt in
   fun ?(except_lit_only = true) ?(target_mode = `Normal) ~f igdt_map
       target_types ->
   List.map target_types ~f:(fun typ_lst ->
       let (targets : (ISet.t * ISet.t * ISet.t * mich_t cc) list) =
          List.map typ_lst ~f:(fun typ ->
              let ({ lit = lit_set; non_lit = nlit_set } : igdt_delim) =
                 find_igdt_sets igdt_map typ
              in
              let (all_set : ISet.t) = ISet.union lit_set nlit_set in
              (lit_set, nlit_set, all_set, typ)
          )
       in
       let (target_comb : ILSet.t) =
          ( if not except_lit_only
          then
            combination (List.map targets ~f:(fun (_, _, all_set, _) -> all_set))
          else
            List.mapi targets ~f:(fun idx (_, n_set, _, _) ->
                let ((l_set_lst : ISet.t list), (a_set_lst : ISet.t list)) =
                   List.split_n targets idx
                   |> fun (fst_lst, snd_lst) ->
                   ( List.map fst_lst ~f:(fun (l_set, _, _, _) -> l_set),
                     List.tl_exn snd_lst
                     |> List.map ~f:(fun (_, _, a_set, _) -> a_set)
                   )
                in
                combination (List.join [ l_set_lst; [ n_set ]; a_set_lst ])
            )
            |> ILSet.union_list
          )
          |>
          match target_mode with
          | `Normal    -> Fun.id
          | `Asymm     -> filter_symmetry
          | `Asymm_rfl -> filter_equal
       in
       MFOSet.map target_comb ~f:(fun c_lst ->
           f (List.map c_lst ~f:(fun i -> (i.ig_typ.cc_v, i.ig_value)))
       )
       |> MFSet.filter_map ~f:Fun.id
   )
   |> MFSet.union_list
(* function gen_template end *)

let tmp_eq : Igdt.igdt_sets -> MFSet.t =
   let open Tz in
   let open TzUtil in
   let gctx = gen_mich_v_ctx ~ctx:dummy_ctx in
   (* syntax sugar *)
   fun igdt_map ->
   let (target_types : Tz.mich_t Tz.cc list list) =
      MTMap.keys igdt_map |> List.map ~f:(fun t -> List.init 2 ~f:(fun _ -> t))
   in
   gen_template igdt_map target_types ~target_mode:`Asymm ~f:(fun tvl ->
       match tvl with
       | [ (_, v1); (_, v2) ] when not (equal_cc equal_mich_v v1 v2) ->
         Some (MF_eq (gctx v1, gctx v2))
       | [ (_, _); (_, _) ] -> None
       | _ -> InvError "tmp_eq : wrong ingredient length" |> raise
   )
(* function tmp_eq end *)

let tmp_ge : Igdt.igdt_sets -> MFSet.t =
   let open Tz in
   let open TzUtil in
   let gctx = gen_mich_v_ctx ~ctx:dummy_ctx in
   (* syntax sugar *)
   let (empty_str : mich_v cc) = gen_dummy_cc (MV_lit_string "") in
   let (max_mtz : mich_v cc) =
      MV_lit_mutez (Bigint.of_int64 Int64.max_value) |> gen_dummy_cc
   in
   let (zero_mtz : mich_v cc) = gen_dummy_cc (MV_lit_mutez Bigint.zero) in
   let (zero_nat : mich_v cc) = gen_dummy_cc (MV_lit_nat Bigint.zero) in
   let (zero : mich_v cc) = gen_dummy_cc (MV_lit_int Bigint.zero) in
   let equal_mich_v_cc = equal_cc equal_mich_v in
   let make_ge : mich_v cc * mich_v cc -> mich_f option =
     fun (v1, v2) ->
     let (cmp : mich_v cc) = gen_dummy_cc (MV_compare (v1, v2)) in
     Some (MF_is_true (gctx (gen_dummy_cc (MV_geq_ib (cmp, zero)))))
   in
   fun igdt_map ->
   let (target_types : Tz.mich_t Tz.cc list list) =
      List.map [ MT_int; MT_nat; MT_mutez; MT_string ] ~f:(fun t ->
          List.init 2 ~f:(fun _ -> gen_dummy_cc t)
      )
   in
   gen_template igdt_map target_types ~f:(fun tvl ->
       match tvl with
       | [ (MT_mutez, v1); (MT_mutez, v2) ]
         when (not (equal_cc equal_mich_v v1 v2))
              && (not (equal_mich_v_cc v1 max_mtz))
              && not (equal_mich_v_cc v2 zero_mtz) ->
         make_ge (v1, v2)
       | [ (MT_nat, v1); (MT_nat, v2) ]
         when (not (equal_cc equal_mich_v v1 v2))
              && not (equal_mich_v_cc v2 zero_nat) ->
         make_ge (v1, v2)
       | [ (MT_string, v1); (MT_string, v2) ]
         when (not (equal_cc equal_mich_v v1 v2))
              && not (equal_mich_v_cc v2 empty_str) ->
         make_ge (v1, v2)
       | [ (t1, v1); (t2, v2) ]
         when equal_mich_t t1 t2 && not (equal_cc equal_mich_v v1 v2) ->
         make_ge (v1, v2)
       | [ (_, _); (_, _) ] -> None
       | _ -> InvError "tmp_ge : wrong ingredient length" |> raise
   )
(* function tmp_ge end *)

let tmp_gt : Igdt.igdt_sets -> MFSet.t =
   let open Tz in
   let open TzUtil in
   let gctx = gen_mich_v_ctx ~ctx:dummy_ctx in
   (* syntax sugar *)
   let (zero : mich_v cc) = gen_dummy_cc (MV_lit_int Bigint.zero) in
   let make_gt : mich_v cc * mich_v cc -> mich_f option =
     fun (v1, v2) ->
     let (cmp : mich_v cc) = gen_dummy_cc (MV_compare (v1, v2)) in
     Some (MF_is_true (gctx (gen_dummy_cc (MV_gt_ib (cmp, zero)))))
   in
   fun igdt_map ->
   let (target_types : Tz.mich_t Tz.cc list list) =
      List.map [ MT_int; MT_nat; MT_mutez; MT_string ] ~f:(fun t ->
          List.init 2 ~f:(fun _ -> gen_dummy_cc t)
      )
   in
   gen_template igdt_map target_types ~f:(fun tvl ->
       match tvl with
       | [ (_, v1); (_, v2) ] when not (equal_cc equal_mich_v v1 v2) ->
         make_gt (v1, v2)
       | [ (_, _); (_, _) ] -> None
       | _ -> InvError "tmp_gt : wrong ingredient length" |> raise
   )
(* function tmp_gt end *)

let tmp_add_2_eq : Igdt.igdt_sets -> MFSet.t =
   let open Tz in
   let open TzUtil in
   let gctx = gen_mich_v_ctx ~ctx:dummy_ctx in
   (* syntax sugar *)
   let (zero : mich_v cc) = gen_dummy_cc (MV_lit_int Bigint.zero) in
   let make_add_2_eq_mtz : mich_v cc * mich_v cc * mich_v cc -> mich_f option =
     fun (v1, v2, v3) ->
     let (add : mich_v cc) = gen_dummy_cc (MV_add_mmm (v1, v2)) in
     Some (MF_eq (gctx add, gctx v3))
   in
   fun igdt_map ->
   let (target_types : Tz.mich_t Tz.cc list list) =
      List.map [ MT_mutez ] ~f:(fun t ->
          List.init 3 ~f:(fun _ -> gen_dummy_cc t)
      )
   in
   gen_template igdt_map target_types ~f:(fun tvl ->
       match tvl with
       | [ (MT_mutez, v1); (MT_mutez, v2); (MT_mutez, v3) ]
         when (not (equal_cc equal_mich_v v1 v3))
              && (not (equal_cc equal_mich_v v2 v3))
              && (not (equal_cc equal_mich_v v1 zero))
              && not (equal_cc equal_mich_v v2 zero) ->
         make_add_2_eq_mtz (v1, v2, v3)
       | [ (_, _); (_, _); (_, _) ] -> None
       | _ -> InvError "tmp_add_2_eq : wrong ingredient length" |> raise
   )
(* function tmp_add_2_eq end *)

let tmp_add_3_eq : Igdt.igdt_sets -> MFSet.t =
   let open Tz in
   let open TzUtil in
   let gctx = gen_mich_v_ctx ~ctx:dummy_ctx in
   (* syntax sugar *)
   let (zero : mich_v cc) = gen_dummy_cc (MV_lit_int Bigint.zero) in
   let make_add_3_eq_mtz :
       mich_v cc * mich_v cc * mich_v cc * mich_v cc -> mich_f option =
     fun (v1, v2, v3, v4) ->
     let (add : mich_v cc) =
        gen_dummy_cc (MV_add_mmm (gen_dummy_cc (MV_add_mmm (v1, v2)), v3))
     in
     Some (MF_eq (gctx add, gctx v4))
   in
   fun igdt_map ->
   let (target_types : Tz.mich_t Tz.cc list list) =
      List.map [ MT_mutez ] ~f:(fun t ->
          List.init 4 ~f:(fun _ -> gen_dummy_cc t)
      )
   in
   gen_template igdt_map target_types ~f:(fun tvl ->
       match tvl with
       | [ (MT_mutez, v1); (MT_mutez, v2); (MT_mutez, v3); (MT_mutez, v4) ]
         when (not (equal_cc equal_mich_v v1 v4))
              && (not (equal_cc equal_mich_v v2 v4))
              && (not (equal_cc equal_mich_v v3 v4))
              && (not (equal_cc equal_mich_v v1 zero))
              && (not (equal_cc equal_mich_v v2 zero))
              && not (equal_cc equal_mich_v v3 zero) ->
         make_add_3_eq_mtz (v1, v2, v3, v4)
       | [ (_, _); (_, _); (_, _); (_, _) ] -> None
       | _ -> InvError "tmp_add_3_eq : wrong ingredient length" |> raise
   )
(* function tmp_add_3_eq end *)

(******************************************************************************)
(* Invariants & Invariant Candidates                                          *)
(******************************************************************************)

let cvt_mci_pair : Tz.mich_cut_info * Tz.mich_cut_info -> mci_pair =
   let open TzUtil in
   fun (mci1, mci2) ->
   { mp_start = get_reduced_mci mci1; mp_block = get_reduced_mci mci2 }
(* function cvt_mci_pair end *)

let cvt_cand_pair : MFSet.t * MFSet.t -> cand_pair =
  (fun (fset1, fset2) -> { cp_start = fset1; cp_block = fset2 })
(* function cvt_cand_pair end *)

(* Invariants *****************************************************************)

let gen_true_inv_map : Se.se_result -> inv_map =
   let open Tz in
   let open TzUtil in
   fun se_res ->
   SSet.to_list se_res.Se.sr_blocked
   |> List.map ~f:(fun ss ->
          List.map [ ss.ss_start_mci; ss.ss_block_mci ] ~f:get_reduced_mci
          |> RMCISet.of_list
      )
   |> RMCISet.union_list
   |> RMCISet.to_list
   |> List.map ~f:(fun rmci -> (rmci, MFSet.singleton MF_true))
   |> RMCIMap.of_alist
   |> function
   | `Ok mmm            -> mmm
   | `Duplicate_key ttt ->
     InvError
       ("gen_true_inv_map : " ^ Sexp.to_string (sexp_of_r_mich_cut_info ttt))
     |> raise
(* function gen_true_inv_map end *)

let gen_initial_inv_map : Se.se_result -> inv_map =
  (fun se_res -> gen_true_inv_map se_res)
(* function gen_initial_inv_map end *)

let find_inv_by_rmci : inv_map -> Tz.r_mich_cut_info -> MFSet.t =
  fun imap rmci ->
  RMCIMap.find imap rmci
  |> function
  | Some sss -> sss
  | None     ->
    InvError
      ("find_inv_map : " ^ Sexp.to_string (Tz.sexp_of_r_mich_cut_info rmci))
    |> raise
(* function find_inv_map end *)

let find_inv : inv_map -> Tz.mich_cut_info -> MFSet.t =
  (fun imap mci -> find_inv_by_rmci imap (TzUtil.get_reduced_mci mci))
(* function find_inv_map end *)

let update_inv_map :
    inv_map -> key:Tz.r_mich_cut_info -> value:MFSet.t -> inv_map =
  fun imap ~key:rmci ~value:fset ->
  RMCIMap.update imap rmci ~f:(function
  | Some inv -> MFSet.union inv fset
  | None     -> fset
  )
(* function update_inv_map end *)

let merge_inv_map : inv_map -> inv_map -> inv_map =
  fun imap1 imap2 ->
  RMCIMap.merge imap1 imap2 ~f:(fun ~key:_ opt ->
      match opt with
      | `Both (inv1, inv2) -> Some (MFSet.union inv1 inv2)
      | `Left inv1         -> Some inv1
      | `Right inv2        -> Some inv2
  )
(* function merge_inv_map end *)

let strengthen_inv_map : InvSet.t -> inv_map -> InvSet.t =
  (fun invset imap -> InvSet.map invset ~f:(merge_inv_map imap))
(* function strengthen_inv_map end *)

let check_contain_pair : inv_map -> mci_pair -> cand_pair -> bool =
  fun imap mcip candp ->
  let (inv_start : MFSet.t) = find_inv_by_rmci imap mcip.mp_start in
  let (inv_block : MFSet.t) = find_inv_by_rmci imap mcip.mp_block in
  let (inv_cp : cand_pair) = cvt_cand_pair (inv_start, inv_block) in
  equal_cand_pair candp inv_cp
(* function check_contain_pair end *)

(* Invariant Candidates *******************************************************)

let gen_initial_cand_map :
    is_fset_sat:(MFSet.t -> bool) -> Igdt.igdts_map -> cand_map =
  fun ~is_fset_sat igdt_map ->
  RMCIMap.map igdt_map ~f:(fun igdt_sets ->
      [ tmp_eq; tmp_ge; tmp_gt; tmp_add_2_eq; tmp_add_3_eq ]
      |> List.map ~f:(fun tmp -> tmp igdt_sets)
      |> MFSet.union_list
      |> MFSet.fold ~init:MFSMap.empty ~f:(fun acc_cmap fmla ->
             let (fset : MFSet.t) = MFSet.singleton fmla in
             if not (is_fset_sat fset)
             then acc_cmap
             else
               MFSMap.add acc_cmap ~key:fset ~data:(true, 0)
               |> function
               | `Duplicate ->
                 InvError "gen_initial_cand_map : duplicate key" |> raise
               | `Ok cmap   -> cmap
         )
  )
(* function gen_initial_cand_map end *)

let find_cand_by_rmci : cand_map -> Tz.r_mich_cut_info -> cands =
  fun cmap rmci ->
  RMCIMap.find cmap rmci
  |> function
  | Some sss -> sss
  | None     -> MFSMap.empty
(* function cand_map_find end *)

let find_cand : cand_map -> Tz.mich_cut_info -> cands =
  (fun cmap mci -> find_cand_by_rmci cmap (TzUtil.get_reduced_mci mci))
(* function cand_map_find end *)

let find_cand_top_k_by_rmci :
    top_k:int -> cand_map -> Tz.r_mich_cut_info -> MFSet.t list =
  fun ~top_k cmap rmci ->
  find_cand_by_rmci cmap rmci
  |> MFSMap.to_alist
  (* CHECK *)
  |> List.sort ~compare:(fun (_, (_, s1)) (_, (_, s2)) -> compare_int s1 s2)
  |> List.map ~f:fst
  |> List.rev
  |> (fun lst -> List.split_n lst top_k)
  |> fst
(* function finc_cand_map_top_k_by_tmci end *)

let find_cand_top_k : top_k:int -> cand_map -> Tz.mich_cut_info -> MFSet.t list
    =
  fun ~top_k cmap mci ->
  find_cand_top_k_by_rmci ~top_k cmap (TzUtil.get_reduced_mci mci)
(* function find_cand_map_top_k end *)

let strengthen_cand_map :
    is_fset_sat:(MFSet.t -> bool) -> cand_map -> inv_map -> cand_map =
  fun ~is_fset_sat cmap imap ->
  RMCIMap.mapi cmap ~f:(fun ~key:rmci ~data:cset ->
      let (cur_inv : MFSet.t) = find_inv_by_rmci imap rmci in
      MFSMap.fold cset ~init:MFSMap.empty
        ~f:(fun ~key ~data:(f1, s1) new_cmap ->
          let (fset : MFSet.t) = MFSet.union cur_inv key in
          if not (is_fset_sat fset)
          then new_cmap
          else
            MFSMap.update new_cmap fset ~f:(function
            | None         -> (f1, s1)
            | Some (f2, _) -> (f1 && f2, 0)
            )
      )
  )
(* function strengthen_cand_map *)

let score_cand :
    cand_map -> key:Tz.r_mich_cut_info -> value:MFSet.t -> point:int -> cand_map
    =
  fun cmap ~key ~value ~point ->
  RMCIMap.update cmap key ~f:(function
  | Some cands ->
    MFSMap.update cands value ~f:(function
    | Some (flag, score) -> (flag, score + point)
    | None               -> (true, point)
    )
  | None       -> InvError "score_cand : wrong mci" |> raise
  )
(* function score_cand end *)

let unflag_cand :
    cand_map -> key:Tz.r_mich_cut_info -> value:MFSet.t -> cand_map =
  fun cmap ~key ~value ->
  RMCIMap.update cmap key ~f:(function
  | Some cands ->
    MFSMap.update cands value ~f:(function
    | Some (_, score) -> (false, score)
    | None            -> (false, 0)
    )
  | None       -> InvError "unflag_cand : wrong mci" |> raise
  )
(* function unflag_cand end *)

(* Failed Candidate Pair ******************************************************)

(* function cvt_cand_pair end *)
let gen_initial_failed_cp : unit -> failed_cp = (fun () -> MPMap.empty)
(* function gen_initial_failed_cp end *)

let find_failed_cp_by_rmci : failed_cp -> mci_pair -> CPSet.t =
  fun fmap rmcip ->
  MPMap.find fmap rmcip
  |> function
  | Some sss -> sss
  | None     -> CPSet.empty
(* function find_failed_cp end *)

let find_failed_cp : failed_cp -> Tz.mich_cut_info * Tz.mich_cut_info -> CPSet.t
    =
  (fun fmap mcip -> find_failed_cp_by_rmci fmap (cvt_mci_pair mcip))
(* function find_failed_cp *)

let is_already_failed_by_rmci : failed_cp -> mci_pair -> cand_pair -> bool =
  (fun fmap rmcip candp -> CPSet.mem (find_failed_cp_by_rmci fmap rmcip) candp)
(* function is_already_failed_by_rmci end *)

let is_already_failed :
    failed_cp ->
    Tz.mich_cut_info * Tz.mich_cut_info ->
    MFSet.t * MFSet.t ->
    bool =
  fun fmap mcip fsetp ->
  is_already_failed_by_rmci fmap (cvt_mci_pair mcip) (cvt_cand_pair fsetp)
(* function is_already_failed end *)

let add_failed_cp : failed_cp -> key:mci_pair -> value:cand_pair -> failed_cp =
  fun failed_cp ~key ~value ->
  MPMap.update failed_cp key ~f:(function
  | Some cpset -> CPSet.add cpset value
  | None       -> CPSet.singleton value
  )
(* function add_failed_cp end *)
