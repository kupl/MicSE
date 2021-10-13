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

(* Set of Tz.qid *)
module QIDSet = Set.Make (Tz.QId_cmp)

(* Map of Tz.qid *)
module QIDMap = Map.Make (Tz.QId_cmp)

(* Set of Tz.sym_state *)
module SSet = Set.Make (Tz.SymState_cmp)

(* Map of Tz.sym_state_id *)
module SIDMap = Map.Make (Tz.SymStateID_cmp)

(* Set of Igdt.igdt *)
module ISet = Set.Make (Igdt.IGDT_cmp)

(******************************************************************************)
(******************************************************************************)
(* Invariant Candidates                                                       *)
(******************************************************************************)
(******************************************************************************)

type cand = {
  c_fmla : MFSet.t;
  c_cond : MFSet.t;
  c_igdt : ISet.t;
}
[@@deriving sexp, compare, equal]

module Cand_cmp = struct
  type t = cand [@@deriving sexp, compare]
end

(* Set of cand *)
module CSet = Set.Make (Cand_cmp)

(* Map of cand *)
module CMap = Map.Make (Cand_cmp)

(******************************************************************************)
(******************************************************************************)
(* Invariants                                                                 *)
(******************************************************************************)
(******************************************************************************)

type inv_map = cand RMCIMap.t [@@deriving sexp, compare, equal]

module InvMap_cmp = struct
  type t = inv_map [@@deriving sexp, compare]
end

(* Set of inv_map *)
module InvSet = Set.Make (InvMap_cmp)

(******************************************************************************)
(******************************************************************************)
(* Map of Invariant Candidates                                                *)
(******************************************************************************)
(******************************************************************************)

type cands = (bool * (int * int) QIDMap.t) CMap.t
[@@deriving sexp, compare, equal]

type cand_map = cands RMCIMap.t [@@deriving sexp, compare, equal]

(******************************************************************************)
(******************************************************************************)
(* Inductiveness of Candidates                                                *)
(******************************************************************************)
(******************************************************************************)

type cand_pair = {
  cp_start : cand;
  cp_block : cand;
}
[@@deriving sexp, compare, equal]

module CandPair_cmp = struct
  type t = cand_pair [@@deriving compare, sexp]
end

module CPSet = Set.Make (CandPair_cmp)
module CPMap = Map.Make (CandPair_cmp)

type cp_inductiveness = {
  ir_valid : CPSet.t;
  ir_invalid : CPSet.t;
}
[@@deriving compare, sexp, equal]

type inductive_info = cp_inductiveness SIDMap.t
[@@deriving compare, sexp, equal]

type mci_pair = {
  mp_start : Tz.r_mich_cut_info;
  mp_block : Tz.r_mich_cut_info;
}
[@@deriving sexp, compare, equal]

module MciPair_cmp = struct
  type t = mci_pair [@@deriving compare, sexp]
end

module MPMap = Map.Make (MciPair_cmp)

type inductive_info_by_mp = bool CPMap.t MPMap.t
[@@deriving sexp, compare, equal]

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

(* shuffle [a; b; c;] === {[a; b; c;]; [a; c; b;]; [b; a; c;]; [b; c; a;]; [c; a; b;]; [c; b; a;];} *)
let rec shuffle : Igdt.igdt list -> ILSet.t =
  fun il ->
  if List.is_empty il
  then ILSet.singleton []
  else
    List.mapi il ~f:(fun idx i ->
        let ((fst : Igdt.igdt list), (hd_snd : Igdt.igdt list)) =
           List.split_n il idx
        in
        shuffle (fst @ List.tl_exn hd_snd)
        |> ILSet.map ~f:(fun new_il -> i :: new_il)
    )
    |> ILSet.union_list
(* function shuffle end *)

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

(* filter_symmetry {[a; a;]; [a; b;]; [a; c;] [b; a;]; [b; b;]; [b; c;]; [c; a;]; [c; b;]; [c; c;];} === {[a; b;]; [a; c;]; [b; c;];}  *)
(* This function is only working at ingredient list size 2 *)
let filter_symmetry : int -> ILSet.t -> ILSet.t =
   let open Igdt in
   fun pos ilset ->
   if ILSet.is_empty ilset
   then ilset
   else
     ILSet.fold ilset ~init:(ILSet.empty, ILSet.empty)
       ~f:(fun (acc_removed, acc_il) il ->
         if ILSet.mem acc_removed il
         then (acc_removed, acc_il)
         else (
           let ((shuffle_i : igdt list), (normal_i : igdt list)) =
              List.split_n il pos
           in
           let (new_removed : ILSet.t) =
              shuffle shuffle_i |> ILSet.map ~f:(fun il -> il @ normal_i)
           in
           (ILSet.union acc_removed new_removed, ILSet.add acc_il il)
         )
     )
     |> snd
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

let gen_cand_by_fmla : ?cond:MFSet.t -> ?igdt:ISet.t -> Tz.mich_f -> cand =
  fun ?(cond = MFSet.empty) ?(igdt = ISet.empty) fmla ->
  { c_fmla = MFSet.singleton fmla; c_cond = cond; c_igdt = igdt }
(* function gen_cand_by_fmla end *)

let join_cands : cand -> cand -> cand =
  fun cand1 cand2 ->
  {
    c_fmla = MFSet.union cand1.c_fmla cand2.c_fmla;
    c_cond = MFSet.union cand1.c_cond cand2.c_cond;
    c_igdt = ISet.union cand1.c_igdt cand2.c_igdt;
  }
(* function join_cand end *)

let is_subcand : cand -> of_:cand -> bool =
  (fun cand1 ~of_ -> MFSet.is_subset cand1.c_fmla ~of_:of_.c_fmla)
(* function is_subcand end *)

let fmla_of_cand_pre : cand -> Tz.mich_f =
  (fun cand -> MF_and (MFSet.to_list cand.c_fmla @ MFSet.to_list cand.c_cond))
(* function fmla_of_cand_pre end *)

let fmla_of_cand_post : cand -> Tz.mich_f =
  fun cand ->
  MF_imply
    (MF_and (MFSet.to_list cand.c_cond), MF_and (MFSet.to_list cand.c_fmla))
(* function fmla_of_cand_pre end *)

(******************************************************************************)
(* Invariant Candidate Templates                                              *)
(******************************************************************************)

let dummy_ctx : Tz.mich_sym_ctxt = []

let gen_template :
    ?except_lit_only:bool ->
    ?target_mode:[ `Normal | `Asymm     of int | `Asymm_rfl ] ->
    f:((Tz.mich_t * Tz.mich_v Tz.cc) list -> Tz.mich_f option) ->
    Igdt.igdt_sets ->
    Tz.mich_t Tz.cc list list ->
    CSet.t =
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
          | `Asymm n   -> filter_symmetry n
          | `Asymm_rfl -> filter_equal
       in
       ILSet.fold target_comb ~init:CSet.empty ~f:(fun acc c_lst ->
           f (List.map c_lst ~f:(fun i -> (i.ig_typ.cc_v, i.ig_value)))
           |> function
           | None      -> acc
           | Some fmla ->
             CSet.add acc
               (gen_cand_by_fmla fmla ~cond:(fold_precond_lst c_lst)
                  ~igdt:(ISet.of_list c_lst)
               )
       )
   )
   |> CSet.union_list
(* function gen_template end *)

let tmp_eq : Igdt.igdt_sets -> CSet.t =
   let open Tz in
   let open TzUtil in
   let gctx = gen_mich_v_ctx ~ctx:dummy_ctx in
   (* syntax sugar *)
   fun igdt_map ->
   let (target_types : Tz.mich_t Tz.cc list list) =
      MTMap.keys igdt_map |> List.map ~f:(fun t -> List.init 2 ~f:(fun _ -> t))
   in
   gen_template igdt_map target_types ~target_mode:(`Asymm 2) ~f:(fun tvl ->
       match tvl with
       | [ (t1, v1); (t2, v2) ] when equal_mich_t t1 t2 ->
         if not (equal_cc equal_mich_v v1 v2)
         then Some (MF_eq (gctx v1, gctx v2))
         else None
       | [ (_, _); (_, _) ] -> None
       | _ -> InvError "tmp_eq : wrong ingredient length" |> raise
   )
(* function tmp_eq end *)

let tmp_ge : Igdt.igdt_sets -> CSet.t =
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
   let equal_mich_v_cc = equal_cc equal_mich_v in
   let make_ge : mich_v cc * mich_v cc -> mich_f option =
     fun (v1, v2) ->
     Some (MF_is_true (gctx (gen_dummy_cc (MV_geq_ib (v1, v2)))))
   in
   fun igdt_map ->
   let (target_types : Tz.mich_t Tz.cc list list) =
      List.map [ MT_int; MT_nat; MT_mutez; MT_string ] ~f:(fun t ->
          List.init 2 ~f:(fun _ -> gen_dummy_cc t)
      )
   in
   gen_template igdt_map target_types ~target_mode:`Asymm_rfl ~f:(fun tvl ->
       match tvl with
       | [ (MT_mutez, v1); (MT_mutez, v2) ] ->
         if (not (equal_mich_v_cc v1 max_mtz))
            && not (equal_mich_v_cc v2 zero_mtz)
         then make_ge (v1, v2)
         else None
       | [ (MT_nat, v1); (MT_nat, v2) ] ->
         if not (equal_mich_v_cc v2 zero_nat) then make_ge (v1, v2) else None
       | [ (MT_string, v1); (MT_string, v2) ] ->
         if not (equal_mich_v_cc v2 empty_str) then make_ge (v1, v2) else None
       | [ (t1, v1); (t2, v2) ] when equal_mich_t t1 t2 -> make_ge (v1, v2)
       | [ (_, _); (_, _) ] -> None
       | _ -> InvError "tmp_ge : wrong ingredient length" |> raise
   )
(* function tmp_ge end *)

let tmp_gt : Igdt.igdt_sets -> CSet.t =
   let open Tz in
   let open TzUtil in
   let gctx = gen_mich_v_ctx ~ctx:dummy_ctx in
   (* syntax sugar *)
   let make_gt : mich_v cc * mich_v cc -> mich_f option =
     fun (v1, v2) ->
     Some (MF_is_true (gctx (gen_dummy_cc (MV_gt_ib (v1, v2)))))
   in
   fun igdt_map ->
   let (target_types : Tz.mich_t Tz.cc list list) =
      List.map [ MT_int; MT_nat; MT_mutez; MT_string ] ~f:(fun t ->
          List.init 2 ~f:(fun _ -> gen_dummy_cc t)
      )
   in
   gen_template igdt_map target_types ~target_mode:`Asymm_rfl ~f:(fun tvl ->
       match tvl with
       | [ (t1, v1); (t2, v2) ] when equal_mich_t t1 t2 -> make_gt (v1, v2)
       | [ (_, _); (_, _) ] -> None
       | _ -> InvError "tmp_gt : wrong ingredient length" |> raise
   )
(* function tmp_gt end *)

let tmp_add_2_eq : Igdt.igdt_sets -> CSet.t =
   let open Tz in
   let open TzUtil in
   let gctx = gen_mich_v_ctx ~ctx:dummy_ctx in
   (* syntax sugar *)
   let (zero_mtz : mich_v cc) = gen_dummy_cc (MV_lit_mutez Bigint.zero) in
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
   gen_template igdt_map target_types ~target_mode:(`Asymm 2) ~f:(fun tvl ->
       match tvl with
       | [ (MT_mutez, v1); (MT_mutez, v2); (MT_mutez, v3) ] ->
         if (not (equal_cc equal_mich_v v1 v3))
            && (not (equal_cc equal_mich_v v2 v3))
            && (not (equal_cc equal_mich_v v1 zero_mtz))
            && not (equal_cc equal_mich_v v2 zero_mtz)
         then make_add_2_eq_mtz (v1, v2, v3)
         else None
       | [ (_, _); (_, _); (_, _) ] -> None
       | _ -> InvError "tmp_add_2_eq : wrong ingredient length" |> raise
   )
(* function tmp_add_2_eq end *)

let tmp_add_3_eq : Igdt.igdt_sets -> CSet.t =
   let open Tz in
   let open TzUtil in
   let gctx = gen_mich_v_ctx ~ctx:dummy_ctx in
   (* syntax sugar *)
   let (zero_mtz : mich_v cc) = gen_dummy_cc (MV_lit_mutez Bigint.zero) in
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
   gen_template igdt_map target_types ~target_mode:(`Asymm 3) ~f:(fun tvl ->
       match tvl with
       | [ (MT_mutez, v1); (MT_mutez, v2); (MT_mutez, v3); (MT_mutez, v4) ] ->
         if (not (equal_cc equal_mich_v v1 v4))
            && (not (equal_cc equal_mich_v v2 v4))
            && (not (equal_cc equal_mich_v v3 v4))
            && (not (equal_cc equal_mich_v v1 zero_mtz))
            && (not (equal_cc equal_mich_v v2 zero_mtz))
            && not (equal_cc equal_mich_v v3 zero_mtz)
         then make_add_3_eq_mtz (v1, v2, v3, v4)
         else None
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

let cvt_cand_pair : cand * cand -> cand_pair =
  (fun (cand1, cand2) -> { cp_start = cand1; cp_block = cand2 })
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
   |> List.map ~f:(fun rmci -> (rmci, gen_cand_by_fmla MF_true))
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

let find_inv_by_rmci : inv_map -> Tz.r_mich_cut_info -> cand =
  fun imap rmci ->
  RMCIMap.find imap rmci
  |> function
  | Some sss -> sss
  | None     ->
    InvError
      ("find_inv_map : " ^ Sexp.to_string (Tz.sexp_of_r_mich_cut_info rmci))
    |> raise
(* function find_inv_map end *)

let find_inv : inv_map -> Tz.mich_cut_info -> cand =
  (fun imap mci -> find_inv_by_rmci imap (TzUtil.get_reduced_mci mci))
(* function find_inv_map end *)

let update_inv_map : inv_map -> key:Tz.r_mich_cut_info -> value:cand -> inv_map
    =
  fun imap ~key:rmci ~value:cand ->
  RMCIMap.update imap rmci ~f:(function
  | Some inv -> join_cands inv cand
  | None     -> cand
  )
(* function update_inv_map end *)

let merge_inv_map : inv_map -> inv_map -> inv_map =
  fun imap1 imap2 ->
  RMCIMap.merge imap1 imap2 ~f:(fun ~key:_ opt ->
      match opt with
      | `Both (inv1, inv2) -> Some (join_cands inv1 inv2)
      | `Left inv1         -> Some inv1
      | `Right inv2        -> Some inv2
  )
(* function merge_inv_map end *)

let strengthen_inv_map : InvSet.t -> inv_map -> InvSet.t =
  (fun invset imap -> InvSet.map invset ~f:(merge_inv_map imap))
(* function strengthen_inv_map end *)

let check_contain_pair : inv_map -> mci_pair -> cand_pair -> bool =
  fun imap mcip candp ->
  let (inv_start : cand) = find_inv_by_rmci imap mcip.mp_start in
  let (inv_block : cand) = find_inv_by_rmci imap mcip.mp_block in
  let (inv_cp : cand_pair) = cvt_cand_pair (inv_start, inv_block) in
  equal_cand_pair candp inv_cp
(* function check_contain_pair end *)

(* Invariant Candidates *******************************************************)

let gen_initial_cand_map :
    is_cand_sat:(cand -> bool) ->
    do_cand_sat_istrg:(Tz.r_mich_cut_info -> cand -> bool) ->
    QIDSet.t ->
    Igdt.igdts_map ->
    cand_map =
  fun ~is_cand_sat ~do_cand_sat_istrg qset igdt_map ->
  let (default_score : (int * int) QIDMap.t) =
     QIDSet.to_list qset
     |> List.map ~f:(fun rmci -> (rmci, (0, 0)))
     |> QIDMap.of_alist_exn
  in
  RMCIMap.mapi igdt_map ~f:(fun ~key:rmci ~data:igdt_sets ->
      [ tmp_eq; tmp_ge; tmp_gt; tmp_add_2_eq; tmp_add_3_eq ]
      |> List.map ~f:(fun tmp -> tmp igdt_sets)
      |> CSet.union_list
      |> CSet.fold ~init:CMap.empty ~f:(fun acc_cmap cand ->
             if (not (is_cand_sat cand)) || not (do_cand_sat_istrg rmci cand)
             then acc_cmap
             else
               CMap.add acc_cmap ~key:cand ~data:(true, default_score)
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
  | None     -> CMap.empty
(* function cand_map_find end *)

let find_cand : cand_map -> Tz.mich_cut_info -> cands =
  (fun cmap mci -> find_cand_by_rmci cmap (TzUtil.get_reduced_mci mci))
(* function cand_map_find end *)

let mem_by_rmci : cand_map -> key:Tz.r_mich_cut_info -> value:cand -> bool =
  (fun cmap ~key ~value -> Map.mem (find_cand_by_rmci cmap key) value)
(* function mem_by_rmci end *)

let mem : cand_map -> key:Tz.mich_cut_info -> value:cand -> bool =
  fun cmap ~key ~value ->
  mem_by_rmci cmap ~key:(TzUtil.get_reduced_mci key) ~value
(* function mem end *)

let get_score_by_rmci :
    cand_map -> key:Tz.r_mich_cut_info -> value:cand -> qid:Tz.qid -> int =
  fun cmap ~key ~value ~qid ->
  CMap.find (find_cand_by_rmci cmap key) value
  |> function
  | Some (_, smap) -> QIDMap.find_exn smap qid |> fst
  | None           -> InvError "get_score : wrong value" |> raise
(* function get_score_by_rmci end *)

let get_score :
    cand_map -> key:Tz.mich_cut_info -> value:cand -> qid:Tz.qid -> int =
  fun cmap ~key ~value ~qid ->
  get_score_by_rmci cmap ~key:(TzUtil.get_reduced_mci key) ~value ~qid
(* function get_score end *)

let get_count_by_rmci :
    cand_map -> key:Tz.r_mich_cut_info -> value:cand -> qid:Tz.qid -> int =
  fun cmap ~key ~value ~qid ->
  CMap.find (find_cand_by_rmci cmap key) value
  |> function
  | Some (_, smap) -> QIDMap.find_exn smap qid |> snd
  | None           -> InvError "get_score : wrong value" |> raise
(* function get_count_by_rmci end *)

let find_ordered_cand_by_rmci :
    ?order:[ `Increasing | `Decreasing ] ->
    ?remove_unflaged:bool ->
    ?remove_not_precond:bool ->
    cand_map ->
    Tz.r_mich_cut_info ->
    Tz.qid ->
    cand list =
  fun ?(order = `Decreasing) ?(remove_unflaged = false)
      ?(remove_not_precond = false) cmap rmci qid ->
  find_cand_by_rmci cmap rmci
  |> (if remove_unflaged then CMap.filter ~f:fst else Fun.id)
  |> ( if remove_not_precond
     then CMap.filter ~f:(fun c -> QIDMap.find_exn (snd c) qid |> snd = 0)
     else Fun.id
     )
  |> CMap.to_alist
  |> List.sort ~compare:(fun (_, (_, smap1)) (_, (_, smap2)) ->
         compare_int
           (QIDMap.find_exn smap1 qid |> fst)
           (QIDMap.find_exn smap2 qid |> fst)
         |>
         match order with
         | `Increasing -> Fun.id
         | `Decreasing -> Int.neg
     )
  |> List.map ~f:fst
(* function find_ordered_cand_by_rmci end *)

let find_ordered_cand :
    ?order:[ `Increasing | `Decreasing ] ->
    ?remove_unflaged:bool ->
    ?remove_not_precond:bool ->
    cand_map ->
    Tz.mich_cut_info ->
    Tz.qid ->
    cand list =
  fun ?(order = `Decreasing) ?(remove_unflaged = false)
      ?(remove_not_precond = false) cmap mci qid ->
  find_ordered_cand_by_rmci ~order ~remove_unflaged ~remove_not_precond cmap
    (TzUtil.get_reduced_mci mci)
    qid
(* function find_ordered_cand end *)

let find_cand_top_k_by_rmci :
    ?order:[ `Increasing | `Decreasing ] ->
    ?remove_unflaged:bool ->
    ?remove_not_precond:bool ->
    top_k:int ->
    cand_map ->
    Tz.r_mich_cut_info ->
    Tz.qid ->
    cand list =
  fun ?(order = `Decreasing) ?(remove_unflaged = false)
      ?(remove_not_precond = false) ~top_k cmap rmci qid ->
  find_ordered_cand_by_rmci ~order ~remove_unflaged ~remove_not_precond cmap
    rmci qid
  |> (fun lst -> List.split_n lst top_k)
  |> fst
(* function finc_cand_map_top_k_by_tmci end *)

let find_cand_top_k :
    ?order:[ `Increasing | `Decreasing ] ->
    ?remove_unflaged:bool ->
    ?remove_not_precond:bool ->
    top_k:int ->
    cand_map ->
    Tz.mich_cut_info ->
    Tz.qid ->
    cand list =
  fun ?(order = `Decreasing) ?(remove_unflaged = false)
      ?(remove_not_precond = false) ~top_k cmap mci qid ->
  find_cand_top_k_by_rmci ~order ~remove_unflaged ~remove_not_precond ~top_k
    cmap
    (TzUtil.get_reduced_mci mci)
    qid
(* function find_cand_map_top_k end *)

let strengthen_cand_map :
    is_cand_sat:(cand -> bool) -> cand_map -> inv_map -> cand_map =
  fun ~is_cand_sat cmap imap ->
  RMCIMap.mapi cmap ~f:(fun ~key:rmci ~data:cset ->
      let (cur_inv : cand) = find_inv_by_rmci imap rmci in
      CMap.fold cset ~init:CMap.empty ~f:(fun ~key ~data:(f1, s1) new_cmap ->
          if equal_cand cur_inv key
          then new_cmap
          else (
            let (cand : cand) = join_cands cur_inv key in
            if not (is_cand_sat cand)
            then new_cmap
            else
              CMap.update new_cmap cand ~f:(function
              | None          -> (f1, s1)
              | Some (f2, s2) -> (f1 && f2, s2)
              )
          )
      )
  )
(* function strengthen_cand_map *)

let update_score_by_rmci :
    cand_map ->
    key:Tz.r_mich_cut_info ->
    value:cand ->
    qid:Tz.qid ->
    point:int * int ->
    cand_map =
  fun cmap ~key ~value ~qid ~point ->
  RMCIMap.update cmap key ~f:(function
  | Some cands ->
    CMap.update cands value ~f:(function
    | Some (flag, smap) ->
      ( flag,
        QIDMap.update smap qid ~f:(function
        | Some (prec, fail) -> (prec + fst point, fail + snd point)
        | None              -> point
        )
      )
    | None              -> InvError "update_score_by_rmci : wrong cand" |> raise
    )
  | None       -> InvError "update_score_by_rmci : wrong mci" |> raise
  )
(* function update_score_by_rmci end *)

let unflag_cand : cand_map -> key:Tz.r_mich_cut_info -> value:cand -> cand_map =
  fun cmap ~key ~value ->
  RMCIMap.update cmap key ~f:(function
  | Some cands ->
    CMap.update cands value ~f:(function
    | Some (_, score) -> (false, score)
    | None            -> InvError "score_cand : wrong cand" |> raise
    )
  | None       -> InvError "unflag_cand : wrong mci" |> raise
  )
(* function unflag_cand end *)

(* Failed Candidate Pair ******************************************************)

let gen_initial_inductive_info_map : SSet.t -> inductive_info =
  fun bsset ->
  SSet.fold bsset ~init:SIDMap.empty ~f:(fun iimap bs ->
      SIDMap.add_exn iimap ~key:bs.ss_id
        ~data:{ ir_valid = CPSet.empty; ir_invalid = CPSet.empty }
  )
(* function gen_initial_inductive_info_map end *)

let get_inductiveness_from_bs ?(ss_id_normalize = true) :
    inductive_info -> Tz.sym_state -> cp_inductiveness =
  fun iimap bs ->
  let ss_id = if ss_id_normalize then [ List.hd_exn bs.ss_id ] else bs.ss_id in
  SIDMap.find iimap ss_id
  |> function
  | Some cpi -> cpi
  | None     -> { ir_valid = CPSet.empty; ir_invalid = CPSet.empty }
(* function get_inductiveness_from_bs end *)

let count_each_cands : inductive_info -> Tz.sym_state -> cand -> int * int =
  fun iimap bs cand ->
  let (cpi : cp_inductiveness) =
     get_inductiveness_from_bs ~ss_id_normalize:true iimap bs
  in
  let (valid_cp : CPSet.t) =
     CPSet.filter cpi.ir_valid ~f:(fun cp -> equal_cand cp.cp_start cand)
  in
  let (invalid_cp : CPSet.t) =
     CPSet.filter cpi.ir_invalid ~f:(fun cp -> equal_cand cp.cp_start cand)
  in
  (CPSet.length valid_cp, CPSet.length invalid_cp)
(* function count_each_cands end *)

let add_inductiveness :
    inductive_info -> Tz.sym_state * cand_pair * bool -> inductive_info =
  fun iimap (bs, cp, inductiveness) ->
  SIDMap.update iimap bs.ss_id ~f:(fun cpi_opt ->
      let (cpi : cp_inductiveness) =
         Option.value cpi_opt
           ~default:{ ir_valid = CPSet.empty; ir_invalid = CPSet.empty }
      in
      let (ir_valid : CPSet.t) =
         if inductiveness then CPSet.add cpi.ir_valid cp else cpi.ir_valid
      in
      let (ir_invalid : CPSet.t) =
         if inductiveness then cpi.ir_invalid else CPSet.add cpi.ir_invalid cp
      in
      { ir_valid; ir_invalid }
  )
(* function add_inductiveness end *)

let get_inductive_info_by_mp : inductive_info -> SSet.t -> inductive_info_by_mp
    =
   let open Tz in
   let open TzUtil in
   fun iimap bsset ->
   SIDMap.fold iimap ~init:MPMap.empty ~f:(fun ~key ~data acc ->
       let (bs : sym_state) =
          find_sym_state_by_id bsset key
          |> function
          | Some bs -> bs
          | None    ->
            InvError "get_inductive_info_by_mp : wrong sym_state set" |> raise
       in
       let (mp : mci_pair) = cvt_mci_pair (bs.ss_start_mci, bs.ss_block_mci) in
       let (cur_cpmap : bool CPMap.t) =
          MPMap.find acc mp
          |> function
          | Some cpmap -> cpmap
          | None       -> CPMap.empty
       in
       let (valid_updated_cpmap : bool CPMap.t) =
          CPSet.fold data.ir_valid ~init:cur_cpmap
            ~f:(fun valid_updated_cpmap cp ->
              CPMap.update valid_updated_cpmap cp ~f:(function
              | Some b -> b
              | None   -> true
              )
          )
       in
       let (invalid_updated_cpmap : bool CPMap.t) =
          CPSet.fold data.ir_invalid ~init:valid_updated_cpmap
            ~f:(fun invalid_updated_cpmap cp ->
              CPMap.set invalid_updated_cpmap ~key:cp ~data:false
          )
       in
       MPMap.set acc ~key:mp ~data:invalid_updated_cpmap
   )
(* function get_inductive_info_by_mp end *)

let is_already_succeeded_by_rmci :
    inductive_info_by_mp -> mci_pair -> cand_pair -> bool =
  fun iimap' mp cp ->
  MPMap.find iimap' mp
  |> function
  | None       -> false
  | Some cpmap -> (
    CPMap.find cpmap cp
    |> function
    | Some b -> b
    | None   -> false
  )
(* function is_already_succeeded_by_rmci end *)

let is_already_succeeded :
    inductive_info_by_mp ->
    Tz.mich_cut_info * Tz.mich_cut_info ->
    cand * cand ->
    bool =
  fun iimap' mcip candp ->
  is_already_succeeded_by_rmci iimap' (cvt_mci_pair mcip) (cvt_cand_pair candp)
(* function is_already_succeeded end *)

let is_already_failed_by_rmci :
    inductive_info_by_mp -> mci_pair -> cand_pair -> bool =
  fun iimap' mp cp ->
  MPMap.find iimap' mp
  |> function
  | None       -> false
  | Some cpmap -> (
    CPMap.find cpmap cp
    |> function
    | Some b -> not b
    | None   -> false
  )
(* function is_already_failed_by_rmci end *)

let is_already_failed :
    inductive_info_by_mp ->
    Tz.mich_cut_info * Tz.mich_cut_info ->
    cand * cand ->
    bool =
  fun iimap' mcip candp ->
  is_already_failed_by_rmci iimap' (cvt_mci_pair mcip) (cvt_cand_pair candp)
(* function is_already_failed end *)
