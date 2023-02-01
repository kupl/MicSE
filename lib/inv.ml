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

(* igdt_combination [{1; 2;}; {a; b;}; ...] === {[1; a; ...]; [1; b; ...]; [2; a; ...]; [2; b; ...];} *)
let igdt_combination : ISet.t list -> ILSet.t =
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
(* function igdt_combination end *)

(* igdt_combination_self {a; b;} 2 === {[a; a;]; [a; b;]; [b; a;]; [b; b;];} *)
let igdt_combination_self : ISet.t -> size:int -> ILSet.t =
  fun iset ~size ->
  let (set_lst : ISet.t list) = List.init size ~f:(fun _ -> iset) in
  igdt_combination set_lst
(* function igdt_combination_self end *)

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
           let (shuffled : ILSet.t) = shuffle shuffle_i in
           let (new_removed : ILSet.t) =
              ILSet.map shuffled ~f:(fun il -> il @ normal_i)
           in
           let (new_il : ILSet.t) =
              if ILSet.length shuffled > 1
              then ILSet.singleton il
              else ILSet.empty
           in
           (ILSet.union acc_removed new_removed, ILSet.union acc_il new_il)
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
  { c_fmla = MFSet.of_list [ MF_true; fmla ]; c_cond = cond; c_igdt = igdt }
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

let cand_combination : CSet.t list -> CSet.t =
  fun cset_lst ->
  if List.for_all cset_lst ~f:(fun cset -> not (CSet.is_empty cset))
  then
    List.fold cset_lst ~init:CSet.empty ~f:(fun acc_cset cset ->
        if CSet.is_empty acc_cset
        then cset
        else
          CSet.to_list cset
          |> List.map ~f:(fun c -> CSet.map acc_cset ~f:(join_cands c))
          |> CSet.union_list
    )
  else CSet.empty

(******************************************************************************)
(* Invariant Candidate Templates                                              *)
(******************************************************************************)

let dummy_ctx : Tz.mich_sym_ctxt = []

let gen_template :
    ?except_lit_only:bool ->
    ?target_mode:[ `Normal | `Asymm     of int | `Asymm_rfl ] ->
    f:((Tz.mich_t * Tz.mich_v Tz.cc) list -> (Tz.mich_f * MFSet.t) option) ->
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
            igdt_combination
              (List.map targets ~f:(fun (_, _, a_set, _) -> a_set))
          else
            List.mapi targets ~f:(fun idx (_, n_set, _, _) ->
                let ( (targets_for_lit :
                        (ISet.t * ISet.t * ISet.t * mich_t cc) list
                        ),
                      (targets_for_all :
                        (ISet.t * ISet.t * ISet.t * mich_t cc) list
                        )
                    ) =
                   List.split_n targets idx
                   |> (fun (fst_lst, snd_lst) -> (fst_lst, List.tl_exn snd_lst))
                in
                let (l_set_lst : ISet.t list) =
                   List.map targets_for_lit ~f:(fun (l_set, _, _, _) -> l_set)
                in
                let (a_set_lst : ISet.t list) =
                   List.map targets_for_all ~f:(fun (_, _, a_set, _) -> a_set)
                in
                igdt_combination (List.join [ l_set_lst; [ n_set ]; a_set_lst ])
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
           | None                  -> acc
           | Some (fmla, prec_set) ->
             CSet.add acc
               (gen_cand_by_fmla fmla
                  ~cond:(MFSet.union prec_set (fold_precond_lst c_lst))
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
         then Some (MF_eq (gctx v1, gctx v2), MFSet.empty)
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
   let (zero_int : mich_v cc) = gen_dummy_cc (MV_lit_int Bigint.zero) in
   let (zero_mtz : mich_v cc) = gen_dummy_cc (MV_lit_mutez Bigint.zero) in
   let (zero_nat : mich_v cc) = gen_dummy_cc (MV_lit_nat Bigint.zero) in
   let equal_mich_v_cc = equal_cc equal_mich_v in
   let make_ge_by_number : mich_v cc * mich_v cc -> (mich_f * MFSet.t) option =
     fun (v1, v2) ->
     Some (MF_is_true (gctx (gen_dummy_cc (MV_geq_ib (v1, v2)))), MFSet.empty)
   in
   let make_ge_by_string : mich_v cc * mich_v cc -> (mich_f * MFSet.t) option =
     fun (v1, v2) ->
     let (cmp : mich_v cc) = gen_dummy_cc (MV_compare (v1, v2)) in
     Some
       ( MF_is_true (gctx (gen_dummy_cc (MV_geq_ib (cmp, zero_int)))),
         MFSet.empty
       )
   in
   fun igdt_map ->
   let (target_types : Tz.mich_t Tz.cc list list) =
      List.map
        [
          [ MT_int; MT_int ];
          [ MT_nat; MT_nat ];
          [ MT_mutez; MT_mutez ];
          [ MT_string; MT_string ];
          [ MT_mutez; MT_nat ];
        ] ~f:(fun tl -> List.map tl ~f:(fun t -> gen_dummy_cc t)
      )
   in
   gen_template igdt_map target_types ~target_mode:`Asymm_rfl ~f:(fun tvl ->
       match tvl with
       | [ (MT_int, v1); (MT_int, v2) ] -> make_ge_by_number (v1, v2)
       | [ (MT_mutez, v1); (MT_nat, v2) ] ->
         if not (equal_mich_v_cc v2 zero_nat)
         then make_ge_by_number (v1, v2)
         else None
       | [ (MT_mutez, v1); (MT_mutez, v2) ] ->
         if (not (equal_mich_v_cc v1 max_mtz))
            && not (equal_mich_v_cc v2 zero_mtz)
         then make_ge_by_number (v1, v2)
         else None
       | [ (MT_nat, v1); (MT_nat, v2) ] ->
         if not (equal_mich_v_cc v2 zero_nat)
         then make_ge_by_number (v1, v2)
         else None
       | [ (MT_string, v1); (MT_string, v2) ] ->
         if not (equal_mich_v_cc v2 empty_str)
         then make_ge_by_string (v1, v2)
         else None
       | [ (_, _); (_, _) ] -> None
       | _ -> InvError "tmp_ge : wrong ingredient length" |> raise
   )
(* function tmp_ge end *)

let tmp_or : Igdt.igdt_sets -> CSet.t =
  let open Tz in
  let open TzUtil in
  let gctx = gen_mich_v_ctx ~ctx:dummy_ctx in
  (* syntax sugar *)
  let make_or_le : mich_v cc * mich_v cc -> (mich_f * MFSet.t) option =
    fun (v1, v2) ->
    let (unlift_left : mich_v cc) = gen_dummy_cc (MV_unlift_left v1) in
    let (unlift_right : mich_v cc) = gen_dummy_cc (MV_unlift_right v1) in
    Some (
      MF_and [
        MF_imply (
          MF_is_left (gctx v1),
          MF_is_true (gctx (gen_dummy_cc (MV_leq_ib (unlift_left, v2))))
        );
        MF_imply (
          MF_not (MF_is_left (gctx v1)),
          MF_is_true (gctx (gen_dummy_cc (MV_leq_ib (unlift_right, v2))))
        )
      ],
      MFSet.empty
    )
  in
  fun igdt_map ->
  let (target_types : Tz.mich_t Tz.cc list list) =
    MTMap.keys igdt_map
    |> List.map ~f:(fun t ->
      match t.cc_v with
      | MT_or (t1, t2) when equal_mich_t t1.cc_v t2.cc_v ->
        if equal_mich_t t1.cc_v MT_int then [ t; t1 ]
        else []
      | _ -> [])
  in
  gen_template igdt_map target_types ~target_mode:`Asymm_rfl ~f:(fun tvl ->
      match tvl with
      | [ (MT_or (_, _), v1); (MT_int, v2) ] ->
       make_or_le (v1, v2)
      | [ (_, _); (_, _) ] -> None
      | _ -> InvError "tmp_ge : wrong ingredient length" |> raise
  )
(* function tmp_or end *)

let tmp_gt : Igdt.igdt_sets -> CSet.t =
   let open Tz in
   let open TzUtil in
   let gctx = gen_mich_v_ctx ~ctx:dummy_ctx in
   (* syntax sugar *)
   let (zero_int : mich_v cc) = gen_dummy_cc (MV_lit_int Bigint.zero) in
   let make_gt_by_number : mich_v cc * mich_v cc -> (mich_f * MFSet.t) option =
     fun (v1, v2) ->
     Some (MF_is_true (gctx (gen_dummy_cc (MV_gt_ib (v1, v2)))), MFSet.empty)
   in
   let make_ge_by_string : mich_v cc * mich_v cc -> (mich_f * MFSet.t) option =
     fun (v1, v2) ->
     let (cmp : mich_v cc) = gen_dummy_cc (MV_compare (v1, v2)) in
     Some
       ( MF_is_true (gctx (gen_dummy_cc (MV_geq_ib (cmp, zero_int)))),
         MFSet.empty
       )
   in
   fun igdt_map ->
   let (target_types : Tz.mich_t Tz.cc list list) =
      List.map [ MT_int; MT_nat; MT_mutez; MT_string ] ~f:(fun t ->
          List.init 2 ~f:(fun _ -> gen_dummy_cc t)
      )
   in
   gen_template igdt_map target_types ~target_mode:`Asymm_rfl ~f:(fun tvl ->
       match tvl with
       | [ (MT_int, v1); (MT_int, v2) ] -> make_gt_by_number (v1, v2)
       | [ (MT_nat, v1); (MT_nat, v2) ] -> make_gt_by_number (v1, v2)
       | [ (MT_mutez, v1); (MT_mutez, v2) ] -> make_gt_by_number (v1, v2)
       | [ (MT_string, v1); (MT_string, v2) ] -> make_ge_by_string (v1, v2)
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
   let make_add_2_eq_mtz :
       mich_v cc * mich_v cc * mich_v cc -> (mich_f * MFSet.t) option =
     fun (v1, v2, v3) ->
     let (add : mich_v cc) = gen_dummy_cc (MV_add_mmm (v1, v2)) in
     Some (MF_eq (gctx add, gctx v3), MFSet.empty)
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
   let (zero_nat : mich_v cc) = gen_dummy_cc (MV_lit_nat Bigint.zero) in
   let make_add_3_eq_mnnm :
       mich_v cc * mich_v cc * mich_v cc * mich_v cc ->
       (mich_f * MFSet.t) option =
     fun (v1, v2, v3, v4) ->
     let (add : mich_v cc) =
        gen_dummy_cc (MV_add_nnn (gen_dummy_cc (MV_add_mnn (v1, v2)), v3))
     in
     Some
       (MF_eq (gctx add, gctx (gen_dummy_cc (MV_mtz_of_nat_mn v4))), MFSet.empty)
   in
   fun igdt_map ->
   let (target_types : Tz.mich_t Tz.cc list list) =
      [ [ MT_mutez; MT_nat; MT_nat; MT_mutez ] ]
      |> List.map ~f:(fun tl -> List.map tl ~f:(fun t -> gen_dummy_cc t))
   in
   gen_template igdt_map target_types ~target_mode:(`Asymm 3) ~f:(fun tvl ->
       match tvl with
       | [ (MT_mutez, v1); (MT_nat, v2); (MT_nat, v3); (MT_mutez, v4) ] ->
         if (not (equal_cc equal_mich_v v1 v4))
            && (not (equal_cc equal_mich_v v1 zero_mtz))
            && (not (equal_cc equal_mich_v v2 zero_nat))
            && not (equal_cc equal_mich_v v3 zero_nat)
         then make_add_3_eq_mnnm (v1, v2, v3, v4)
         else None
       | [ (_, _); (_, _); (_, _); (_, _) ] -> None
       | _ -> InvError "tmp_add_3_eq : wrong ingredient length" |> raise
   )
(* function tmp_add_3_eq end *)

let tmp_add_2_ge : Igdt.igdt_sets -> CSet.t =
   let open Tz in
   let open TzUtil in
   let gctx = gen_mich_v_ctx ~ctx:dummy_ctx in
   (* syntax sugar *)
   let (zero_mtz : mich_v cc) = gen_dummy_cc (MV_lit_mutez Bigint.zero) in
   let make_add_2_ge_mtz :
       mich_v cc * mich_v cc * mich_v cc -> (mich_f * MFSet.t) option =
     fun (v1, v2, v3) ->
     let (add : mich_v cc) = gen_dummy_cc (MV_add_mmm (v1, v2)) in
     Some (MF_is_true (gctx (gen_dummy_cc (MV_geq_ib (add, v3)))), MFSet.empty)
   in
   let make_ge_add_2_mtz :
       mich_v cc * mich_v cc * mich_v cc -> (mich_f * MFSet.t) option =
     fun (v1, v2, v3) ->
     let (add : mich_v cc) = gen_dummy_cc (MV_add_mmm (v2, v3)) in
     Some (MF_is_true (gctx (gen_dummy_cc (MV_geq_ib (v1, add)))), MFSet.empty)
   in
   fun igdt_map ->
   let (target_types : Tz.mich_t Tz.cc list list) =
      List.map [ MT_mutez ] ~f:(fun t ->
          List.init 3 ~f:(fun _ -> gen_dummy_cc t)
      )
   in
   let (cset_add_2_ge : CSet.t) =
      gen_template igdt_map target_types ~target_mode:(`Asymm 2) ~f:(fun tvl ->
          match tvl with
          | [ (MT_mutez, v1); (MT_mutez, v2); (MT_mutez, v3) ] ->
            if (not (equal_cc equal_mich_v v1 v3))
               && (not (equal_cc equal_mich_v v2 v3))
               && (not (equal_cc equal_mich_v v1 zero_mtz))
               && (not (equal_cc equal_mich_v v2 zero_mtz))
               && not (equal_cc equal_mich_v v3 zero_mtz)
            then make_add_2_ge_mtz (v1, v2, v3)
            else None
          | [ (_, _); (_, _); (_, _) ] -> None
          | _ -> InvError "tmp_add_2_ge : wrong ingredient length" |> raise
      )
   in
   let (cset_ge_add_2 : CSet.t) =
      gen_template igdt_map target_types ~target_mode:`Normal ~f:(fun tvl ->
          match tvl with
          | [ (MT_mutez, v1); (MT_mutez, v2); (MT_mutez, v3) ] ->
            if (not (equal_cc equal_mich_v v1 v2))
               && (not (equal_cc equal_mich_v v1 v3))
               && (not (equal_cc equal_mich_v v1 zero_mtz))
               && (not (equal_cc equal_mich_v v2 zero_mtz))
               && not (equal_cc equal_mich_v v3 zero_mtz)
            then make_ge_add_2_mtz (v1, v2, v3)
            else None
          | [ (_, _); (_, _); (_, _) ] -> None
          | _ -> InvError "tmp_add_2_ge : wrong ingredient length" |> raise
      )
   in
   CSet.union cset_add_2_ge cset_ge_add_2
(* function tmp_add_2_ge end *)

let tmp_all_elem_eq : Igdt.igdt_sets -> CSet.t =
   let open Tz in
   let open TzUtil in
   let gctx = gen_mich_v_ctx ~ctx:dummy_ctx in
   (* syntax sugar *)
   fun igdt_map ->
   let (target_types : Tz.mich_t Tz.cc list list) =
      MTMap.keys igdt_map
      |> List.map ~f:(fun t ->
             match t.cc_v with
             | MT_map (_, vtcc)
             | MT_big_map (_, vtcc) ->
               [ t; vtcc ]
             | _ -> []
         )
      |> List.filter ~f:(fun l -> not (List.is_empty l))
   in
   gen_template igdt_map target_types ~f:(fun tvl ->
       match tvl with
       | [ (_, v1); (_, v2) ] ->
         Some (MF_all_element_equal_to (gctx v1, gctx v2), MFSet.empty)
       | _                    ->
         InvError "tmp_all_elem_eq : wrong ingredient length" |> raise
   )
(* function tmp_all_elem_eq end *)

let tmp_eq_and_all_elem_eq : Igdt.igdt_sets -> CSet.t =
   (* This template is what KT1CSfR needs. *)
   let open Tz in
   let open TzUtil in
   let gctx = gen_mich_v_ctx ~ctx:dummy_ctx in
   (* syntax sugar *)
   fun igdt_map ->
   let (target_type_pair : (Tz.mich_t Tz.cc list * Tz.mich_t Tz.cc list) list) =
      MTMap.keys igdt_map
      |> List.map ~f:(fun t ->
             match t.cc_v with
             | MT_map (_, vtcc)
             | MT_big_map (_, vtcc) ->
               ([ t; t ], [ t; vtcc ])
             | _ -> ([], [])
         )
      |> List.filter ~f:(fun (l1, l2) ->
             not (List.is_empty l1 || List.is_empty l2)
         )
   in
   let ( (target_types_1 : Tz.mich_t Tz.cc list list),
         (target_types_2 : Tz.mich_t Tz.cc list list)
       ) =
      List.unzip target_type_pair
   in
   let (cands_1 : CSet.t) =
      gen_template igdt_map target_types_1 ~target_mode:(`Asymm 2)
        ~f:(fun tvl ->
          match tvl with
          | [ (_, v1); (_, v2) ] ->
            if not (equal_cc equal_mich_v v1 v2)
            then Some (MF_eq (gctx v1, gctx v2), MFSet.empty)
            else None
          | _                    ->
            InvError "tmp_eq_and_all_elem_eq : wrong ingredient length" |> raise
      )
   in
   let (cands_2 : CSet.t) =
      gen_template igdt_map target_types_2 ~f:(fun tvl ->
          match tvl with
          | [ (_, v3); (_, v4) ] ->
            Some (MF_all_element_equal_to (gctx v3, gctx v4), MFSet.empty)
          | _                    ->
            InvError "tmp_eq_and_all_elem_eq : wrong ingredient length" |> raise
      )
   in
   cand_combination [ cands_1; cands_2 ]
(* function tmp_eq_and_all_elem_eq end *)

(******************************************************************************)
(* Invariants & Invariant Candidates                                          *)
(******************************************************************************)

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

let check_contain_pair : inv_map -> Tz.sym_state -> cand_pair -> bool =
  fun imap ss candp ->
  let (inv_start : cand) = find_inv imap ss.ss_start_mci in
  let (inv_block : cand) = find_inv imap ss.ss_block_mci in
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
  let _ = Utils.Log.debug (fun m -> m "Inv.gen_initial_cand_map start") in
  let (default_score : (int * int) QIDMap.t) =
     QIDSet.to_list qset
     |> List.map ~f:(fun rmci -> (rmci, (0, 0)))
     |> QIDMap.of_alist_exn
  in
  let res = RMCIMap.mapi igdt_map ~f:(fun ~key:rmci ~data:igdt_sets ->
      [
        tmp_eq;
        tmp_ge;
        tmp_or;
        tmp_gt;
        tmp_add_2_eq;
        tmp_add_3_eq;
        (* tmp_add_2_ge;
        tmp_all_elem_eq;
        tmp_eq_and_all_elem_eq; *)
      ]
      |> List.map ~f:(fun tmp -> tmp igdt_sets)
      |> CSet.union_list
      |> (fun cset ->
        let _ = Utils.Log.debug (fun m ->
          m "Inv.gen_initial_cand_map: cset length:\n%s\n\t> # of raw candidates: %d"
            (Tz.sexp_of_r_mich_cut_info rmci |> SexpUtil.to_string)
            (CSet.length cset)
        ) in
        cset)
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
    ) in
  let _ = Utils.Log.debug (fun m -> m "Inv.gen_initial_cand_map end") in
  res
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
    is_cand_sat:(cand -> bool) ->
    do_cand_sat_istrg:(Tz.r_mich_cut_info -> cand -> bool) ->
    cand_map ->
    inv_map ->
    cand_map =
  fun ~is_cand_sat ~do_cand_sat_istrg cmap imap ->
  RMCIMap.mapi cmap ~f:(fun ~key:rmci ~data:cset ->
      let (cur_inv : cand) = find_inv_by_rmci imap rmci in
      CMap.fold cset ~init:CMap.empty ~f:(fun ~key ~data:(f1, s1) new_cmap ->
          if equal_cand cur_inv key
          then new_cmap
          else (
            let (cand : cand) = join_cands cur_inv key in
            if (not (is_cand_sat cand)) || not (do_cand_sat_istrg rmci cand)
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

let count_each_cands :
    inductive_info -> Tz.sym_state -> CSet.t * CSet.t -> int * int =
  fun iimap bs (start_prec_set, block_prec_set) ->
  let (cpi : cp_inductiveness) =
     get_inductiveness_from_bs ~ss_id_normalize:true iimap bs
  in
  let check_cp : cand_pair -> bool =
    fun cp ->
    CSet.mem start_prec_set cp.cp_start && CSet.mem block_prec_set cp.cp_block
  in
  let (valid_cp : CPSet.t) = CPSet.filter cpi.ir_valid ~f:check_cp in
  let (invalid_cp : CPSet.t) = CPSet.filter cpi.ir_invalid ~f:check_cp in
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

let is_already_succeeded : inductive_info -> Tz.sym_state -> cand_pair -> bool =
  fun idtmap ss cp ->
  get_inductiveness_from_bs ~ss_id_normalize:true idtmap ss
  |> (fun { ir_valid; _ } -> CPSet.mem ir_valid cp)
(* function is_already_succeeded end *)

let is_already_failed : inductive_info -> Tz.sym_state -> cand_pair -> bool =
  fun idtmap ss cp ->
  get_inductiveness_from_bs ~ss_id_normalize:true idtmap ss
  |> (fun { ir_invalid; _ } -> CPSet.mem ir_invalid cp)
(* function is_already_failed end *)
