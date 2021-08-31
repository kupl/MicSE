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

(* Set of set of Tz.mich_f *)
module MFSSet = Set.Make (MFSet)

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
(* Invariants & Invariant Candidates                                          *)
(******************************************************************************)
(******************************************************************************)

type inv_map = MFSet.t RMCIMap.t [@@deriving sexp, compare, equal]

type cand_map = MFSSet.t RMCIMap.t [@@deriving sexp, compare, equal]

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
  List.fold_right set_lst
    ~f:(fun i_set acc_l_set ->
      ISet.to_list i_set
      |> List.map ~f:(fun i -> ILSet.map acc_l_set ~f:(List.cons i))
      |> ILSet.union_list)
    ~init:ILSet.empty
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
          ( if except_lit_only
          then
            combination (List.map targets ~f:(fun (_, _, all_set, _) -> all_set))
          else
            List.mapi targets ~f:(fun idx (n_set, _, _, _) ->
                let ((l_set_lst : ISet.t list), (a_set_lst : ISet.t list)) =
                   List.split_n targets idx
                   |> fun (fst_lst, snd_lst) ->
                   ( List.map fst_lst ~f:(fun (_, l_set, _, _) -> l_set),
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
  fun igdt_map ->
  let (target_types : Tz.mich_t Tz.cc list list) =
     MTMap.keys igdt_map |> List.map ~f:(fun t -> List.init 2 ~f:(fun _ -> t))
  in
  gen_template igdt_map target_types ~target_mode:`Asymm ~f:(fun tvl ->
      match tvl with
      | [ (_, v1); (_, v2) ] -> Some (MF_eq (v1, v2))
      | _                    ->
        InvError "tmp_eq : wrong ingredient length" |> raise
  )
(* function tmp_eq end *)

let tmp_ge : Igdt.igdt_sets -> MFSet.t =
   let open Tz in
   let open TzUtil in
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
     Some (MF_is_true (gen_dummy_cc (MV_geq_ib (cmp, zero))))
   in
   fun igdt_map ->
   let (target_types : Tz.mich_t Tz.cc list list) =
      List.map [ MT_int; MT_nat; MT_mutez; MT_string ] ~f:(fun t ->
          List.init 2 ~f:(fun _ -> gen_dummy_cc t)
      )
   in
   gen_template igdt_map target_types ~f:(fun tvl ->
       match tvl with
       | [ (MT_mutez, v1); (MT_mutez, v2) ] ->
         if equal_mich_v_cc v1 max_mtz || equal_mich_v_cc v2 zero_mtz
         then None
         else make_ge (v1, v2)
       | [ (MT_nat, v1); (MT_nat, v2) ] ->
         if equal_mich_v_cc v2 zero_nat then None else make_ge (v1, v2)
       | [ (MT_string, v1); (MT_string, v2) ] ->
         if equal_mich_v_cc v2 empty_str then None else make_ge (v1, v2)
       | [ (_, v1); (_, v2) ] -> make_ge (v1, v2)
       | _ -> InvError "tmp_ge : wrong ingredient length" |> raise
   )
(* function tmp_ge end *)

let tmp_gt : Igdt.igdt_sets -> MFSet.t =
   let open Tz in
   let open TzUtil in
   let (zero : mich_v cc) = gen_dummy_cc (MV_lit_int Bigint.zero) in
   let make_gt : mich_v cc * mich_v cc -> mich_f option =
     fun (v1, v2) ->
     let (cmp : mich_v cc) = gen_dummy_cc (MV_compare (v1, v2)) in
     Some (MF_is_true (gen_dummy_cc (MV_gt_ib (cmp, zero))))
   in
   fun igdt_map ->
   let (target_types : Tz.mich_t Tz.cc list list) =
      List.map [ MT_int; MT_nat; MT_mutez; MT_string ] ~f:(fun t ->
          List.init 2 ~f:(fun _ -> gen_dummy_cc t)
      )
   in
   gen_template igdt_map target_types ~f:(fun tvl ->
       match tvl with
       | [ (_, v1); (_, v2) ] -> make_gt (v1, v2)
       | _                    ->
         InvError "tmp_gt : wrong ingredient length" |> raise
   )
(* function tmp_gt end *)

let tmp_add_2_eq : Igdt.igdt_sets -> MFSet.t =
   let open Tz in
   let open TzUtil in
   let make_add_2_eq_mtz : mich_v cc * mich_v cc * mich_v cc -> mich_f option =
     fun (v1, v2, v3) ->
     let (add : mich_v cc) = gen_dummy_cc (MV_add_mmm (v1, v2)) in
     Some (MF_eq (add, v3))
   in
   fun igdt_map ->
   let (target_types : Tz.mich_t Tz.cc list list) =
      List.map [ MT_mutez ] ~f:(fun t ->
          List.init 3 ~f:(fun _ -> gen_dummy_cc t)
      )
   in
   gen_template igdt_map target_types ~f:(fun tvl ->
       match tvl with
       | [ (MT_mutez, v1); (MT_mutez, v2); (MT_mutez, v3) ] ->
         make_add_2_eq_mtz (v1, v2, v3)
       | _ -> InvError "tmp_add_2_eq : wrong ingredient length" |> raise
   )
(* function tmp_add_2_eq end *)

let tmp_add_3_eq : Igdt.igdt_sets -> MFSet.t =
   let open Tz in
   let open TzUtil in
   let make_add_3_eq_mtz :
       mich_v cc * mich_v cc * mich_v cc * mich_v cc -> mich_f option =
     fun (v1, v2, v3, v4) ->
     let (add : mich_v cc) =
        gen_dummy_cc (MV_add_mmm (gen_dummy_cc (MV_add_mmm (v1, v2)), v3))
     in
     Some (MF_eq (add, v4))
   in
   fun igdt_map ->
   let (target_types : Tz.mich_t Tz.cc list list) =
      List.map [ MT_mutez ] ~f:(fun t ->
          List.init 4 ~f:(fun _ -> gen_dummy_cc t)
      )
   in
   gen_template igdt_map target_types ~f:(fun tvl ->
       match tvl with
       | [ (MT_mutez, v1); (MT_mutez, v2); (MT_mutez, v3); (MT_mutez, v4) ] ->
         make_add_3_eq_mtz (v1, v2, v3, v4)
       | _ -> InvError "tmp_add_3_eq : wrong ingredient length" |> raise
   )
(* function tmp_add_3_eq end *)

(******************************************************************************)
(* Invariants & Invariant Candidates                                          *)
(******************************************************************************)

let gen_initial_cand_map :
    Se.se_result -> Tz.mich_v Tz.cc -> MVSet.t -> cand_map =
  fun se_res init_strg lit_set ->
  let (igdt_map : Igdt.igdts_map) =
     Igdt.get_igdts_map se_res.Se.sr_blocked init_strg lit_set
  in
  RMCIMap.map igdt_map ~f:(fun igdt_sets ->
      [ tmp_eq; tmp_ge; tmp_gt; tmp_add_2_eq; tmp_add_3_eq ]
      |> List.map ~f:(fun tmp -> tmp igdt_sets)
      |> MFSet.union_list
      |> MFSSet.map ~f:MFSet.singleton
  )
(* function gen_initial_cand_map end *)

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

let find_cand_map_by_rmci : cand_map -> Tz.r_mich_cut_info -> MFSSet.t =
  fun cmap rmci ->
  RMCIMap.find cmap rmci
  |> function
  | Some sss -> sss
  | None     -> MFSSet.empty
(* function cand_map_find end *)

let find_cand_map : cand_map -> Tz.mich_cut_info -> MFSSet.t =
  (fun cmap mci -> find_cand_map_by_rmci cmap (TzUtil.get_reduced_mci mci))
(* function cand_map_find end *)

let find_inv_map_by_rmci : inv_map -> Tz.r_mich_cut_info -> MFSet.t =
  fun imap rmci ->
  RMCIMap.find imap rmci
  |> function
  | Some sss -> sss
  | None     ->
    InvError
      ("find_inv_map : " ^ Sexp.to_string (Tz.sexp_of_r_mich_cut_info rmci))
    |> raise
(* function find_inv_map end *)

let find_inv_map : inv_map -> Tz.mich_cut_info -> MFSet.t =
  (fun imap mci -> find_inv_map_by_rmci imap (TzUtil.get_reduced_mci mci))
(* function find_inv_map end *)

let update_inv_map :
    inv_map -> key:Tz.r_mich_cut_info -> value:MFSet.t -> inv_map =
  fun imap ~key:rmci ~value:fset ->
  RMCIMap.update imap rmci ~f:(function
  | Some inv -> MFSet.union inv fset
  | None     -> MFSet.empty
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

let strengthen_cand_map : cand_map -> inv_map -> cand_map =
  fun cmap imap ->
  RMCIMap.mapi cmap ~f:(fun ~key:rmci ~data:cset ->
      let (cur_inv : MFSet.t) = find_inv_map_by_rmci imap rmci in
      MFSSet.map cset ~f:(MFSet.union cur_inv)
  )
(* function strengthen_cand_map *)

let strengthen_inv_map : inv_map list -> inv_map =
  fun imap_lst ->
  List.fold imap_lst ~init:RMCIMap.empty ~f:(fun acc_imap imap ->
      merge_inv_map acc_imap imap
  )
(* function strengthen_inv_map end *)
