(* Igdt : Invariant ingredient manager of Michelson program *)

exception IgdtError of string

open! Core

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(* Map of Tz.mich_t Tz.cc *)
module MTMap = Map.Make (Tz.MichTCC_cmp)

(* Map of Tz.r_mich_cut_info *)
module RMCIMap = Map.Make (Tz.RMichCutInfo_cmp)

(* Set of Tz.mich_v Tz.cc *)
module MVSet = Set.Make (Tz.MichVCC_cmp)

(* Set of Tz.sym_state *)
module SSet = Set.Make (Tz.SymState_cmp)

(******************************************************************************)
(******************************************************************************)
(* Ingredients                                                                *)
(******************************************************************************)
(******************************************************************************)

type igdt = {
  (* component information *)
  ig_value : Tz.mich_v Tz.cc;
  ig_typ : Tz.mich_t Tz.cc;
  (* accessor of component *)
  ig_precond_lst : Tz.mich_f list; [@ignore]
  (* stack status of component *)
  ig_base_value : Tz.mich_v Tz.cc; [@ignore]
}
[@@deriving sexp, compare, equal]

module IGDT_cmp = struct
  type t = igdt

  let compare = compare_igdt

  let t_of_sexp = igdt_of_sexp

  let sexp_of_t = sexp_of_igdt
end

(* Set of igdt *)
module ISet = Set.Make (IGDT_cmp)

type igdt_delim = {
  lit : ISet.t;
  non_lit : ISet.t;
}
[@@deriving sexp, compare, equal]

type igdt_sets = igdt_delim MTMap.t [@@deriving sexp, compare, equal]

type igdts_map = igdt_sets RMCIMap.t [@@deriving sexp, compare, equal]

(******************************************************************************)
(******************************************************************************)
(* Utility functions                                                          *)
(******************************************************************************)
(******************************************************************************)

let fold_precond_lst : igdt list -> Tz.mich_f =
   let open Tz in
   fun igdt_lst ->
   let (prec_lst : Tz.mich_f list) =
      List.map igdt_lst ~f:(fun igdt -> igdt.ig_precond_lst) |> List.join
   in
   if List.is_empty prec_lst then MF_true else MF_and prec_lst

let tmap_from_iset : ISet.t -> ISet.t MTMap.t =
   let open Tz in
   fun igdt_set ->
   ISet.group_by igdt_set ~equiv:(fun a b ->
       equal_cc equal_mich_t a.ig_typ b.ig_typ
   )
   |> List.map ~f:(fun p_igdt_set ->
          let (cur_typ : mich_t cc) = (ISet.choose_exn p_igdt_set).ig_typ in
          (cur_typ, p_igdt_set)
      )
   |> MTMap.of_alist
   |> function
   | `Ok mmm            -> mmm
   | `Duplicate_key ttt ->
     IgdtError
       ("tmap_from_iset : " ^ Sexp.to_string (sexp_of_cc sexp_of_mich_t ttt))
     |> raise
(* function tmap_from_iset end *)

let tmap_merge_with_delim :
    lit:ISet.t MTMap.t -> non_lit:ISet.t MTMap.t -> igdt_sets =
  fun ~lit ~non_lit ->
  MTMap.merge lit non_lit ~f:(fun ~key:_ opt ->
      match opt with
      | `Both (a', b') -> Some { lit = a'; non_lit = b' }
      | `Left a'       -> Some { lit = a'; non_lit = ISet.empty }
      | `Right b'      -> Some { lit = ISet.empty; non_lit = b' }
  )
(* function tmap_merge end *)

let find_igdt_sets : igdt_sets -> Tz.mich_t Tz.cc -> igdt_delim =
  fun igdt_map typ ->
  Map.find igdt_map typ
  |> function
  | Some ddd -> ddd
  | None     -> { lit = ISet.empty; non_lit = ISet.empty }

(******************************************************************************)
(******************************************************************************)
(* Ingredient Creation                                                        *)
(******************************************************************************)
(******************************************************************************)

let gen_custom_igdt : Tz.mich_v Tz.cc -> igdt =
  fun base_value ->
  {
    ig_value = base_value;
    ig_typ = Tz.typ_of_val base_value;
    ig_precond_lst = [];
    ig_base_value = base_value;
  }
(* function gen_igdt end *)

let collect_igdt_from_option : igdt -> ISet.t * ISet.t =
   let open Tz in
   fun cur_igdt ->
   let (cur_val : mich_v cc) = cur_igdt.ig_value in
   let (cur_typ : mich_t cc) = cur_igdt.ig_typ in
   let (cur_plst : mich_f list) = cur_igdt.ig_precond_lst in
   match cur_typ.cc_v with
   | MT_option t1cc ->
     (* preconditions *)
     let (prec_none : mich_f list) = MF_is_none cur_val :: cur_plst in
     let (prec_some : mich_f list) = MF_not (MF_is_none cur_val) :: cur_plst in
     (* values *)
     (* TODO: should be optimized *)
     let (val_unlifted : mich_v cc) =
        gen_custom_cc cur_val (MV_unlift_option cur_val)
     in
     let (val_none : mich_v cc) = gen_custom_cc cur_val (MV_none t1cc) in
     let (val_some : mich_v cc) =
        gen_custom_cc cur_val (MV_some val_unlifted)
     in
     (* ingredients *)
     let (igdt_unlifted : igdt) =
        {
          cur_igdt with
          ig_value = val_unlifted;
          ig_typ = t1cc;
          ig_precond_lst = prec_some;
        }
     in
     let (igdt_none : igdt) =
        { cur_igdt with ig_value = val_none; ig_precond_lst = prec_none }
     in
     let (igdt_some : igdt) =
        { cur_igdt with ig_value = val_some; ig_precond_lst = prec_some }
     in
     (ISet.of_list [ igdt_unlifted ], ISet.of_list [ igdt_none; igdt_some ])
   | _              -> IgdtError "collect_igdt_from_option : _" |> raise
(* function collect_igdt_from_option end *)

let collect_igdt_from_pair : igdt -> ISet.t * ISet.t =
   let open Tz in
   fun cur_igdt ->
   let (cur_val : mich_v cc) = cur_igdt.ig_value in
   let (cur_typ : mich_t cc) = cur_igdt.ig_typ in
   match cur_typ.cc_v with
   | MT_pair (t1cc, t2cc) ->
     (* values *)
     (* TODO: should be optimized *)
     let (val_fst : mich_v cc) = gen_custom_cc cur_val (MV_car cur_val) in
     let (val_snd : mich_v cc) = gen_custom_cc cur_val (MV_cdr cur_val) in
     let (val_pair : mich_v cc) =
        gen_custom_cc cur_val (MV_pair (val_fst, val_snd))
     in
     (* ingredients *)
     let (igdt_fst : igdt) =
        { cur_igdt with ig_value = val_fst; ig_typ = t1cc }
     in
     let (igdt_snd : igdt) =
        { cur_igdt with ig_value = val_snd; ig_typ = t2cc }
     in
     let (igdt_pair : igdt) = { cur_igdt with ig_value = val_pair } in
     (ISet.of_list [ igdt_fst; igdt_snd ], ISet.of_list [ igdt_pair ])
   | _                    -> IgdtError "collect_igdt_from_pair : _" |> raise
(* function collect_igdt_from_pair end *)

let collect_igdt_from_or : igdt -> ISet.t * ISet.t =
   let open Tz in
   fun cur_igdt ->
   let (cur_val : mich_v cc) = cur_igdt.ig_value in
   let (cur_typ : mich_t cc) = cur_igdt.ig_typ in
   let (cur_plst : mich_f list) = cur_igdt.ig_precond_lst in
   match cur_typ.cc_v with
   | MT_or (t1cc, t2cc) ->
     (* preconditions *)
     let (prec_left : mich_f list) = MF_is_left cur_val :: cur_plst in
     let (prec_right : mich_f list) = MF_not (MF_is_left cur_val) :: cur_plst in
     (* values *)
     (* TODO: should be optimized *)
     let (val_left_unlifted : mich_v cc) =
        gen_custom_cc cur_val (MV_unlift_left cur_val)
     in
     let (val_right_unlifted : mich_v cc) =
        gen_custom_cc cur_val (MV_unlift_right cur_val)
     in
     let (val_left : mich_v cc) =
        gen_custom_cc cur_val (MV_left (cur_typ, val_left_unlifted))
     in
     let (val_right : mich_v cc) =
        gen_custom_cc cur_val (MV_right (cur_typ, val_right_unlifted))
     in
     (* ingredients *)
     let (igdt_left_unlifted : igdt) =
        {
          cur_igdt with
          ig_value = val_left_unlifted;
          ig_typ = t1cc;
          ig_precond_lst = prec_left;
        }
     in
     let (igdt_right_unlifted : igdt) =
        {
          cur_igdt with
          ig_value = val_right_unlifted;
          ig_typ = t2cc;
          ig_precond_lst = prec_right;
        }
     in
     let (igdt_left : igdt) =
        { cur_igdt with ig_value = val_left; ig_precond_lst = prec_left }
     in
     let (igdt_right : igdt) =
        { cur_igdt with ig_value = val_right; ig_precond_lst = prec_right }
     in
     ( ISet.of_list [ igdt_left_unlifted; igdt_right_unlifted ],
       ISet.of_list [ igdt_left; igdt_right ]
     )
   | _                  -> IgdtError "collect_igdt_from_or : _" |> raise
(* function collect_igdt_from_or end *)

let collect_igdt_from_list : igdt -> ISet.t * ISet.t =
   let open Tz in
   fun cur_igdt ->
   let (cur_val : mich_v cc) = cur_igdt.ig_value in
   let (cur_typ : mich_t cc) = cur_igdt.ig_typ in
   let (cur_plst : mich_f list) = cur_igdt.ig_precond_lst in
   match cur_typ.cc_v with
   | MT_list t1cc ->
     let (sigma_set : ISet.t) =
        match t1cc.cc_v with
        | MT_pair (t11cc, t12cc) -> (
          match (t11cc.cc_v, t12cc.cc_v) with
          | (MT_timestamp, MT_mutez) ->
            (* preconditions *)
            let (prec_cons : mich_f list) = MF_is_cons cur_val :: cur_plst in
            let (prec_nil : mich_f list) =
               MF_not (MF_is_cons cur_val) :: cur_plst
            in
            (* values *)
            let (val_cons : mich_v cc) =
               gen_custom_cc cur_val (MV_sigma_tmplm cur_val)
            in
            let (val_nil : mich_v cc) =
               gen_custom_cc cur_val (MV_lit_mutez Bigint.zero)
            in
            (* type *)
            let (typ_sigma : mich_t cc) = gen_custom_cc cur_val MT_mutez in
            (* ingredients *)
            let (igdt_cons : igdt) =
               {
                 cur_igdt with
                 ig_value = val_cons;
                 ig_typ = typ_sigma;
                 ig_precond_lst = prec_cons;
               }
            in
            let (igdt_nil : igdt) =
               {
                 cur_igdt with
                 ig_value = val_nil;
                 ig_typ = typ_sigma;
                 ig_precond_lst = prec_nil;
               }
            in
            ISet.of_list [ igdt_cons; igdt_nil ]
          | _                        -> ISet.empty
        )
        | _                      -> ISet.empty
     in
     (ISet.empty, ISet.add sigma_set cur_igdt)
   | _            -> IgdtError "collect_igdt_from_list : _" |> raise
(* function collect_igdt_from_list end *)

let collect_igdt_from_mutez : igdt -> ISet.t * ISet.t =
   let open Tz in
   fun cur_igdt ->
   let (cur_val : mich_v cc) = cur_igdt.ig_value in
   let (cur_typ : mich_t cc) = cur_igdt.ig_typ in
   let (cur_plst : mich_f list) = cur_igdt.ig_precond_lst in
   match cur_typ.cc_v with
   | MT_mutez ->
     (* preconditions *)
     let (prec_mutez : mich_f list) = MF_mutez_bound cur_val :: cur_plst in
     (* ingredients *)
     let (igdt_mutez : igdt) = { cur_igdt with ig_precond_lst = prec_mutez } in
     (ISet.empty, ISet.singleton igdt_mutez)
   | _        -> IgdtError "collect_igdt_from_mutez : _" |> raise
(* function collect_igdt_from_mutez end *)

let collect_igdt_from_nat : igdt -> ISet.t * ISet.t =
   let open Tz in
   fun cur_igdt ->
   let (cur_val : mich_v cc) = cur_igdt.ig_value in
   let (cur_typ : mich_t cc) = cur_igdt.ig_typ in
   let (cur_plst : mich_f list) = cur_igdt.ig_precond_lst in
   match cur_typ.cc_v with
   | MT_nat ->
     (* preconditions *)
     let (prec_nat : mich_f list) = MF_nat_bound cur_val :: cur_plst in
     (* ingredients *)
     let (igdt_nat : igdt) = { cur_igdt with ig_precond_lst = prec_nat } in
     (ISet.empty, ISet.singleton igdt_nat)
   | _      -> IgdtError "collect_igdt_from_nat : _" |> raise
(* function collect_igdt_from_nat end *)

let collect_igdt_from_igdt : igdt -> ISet.t =
   let open Tz in
   let rec collect_from_igdt_i : ISet.t -> igdt -> ISet.t =
     fun acc_set cur_igdt ->
     let (target_set, cur_igdt_set) : ISet.t * ISet.t =
        match cur_igdt.ig_typ.cc_v with
        (* Container Types *)
        | MT_option _ -> collect_igdt_from_option cur_igdt
        | MT_pair _ -> collect_igdt_from_pair cur_igdt
        | MT_or _ -> collect_igdt_from_or cur_igdt
        (* Add Sigma Representation of Each Types *)
        | MT_list _ -> collect_igdt_from_list cur_igdt
        (* Add Special Precondition of Each Types *)
        | MT_mutez -> collect_igdt_from_mutez cur_igdt
        | MT_nat -> collect_igdt_from_nat cur_igdt
        (* Other Terminal Types *)
        | _ -> (ISet.empty, ISet.singleton cur_igdt)
     in
     ISet.fold target_set
       ~init:(ISet.union acc_set cur_igdt_set)
       ~f:collect_from_igdt_i
   in
   (* inner-function collect_from_igdt_i end *)
   (fun base_igdt -> collect_from_igdt_i ISet.empty base_igdt)
(* function collect_igdt_from_igdt end *)

let collect_igdt_from_mich_v : Tz.mich_v Tz.cc -> ISet.t =
  fun base_value ->
  let (base_igdt : igdt) = gen_custom_igdt base_value in
  collect_igdt_from_igdt base_igdt
(* function collect_igdt_from_mich_v end *)

(******************************************************************************)
(******************************************************************************)
(* Ingredient Collection                                                      *)
(******************************************************************************)
(******************************************************************************)

let igdt_from_mich_stack : Tz.mich_cut_info -> Tz.mich_v Tz.cc list -> ISet.t =
   let open Tz in
   fun start_mci v_stack ->
   let (len : int) = List.length v_stack - 1 in
   List.mapi v_stack ~f:(fun cur_loc cur_val ->
       match (start_mci.mci_cutcat, cur_loc) with
       | (MCC_trx_entry, l) when l = len ->
         collect_igdt_from_mich_v (gen_custom_cc cur_val (MV_cdr cur_val))
       | (MCC_ln_loopleft, l)
       | (MCC_lb_loopleft, l)
         when l = len ->
         ISet.empty
       | (MCC_ln_map, l) when l = len ->
         let (cur_typ : mich_t cc) = typ_of_val cur_val in
         collect_igdt_from_mich_v
           (gen_custom_cc cur_val (MV_inv_symbol (cur_typ, MIC_map_exit_cont)))
       | (MCC_lb_map, l)
       | (MCC_lb_iter, l)
         when l = len ->
         ISet.empty
       | (MCC_ln_loopleft, l)
       | (MCC_ln_map, l)
       | (MCC_lb_loopleft, l)
       | (MCC_lb_map, l)
       | (MCC_lb_iter, l)
         when 0 <= l && l < len ->
         collect_igdt_from_mich_v cur_val
       | (MCC_ln_loop, l)
       | (MCC_ln_iter, l)
       | (MCC_lb_loop, l)
         when 0 <= l && l <= len ->
         collect_igdt_from_mich_v cur_val
       | (MCC_query _, _) ->
         IgdtError "igdt_from_mich_stack : MCC_trx_*, MCC_query" |> raise
       | _ ->
         IgdtError
           ("igdt_from_mich_stack : "
           ^ (start_mci |> sexp_of_mich_cut_info |> Sexp.to_string)
           ^ " : "
           ^ string_of_int cur_loc
           )
         |> raise
   )
   |> ISet.union_list
(* function igdt_from_mich_stack end *)

let igdt_from_dip_stack : Tz.mich_cut_info -> Tz.mich_v Tz.cc list -> ISet.t =
  fun start_mci v_stack ->
  List.map v_stack ~f:(fun cur_val ->
      match start_mci.mci_cutcat with
      | MCC_trx_entry
      | MCC_trx_exit
      | MCC_query _ ->
        IgdtError "igdt_from_dip_stack : MCC_trx_*, MCC_query" |> raise
      | _ -> collect_igdt_from_mich_v cur_val
  )
  |> ISet.union_list
(* function igdt_from_dip_stack end *)

let igdt_from_map_entry_stack :
    Tz.mich_cut_info -> Tz.mich_v Tz.cc list -> ISet.t =
   let open Tz in
   fun start_mci v_stack ->
   let (len : int) = List.length v_stack - 1 in
   List.mapi v_stack ~f:(fun cur_loc cur_val ->
       match (start_mci.mci_cutcat, cur_loc) with
       | (MCC_trx_entry, l)
       | (MCC_trx_exit, l)
       | (MCC_query _, l)
         when 0 <= l && l <= len ->
         IgdtError "igdt_from_map_entry_stack : MCC_trx_*, MCC_query" |> raise
       | (MCC_lb_map, l) when l = len ->
         let (cur_typ : mich_t cc) = typ_of_val cur_val in
         collect_igdt_from_mich_v
           (gen_custom_cc cur_val (MV_inv_symbol (cur_typ, MIC_map_entry_cont)))
       | (_, l) when 0 <= l && l <= len -> collect_igdt_from_mich_v cur_val
       | _ ->
         IgdtError
           ("igdt_from_map_entry_stack : "
           ^ (start_mci |> sexp_of_mich_cut_info |> Sexp.to_string)
           ^ " : "
           ^ string_of_int cur_loc
           )
         |> raise
   )
   |> ISet.union_list
(* function igdt_from_map_entry_stack end *)

let igdt_from_map_exit_stack :
    Tz.mich_cut_info -> Tz.mich_v Tz.cc list -> ISet.t =
   let open Tz in
   fun start_mci v_stack ->
   let (len : int) = List.length v_stack - 1 in
   List.mapi v_stack ~f:(fun cur_loc cur_val ->
       match (start_mci.mci_cutcat, cur_loc) with
       | (MCC_trx_entry, l)
       | (MCC_trx_exit, l)
       | (MCC_query _, l)
         when 0 <= l && l <= len ->
         IgdtError "igdt_from_map_exit_stack : MCC_trx_*, MCC_query" |> raise
       | (_, l) when 0 <= l && l <= len -> collect_igdt_from_mich_v cur_val
       | _ ->
         IgdtError
           ("igdt_from_map_exit_stack : "
           ^ (start_mci |> sexp_of_mich_cut_info |> Sexp.to_string)
           ^ " : "
           ^ string_of_int cur_loc
           )
         |> raise
   )
   |> ISet.union_list
(* function igdt_from_map_exit_stack end *)

let igdt_from_iter_stack : Tz.mich_cut_info -> Tz.mich_v Tz.cc list -> ISet.t =
   let open Tz in
   fun start_mci v_stack ->
   let (len : int) = List.length v_stack - 1 in
   List.mapi v_stack ~f:(fun cur_loc cur_val ->
       match (start_mci.mci_cutcat, cur_loc) with
       | (MCC_trx_entry, l)
       | (MCC_trx_exit, l)
       | (MCC_query _, l)
         when 0 <= l && l <= len ->
         IgdtError "igdt_from_map_entry_stack : MCC_trx_*, MCC_query" |> raise
       | (MCC_lb_iter, l) when l = len ->
         let (cur_typ : mich_t cc) = typ_of_val cur_val in
         collect_igdt_from_mich_v
           (gen_custom_cc cur_val (MV_inv_symbol (cur_typ, MIC_iter_cont)))
       | (_, l) when 0 <= l && l <= len -> collect_igdt_from_mich_v cur_val
       | _ ->
         IgdtError
           ("igdt_from_map_entry_stack : "
           ^ (start_mci |> sexp_of_mich_cut_info |> Sexp.to_string)
           ^ " : "
           ^ string_of_int cur_loc
           )
         |> raise
   )
   |> ISet.union_list
(* function igdt_from_map_entry_stack end *)

let igdt_from_balances : Tz.mich_v Tz.cc * Tz.mich_v Tz.cc -> ISet.t =
   let open Tz in
   fun (v_balance, v_bc_balance) ->
   let (t_balance : mich_t cc) = typ_of_val v_balance in
   let (t_bc_balance : mich_t cc) = typ_of_val v_bc_balance in
   match (t_balance.cc_v, t_bc_balance.cc_v) with
   | (MT_mutez, MT_mutez) ->
     let (igdt_balance : ISet.t) =
        collect_igdt_from_mich_v
          (gen_custom_cc v_balance (MV_inv_symbol (t_balance, MIC_balance)))
     in
     let (igdt_bc_balance : ISet.t) =
        collect_igdt_from_mich_v
          (gen_custom_cc v_bc_balance
             (MV_inv_symbol (t_bc_balance, MIC_bc_balance))
          )
     in
     ISet.union igdt_balance igdt_bc_balance
   | _                    -> IgdtError "igdt_from_balances : _" |> raise

let igdt_from_sym_state : Tz.sym_state -> ISet.t =
   let open Tz in
   fun sstate ->
   let ({ ss_start_mci = start_mci; ss_start_si = start_si; _ } : sym_state) =
      sstate
   in
   let (mich_set : ISet.t) = igdt_from_mich_stack start_mci start_si.si_mich in
   let (dip_set : ISet.t) = igdt_from_dip_stack start_mci start_si.si_dip in
   let (map_entry_set : ISet.t) =
      igdt_from_map_entry_stack start_mci start_si.si_map_entry
   in
   let (map_exit_set : ISet.t) =
      igdt_from_map_exit_stack start_mci start_si.si_map_exit
   in
   let (iter_set : ISet.t) = igdt_from_iter_stack start_mci start_si.si_iter in
   let (balance_set : ISet.t) =
      igdt_from_balances (start_si.si_balance, start_si.si_bc_balance)
   in
   ISet.union_list
     [ mich_set; dip_set; map_entry_set; map_exit_set; iter_set; balance_set ]
(* function igdt_from_sym_state end *)

(******************************************************************************)
(******************************************************************************)
(* Ingredient Map                                                             *)
(******************************************************************************)
(******************************************************************************)

let get_igdts_map : SSet.t -> Tz.mich_v Tz.cc -> MVSet.t -> igdts_map =
   let open Tz in
   fun blocked_sset init_strg lit_set ->
   let (init_strg_igdt_set : ISet.t) = collect_igdt_from_mich_v init_strg in
   let (lit_igdt_set : ISet.t) =
      ISet.union_list
        (MVSet.to_list lit_set |> List.map ~f:collect_igdt_from_mich_v)
   in
   let (rmci_igdt_map : igdts_map) =
      SSet.group_by blocked_sset ~equiv:(fun a b ->
          equal_r_mich_cut_info
            (get_reduced_mci a.ss_start_mci)
            (get_reduced_mci b.ss_start_mci)
      )
      |> List.map ~f:(fun p_blocked_sset ->
             let (p_blocked_slst : Tz.sym_state list) =
                SSet.to_list p_blocked_sset
             in
             let (cur_rmci : r_mich_cut_info) =
                get_reduced_mci (List.hd_exn p_blocked_slst).ss_start_mci
             in
             let (non_lit_igdt_set : ISet.t) =
                ISet.union_list (List.map p_blocked_slst ~f:igdt_from_sym_state)
             in
             let (lit_igdt_set : ISet.t) =
                ISet.union_list [ init_strg_igdt_set; lit_igdt_set ]
             in
             let (non_lit_igdt_tmap : ISet.t MTMap.t) =
                tmap_from_iset non_lit_igdt_set
             in
             let (lit_igdt_tmap : ISet.t MTMap.t) =
                tmap_from_iset lit_igdt_set
             in
             let (merged_tmap : igdt_sets) =
                tmap_merge_with_delim ~lit:lit_igdt_tmap
                  ~non_lit:non_lit_igdt_tmap
             in
             (cur_rmci, merged_tmap)
         )
      |> RMCIMap.of_alist
      |> function
      | `Ok mmm            -> mmm
      | `Duplicate_key rrr ->
        IgdtError
          ("get_igdt_map : " ^ Sexp.to_string (sexp_of_r_mich_cut_info rrr))
        |> raise
   in
   rmci_igdt_map
(* function get_igdt_map end *)
