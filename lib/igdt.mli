(* Igdt : Invariant ingredient manager of Michelson program *)

exception IgdtError of string

(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

(* Map of Tz.mich_f Tz.cc *)
module MTMap : module type of Core.Map.Make (Tz.MichTCC_cmp)

(* Map of Tz.r_mich_cut_info *)
module RMCIMap : module type of Core.Map.Make (Tz.RMichCutInfo_cmp)

(* Set of Tz.mich_v Tz.cc *)
module MVSet : module type of Core.Set.Make (Tz.MichVCC_cmp)

(* Set of Tz.mich_f *)
module MFSet : module type of Core.Set.Make (Tz.MichF_cmp)

(* Set of Tz.sym_state & Set of Tz.mich_cut_info *)
module SSet : module type of Core.Set.Make (Tz.SymState_cmp)

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
  ig_precond_lst : Tz.mich_f list;
  (* stack status of component *)
  ig_base_value : Tz.mich_v Tz.cc;
}
[@@deriving sexp, compare, equal]

module IGDT_cmp : sig
  type t = igdt [@@deriving compare, sexp]
end

(* Set of igdt *)
module ISet : module type of Core.Set.Make (IGDT_cmp)

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

val fold_precond_lst : igdt list -> MFSet.t

val tmap_from_iset : ISet.t -> ISet.t MTMap.t

val tmap_merge_with_delim :
  lit:ISet.t MTMap.t -> non_lit:ISet.t MTMap.t -> igdt_sets

val find_igdt_sets : igdt_sets -> Tz.mich_t Tz.cc -> igdt_delim

(******************************************************************************)
(******************************************************************************)
(* Ingredient Creation                                                        *)
(******************************************************************************)
(******************************************************************************)

val gen_custom_igdt : Tz.mich_v Tz.cc -> igdt

val collect_igdt_from_option : igdt -> ISet.t * ISet.t

val collect_igdt_from_pair : igdt -> ISet.t * ISet.t

val collect_igdt_from_or : igdt -> ISet.t * ISet.t

val collect_igdt_from_list : igdt -> ISet.t * ISet.t

val collect_igdt_from_igdt : igdt -> ISet.t

val collect_igdt_from_mich_v : Tz.mich_v Tz.cc -> ISet.t

(******************************************************************************)
(******************************************************************************)
(* Ingredient Collection                                                      *)
(******************************************************************************)
(******************************************************************************)

val igdt_from_mich_stack : Tz.mich_cut_info -> Tz.mich_v Tz.cc list -> ISet.t

val igdt_from_dip_stack : Tz.mich_cut_info -> Tz.mich_v Tz.cc list -> ISet.t

val igdt_from_map_entry_stack :
  Tz.mich_cut_info -> Tz.mich_v Tz.cc list -> ISet.t

val igdt_from_map_exit_stack :
  Tz.mich_cut_info -> Tz.mich_v Tz.cc list -> ISet.t

val igdt_from_iter_stack : Tz.mich_cut_info -> Tz.mich_v Tz.cc list -> ISet.t

val igdt_from_balances : Tz.mich_v Tz.cc * Tz.mich_v Tz.cc -> ISet.t

val igdt_from_sym_state : Tz.sym_state -> ISet.t

(******************************************************************************)
(******************************************************************************)
(* Ingredient Map                                                             *)
(******************************************************************************)
(******************************************************************************)

val get_igdts_map : SSet.t -> Tz.mich_v Tz.cc -> MVSet.t -> igdts_map
