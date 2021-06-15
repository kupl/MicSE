(* Component of value *)

exception Error of string


(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

module TComparable = struct
  exception Error of string

  module T = struct
    type t = Tz.mich_t Tz.cc
    let compare : t -> t -> int
    = fun x y -> Stdlib.compare (TzCvt.T2Nnocc.cv_mtcc x) (TzCvt.T2Nnocc.cv_mtcc y)
    let sexp_of_t : t -> Core.Sexp.t
    = TzCvt.T2CS.cv_mtcc

    let t_of_sexp : Core.Sexp.t -> t
    = TzCvt.CS2Tnocc.cv_mtcc
  end

  include T
  include Core.Comparable.Make(T)
end

module CTMap = TComparable.Map


(******************************************************************************)
(******************************************************************************)
(* Component Collector                                                        *)
(******************************************************************************)
(******************************************************************************)

(*****************************************************************************
  The type t is information from each component of the Tz.mich_v.
  Each component is extracted from the given Tz.mich_v.
*****************************************************************************)
type t = {
  cp_typ          : Tz.mich_t Tz.cc;        (* type of component *)
  cp_loc          : int;                    (* location of component in stack *)
  cp_base_var     : Tz.mich_v Tz.cc option; (* base variable expression *)
  cp_precond_lst  : Tz.mich_f list;         (* precondition list of component *)
  cp_value        : Tz.mich_v Tz.cc;        (* value expression of component *)
}

let base_comp_from_v : ?loc:int -> Tz.mich_v Tz.cc -> t
= let open Tz in
  (* function base_comp_from_f start *)
  fun ?(loc=0) v -> begin
  { cp_typ=(v |> typ_of_val);
    cp_loc=loc;
    cp_base_var=None;
    cp_precond_lst=[];
    cp_value=v}
end (* function base_comp_from_v end *)

let base_comp_from_mci : (Tz.mich_cut_info) -> (Tz.mich_t Tz.cc) list -> (t option) list
= let module CList = Core.List in
  (* function base_comp_from_mci start *)
  fun entry_mci tstack -> begin
  CList.mapi tstack
    ~f:(fun cur_loc cur_typ -> (
      match entry_mci.mci_cutcat, cur_loc with
      | MCC_trx_entry   , 0 -> (
        match cur_typ.cc_v with
        | MT_pair (_, strg_typ) -> (
          let bvar : Tz.mich_v Tz.cc = Tz.gen_custom_cc strg_typ (Tz.MV_symbol (strg_typ, (Jc.Locvn.for_strg))) in
          Some ({ cp_typ=strg_typ; cp_loc=cur_loc; cp_base_var=Some bvar; cp_precond_lst=[]; cp_value=bvar; }))
        | _ -> Error ("base_comp_from_mci : MCC_trx_entry : 0 : " ^ (cur_typ |> TzCvt.T2Jnocc.cv_mtcc |> Yojson.Safe.to_string)) |> Stdlib.raise)
      | MCC_ln_loopleft , 0
      | MCC_ln_map      , 0
      | MCC_lb_loopleft , 0
      | MCC_lb_map      , 0
      | MCC_lb_iter     , 0 -> None
      | MCC_ln_loopleft , _
      | MCC_ln_map      , _
      | MCC_lb_loopleft , _
      | MCC_lb_map      , _
      | MCC_lb_iter     , _ -> (
        let cur_loc' : int = cur_loc - 1 in
        let bvar : Tz.mich_v Tz.cc = Tz.gen_custom_cc cur_typ (Tz.MV_symbol (cur_typ, (Jc.Locvn.to_string {loc=cur_loc'; acc_l=[]}))) in
        Some ({ cp_typ=cur_typ; cp_loc=cur_loc'; cp_base_var=Some bvar; cp_precond_lst=[]; cp_value=bvar; }))
      | MCC_ln_loop     , _
      | MCC_ln_iter     , _
      | MCC_lb_loop     , _ -> (
        let bvar : Tz.mich_v Tz.cc = Tz.gen_custom_cc cur_typ (Tz.MV_symbol (cur_typ, (Jc.Locvn.to_string {loc=cur_loc; acc_l=[]}))) in
        Some ({ cp_typ=cur_typ; cp_loc=cur_loc; cp_base_var=Some bvar; cp_precond_lst=[]; cp_value=bvar; }))
      | _ -> Error ("base_comp_from_mci : " ^ (entry_mci |> TzCvt.T2Jnocc.cv_mich_cut_info |> Yojson.Safe.to_string) ^ " : " ^ (string_of_int cur_loc)) |> Stdlib.raise))
end (* function base_comp_from_mci end *)

let collect : t -> (t Core.Set.Poly.t) CTMap.t -> (t Core.Set.Poly.t) CTMap.t
= let module CList = Core.List in
  let module CPSet = Core.Set.Poly in
  let module CPMap = Core.Map.Poly in
  let tmap_update k v tmap = CTMap.update tmap k ~f:(function None -> CPSet.singleton v | Some sss -> CPSet.add sss v) in
  let rec collect_i : t -> (t CPSet.t) CTMap.t -> (t CPSet.t) CTMap.t
  = let open Tz in
    (* function collect_i start *)
    fun cur_comp acc -> begin
    let { cp_typ=cur_typ; cp_loc=_; cp_base_var=_; cp_precond_lst=cur_prec_lst; cp_value=cur_val; } : t = cur_comp in
    match cur_typ.cc_v with
    | MT_option t1cc -> (
      let precond_none : mich_f list = (MF_is_none cur_val) :: cur_prec_lst in
      let precond_some : mich_f list = (MF_not (MF_is_none cur_val)) :: cur_prec_lst in
      let value_unlifted : mich_v cc = gen_custom_cc cur_val (MV_unlift_option cur_val) |> Tz.optimize_v in
      let comp_none : t = { cur_comp with cp_precond_lst=precond_none; cp_value=(gen_custom_cc cur_val (MV_none t1cc)); } in
      let comp_some : t = { cur_comp with cp_precond_lst=precond_some; cp_value=(gen_custom_cc cur_val (MV_some value_unlifted)); } in
      let comp_some_unlifted : t = { cur_comp with cp_typ=t1cc; cp_precond_lst=precond_some; cp_value=value_unlifted; } in
      let acc' : (t CPSet.t) CTMap.t = 
        CTMap.update
          acc
          cur_typ
          ~f:(function
              | None -> CPSet.of_list [comp_none; comp_some;]
              | Some cc -> CPSet.add (CPSet.add cc comp_none) comp_some) in
      collect_i comp_some_unlifted acc')
    | MT_pair (t1cc, t2cc) -> (
      let comp_pair : t = cur_comp in
      let comp_fst : t = { cur_comp with cp_typ=t1cc; cp_value=(gen_custom_cc cur_val (MV_car cur_val) |> Tz.optimize_v); } in
      let comp_snd : t = { cur_comp with cp_typ=t2cc; cp_value=(gen_custom_cc cur_val (MV_cdr cur_val) |> Tz.optimize_v); } in
      let acc' : (t CPSet.t) CTMap.t =
        CTMap.update
          acc
          cur_typ
          ~f:(function None -> CPSet.singleton comp_pair | Some cc -> CPSet.add cc comp_pair) in
      let acc_fst : (t CPSet.t) CTMap.t = collect_i comp_fst acc' in
      collect_i comp_snd acc_fst)
    | MT_or (t1cc, t2cc) -> (
      let precond_left : mich_f list = (MF_is_left cur_val) :: cur_prec_lst in
      let precond_right : mich_f list = (MF_not (MF_is_left cur_val)) :: cur_prec_lst in
      let value_left_unlifted : mich_v cc = gen_custom_cc cur_val (MV_unlift_left cur_val) |> Tz.optimize_v in
      let value_right_unlifted : mich_v cc = gen_custom_cc cur_val (MV_unlift_right cur_val) |> Tz.optimize_v in
      let comp_left : t = { cur_comp with cp_precond_lst=precond_left; cp_value=(gen_custom_cc cur_val (MV_left (cur_typ, value_left_unlifted))); } in
      let comp_right : t = { cur_comp with cp_precond_lst=precond_right; cp_value=(gen_custom_cc cur_val (MV_right (cur_typ, value_right_unlifted))); } in
      let comp_left_unlifted : t = { cur_comp with cp_typ=t1cc; cp_precond_lst=precond_left; cp_value=value_left_unlifted; } in
      let comp_right_unlifted : t = { cur_comp with cp_typ=t2cc; cp_precond_lst=precond_right; cp_value=value_right_unlifted; } in
      let acc' : (t CPSet.t) CTMap.t =
        CTMap.update
          acc
          cur_typ
          ~f:(function
              | None -> CPSet.of_list [comp_left; comp_right;]
              | Some cc -> CPSet.add (CPSet.add cc comp_left) comp_right) in
      let acc_left : (t CPSet.t) CTMap.t =
        collect_i comp_left_unlifted acc' in
      collect_i comp_right_unlifted acc_left)
    | MT_list t1cc -> (
      let acc' = (
        match t1cc.cc_v with
        | MT_pair (t11cc, t12cc) -> (
          match (t11cc.cc_v, t12cc.cc_v) with
          | (MT_timestamp, MT_mutez) -> (
            tmap_update (MT_mutez |> gen_dummy_cc) ({ cur_comp with cp_value=(gen_custom_cc cur_val (MV_sigma_tmplm cur_val)); }) acc)
          | _ -> acc)
        | _ -> acc) in
      CTMap.update acc' cur_typ ~f:(function None -> CPSet.singleton cur_comp | Some cc -> CPSet.add cc cur_comp))
    | _ -> CTMap.update acc cur_typ ~f:(function None -> CPSet.singleton cur_comp | Some cc -> CPSet.add cc cur_comp)
  end in (* function collect_i end *)
  (* function collect start *)
  fun base_comp base_ctmap -> begin
  collect_i base_comp base_ctmap
end (* function collect end *)

let merge : (t Core.Set.Poly.t) CTMap.t -> (t Core.Set.Poly.t) CTMap.t -> (t Core.Set.Poly.t) CTMap.t
= let module CPSet = Core.Set.Poly in
  (* function merge start *)
  fun a b -> begin
  CTMap.merge a b
    ~f:(fun ~key opt -> (
          let _ = key in
          match opt with
          | `Both (a', b') -> Some (CPSet.union a' b')
          | `Left a' -> Some a'
          | `Right b' -> Some b'))
end (* function merge end *)


(******************************************************************************)
(******************************************************************************)
(* Utils                                                                      *)
(******************************************************************************)
(******************************************************************************)

let fold_precond : t list -> Tz.mich_f
= let module CList = Core.List in
  (* function fold_precond start *)
  fun comp_lst -> begin
  Tz.MF_and (MF_true::(comp_lst
            |> CList.map ~f:(fun c -> c.cp_precond_lst)
            |> CList.join))
  (* function fold_precond end *)
end