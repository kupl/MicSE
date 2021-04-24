(* Invariant Synthesizer *)

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
    = TzCvt.T2CSnocc.cv_mtcc

    let t_of_sexp : Core.Sexp.t -> t
    = TzCvt.CS2Tnocc.cv_mtcc
  end
  include T
  include Core.Comparable.Make(T)
end

module CTMap = TComparable.Map


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
    The type component is information from each component of the symbolic stack.
    Each component is extracted from the given symbolic stack.
    The type of component is statically baked from the type stack.
  *****************************************************************************)
type component = {
  cp_typ          : Tz.mich_t Tz.cc;      (* type of component *)
  cp_loc          : int;                  (* location of component in stack *)
  cp_base_var     : Tz.mich_v Tz.cc;      (* base variable expression *)
  cp_precond_lst  : Tz.mich_f list;       (* precondition list of component *)
  cp_value        : Tz.mich_v Tz.cc;      (* value expression of component *)
}

  (*****************************************************************************
    The type comp_map is a pre-baked component map.
    Function bake_comp_map makes a set of components from the type stack of each MCI.
    The component set which is the value of comp_map is used to make a set of new invariants by recipe.
    type comp_map = MCI |-> (mich_t |-> component set)
  *****************************************************************************)
type comp_map = (Tz.mich_cut_info, (component Core.Set.Poly.t) CTMap.t) Core.Map.Poly.t


let bake_comp_map : Se.state_set -> comp_map
= let module CList = Core.List in
  let module CPSet = Core.Set.Poly in
  let module CPMap = Core.Map.Poly in
  let get_type_stack : vstack CPSet.t -> tstack option
  = let tstack_equal : tstack -> tstack -> bool
    = (* function tstack_equal start *)
      fun ts1 ts2 -> begin
      CList.fold2
        ts1
        ts2
        ~init:true
        ~f:(fun c t1 t2 -> if t1.cc_v <> t2.cc_v then false else c)
      |> function
          | Ok cc -> cc
          | Unequal_lengths -> false
    end in (* function tstack_equal end *)
    (* function get_type_stack start *)
    fun vsset -> begin
    let vs : vstack = 
      CPSet.choose vsset
      |> function 
          | Some ss -> ss
          | None -> Error "bake_comp_map : get_type_stack : vs" |> Stdlib.raise in
    let ts : tstack = vs |> Tz.extract_typ_stack in
    CPSet.fold
      (CPSet.remove vsset vs)
      ~init:(Some ts)
      ~f:(fun ts_opt vs -> (
            if Option.is_none ts_opt then None
            else 
              let ts = Option.get ts_opt in
              if tstack_equal ts (vs |> Tz.extract_typ_stack) then ts_opt
              else None))
  end in (* function get_type_stack end *)
  let create_base_comp : Tz.mich_t Tz.cc -> (Tz.mich_cut_info * int) -> component option
  = let open Tz in
    (* function create_base_comp start *)
    fun cur_typ (cur_mci, cur_loc) -> begin
    match (cur_mci.mci_cutcat, cur_loc) with
    | MCC_ln_loopleft, 0
    | MCC_ln_map, 0
    | MCC_lb_loopleft, 0 
    | MCC_lb_map, 0
    | MCC_lb_iter, 0 -> None
    | MCC_trx_entry, 0 -> begin
      match cur_typ.cc_v with
      | MT_pair (_, strg_typ) -> begin
        let bvar : mich_v cc = Tz.make_base_var cur_loc cur_typ in
        Some { cp_typ=strg_typ;
               cp_loc=cur_loc;
               cp_base_var=bvar;
               cp_precond_lst=[];
               cp_value=(gen_custom_cc bvar (MV_cdr bvar)) }
        end
      | _ -> (Error "bake_comp_map : create_base_comp : MCC_trx_entry, 0 : cur_typ" |> Stdlib.raise)
      end
    | _ -> begin
      let bvar : mich_v cc = Tz.make_base_var cur_loc cur_typ in
      Some { cp_typ=cur_typ;
             cp_loc=cur_loc;
             cp_base_var=bvar;
             cp_precond_lst=[];
             cp_value=bvar }
    end
  end in (* function create_base_comp end *)
  let rec collect_components : component -> (component Core.Set.Poly.t) CTMap.t -> (component Core.Set.Poly.t) CTMap.t
  = let open Tz in
    (* function collect_components start *)
    fun cur_comp acc -> begin
    let { cp_typ=cur_typ;
          cp_loc=_;
          cp_base_var=_;
          cp_precond_lst=cur_prec_lst;
          cp_value=cur_val; } : component = 
      cur_comp in
    match cur_typ.cc_v with
    | MT_option t1cc -> begin
      let precond_none : mich_f list = (MF_is_none cur_val) :: cur_prec_lst in
      let precond_some : mich_f list = (MF_not (MF_is_none cur_val)) :: cur_prec_lst in
      let value_unlifted : mich_v cc = gen_custom_cc cur_val (MV_unlift_option cur_val) in
      let comp_none : component =
        { cur_comp with cp_precond_lst=precond_none; cp_value=(gen_custom_cc cur_val (MV_none t1cc)); } in
      let comp_some : component =
        { cur_comp with cp_precond_lst=precond_some; cp_value=(gen_custom_cc cur_val (MV_some value_unlifted)); } in
      let comp_some_unlifted : component =
        { cur_comp with cp_typ=t1cc; cp_precond_lst=precond_some; cp_value=value_unlifted; } in
      let acc' : (component Core.Set.Poly.t) CTMap.t = 
        CTMap.update
          acc
          cur_typ
          ~f:(function
              | None -> CPSet.of_list [comp_none; comp_some;]
              | Some cc -> CPSet.add (CPSet.add cc comp_none) comp_some) in
      collect_components comp_some_unlifted acc'
      end
    | MT_pair (t1cc, t2cc) -> begin
      let comp_pair : component = cur_comp in
      let comp_fst : component =
        { cur_comp with cp_typ=t1cc; cp_value=(gen_custom_cc cur_val (MV_car cur_val)); } in
      let comp_snd : component =
        { cur_comp with cp_typ=t2cc; cp_value=(gen_custom_cc cur_val (MV_cdr cur_val)); } in
      let acc' : (component Core.Set.Poly.t) CTMap.t =
        CTMap.update
          acc
          cur_typ
          ~f:(function None -> CPSet.singleton comp_pair | Some cc -> CPSet.add cc comp_pair) in
      let acc_fst : (component Core.Set.Poly.t) CTMap.t =
        collect_components comp_fst acc' in
      collect_components comp_snd acc_fst
      end
    | MT_or (t1cc, t2cc) -> begin
      let precond_left : mich_f list = (MF_is_left cur_val) :: cur_prec_lst in
      let precond_right : mich_f list = (MF_not (MF_is_left cur_val)) :: cur_prec_lst in
      let value_left_unlifted : mich_v cc = gen_custom_cc cur_val (MV_unlift_left cur_val) in
      let value_right_unlifted : mich_v cc = gen_custom_cc cur_val (MV_unlift_right cur_val) in
      let comp_left : component =
        { cur_comp with cp_precond_lst=precond_left; cp_value=(gen_custom_cc cur_val (MV_left (cur_typ, value_left_unlifted))); } in
      let comp_right : component =
        { cur_comp with cp_precond_lst=precond_right; cp_value=(gen_custom_cc cur_val (MV_right (cur_typ, value_right_unlifted))); } in
      let comp_left_unlifted : component =
        { cur_comp with cp_typ=t1cc; cp_precond_lst=precond_left; cp_value=value_left_unlifted; } in
      let comp_right_unlifted : component =
        { cur_comp with cp_typ=t2cc; cp_precond_lst=precond_right; cp_value=value_right_unlifted; } in
      let acc' : (component Core.Set.Poly.t) CTMap.t =
        CTMap.update
          acc
          cur_typ
          ~f:(function
              | None -> CPSet.of_list [comp_left; comp_right;]
              | Some cc -> CPSet.add (CPSet.add cc comp_left) comp_right) in
      let acc_left : (component Core.Set.Poly.t) CTMap.t =
        collect_components comp_left_unlifted acc' in
      collect_components comp_right_unlifted acc_left
      end
    | _ -> CTMap.update acc cur_typ ~f:(function None -> CPSet.singleton cur_comp | Some cc -> CPSet.add cc cur_comp)
  end in (* function collect_components end *)
  (* function bake_comp_map start *)
  fun sset -> begin
  let mci_vstack_set : (Tz.mich_cut_info, vstack CPSet.t) CPMap.t =
    CPSet.fold
      sset.blocked
      ~init:CPMap.empty
      ~f:(fun acc s -> (
            CPMap.update
              acc
              s.ss_entry_mci
              ~f:(function
                  | None -> CPSet.singleton s.ss_entry_symstack
                  | Some ss -> CPSet.add ss s.ss_entry_symstack))) in
  let mci_tstack : (Tz.mich_cut_info, tstack) CPMap.t =
    CPMap.map
      mci_vstack_set
      ~f:(fun vsset -> (
            vsset
            |> get_type_stack
            |> function
                | Some ts -> ts
                | None -> Error "bake_comp_map : mci_tstack" |> Stdlib.raise)) in
  CPMap.mapi
    mci_tstack
    ~f:(fun ~key ~data -> (
          CList.foldi
            data
            ~init:CTMap.empty
            ~f:(fun i ctmap t -> (
                  let base_comp_opt : component option = create_base_comp t (key, i) in
                  match base_comp_opt with
                  | None -> ctmap
                  | Some base_comp -> collect_components base_comp ctmap))))
end (* function bake_comp_map end *)

let fold_precond : component list -> Tz.mich_f
= let module CList = Core.List in
  (* function fold_precond start *)
  fun comp_lst -> begin
  Tz.MF_and (comp_lst
            |> CList.map ~f:(fun c -> c.cp_precond_lst)
            |> CList.join)
  (* function fold_precond end *)
end


(*****************************************************************************)
(*****************************************************************************)
(* Recipe                                                                    *)
(*****************************************************************************)
(*****************************************************************************)

let mutez_equal : (component Core.Set.Poly.t) CTMap.t -> Tz.mich_f Core.Set.Poly.t
= let open Tz in
  let module CList = Core.List in
  let module CPSet = Core.Set.Poly in
  (* function mutez_equal start *)
  fun ctmap -> begin
  MT_mutez
    |> gen_dummy_cc
    |> CTMap.find ctmap
    |> (function Some cset -> cset | None -> CPSet.empty)
    |> combination_self_two_diff
    |> CPSet.map
        ~f:(fun (c1, c2) -> MF_imply ((fold_precond [c1; c2;]), MF_eq (c1.cp_value, c2.cp_value)))
end (* function mutez_equal end *)

let all_equal : (component Core.Set.Poly.t) CTMap.t -> Tz.mich_f Core.Set.Poly.t
= let open Tz in
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

(*****************************************************************************)
(*****************************************************************************)
(* Synthesizer                                                               *)
(*****************************************************************************)
(*****************************************************************************)

type generate_param = 
  (* igi_failed_set *)  ((Tz.sym_state * Se.query_category) * (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option) * Tz.mich_f * Utils.Timer.time) Core.Set.Poly.t *
  (* igi_cur_inv *)     Se.invmap *
  (* igi_istrg_opt *)   (Tz.mich_v Tz.cc * Tz.sym_state) option *
  (* igi_comp_map *)    comp_map *
  (* igi_collected *)   Se.invmap Core.Set.Poly.t

type ingredients = {
  igdt_query_category : Se.query_category;
  igdt_model_opt      : ProverLib.Smt.ZModel.t option;
  igdt_vc             : Tz.mich_f;
  igdt_sym_state      : Tz.sym_state;
  igdt_comp_type_map  : (component Core.Set.Poly.t) CTMap.t
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

let refine_t : Se.invmap * (Tz.mich_v Tz.cc * Tz.sym_state) option -> ingredients -> Se.invmap Core.Set.Poly.t
= let open Tz in
  let module CPSet = Core.Set.Poly in
  let module CPMap = Core.Map.Poly in
  (* function refine_t start *)
  fun (cur_inv, istrg_opt) igdt -> begin
  let _ = istrg_opt in
  (* 0. extract component of storage variable *)
  let ctmap = igdt.igdt_comp_type_map in
  (* 1. generate recipe *)
  let all_eq_fmlas : Tz.mich_f CPSet.t = all_equal ctmap in
  (* 2. generate invariant map *)
  let fmlas : Tz.mich_f CPSet.t = 
    [ all_eq_fmlas; ]
    |> collect_set in
  CPSet.map
    fmlas
    ~f:(fun fmla -> (
          CPMap.mapi
            cur_inv
            ~f:(fun ~key ~data ->
                  if key.mci_cutcat = MCC_trx_entry || key.mci_cutcat = MCC_trx_exit
                  then MF_and [fmla; data]
                  else data)))
end (* function refine_t end *)

let refine_l : Se.invmap -> ingredients -> Se.invmap Core.Set.Poly.t
= let module CPSet = Core.Set.Poly in
  fun cur_inv igdt -> begin
  (* refine_l function start *)
  let _ = cur_inv, igdt in
  CPSet.empty (* TODO *)
  (* refine_l function end *)
end

let generate : generate_param -> Se.invmap Core.Set.Poly.t
= let open Tz in
  let module CPSet = Core.Set.Poly in
  let module CPMap = Core.Map.Poly in
  fun (igi_failed_set, igi_cur_inv, igi_istrg_opt, igi_comp_map, igi_collected) -> begin
  (* generate function start *)
  (* 1. collect refine targets *)
  let refine_targets : (Tz.mich_cut_info, (ingredients CPSet.t)) CPMap.t =
    CPSet.fold
      igi_failed_set
      ~init:CPMap.empty
      ~f:(fun acc ((fs, qctg), (_, mopt), vc, _) -> 
          (* 1-1. get accumulated ingredients *)
          let (acc', acc_igdt_set) : ((Tz.mich_cut_info, (ingredients CPSet.t)) CPMap.t) * (ingredients CPSet.t) =
            CPMap.find acc fs.ss_entry_mci
            |> function
                | Some ss -> ((CPMap.remove acc fs.ss_entry_mci), ss)
                | None -> (acc, CPSet.empty) in
          (* 1-2. make new ingredients *)
          let ctmap : (component CPSet.t) CTMap.t =
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
          (* 1-3. accumulate new ingredients *)
          let new_acc_igdt_set : ingredients CPSet.t = CPSet.add acc_igdt_set new_igdt in
          CPMap.add
            acc'
            ~key:fs.ss_entry_mci
            ~data:new_acc_igdt_set
          |> function `Ok mm -> mm | `Duplicate -> Error "generate : refine_targets" |> Stdlib.raise) in
  (* 2. generate invariants from current invariant *)
  let newly_generated_inv : Se.invmap CPSet.t =
    CPMap.fold
      refine_targets
      ~init:CPSet.empty
      ~f:(fun ~key ~data acc -> 
          (* select refine function whether it is entry or not *)
          let rf = if (key.mci_cutcat = MCC_trx_entry) then (refine_t (igi_cur_inv, igi_istrg_opt)) else (refine_l igi_cur_inv) in
          (* generate new invariants and accumulate it *)
          CPSet.fold
            data
            ~init:acc
            ~f:(fun acc igdt -> CPSet.union acc (rf igdt))) in
  CPSet.diff newly_generated_inv igi_collected
  (* generate function end *)
end