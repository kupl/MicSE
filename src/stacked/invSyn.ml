(* Invariant Synthesizer *)

exception Error of string


(*****************************************************************************)
(*****************************************************************************)
(* Common Datatypes                                                          *)
(*****************************************************************************)
(*****************************************************************************)

module PSet = Core.Set.Poly
module PMap = Core.Map.Poly

type 'a set = 'a Tz.PSet.t
type ('a, 'b) map = ('a, 'b) Tz.PMap.t

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

module TMap = TComparable.Map

type 'a tmap = 'a TMap.t


(*****************************************************************************)
(*****************************************************************************)
(* List Combination                                                          *)
(*****************************************************************************)
(*****************************************************************************)

(* bind 1 {1; 2; 3} === {(1, 1); (1, 2); (1, 3)} *)
let bind : 'a -> 'b list -> ('a * 'b) list
= let module CList = Core.List in
  fun a blst -> begin
  (* bind function start *)
  CList.fold_left
    blst
    ~init:[]
    ~f:(fun acc b -> (a, b)::acc)
  (* bind function end *)
end

(* combination {1; 2} {a; b} === {(1, a); (1, b); (2, a); (2, b)} *)
let combination : 'a list -> 'b list -> ('a * 'b) list
= let module CList = Core.List in
  fun alst blst -> begin
  (* combination function start *)
  CList.fold_left
    alst
    ~init:[]
    ~f:(fun acc a -> (bind a blst)@acc)
  (* combination function end *)
end

(* combination_rfl {1; 2} === {(1, 1); (1, 2); (2, 2)} *)
let combination_rfl : 'a list -> ('a * 'a) list
= fun s -> begin
  (* combination_rfl function start *)
  combination s s
  (* combination_rfl function end *)
end

(* combination_self_two_diff {1; 2; 3} === {(1, 2); (1, 3); (2, 3)} *)
let combination_self_two_diff : 'a list -> ('a * 'a) list
= let rec combination_self_two_diff_i : 'a list -> ('a * 'a) list
  = fun lst -> begin
    match lst with
    | [] -> []
    | hd::tl -> begin
      let comb : ('a * 'a) list = bind hd tl in
      comb@(combination_self_two_diff_i tl)
      end
  end in
  fun lst -> begin
  (* combination_self_two_diff function start *)
  combination_self_two_diff_i lst
  (* combination_self_two_diff function end *)
end

(* combination_self_two_diff_rf {1; 2; 3} === {(1, 2); (1, 3); (2, 1); (2, 3); (3, 1); (3, 2)} *)
let combination_self_two_diff_rf : 'a list -> ('a * 'a) list
= let module CList = Core.List in
  fun lst -> begin
  (* combination_self_two_diff_rf function start *)
  let comb = combination_self_two_diff lst in
  CList.fold_left
    comb
    ~init:comb
    ~f:(fun acc (x, y) -> (y, x)::acc)
  (* combination_self_two_diff_rf function end *)
end


(*****************************************************************************)
(*****************************************************************************)
(* Component Collector                                                       *)
(*****************************************************************************)
(*****************************************************************************)

type vstack = Tz.mich_v Tz.cc list (* syntax sugar *)
type tstack = Tz.mich_t Tz.cc list (* syntax sugar *)

type comp_body = {
  cpb_precond_lst : Tz.mich_f list;   (* precondition list of component *)
  cpb_value       : Tz.mich_v Tz.cc;  (* value expression of component *)
}

  (****************************************************************************
    The type component is information from each component of the symbolic stack.
    Each component is extracted from the given symbolic stack.
    The type of component is statically baked from the type stack.
    type component = type * (sym-stack -> component-body)
  ****************************************************************************)
type component = {
  cp_typ  : Tz.mich_t Tz.cc;      (* type of component *)
  cp_loc  : int;                  (* location of component in stack *)
  cp_body : vstack -> comp_body;  (* component body which made from stack *)
}

  (****************************************************************************
    The type comp_map is a pre-baked component map.
    Function bake_comp_map makes a set of components from the type stack of each MCI.
    The component list which is the value of comp_map is used to make a set of new invariants by recipe.
    type comp_map = MCI |-> component list
  ****************************************************************************)
type comp_map = (Tz.mich_cut_info, component list tmap) map

let bake_comp_map : Se.state_set -> comp_map
= let module CList = Core.List in
  let get_type_stack : vstack set -> tstack option
  = let extract_type : vstack -> tstack
    = fun vs -> begin
      (* extract_type function start *)
      CList.fold_right
        vs
        ~f:(fun v ts -> (v |> Tz.typ_of_val)::ts)
        ~init:[]
      (* extract_type function end *)
    end in
    let tstack_equal : tstack -> tstack -> bool
    = fun ts1 ts2 -> begin
      (* tstack_equal function start *)
      CList.fold2
        ts1
        ts2
        ~init:true
        ~f:(fun c t1 t2 -> if t1.cc_v <> t2.cc_v then false else c)
      |> function
          | Ok cc -> cc
          | Unequal_lengths -> false
      (* tstack_equal function end *)
    end in
    fun vsset -> begin
    (* get_type_stack function start *)
    let vs : vstack = 
      PSet.choose vsset
      |> function 
          | Some ss -> ss
          | None -> Error "bake_comp_map : get_type_stack : vs" |> Stdlib.raise in
    let ts : tstack = vs |> extract_type in
    PSet.fold
      (PSet.remove vsset vs)
      ~init:(Some ts)
      ~f:(fun ts_opt vs -> (
            if Option.is_none ts_opt then None
            else 
              let ts = Option.get ts_opt in
              if tstack_equal ts (vs |> extract_type) then ts_opt
              else None))
    (* get_type_stack function end *)
  end in
  let create_base_comp : Tz.mich_t Tz.cc -> (Tz.mich_cut_info * int) -> component option
  = let open Tz in
    fun cur_typ (cur_mci, cur_loc) -> begin
    (* create_comp function start *)
    match (cur_mci.mci_cutcat, cur_loc) with
    | MCC_ln_loopleft, 0
    | MCC_ln_map, 0
    | MCC_lb_loopleft, 0 
    | MCC_lb_map, 0
    | MCC_lb_iter, 0 -> None
    | MCC_trx_entry, 0 -> begin
      match cur_typ.cc_v with
      | MT_pair (_, strg_typ) -> begin
        let cur_body : vstack -> comp_body =
          (fun vl -> (
            let value : Tz.mich_v Tz.cc =
              CList.hd vl
              |> function 
                  | Some vv -> vv
                  | None -> (Error "bake_comp_map : create_base_comp : MCC_trx_entry, 0 : cur_body" |> Stdlib.raise) in
            { cpb_precond_lst=[]; cpb_value=(gen_custom_cc value (MV_cdr value)); })) in
        Some { cp_typ=strg_typ; cp_loc=cur_loc; cp_body=cur_body; }
        end
      | _ -> (Error "bake_comp_map : create_base_comp : MCC_trx_entry, 0 : cur_typ" |> Stdlib.raise)
      end
    | _ -> begin
      let cur_body : vstack -> comp_body =
        (fun vl -> (
          let value : Tz.mich_v Tz.cc =
            CList.nth vl cur_loc 
            |> function Some vv -> vv | None -> (Error "bake_comp_map : create_base_comp : _ : cur_body" |> Stdlib.raise) in 
          { cpb_precond_lst=[]; cpb_value=value; })) in
      Some { cp_typ=cur_typ; cp_loc=cur_loc; cp_body=cur_body; }
    end
    (* create_comp function end *)
  end in
  let rec collect_components : component -> component list tmap -> component list tmap
  = let open Tz in
    fun cur_comp acc -> begin
    (* collect_components function start *)
    let { cp_typ=cur_typ; cp_loc=cur_loc; cp_body=cur_body; } : component = cur_comp in
    match cur_typ.cc_v with
    | MT_option t1cc -> begin
      let comp_none_body : vstack -> comp_body =
        (fun vl -> (
          let { cpb_precond_lst=cur_prec_lst; cpb_value=cur_val; } : comp_body = cur_body vl in
          let precond : mich_f list = (MF_is_none cur_val) :: cur_prec_lst in
          let value : mich_v cc = gen_custom_cc cur_val (MV_none t1cc) in
          { cpb_precond_lst=precond; cpb_value=value; })) in
      let comp_some_body : vstack -> comp_body =
        (fun vl -> (
          let { cpb_precond_lst=cur_prec_lst; cpb_value=cur_val; } : comp_body = cur_body vl in
          let precond : mich_f list = (MF_not (MF_is_none cur_val)) :: cur_prec_lst in
          let value : mich_v cc = gen_custom_cc cur_val (MV_some (gen_custom_cc cur_val (MV_unlift_option cur_val))) in
          { cpb_precond_lst=precond; cpb_value=value; })) in
      let comp_some_unlifted_body : vstack -> comp_body =
        (fun vl -> (
          let { cpb_precond_lst=cur_prec_lst; cpb_value=cur_val; } : comp_body = cur_body vl in
          let precond : mich_f list = (MF_not (MF_is_none cur_val)) :: cur_prec_lst in
          let value : mich_v cc = gen_custom_cc cur_val (MV_unlift_option cur_val) in
          { cpb_precond_lst=precond; cpb_value=value; })) in
      let comp_none : component = { cp_typ=cur_typ; cp_loc=cur_loc; cp_body=comp_none_body; } in
      let comp_some : component = { cp_typ=cur_typ; cp_loc=cur_loc; cp_body=comp_some_body; } in
      let comp_some_unlifted : component = { cp_typ=t1cc; cp_loc=cur_loc; cp_body=comp_some_unlifted_body; } in
      let acc' : component list tmap = 
        TMap.update
          acc
          cur_typ
          ~f:(function None -> [comp_none; comp_some;] | Some cc -> comp_none::comp_some::cc) in
      collect_components comp_some_unlifted acc'
      end
    | MT_pair (t1cc, t2cc) -> begin
      let comp_fst_body : vstack -> comp_body =
        (fun vl -> (
          let { cpb_precond_lst=cur_prec_lst; cpb_value=cur_val; } : comp_body = cur_body vl in
          let value : mich_v cc = gen_custom_cc cur_val (MV_car cur_val) in
          { cpb_precond_lst=cur_prec_lst; cpb_value=value; })) in
      let comp_snd_body : vstack -> comp_body =
        (fun vl -> (
          let { cpb_precond_lst=cur_prec_lst; cpb_value=cur_val; } : comp_body = cur_body vl in
          let value : mich_v cc = gen_custom_cc cur_val (MV_cdr cur_val) in
          { cpb_precond_lst=cur_prec_lst; cpb_value=value; })) in
      let comp_pair : component = cur_comp in
      let comp_fst : component = { cp_typ=t1cc; cp_loc=cur_loc; cp_body=comp_fst_body; } in
      let comp_snd : component = { cp_typ=t2cc; cp_loc=cur_loc; cp_body=comp_snd_body; } in
      let acc' : component list tmap =
        TMap.update
          acc
          cur_typ
          ~f:(function None -> [comp_pair;] | Some cc -> comp_pair::cc) in
      let acc_fst : component list tmap = collect_components comp_fst acc' in
      collect_components comp_snd acc_fst
      end
    | MT_or (t1cc, t2cc) -> begin
      let comp_left_body : vstack -> comp_body =
        (fun vl -> (
          let { cpb_precond_lst=cur_prec_lst; cpb_value=cur_val; } : comp_body = cur_body vl in
          let precond : mich_f list = (MF_is_left cur_val) :: cur_prec_lst in
          let value : mich_v cc = gen_custom_cc cur_val (MV_left (cur_typ, (gen_custom_cc cur_val (MV_unlift_left cur_val)))) in
          { cpb_precond_lst=precond; cpb_value=value; })) in
      let comp_right_body : vstack -> comp_body =
        (fun vl -> (
          let { cpb_precond_lst=cur_prec_lst; cpb_value=cur_val; } : comp_body = cur_body vl in
          let precond : mich_f list = (MF_not (MF_is_left cur_val)) :: cur_prec_lst in
          let value : mich_v cc = gen_custom_cc cur_val (MV_right (cur_typ, (gen_custom_cc cur_val (MV_unlift_right cur_val)))) in
          { cpb_precond_lst=precond; cpb_value=value; })) in
      let comp_left_unlifted_body : vstack -> comp_body =
        (fun vl -> (
          let { cpb_precond_lst=cur_prec_lst; cpb_value=cur_val; } : comp_body = cur_body vl in
          let precond : mich_f list = (MF_is_left cur_val) :: cur_prec_lst in
          let value : mich_v cc = gen_custom_cc cur_val (MV_unlift_left cur_val) in
          { cpb_precond_lst=precond; cpb_value=value; })) in
      let comp_right_unlifted_body : vstack -> comp_body =
        (fun vl -> (
          let { cpb_precond_lst=cur_prec_lst; cpb_value=cur_val; } : comp_body = cur_body vl in
          let precond : mich_f list = (MF_not (MF_is_left cur_val)) :: cur_prec_lst in
          let value : mich_v cc = gen_custom_cc cur_val (MV_unlift_right cur_val) in
          { cpb_precond_lst=precond; cpb_value=value; })) in
      let comp_left : component = { cp_typ=cur_typ; cp_loc=cur_loc; cp_body=comp_left_body; } in
      let comp_right : component = { cp_typ=cur_typ; cp_loc=cur_loc; cp_body=comp_right_body; } in
      let comp_left_unlifted : component = { cp_typ=t1cc; cp_loc=cur_loc; cp_body=comp_left_unlifted_body; } in
      let comp_right_unlifted : component = { cp_typ=t2cc; cp_loc=cur_loc; cp_body=comp_right_unlifted_body; } in
      let acc' : component list tmap =
        TMap.update
          acc
          cur_typ
          ~f:(function None -> [comp_left; comp_right;] | Some cc -> comp_left::comp_right::cc) in
      let acc_left = collect_components comp_left_unlifted acc' in
      collect_components comp_right_unlifted acc_left
      end
    | _ -> TMap.update acc cur_typ ~f:(function None -> [cur_comp;] | Some cc -> cur_comp::cc)
    (* collect_components function end *)
  end in
  fun sset -> begin
  (* bake_comp_map function start *)
  let mci_vstack_set : (Tz.mich_cut_info, vstack set) map =
    PSet.fold
      sset.blocked
      ~init:PMap.empty
      ~f:(fun acc s -> (
            PMap.update
              acc
              s.ss_entry_mci
              ~f:(function
                  | None -> PSet.singleton s.ss_entry_symstack
                  | Some ss -> PSet.add ss s.ss_entry_symstack))) in
  let mci_tstack : (Tz.mich_cut_info, tstack) map =
    PMap.map
      mci_vstack_set
      ~f:(fun vsset -> (
            vsset
            |> get_type_stack
            |> function
                | Some ts -> ts
                | None -> Error "bake_comp_map : mci_tstack" |> Stdlib.raise)) in
  PMap.mapi
    mci_tstack
    ~f:(fun ~key ~data -> (
          CList.foldi
            data
            ~init:TMap.empty
            ~f:(fun i ctmap t -> (
                  let base_comp_opt : component option = create_base_comp t (key, i) in
                  match base_comp_opt with
                  | None -> ctmap
                  | Some base_comp -> collect_components base_comp ctmap))))
  (* bake_comp_map function end *)
end

let fold_precond : vstack -> component list -> Tz.mich_f
= let module CList = Core.List in
  fun sst comp_lst -> begin
  (* fold_precond function start *)
  Tz.MF_and (comp_lst
            |> CList.map ~f:(fun c -> 
                              let cb = c.cp_body sst in
                              cb.cpb_precond_lst)
            |> CList.join)
  (* fold_precond function end *)
end

let get_value : vstack -> component -> Tz.mich_v Tz.cc
= fun vl cp -> begin
  (* get_value function start *)
  (cp.cp_body vl).cpb_value
  (* get_value function end *)
end


(*****************************************************************************)
(*****************************************************************************)
(* Recipe                                                                    *)
(*****************************************************************************)
(*****************************************************************************)

type invariant = vstack -> Tz.mich_f

let mutez_equal : component list tmap -> invariant list
= let open Tz in
  let module CList = Core.List in
  fun ctmap -> begin
  (* mutez_equal function start *)
  let cb : (component * component) list =
    MT_mutez
    |> gen_dummy_cc
    |> TMap.find ctmap
    |> (function Some clst -> clst | None -> [])
    |> combination_self_two_diff in
  CList.map
    cb
    ~f:(fun (c1, c2) -> (
          (fun vl -> (MF_imply (
                        (fold_precond vl [c1; c2]), 
                        MF_eq ((get_value vl c1), (get_value vl c2)))))))
  (* mutez_equal function end *)
end

let all_equal : component list tmap -> invariant list
= let open Tz in
  let module CList = Core.List in
  fun ctmap -> begin
  (* all_equal function start *)
  TMap.fold
    ctmap
    ~init:[]
    ~f:(fun ~key ~data acc -> (
          let _ = key in
          let invs : invariant list =      
            data
            |> combination_self_two_diff
            |> CList.map
                ~f:(fun (c1, c2) -> 
                      (fun vl -> (MF_imply (
                                    (fold_precond vl [c1; c2]), 
                                    MF_eq ((get_value vl c1), (get_value vl c2)))))) in
          invs@acc))
  (* all_equal function end *)
end

(*****************************************************************************)
(*****************************************************************************)
(* Synthesizer                                                               *)
(*****************************************************************************)
(*****************************************************************************)

type generate_param = 
  (* igi_failed_set *)  ((Tz.sym_state * Se.query_category) * (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option) * Tz.mich_f * Utils.Timer.time) set *
  (* igi_cur_inv *)     Se.invmap *
  (* igi_istrg_opt *)   (Tz.mich_v Tz.cc * Tz.sym_state) option *
  (* igi_comp_map *)    comp_map

type ingredients = {
  igdt_query_category : Se.query_category;
  igdt_model_opt      : ProverLib.Smt.ZModel.t option;
  igdt_vc             : Tz.mich_f;
  igdt_sym_state      : Tz.sym_state;
  igdt_comp_type_map  : component list tmap
}

let refine_t : Se.invmap * (Tz.mich_v Tz.cc * Tz.sym_state) option -> ingredients -> Se.invmap list
= let open Tz in
  let module CList = Core.List in
  fun (cur_inv, istrg_opt) igdt -> begin
  (* refine_t function start *)
  (*0. extract component of storage variable *)
  let ctmap = igdt.igdt_comp_type_map in
  (* 1. generate recipe *)
  let all_eq_fmlas : invariant list = all_equal ctmap in
  (* 2. generate invariant map *)
  let fmlas : invariant list = 
    [ all_eq_fmlas; ]
    |> CList.join in
  let _ = (cur_inv, istrg_opt), igdt in
  CList.map
    fmlas
    ~f:(fun fmla -> (
          PMap.mapi
            cur_inv
            ~f:(fun ~key ~data ->
                  if key.mci_cutcat = MCC_trx_entry || key.mci_cutcat = MCC_trx_exit
                  then (function vl -> MF_and [(fmla vl); (data vl)])
                  else data)))
  (* refine_t function end *)
end

let refine_l : Se.invmap -> ingredients -> Se.invmap list
= 
  fun cur_inv igdt -> begin
  (* refine_l function start *)
  let _ = cur_inv, igdt in
  [] (* TODO *)
  (* refine_l function end *)
end

let generate : generate_param -> Se.invmap list
= let open Tz in
  let module CList = Core.List in
  fun (igi_failed_set, igi_cur_inv, igi_istrg_opt, igi_comp_map) -> begin
  (* generate function start *)
  (* 1. collect refine targets *)
  let refine_targets : (Tz.mich_cut_info, (ingredients list)) map =
    PSet.fold
      igi_failed_set
      ~init:PMap.empty
      ~f:(fun acc ((fs, qctg), (_, mopt), vc, _) -> 
          (* 1-1. get accumulated ingredients *)
          let (acc', acc_igdt_set) : ((Tz.mich_cut_info, (ingredients list)) map) * (ingredients list) =
            PMap.find acc fs.ss_entry_mci
            |> function Some ss -> ((PMap.remove acc fs.ss_entry_mci), ss) | None -> (acc, []) in
          (* 1-2. make new ingredients *)
          let ctmap : component list tmap =
            PMap.find igi_comp_map fs.ss_entry_mci
            |> function Some ccss -> ccss | None -> Error "generate : refine_targets : clst" |> Stdlib.raise in
          let new_igdt : ingredients =
            { igdt_query_category=qctg;
              igdt_model_opt=mopt;
              igdt_vc=vc;
              igdt_sym_state=fs;
              igdt_comp_type_map=ctmap} in
          (* 1-3. accumulate new ingredients *)
          let new_acc_igdt_set : ingredients list = new_igdt::acc_igdt_set in
          PMap.add
            acc'
            ~key:fs.ss_entry_mci
            ~data:new_acc_igdt_set
          |> function `Ok mm -> mm | `Duplicate -> Error "generate : refine_targets" |> Stdlib.raise) in
  (* 2. generate invariants from current invariant *)
  PMap.fold
    refine_targets
    ~init:[]
    ~f:(fun ~key ~data acc -> 
        (* select refine function whether it is entry or not *)
        let rf = if (key.mci_cutcat = MCC_trx_entry) then (refine_t (igi_cur_inv, igi_istrg_opt)) else (refine_l igi_cur_inv) in
        (* generate new invariants and accumulate it *)
        CList.fold_left
          data
          ~init:acc
          ~f:(fun acc igdt ->
                (rf igdt)@acc))
  (* generate function end *)
end