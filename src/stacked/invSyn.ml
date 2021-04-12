(* Invariant Synthesizer *)

exception Error of string


(*****************************************************************************)
(*****************************************************************************)
(* Common Datatypes                                                          *)
(*****************************************************************************)
(*****************************************************************************)

module PSet = Core.Set.Poly
module PMap = Core.Map.Poly


(*****************************************************************************)
(*****************************************************************************)
(* Types                                                                     *)
(*****************************************************************************)
(*****************************************************************************)

type 'a set = 'a Tz.PSet.t
type ('a, 'b) map = ('a, 'b) Tz.PMap.t

type generate_param = 
  (* igi_failed_set *)  ((Tz.sym_state * Se.query_category) * (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option) * Tz.mich_f * Utils.Timer.time) set *
  (* igi_cur_inv *)     Se.invmap *
  (* igi_istrg_opt *)   (Tz.mich_v Tz.cc * Tz.sym_state) option *
  (* igi_collected *)   Se.invmap set

type ingredients = {
  igdt_query_category: Se.query_category;
  igdt_model_opt: ProverLib.Smt.ZModel.t option;
  igdt_vc: Tz.mich_f;
  igdt_sym_state: Tz.sym_state;
}


(*****************************************************************************)
(*****************************************************************************)
(* Set Combination                                                           *)
(*****************************************************************************)
(*****************************************************************************)

(* bind 1 {1; 2; 3} === {(1, 1); (1, 2); (1, 3)} *)
let bind : 'a -> 'b set -> ('a * 'b) set
= fun a bset -> begin
  (* bind function start *)
  PSet.fold
    bset
    ~init:PSet.empty
    ~f:(fun acc b -> PSet.add acc (a, b))
  (* bind function end *)
end

(* combination {1; 2} {a; b} === {(1, a); (1, b); (2, a); (2, b)} *)
let combination : 'a set -> 'b set -> ('a * 'b) set
= fun aset bset -> begin
  (* combination function start *)
  PSet.fold
    aset
    ~init:PSet.empty
    ~f:(fun acc a -> PSet.union acc (bind a bset))
  (* combination function end *)
end

(* combination_rfl {1; 2} === {(1, 1); (1, 2); (2, 2)} *)
let combination_rfl : 'a set -> ('a * 'a) set
= fun s -> begin
  (* combination_rfl function start *)
  combination s s
  (* combination_rfl function end *)
end

(* combination_self_two_diff {1; 2; 3} === {(1, 2); (1, 3); (2, 3)} *)
let combination_self_two_diff : 'a set -> ('a * 'a) set
= fun s -> begin
  (* combination_self_two_diff function start *)
  let (comb, _) : ('a * 'a) set * 'a set =
    PSet.fold
      s
      ~init:(PSet.empty, s)
      ~f:(fun (acc_comb, rs) x -> (
        let rs' : 'a set = PSet.remove rs x in
        let comb' : ('a * 'a) set = bind x rs' in
        ((PSet.union acc_comb comb'), rs'))) in
  comb
  (* combination_self_two_diff function end *)
end

(* combination_self_two_diff_rf {1; 2; 3} === {(1, 2); (1, 3); (2, 1); (2, 3); (3, 1); (3, 2)} *)
let combination_self_two_diff_rf : 'a set -> ('a * 'a) set
= fun s -> begin
  (* combination_self_two_diff_rf function start *)
  let comb = combination_self_two_diff s in
  PSet.fold
    comb
    ~init:comb
    ~f:(fun acc (x, y) -> PSet.add acc (y, x))
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
    The component set which is the value of comp_map is used to make a set of new invariants by recipe.
    type comp_map = MCI |-> component set
  ****************************************************************************)
type comp_map = (Tz.mich_cut_info, component set) map

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
  let create_comp : Tz.mich_t Tz.cc -> int -> component
  = fun cur_typ cur_loc -> begin
    (* create_comp function start *)
    let cur_body : Tz.mich_v Tz.cc list -> comp_body =
      (fun vl -> (
        let value : Tz.mich_v Tz.cc =
          CList.nth vl cur_loc 
          |> function Some vv -> vv | None -> (Error "bake_comp_map : create_comp : cur_body" |> Stdlib.raise) in 
        { cpb_precond_lst=[]; cpb_value=value; })) in
    { cp_typ=cur_typ; cp_loc=cur_loc; cp_body=cur_body; }
    (* create_comp function end *)
  end in
  let rec collect_components : component -> component set -> component set
  = let open Tz in
    fun cur_comp acc -> begin
    (* collect_components function start *)
    let { cp_typ=cur_typ; cp_loc=cur_loc; cp_body=cur_body; } : component = cur_comp in
    match cur_typ.cc_v with
    | MT_option t1cc -> begin
      let comp_none_body : mich_v cc list -> comp_body =
        (fun vl -> (
          let { cpb_precond_lst=cur_prec_lst; cpb_value=cur_val; } : comp_body = cur_body vl in
          let precond : mich_f list = (MF_is_none cur_val) :: cur_prec_lst in
          let value : mich_v cc = gen_dummy_cc (MV_none t1cc) in
          { cpb_precond_lst=precond; cpb_value=value; })) in
      let comp_some_body : mich_v cc list -> comp_body =
        (fun vl -> (
          let { cpb_precond_lst=cur_prec_lst; cpb_value=cur_val; } : comp_body = cur_body vl in
          let precond : mich_f list = (MF_not (MF_is_none cur_val)) :: cur_prec_lst in
          let value : mich_v cc = gen_dummy_cc (MV_some (gen_dummy_cc (MV_unlift_option cur_val))) in
          { cpb_precond_lst=precond; cpb_value=value; })) in
      let comp_some_unlifted_body : mich_v cc list -> comp_body =
        (fun vl -> (
          let { cpb_precond_lst=cur_prec_lst; cpb_value=cur_val; } : comp_body = cur_body vl in
          let precond : mich_f list = (MF_not (MF_is_none cur_val)) :: cur_prec_lst in
          let value : mich_v cc = gen_dummy_cc (MV_unlift_option cur_val) in
          { cpb_precond_lst=precond; cpb_value=value; })) in
      let comp_none : component = { cp_typ=cur_typ; cp_loc=cur_loc; cp_body=comp_none_body; } in
      let comp_some : component = { cp_typ=cur_typ; cp_loc=cur_loc; cp_body=comp_some_body; } in
      let comp_some_unlifted : component = { cp_typ=t1cc; cp_loc=cur_loc; cp_body=comp_some_unlifted_body; } in
      collect_components comp_some_unlifted (PSet.add (PSet.add acc comp_none) comp_some)
      end
    | MT_pair (t1cc, t2cc) -> begin
      let comp_fst_body : mich_v cc list -> comp_body =
        (fun vl -> (
          let { cpb_precond_lst=cur_prec_lst; cpb_value=cur_val; } : comp_body = cur_body vl in
          let value : mich_v cc = gen_dummy_cc (MV_car cur_val) in
          { cpb_precond_lst=cur_prec_lst; cpb_value=value; })) in
      let comp_snd_body : mich_v cc list -> comp_body =
        (fun vl -> (
          let { cpb_precond_lst=cur_prec_lst; cpb_value=cur_val; } : comp_body = cur_body vl in
          let value : mich_v cc = gen_dummy_cc (MV_cdr cur_val) in
          { cpb_precond_lst=cur_prec_lst; cpb_value=value; })) in
      let comp_pair : component = cur_comp in
      let comp_fst : component = { cp_typ=t1cc; cp_loc=cur_loc; cp_body=comp_fst_body; } in
      let comp_snd : component = { cp_typ=t2cc; cp_loc=cur_loc; cp_body=comp_snd_body; } in
      let acc_fst : component set = collect_components comp_fst (PSet.add acc comp_pair) in
      collect_components comp_snd acc_fst
      end
    | MT_or (t1cc, t2cc) -> begin
      let comp_left_body : mich_v cc list -> comp_body =
        (fun vl -> (
          let { cpb_precond_lst=cur_prec_lst; cpb_value=cur_val; } : comp_body = cur_body vl in
          let precond : mich_f list = (MF_is_left cur_val) :: cur_prec_lst in
          let value : mich_v cc = gen_dummy_cc (MV_left (cur_typ, (gen_dummy_cc (MV_unlift_left cur_val)))) in
          { cpb_precond_lst=precond; cpb_value=value; })) in
      let comp_right_body : mich_v cc list -> comp_body =
        (fun vl -> (
          let { cpb_precond_lst=cur_prec_lst; cpb_value=cur_val; } : comp_body = cur_body vl in
          let precond : mich_f list = (MF_not (MF_is_left cur_val)) :: cur_prec_lst in
          let value : mich_v cc = gen_dummy_cc (MV_right (cur_typ, (gen_dummy_cc (MV_unlift_right cur_val)))) in
          { cpb_precond_lst=precond; cpb_value=value; })) in
      let comp_left_unlifted_body : mich_v cc list -> comp_body =
        (fun vl -> (
          let { cpb_precond_lst=cur_prec_lst; cpb_value=cur_val; } : comp_body = cur_body vl in
          let precond : mich_f list = (MF_is_left cur_val) :: cur_prec_lst in
          let value : mich_v cc = gen_dummy_cc (MV_unlift_left cur_val) in
          { cpb_precond_lst=precond; cpb_value=value; })) in
      let comp_right_unlifted_body : mich_v cc list -> comp_body =
        (fun vl -> (
          let { cpb_precond_lst=cur_prec_lst; cpb_value=cur_val; } : comp_body = cur_body vl in
          let precond : mich_f list = (MF_not (MF_is_left cur_val)) :: cur_prec_lst in
          let value : mich_v cc = gen_dummy_cc (MV_unlift_right cur_val) in
          { cpb_precond_lst=precond; cpb_value=value; })) in
      let comp_left : component = { cp_typ=cur_typ; cp_loc=cur_loc; cp_body=comp_left_body; } in
      let comp_right : component = { cp_typ=cur_typ; cp_loc=cur_loc; cp_body=comp_right_body; } in
      let comp_left_unlifted : component = { cp_typ=t1cc; cp_loc=cur_loc; cp_body=comp_left_unlifted_body; } in
      let comp_right_unlifted : component = { cp_typ=t2cc; cp_loc=cur_loc; cp_body=comp_right_unlifted_body; } in
      let acc_left = collect_components comp_left_unlifted (PSet.add (PSet.add acc comp_left) comp_right) in
      collect_components comp_right_unlifted acc_left
      end
    | _ -> PSet.add acc cur_comp
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
  PMap.map
    mci_tstack
    ~f:(fun ts -> (
          CList.foldi
            ts
            ~init:PSet.empty
            ~f:(fun i cset t -> (
                  let base_comp : component = create_comp t i in
                  collect_components base_comp cset))))
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

let filter_comp : (Tz.mich_t -> bool) -> component set -> component set
= fun filter_f cset -> begin
  (* filter_comp function start *)
  PSet.filter cset ~f:(fun c -> filter_f c.cp_typ.cc_v)
  (* filter_comp function end *)
end

let classify_comp_with_type : component set -> (Tz.mich_t, component set) map
= fun cset -> begin
  (* classify_comp_with_type function start *)
  PSet.fold
    cset
    ~init:PMap.empty
    ~f:(fun acc c -> (
          PMap.update
            acc
            c.cp_typ.cc_v
            ~f:(function None -> PSet.singleton c | Some cset -> PSet.add cset c)))
  (* classify_comp_with_type function end *)
end


(*****************************************************************************)
(*****************************************************************************)
(* Recipe                                                                    *)
(*****************************************************************************)
(*****************************************************************************)

(* let mutez_equal : component set -> Tz.mich_f set
= let open Tz in
  fun compset -> begin
  (* mutez_equal function start *)
  PSet.map
    (combination_self_two_diff compset)
    ~f:(fun (c1, c2) -> MF_imply ((fold_precond [c1; c2]), MF_eq (c1.body, c2.body)))
  (* mutez_equal function end *)
end

let all_equal : component set -> Tz.mich_f set
= let open Tz in
  fun compset -> begin
  (* all_equal function start *)
  let cb : (component * component) set =
    compset
    |> combination_self_two_diff
    |> PSet.filter ~f:(fun (c1, c2) -> c1.typ = c2.typ) in
  PSet.map
    cb
    ~f:(fun (c1, c2) -> MF_imply ((fold_precond [c1; c2]), MF_eq (c1.body, c2.body)))
  (* all_equal function end *)
end *)


(*****************************************************************************)
(*****************************************************************************)
(* Synthesizer                                                               *)
(*****************************************************************************)
(*****************************************************************************)

let collect_set : ('a set) list -> 'a set
= let module CList = Core.List in
  fun slist -> begin
  (* collect_set function start *)
  CList.fold
    slist
    ~init:PSet.empty
    ~f:(fun acc s -> PSet.union acc s)
  (* collect_set function end *)
end

let refine_t : Se.invmap * (Tz.mich_v Tz.cc * Tz.sym_state) option -> ingredients -> Se.invmap set
= let open Tz in
  fun (cur_inv, istrg_opt) igdt -> begin
  (* refine_t function start *)
  let _ = cur_inv, istrg_opt, igdt in
  PSet.empty (* TODO *)
  (*(* 0. extract component of storage variable *)
  let vstrg : Tz.mich_v Tz.cc = 
    PMap.find
      igdt.igdt_sym_state.ss_dynchain.bc_storage
      igdt.igdt_sym_state.ss_optt.optt_addr
    |> function Some vv -> vv | None -> Error "refine_t : vstrg" |> Stdlib.raise in
  let comp = collect_components vstrg in
  (* 1. generate recipe *)
  let all_eq_fmlas : Tz.mich_f set = all_equal comp in
  (* 2. generate invariant map *)
  let fmlas : Tz.mich_f set = 
    [ all_eq_fmlas; ]
    |> collect_set in
  let _ = (cur_inv, istrg_opt), igdt in
  PSet.map
    fmlas
    ~f:(fun fmla -> (
      PMap.mapi
        cur_inv
        ~f:(fun ~key ~data -> 
              if key.mci_cutcat = MCC_trx_entry || key.mci_cutcat = MCC_trx_exit
              then (function vl -> MF_and [fmla; (data vl)])
              else data))) *)
  (* refine_t function end *)
end

let refine_l : Se.invmap -> ingredients -> Se.invmap set
= 
  fun cur_inv igdt -> begin
  (* refine_l function start *)
  let _ = cur_inv, igdt in
  PSet.empty (* TODO *)
  (* refine_l function end *)
end

let generate : generate_param -> Se.invmap set
= let open Tz in
  fun (igi_failed_set, igi_cur_inv, igi_istrg_opt, igi_collected) -> begin
  (* generate function start *)
  (* 1. collect refine targets *)
  let refine_targets : (Tz.mich_cut_info, (ingredients set)) map =
    PSet.fold
      igi_failed_set
      ~init:PMap.empty
      ~f:(fun acc ((fs, qctg), (_, mopt), vc, _) -> 
          (* 1-1. get accumulated ingredients *)
          let (acc', acc_igdt_set) : ((Tz.mich_cut_info, (ingredients set)) map) * (ingredients set) =
            PMap.find acc fs.ss_entry_mci
            |> function Some ss -> ((PMap.remove acc fs.ss_entry_mci), ss) | None -> (acc, PSet.empty) in
          (* 1-2. make new ingredients *)
          let new_igdt : ingredients =
            { igdt_query_category=qctg;
              igdt_model_opt=mopt;
              igdt_vc=vc;
              igdt_sym_state=fs} in
          (* 1-3. accumulate new ingredients *)
          let new_acc_igdt_set : ingredients set =
            PSet.add acc_igdt_set new_igdt in
          PMap.add
            acc'
            ~key:fs.ss_entry_mci
            ~data:new_acc_igdt_set
          |> function `Ok mm -> mm | `Duplicate -> Error "generate : refine_targets" |> Stdlib.raise) in
  (* 2. generate invariants from current invariant *)
  let newly_generated_inv : Se.invmap set =
    PMap.fold
      refine_targets
      ~init:PSet.empty
      ~f:(fun ~key ~data acc -> 
          (* select refine function whether it is entry or not *)
          let rf = if (key.mci_cutcat = MCC_trx_entry) then (refine_t (igi_cur_inv, igi_istrg_opt)) else (refine_l igi_cur_inv) in
          (* generate new invariants and accumulate it *)
          PSet.fold
            data
            ~init:acc
            ~f:(fun acc igdt ->
                PSet.union acc (rf igdt))) in
  PSet.diff newly_generated_inv igi_collected
  (* generate function end *)
end