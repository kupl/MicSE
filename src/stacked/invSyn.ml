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
(* Component Collector                                                       *)
(*****************************************************************************)
(*****************************************************************************)

type component = {
  precond_lst : Tz.mich_f list;
  typ         : Tz.mich_t Tz.cc;
  body        : Tz.mich_v Tz.cc; 
}

let fold_precond : Tz.mich_f list -> Tz.mich_f
= fun prec_lst -> begin
  (* fold_precond function start *)
  Tz.MF_and prec_lst
  (* fold_precond function end *)
end 

let comp_of_val : ?precond_list:Tz.mich_f list -> Tz.mich_v Tz.cc -> component
= fun ?(precond_list=[]) v -> begin
  (* comp_of_val function start *)
  { precond_lst=precond_list; typ=(Tz.typ_of_val v); body=v; }
  (* comp_of_val function end *)
end

let collect_components : ?precond_list:Tz.mich_f list -> Tz.mich_v Tz.cc -> component set
= (* inner function of component collecting *)
  let rec collect_components_i : component -> component set -> component set
  = let open Tz in
    fun cur_comp acc -> begin
    (* collect_components_i function start *)
    let { precond_lst=cur_prec_lst; typ=cur_typ; body=cur_body; } = cur_comp in
    match cur_typ.cc_v with
    | MT_option t1cc -> begin
      let precond_none = (MF_is_none cur_body) :: cur_prec_lst in
      let precond_some = (MF_not (MF_is_none cur_body)) :: cur_prec_lst in
      let body_unlifted = gen_dummy_cc (MV_unlift_option cur_body) in
      let comp_none = { precond_lst=precond_none; typ=cur_typ; body=(gen_dummy_cc (MV_none t1cc)); } in
      let comp_some = { precond_lst=precond_some; typ=cur_typ; body=(gen_dummy_cc (MV_some body_unlifted)); } in
      let comp_some_unlifted = { precond_lst=precond_some; typ=cur_typ; body=body_unlifted; } in
      collect_components_i comp_some_unlifted (PSet.add (PSet.add acc comp_none) comp_some)
      end
    | MT_pair (t1cc, t2cc) -> begin
      let comp_pair = cur_comp in
      let comp_fst = { precond_lst=cur_prec_lst; typ=t1cc; body=(gen_dummy_cc (MV_car cur_body)); } in
      let comp_snd = { precond_lst=cur_prec_lst; typ=t2cc; body=(gen_dummy_cc (MV_cdr cur_body)); } in
      let acc_fst = collect_components_i comp_fst (PSet.add acc comp_pair) in
      collect_components_i comp_snd acc_fst
      end
    | MT_or (t1cc, t2cc) -> begin
      let precond_left = (MF_is_left cur_body) :: cur_prec_lst in
      let precond_right = (MF_not (MF_is_left cur_body)) :: cur_prec_lst in
      let body_left_unlifted = gen_dummy_cc (MV_unlift_left cur_body) in
      let body_right_unlifted = gen_dummy_cc (MV_unlift_right cur_body) in
      let comp_left = { precond_lst=precond_left; typ=cur_typ; body=(gen_dummy_cc (MV_left (cur_typ, body_left_unlifted))); } in
      let comp_right = { precond_lst=precond_right; typ=cur_typ; body=(gen_dummy_cc (MV_right (cur_typ, body_right_unlifted))); } in
      let comp_left_unlifted = { precond_lst=precond_left; typ=t1cc; body=body_left_unlifted; } in
      let comp_right_unlifted = { precond_lst=precond_right; typ=t2cc; body=body_right_unlifted; } in
      let acc_left = collect_components_i comp_left_unlifted (PSet.add (PSet.add acc comp_left) comp_right) in
      collect_components_i comp_right_unlifted acc_left
      end
    | _ -> PSet.add acc cur_comp
    (* collect_components_i function end *)
  end in
  fun ?(precond_list=[]) v_info -> begin
  (* collect_components function start *)
  collect_components_i (comp_of_val v_info ~precond_list) PSet.empty
  (* collect_components function end *)
end

let filter_comp : (Tz.mich_t -> bool) -> component set -> component set
= fun filter_f cset -> begin
  (* filter_comp function start *)
  PSet.filter cset ~f:(fun c -> filter_f c.typ.cc_v)
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
            c.typ.cc_v
            ~f:(function None -> PSet.singleton c | Some cset -> PSet.add cset c)))
  (* classify_comp_with_type function end *)
end


(*****************************************************************************)
(*****************************************************************************)
(* Synthesizer                                                               *)
(*****************************************************************************)
(*****************************************************************************)

let refine_t : Se.invmap * (Tz.mich_v Tz.cc * Tz.sym_state) option -> ingredients -> Se.invmap set
= 
  fun (cur_inv, istrg_opt) igdt -> begin
  (* refine_t function start *)
  let vstrg : Tz.mich_v Tz.cc = 
    PMap.find
      igdt.igdt_sym_state.ss_dynchain.bc_storage
      igdt.igdt_sym_state.ss_optt.optt_addr
    |> function Some vv -> vv | None -> Error "refine_t : vstrg" |> Stdlib.raise in
  let _ = collect_components vstrg in
  let _ = (cur_inv, istrg_opt), igdt in
  PSet.empty (* TODO *)
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