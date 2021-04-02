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
(* Synthesizer                                                               *)
(*****************************************************************************)
(*****************************************************************************)

let refine_t : Se.invmap * (Tz.mich_v Tz.cc * Tz.sym_state) option -> ingredients -> Se.invmap set
= 
  fun (cur_inv, istrg_opt) igdt -> begin
  (* refine_t function start *)
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
          let rf = if (key.mci_cutcat = MCC_trx_exit) then (refine_t (igi_cur_inv, igi_istrg_opt)) else (refine_l igi_cur_inv) in
          (* generate new invariants and accumulate it *)
          PSet.fold
            data
            ~init:acc
            ~f:(fun acc igdt ->
                PSet.union acc (rf igdt))) in
  PSet.diff newly_generated_inv igi_collected
  (* generate function end *)
end