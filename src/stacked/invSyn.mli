(* Invariant Synthesizer *)

exception Error of string


(******************************************************************************)
(******************************************************************************)
(* Set Combination                                                            *)
(******************************************************************************)
(******************************************************************************)

(* bind 1 {1; 2; 3} === {(1, 1); (1, 2); (1, 3)} *)
val bind : 'a -> 'b Core.Set.Poly.t -> ('a * 'b) Core.Set.Poly.t
(* combination {1; 2} {a; b} === {(1, a); (1, b); (2, a); (2, b)} *)
val combination : 'a Core.Set.Poly.t -> 'b Core.Set.Poly.t -> ('a * 'b) Core.Set.Poly.t
(* combination_rfl {1; 2} === {(1, 1); (1, 2); (2, 2)} *)
val combination_rfl : 'a Core.Set.Poly.t -> ('a * 'a) Core.Set.Poly.t
(* combination_self_two_diff {1; 2; 3} === {(1, 2); (1, 3); (2, 3)} *)
val combination_self_two_diff : 'a Core.Set.Poly.t -> ('a * 'a) Core.Set.Poly.t
(* combination_self_two_diff_rf {1; 2; 3} === {(1, 2); (1, 3); (2, 1); (2, 3); (3, 1); (3, 2)} *)
val combination_self_two_diff_rf : 'a Core.Set.Poly.t -> ('a * 'a) Core.Set.Poly.t


(******************************************************************************)
(******************************************************************************)
(* Component Collector                                                        *)
(******************************************************************************)
(******************************************************************************)

  (*****************************************************************************
    The type comp_map is a pre-baked component map.
    Function bake_comp_map makes a set of components from the type stack of each MCI.
    The component set which is the value of comp_map is used to make a set of new invariants by recipe.
    type comp_map = MCI |-> (mich_t |-> component set)
  *****************************************************************************)
type comp_map = (Tz.mich_cut_info, (Comp.t Core.Set.Poly.t) Comp.CTMap.t) Core.Map.Poly.t

val bake_comp_map : Se.state_set * ((Tz.mich_v Tz.cc) option * Tz.sym_state) -> comp_map
(* val init_invmap : comp_map -> Tz.sym_state -> Se.invmap -> Se.invmap *)

(*****************************************************************************)
(*****************************************************************************)
(* Recipe                                                                    *)
(*****************************************************************************)
(*****************************************************************************)

val all_equal : (Comp.t Core.Set.Poly.t) Comp.CTMap.t -> Tz.mich_f Core.Set.Poly.t
val all_ge : (Comp.t Core.Set.Poly.t) Comp.CTMap.t -> Tz.mich_t list -> Tz.mich_f Core.Set.Poly.t
val all_gt : (Comp.t Core.Set.Poly.t) Comp.CTMap.t -> Tz.mich_t list -> Tz.mich_f Core.Set.Poly.t
val add_2_eq : (Comp.t Core.Set.Poly.t) Comp.CTMap.t -> Tz.mich_t list -> Tz.mich_f Core.Set.Poly.t


(*****************************************************************************)
(*****************************************************************************)
(* Synthesizer                                                               *)
(*****************************************************************************)
(*****************************************************************************)

type generate_param =
  (* igi_failed_set *)  ((Tz.sym_state * Se.query_category) * (ProverLib.Smt.ZSolver.validity * ProverLib.Smt.ZModel.t option) * Tz.mich_f * Utils.Timer.time) Core.Set.Poly.t *
  (* igi_cur_inv *)     Se.invmap *
  (* igi_comp_map *)    comp_map *
  (* igi_collected *)   Se.invmap Core.Set.Poly.t

type ingredients = {
  igdt_mci            : Tz.mich_cut_info;
  igdt_comp_type_map  : (Comp.t Core.Set.Poly.t) Comp.CTMap.t
}

val collect_set : ('a Core.Set.Poly.t) list -> 'a Core.Set.Poly.t
val refine_t : Se.invmap -> ingredients -> Tz.mich_f Core.Set.Poly.t
val refine_l : Se.invmap -> ingredients -> Tz.mich_f Core.Set.Poly.t

val gen_igdt : comp_map -> Tz.mich_cut_info -> ingredients
val gen_cand : Se.invmap -> ingredients -> Tz.mich_f Core.Set.Poly.t
val update_cand : (Tz.mich_cut_info, Tz.mich_f Core.Set.Poly.t) Core.Map.Poly.t -> Tz.mich_cut_info -> Tz.mich_f Core.Set.Poly.t -> (Tz.mich_cut_info, Tz.mich_f Core.Set.Poly.t) Core.Map.Poly.t
val combinate_invmap : Se.invmap -> (Tz.mich_cut_info, Tz.mich_f Core.Set.Poly.t) Core.Map.Poly.t -> Tz.mich_cut_info Core.Set.Poly.t -> Se.invmap Core.Set.Poly.t

val generate : generate_param -> Se.invmap Core.Set.Poly.t