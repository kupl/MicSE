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
val bind : 'a -> 'b set -> ('a * 'b) set
(* combination {1; 2} {a; b} === {(1, a); (1, b); (2, a); (2, b)} *)
val combination : 'a set -> 'b set -> ('a * 'b) set
(* combination_rfl {1; 2} === {(1, 1); (1, 2); (2, 2)} *)
val combination_rfl : 'a set -> ('a * 'a) set
(* combination_self_two_diff {1; 2; 3} === {(1, 2); (1, 3); (2, 3)} *)
val combination_self_two_diff : 'a set -> ('a * 'a) set
(* combination_self_two_diff_rf {1; 2; 3} === {(1, 2); (1, 3); (2, 1); (2, 3); (3, 1); (3, 2)} *)
val combination_self_two_diff_rf : 'a set -> ('a * 'a) set


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

val fold_precond : component list -> Tz.mich_f
val comp_of_val : ?precond_list:Tz.mich_f list -> Tz.mich_v Tz.cc -> component
val collect_components : ?precond_list:Tz.mich_f list -> Tz.mich_v Tz.cc -> component set
val filter_comp : (Tz.mich_t -> bool) -> component set -> component set
val classify_comp_with_type : component set -> (Tz.mich_t, component set) map


(*****************************************************************************)
(*****************************************************************************)
(* Recipe                                                                    *)
(*****************************************************************************)
(*****************************************************************************)

val mutez_equal : component set -> Tz.mich_f set
val all_equal : component set -> Tz.mich_f set


(*****************************************************************************)
(*****************************************************************************)
(* Synthesizer                                                               *)
(*****************************************************************************)
(*****************************************************************************)

val refine_t : Se.invmap * (Tz.mich_v Tz.cc * Tz.sym_state) option -> ingredients -> Se.invmap set
val refine_l : Se.invmap -> ingredients -> Se.invmap set

val generate : generate_param -> Se.invmap set