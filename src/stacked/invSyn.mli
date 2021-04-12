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

val bake_comp_map : Se.state_set -> comp_map
val fold_precond : vstack -> component list -> Tz.mich_f
val filter_comp : (Tz.mich_t -> bool) -> component set -> component set
val classify_comp_with_type : component set -> (Tz.mich_t, component set) map


(*****************************************************************************)
(*****************************************************************************)
(* Recipe                                                                    *)
(*****************************************************************************)
(*****************************************************************************)

type invariant = vstack -> Tz.mich_f

val mutez_equal : component set -> invariant set
val all_equal : component set -> invariant set


(*****************************************************************************)
(*****************************************************************************)
(* Synthesizer                                                               *)
(*****************************************************************************)
(*****************************************************************************)

val collect_set : ('a set) list -> 'a set

val refine_t : Se.invmap * (Tz.mich_v Tz.cc * Tz.sym_state) option -> ingredients -> Se.invmap set
val refine_l : Se.invmap -> ingredients -> Se.invmap set

val generate : generate_param -> Se.invmap set