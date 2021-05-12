(* Invariant Synthesizer *)

exception Error of string


(******************************************************************************)
(******************************************************************************)
(* Common Datatypes                                                           *)
(******************************************************************************)
(******************************************************************************)

module TComparable : sig
  exception Error of string

  module T : sig
    type t = Tz.mich_t Tz.cc
    val compare : t -> t -> int
    val t_of_sexp : Core.Sexp.t -> t
    val sexp_of_t : t -> Core.Sexp.t
  end

  include Core.Comparable
end

module CTMap = TComparable.Map


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

val bake_comp_map : Se.state_set * ((Tz.mich_v Tz.cc) option * Tz.sym_state) -> comp_map
val fold_precond : component list -> Tz.mich_f


(*****************************************************************************)
(*****************************************************************************)
(* Recipe                                                                    *)
(*****************************************************************************)
(*****************************************************************************)

val all_equal : (component Core.Set.Poly.t) CTMap.t -> Tz.mich_f Core.Set.Poly.t
val all_ge : (component Core.Set.Poly.t) CTMap.t -> Tz.mich_t list -> Tz.mich_f Core.Set.Poly.t
val all_gt : (component Core.Set.Poly.t) CTMap.t -> Tz.mich_t list -> Tz.mich_f Core.Set.Poly.t


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
  igdt_query_category : Se.query_category;
  igdt_model_opt      : ProverLib.Smt.ZModel.t option;
  igdt_vc             : Tz.mich_f;
  igdt_sym_state      : Tz.sym_state;
  igdt_comp_type_map  : (component Core.Set.Poly.t) CTMap.t
}

val collect_set : ('a Core.Set.Poly.t) list -> 'a Core.Set.Poly.t
val refine_t : Se.invmap -> ingredients -> Se.invmap Core.Set.Poly.t
val refine_l : Se.invmap -> ingredients -> Se.invmap Core.Set.Poly.t

val generate : generate_param -> Se.invmap Core.Set.Poly.t