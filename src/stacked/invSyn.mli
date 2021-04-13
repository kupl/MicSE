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

module TMap = TComparable.Map

type 'a tmap = 'a TMap.t


(*****************************************************************************)
(*****************************************************************************)
(* List Combination                                                          *)
(*****************************************************************************)
(*****************************************************************************)

(* bind 1 {1; 2; 3} === {(1, 1); (1, 2); (1, 3)} *)
val bind : 'a -> 'b list -> ('a * 'b) list
(* combination {1; 2} {a; b} === {(1, a); (1, b); (2, a); (2, b)} *)
val combination : 'a list -> 'b list -> ('a * 'b) list
(* combination_rfl {1; 2} === {(1, 1); (1, 2); (2, 2)} *)
val combination_rfl : 'a list -> ('a * 'a) list
(* combination_self_two_diff {1; 2; 3} === {(1, 2); (1, 3); (2, 3)} *)
val combination_self_two_diff : 'a list -> ('a * 'a) list
(* combination_self_two_diff_rf {1; 2; 3} === {(1, 2); (1, 3); (2, 1); (2, 3); (3, 1); (3, 2)} *)
val combination_self_two_diff_rf : 'a list -> ('a * 'a) list


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

val bake_comp_map : Se.state_set -> comp_map
val fold_precond : vstack -> component list -> Tz.mich_f
val get_value : vstack -> component -> Tz.mich_v Tz.cc


(*****************************************************************************)
(*****************************************************************************)
(* Recipe                                                                    *)
(*****************************************************************************)
(*****************************************************************************)

type invariant = vstack -> Tz.mich_f

val mutez_equal : component list tmap -> invariant list
val all_equal : component list tmap -> invariant list


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

val refine_t : Se.invmap * (Tz.mich_v Tz.cc * Tz.sym_state) option -> ingredients -> Se.invmap list
val refine_l : Se.invmap -> ingredients -> Se.invmap list

val generate : generate_param -> Se.invmap list