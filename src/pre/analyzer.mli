(*****************************************************************************)
(*****************************************************************************)
(* Available Variable Analysis                                               *)
(*****************************************************************************)
(*****************************************************************************)

(* For each vertex, get the information of which variables are defined "before" and "after" execute that vertex. *)
(* Because it does consider whether the variable comes from *)

module AvailVar : sig
  type vertex = PreLib.Cfg.vertex
  module CPSet : module type of Core.Set.Poly
  module CPMap : module type of Core.Map.Poly

  (* type t = (vertex |-> (available variable "before" execute the stmt) & (available variables "after" execute the stmt)) mapping *)
  type abs_set = Top | S of string CPSet.t  (* variable set - abstract domain *)
  val abs_set_join : abs_set -> abs_set -> abs_set
  val abs_set_meet : abs_set -> abs_set -> abs_set
  val abs_set_eq : abs_set -> abs_set -> bool
  val abs_set_concr : abs_set -> string CPSet.t (* concretize the set. it converts "Top" to "CPSet.empty" *)

  type t = (int, abs_set * abs_set) CPMap.t
  type worklist = vertex CPSet.t

  val run: PreLib.Cfg.t -> t

end (* module AvailVar end *)