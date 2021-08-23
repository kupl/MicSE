(* Common Module Types *)

(* [Necessity]
   Containers in the library Core requires various comparable-sexp-modules
   for Make functor, for example Core.Set.Make or Core.Map.Make requires
   module which satisfies Set_intf.Elt or Map.intf.Key module types.

   The problem arises when defining such Set or Map modules on demand.
   For example, the module "Se" requires set of "Tz.sym_state".
   If that set is defined like
     "module SSet = Core.Set.Make(struct type t = Tz.sym_state [@@deriving sexp, compare] end)"
   in "Se.ml", every other module should treat that set value using
   "Se.SSet" module only.

   Otherwise compiler will emit type error, for example,
   if such set is defined like
     "module SSSet = Core.Set.Make(struct type t = Tz.sym_state [@@deriving sexp, compare] end)"
   again in "ExecFlow.ml", then the compiler can show following error message.

     This expression has type
       Se.SSet.t =
         (Tz.sym_state, Se.SSet.Elt.comparator_witness) Base.Set.t
     but an expression was expected of type
       k = (Tz.sym_state, SSSet.Elt.comparator_witness) Base.Set.t
     Type Se.SSet.Elt.comparator_witness is not compatible with type
       SSSet.Elt.comparator_witness

   To avoid above situation, it is required to define element (or key) type's
   module in the module where the type defined. Here the necessity of this
   file, a file for definitions of common module types, appears.

   Two module types, Set_inf.Elt and Map.inf.Key are same, and no one wants
   to describe their element/key module's type can only be used for set's
   element or map's key.
*)

module type Core_CMP = sig
  type t

  val compare : t -> t -> Core_kernel__.Import.int

  val t_of_sexp : Sexplib0.Sexp.t -> t

  val sexp_of_t : t -> Sexplib0.Sexp.t
end
