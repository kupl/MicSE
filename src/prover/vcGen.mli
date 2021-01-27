(* Verification Condition Generator (from BasicPath) *)
(* IT CONSIDERS VARIABLE RENAMING *)

(* SUGAR *)
module CPSet = Core.Set.Poly

type query_vc = {
  qvc_fml : ProverLib.Vlang.t;            (* verification condition which should be VALID. formula. *)
  qvc_cat : ProverLib.Bp.query_category;  (* query cateogry *)
  qvc_vtx : PreLib.Cfg.vertex;            (* the vertex-location where the query comes from *)
  qvc_bp  : ProverLib.Bp.t;               (* the basic-path which contains this query *)
}

type v_cond = {
  path_vc : ProverLib.Vlang.t;            (* verification condition which should be SATISFIABLE. formula. *)
  query_vcs : query_vc CPSet.t;           (* queries. see above explanation. *)
}

type v_cond_ingr = ProverLib.Inv.t -> v_cond  (* path-vc and query_vcs will be constructed using the given invariant candidate. *)

module NameEnv : sig
  type t = (string, string) Core.Map.Poly.t

  val new_var : string -> string
end

(* renaming process performed here *)
val construct_verifier_vc : PreLib.Cfg.t -> ProverLib.Bp.t -> PreLib.Adt.data option -> v_cond_ingr

val construct_initstg_vc : ProverLib.GlVar.Env.t ref -> PreLib.Cfg.t -> PreLib.Adt.data option -> (ProverLib.Inv.t -> ProverLib.Vlang.t) (* deprecated *)
