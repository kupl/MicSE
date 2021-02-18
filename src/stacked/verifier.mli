(* Verifier checks whether the given formula valid. *)

type invmap = (Tz.mich_cut_info, ((Tz.mich_v Tz.cc list) -> Tz.mich_f)) Tz.PMap.t

val inv_induct_fmla : (Tz.sym_state Tz.PSet.t) -> invmap -> Tz.mich_f
val inv_query_fmla : (Tz.sym_state * Se.query_category) -> invmap -> Tz.mich_f
