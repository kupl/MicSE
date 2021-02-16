(* TzCvt - Convert for Tz module *)


(*****************************************************************************)
(*****************************************************************************)
(* Michelson to Tz                                                           *)
(*****************************************************************************)
(*****************************************************************************)

module M2T : sig
  val cv_pos : PreLib.Mich.pos -> Tz.ccp_pos
  val cv_loc : PreLib.Mich.loc -> Tz.ccp_loc
  val cv_annot : PreLib.Mich.annot -> Tz.ccp_annot
  val cv_t : 'a PreLib.Mich.t -> 'a Tz.cc
  val cv_typ : PreLib.Mich.typ -> Tz.mich_t
  val cv_typt : PreLib.Mich.typ PreLib.Mich.t -> Tz.mich_t Tz.cc
  val cv_inst : PreLib.Mich.inst -> Tz.mich_i
  val cv_instt : PreLib.Mich.inst PreLib.Mich.t -> Tz.mich_i Tz.cc
  val cv_data : PreLib.Mich.typ PreLib.Mich.t -> PreLib.Mich.data -> Tz.mich_v
  val cv_datat : PreLib.Mich.typ PreLib.Mich.t -> PreLib.Mich.data PreLib.Mich.t -> Tz.mich_v Tz.cc
  val cv_program : PreLib.Mich.program -> (Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc)
end (* module M2T end *)

