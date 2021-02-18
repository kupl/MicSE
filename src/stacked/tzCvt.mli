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


(*****************************************************************************)
(*****************************************************************************)
(* Tz to SMT                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

module T2S : sig
  exception Not_Implemented_f of Tz.mich_f
  exception Not_Implemented_e of (Tz.mich_v Tz.cc)
  exception SMT_Encode_Error_f of (Tz.mich_f * string)
  exception SMT_Encode_Error_e of (Tz.mich_v Tz.cc * string)

  val cv_mt : Tz.mich_t -> ProverLib.Smt.ZSort.t
  val cv_mtcc : Tz.mich_t Tz.cc -> ProverLib.Smt.ZSort.t
  val cv_compare : Tz.mich_v Tz.cc -> Tz.mich_v Tz.cc -> ProverLib.Smt.ZExpr.t
  val cv_mv : Tz.mich_v -> ProverLib.Smt.ZExpr.t
  val cv_mvcc : Tz.mich_v Tz.cc -> ProverLib.Smt.ZExpr.t
  val cv_mf : Tz.mich_f -> ProverLib.Smt.ZFormula.t
end (* module T2S end *)


(*****************************************************************************)
(*****************************************************************************)
(* Tz to Json                                                                *)
(*****************************************************************************)
(*****************************************************************************)

module T2J : sig
  type js = Yojson.Safe.t
  val cv_pos : Tz.ccp_pos -> js
  val cv_loc : Tz.ccp_loc -> js
  val cv_annot : Tz.ccp_annot -> js
  val cv_cc : ('a -> js) -> 'a Tz.cc -> js
  val cv_mt : Tz.mich_t -> js
  val cv_mv : Tz.mich_v -> js
  val cv_mi : Tz.mich_i -> js
  val cv_mtcc : Tz.mich_t Tz.cc -> js
  val cv_mvcc : Tz.mich_v Tz.cc -> js
  val cv_micc : Tz.mich_i Tz.cc -> js
  val cv_mf : Tz.mich_f -> js
end (* module T2J end *)