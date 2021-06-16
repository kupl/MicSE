(* TzCvt - Convert for Tz module *)


(*****************************************************************************)
(*****************************************************************************)
(* Michelson to Tz                                                           *)
(*****************************************************************************)
(*****************************************************************************)

module M2T : sig
  val cv_pos : Mich.pos -> Tz.ccp_pos
  val cv_loc : Mich.loc -> Tz.ccp_loc
  val cv_annot : Mich.annot -> Tz.ccp_annot
  val cv_t : 'a Mich.t -> 'a Tz.cc
  val cv_typ : Mich.typ -> Tz.mich_t
  val cv_typt : Mich.typ Mich.t -> Tz.mich_t Tz.cc
  val cv_inst : Mich.inst -> Tz.mich_i
  val cv_instt : Mich.inst Mich.t -> Tz.mich_i Tz.cc
  val cv_data : Mich.typ Mich.t -> Mich.data -> Tz.mich_v
  val cv_datat : Mich.typ Mich.t -> Mich.data Mich.t -> Tz.mich_v Tz.cc
  val cv_program : Mich.program -> (Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc)
end (* module M2T end *)


(*****************************************************************************)
(*****************************************************************************)
(* Tz to Abbreviation                                                        *)
(*****************************************************************************)
(*****************************************************************************)

module T2A : sig
  val cv_mt : Tz.mich_t -> string list
  val cv_mtcc : Tz.mich_t Tz.cc -> string list
  val cv_mv : Tz.mich_v -> string list
  val cv_mvcc : Tz.mich_v Tz.cc -> string list
end


(*****************************************************************************)
(*****************************************************************************)
(* Tz to SMT                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

module T2S : sig
  exception Not_Implemented_f of Tz.mich_f
  exception Not_Implemented_e of (Tz.mich_v Tz.cc)
  exception SMT_Encode_Error_f of (Tz.mich_f * string * int)
  exception SMT_Encode_Error_e of (Tz.mich_v Tz.cc * string * int)

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

  val cv_bc : Tz.blockchain -> js
  val cv_exop : Tz.explicit_operation -> js
  val cv_oper_transfertoken : Tz.oper_transfertoken -> js
  val cv_mich_cut_category : Tz.mich_cut_category -> js
  val cv_mich_cut_info : Tz.mich_cut_info -> js
  val cv_ss : Tz.sym_state -> js

  val cv_p1_ss_strop : Tz.sym_state -> js
  val cv_p1_ss_path : Tz.sym_state -> js
end (* module T2J end *)


(*****************************************************************************)
(*****************************************************************************)
(* Tz to Json (No CC for mich_t, mich_v, mich_i, and mich_f)                 *)
(*****************************************************************************)
(*****************************************************************************)

module T2Jnocc : sig
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

  val cv_bc : Tz.blockchain -> js
  val cv_exop : Tz.explicit_operation -> js
  val cv_oper_transfertoken : Tz.oper_transfertoken -> js
  val cv_mich_cut_category : Tz.mich_cut_category -> js
  val cv_mich_cut_info : Tz.mich_cut_info -> js
  val cv_ss : Tz.sym_state -> js

  val cv_p1_ss_strop : Tz.sym_state -> js
  val cv_p1_ss_path : Tz.sym_state -> js
end (* moduel T2Jnocc end *)
