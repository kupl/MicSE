(* Sexp - Sexpression of Tz *)


(*****************************************************************************)
(*****************************************************************************)
(* Tz to Core.Sexp.t                                                         *)
(*****************************************************************************)
(*****************************************************************************)

module T2CS : sig
  type csexp = Core.Sexp.t

  val gen_template : string -> csexp -> csexp

  val cv_pos : Tz.ccp_pos -> csexp
  val cv_loc : Tz.ccp_loc -> csexp
  val cv_annot : Tz.ccp_annot -> csexp
  val cv_cc : ('a -> csexp) -> 'a Tz.cc -> csexp

  val cv_mt : Tz.mich_t -> csexp
  val cv_mv : Tz.mich_v -> csexp
  val cv_mi : Tz.mich_i -> csexp
  val cv_mtcc: Tz.mich_t Tz.cc -> csexp
  val cv_mvcc : Tz.mich_v Tz.cc -> csexp
  val cv_micc : Tz.mich_i Tz.cc -> csexp

  val cv_mf : Tz.mich_f -> csexp

  val cv_mich_cut_category : Tz.mich_cut_category -> csexp
  val cv_mich_cut_info : Tz.mich_cut_info -> csexp
end


(*****************************************************************************)
(*****************************************************************************)
(* Tz to Core.Sexp.t (No CC)                                                 *)
(*****************************************************************************)
(*****************************************************************************)

module T2CSnocc : sig
  type csexp = Core.Sexp.t

  val gen_template : string -> csexp -> csexp

  val cv_pos : Tz.ccp_pos -> csexp
  val cv_loc : Tz.ccp_loc -> csexp
  val cv_annot : Tz.ccp_annot -> csexp
  val cv_cc : ('a -> csexp) -> 'a Tz.cc -> csexp

  val cv_mt : Tz.mich_t -> csexp
  val cv_mv : Tz.mich_v -> csexp
  val cv_mi : Tz.mich_i -> csexp
  val cv_mtcc: Tz.mich_t Tz.cc -> csexp
  val cv_mvcc : Tz.mich_v Tz.cc -> csexp
  val cv_micc : Tz.mich_i Tz.cc -> csexp

  val cv_mf : Tz.mich_f -> csexp

  val cv_mich_cut_category : Tz.mich_cut_category -> csexp
  val cv_mich_cut_info : Tz.mich_cut_info -> csexp
end


(*****************************************************************************)
(*****************************************************************************)
(* Core.Sexp.t to Tz                                                         *)
(*****************************************************************************)
(*****************************************************************************)

module CS2Tnocc : sig
  exception Error of string

  type csexp = Core.Sexp.t

  val get_body_in_atom_exn : csexp -> string
  val get_body_in_list_exn : csexp -> csexp list
  val get_template_type_exn : csexp -> string
  val get_template_body_exn : string -> csexp -> csexp

  val cv_pos : csexp -> Tz.ccp_pos
  val cv_loc : csexp -> Tz.ccp_loc
  val cv_annot : csexp -> Tz.ccp_annot

  val cv_mt : csexp -> Tz.mich_t
  val cv_mv : csexp -> Tz.mich_v
  val cv_mi : csexp -> Tz.mich_i
  val cv_mtcc : csexp -> Tz.mich_t Tz.cc
  val cv_mvcc : csexp -> Tz.mich_v Tz.cc
  val cv_micc : csexp -> Tz.mich_i Tz.cc
  val cv_mf : csexp -> Tz.mich_f

  val cv_mich_cut_category : csexp -> Tz.mich_cut_category
  val cv_mich_cut_info : csexp -> Tz.mich_cut_info
end
