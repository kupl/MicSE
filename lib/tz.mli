(* Tz : MicSE's Michelson representation *)

(*****************************************************************************)
(*****************************************************************************)
(* Code Component                                                            *)
(*****************************************************************************)
(*****************************************************************************)

type ccp_pos = {
  col : int;
  lin : int;
}

type ccp_loc =
  | CCLOC_Unknown
  | CCLOC_Pos     of ccp_pos * ccp_pos

type ccp_annot =
  (* :type_annot  *)
  | CCA_typ of string
  (* @var_annot   *)
  | CCA_var of string
  (* %field_annot *)
  | CCA_fld of string

type 'a cc = {
  (* code component *)
  cc_loc : ccp_loc;
  cc_anl : ccp_annot list;
  cc_v : 'a;
}

(*****************************************************************************)
(*****************************************************************************)
(* Tezos Types                                                               *)
(*****************************************************************************)
(*****************************************************************************)

type mich_t = MICH_T_TODO

type mich_v = MICH_V_TODO

type mich_i = MICH_I_TODO

type mich_f = MICH_F_TODO

type program = {
  (* parameter type *)
  pgm_ptyp : mich_t cc;
  (* storage type *)
  pgm_styp : mich_t cc;
  (* code *)
  pgm_code : mich_i cc;
}

(*****************************************************************************)
(*****************************************************************************)
(* Mich to Tz                                                                *)
(*****************************************************************************)
(*****************************************************************************)

module M2T : sig
  val cv_pos : Mich.pos -> ccp_pos

  val cv_loc : Mich.loc -> ccp_loc

  val cv_annot : Mich.annot -> ccp_annot

  val cv_t : 'a Mich.t -> 'a cc

  val cv_typ : Mich.typ -> mich_t

  val cv_typt : Mich.typ Mich.t -> mich_t cc

  val cv_inst : Mich.inst -> mich_i

  val cv_instt : Mich.inst Mich.t -> mich_i cc

  val cv_data : Mich.typ Mich.t -> Mich.data -> mich_v

  val cv_datat : Mich.typ Mich.t -> Mich.data Mich.t -> mich_v cc

  val cv_program : Mich.program -> program
end
(* module M2T end *)
