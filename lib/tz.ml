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
  (* michelson code *)
  pgm_code : mich_i cc;
}

(*****************************************************************************)
(*****************************************************************************)
(* Mich to Tz                                                                *)
(*****************************************************************************)
(*****************************************************************************)

module M2T = struct
  let cv_pos : Mich.pos -> ccp_pos =
    (fun { col = c; lin = l } -> { col = c; lin = l })

  let cv_loc : Mich.loc -> ccp_loc = function
  | Unknown      -> CCLOC_Unknown
  | Pos (p1, p2) -> CCLOC_Pos (cv_pos p1, cv_pos p2)

  let cv_annot : Mich.annot -> ccp_annot = function
  | A_typ s -> CCA_typ s
  | A_var s -> CCA_var s
  | A_fld s -> CCA_fld s

  let cv_t : 'a Mich.t -> 'a cc =
    fun { pos; ann; d } ->
    { cc_loc = cv_loc pos; cc_anl = List.map cv_annot ann; cc_v = d }

  let rec cv_typ : Mich.typ -> mich_t = function
  | _ -> MICH_T_TODO

  and cv_typt : Mich.typ Mich.t -> mich_t cc =
    (fun x -> cv_t { x with d = cv_typ x.d })

  let rec cv_inst : Mich.inst -> mich_i = function
  | _ -> MICH_I_TODO

  and cv_instt : Mich.inst Mich.t -> mich_i cc =
    (fun x -> cv_t { x with d = cv_inst x.d })

  and cv_data : Mich.typ Mich.t -> Mich.data -> mich_v =
    fun tt dd ->
    match (tt.Mich.d, dd) with
    | _ -> MICH_V_TODO

  and cv_datat : Mich.typ Mich.t -> Mich.data Mich.t -> mich_v cc =
    (fun t x -> cv_t { x with d = cv_data t x.d })

  let cv_program : Mich.program -> program =
    fun { param; storage; code } ->
    {
      pgm_ptyp = cv_typt param;
      pgm_styp = cv_typt storage;
      pgm_code = cv_instt code;
    }
end
(* module M2T end *)
