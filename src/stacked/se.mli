(* Symbolic Executer *)

exception Error of string
exception DebugInstSS of (Tz.mich_i Tz.cc * Tz.sym_state)
exception DebugTT of (Tz.mich_t * Tz.mich_t)

type query_category =
  (* Each of them are indicator of "State -> Formula" function *)
  | Q_mutez_add_no_overflow
  | Q_mutez_sub_no_underflow
  | Q_mutez_mul_mnm_no_overflow
  | Q_mutez_mul_nmm_no_overflow
  | Q_shiftleft_safe
  | Q_shiftright_safe
  | Q_assertion

type state_set = {
  running : Tz.sym_state Tz.PSet.t;
  blocked : Tz.sym_state Tz.PSet.t;
  queries : (Tz.sym_state * query_category) Tz.PSet.t;
  terminated : Tz.sym_state Tz.PSet.t;
}

type cache = {
  ch_entered_loop : Tz.mich_cut_info Tz.PSet.t;
  ch_entered_lmbd : Tz.mich_cut_info Tz.PSet.t;
}

type invmap = (Tz.mich_cut_info, (Tz.mich_f Tz.PSet.t)) Tz.PMap.t


(*****************************************************************************)
(*****************************************************************************)
(* Se to Json                                                                *)
(*****************************************************************************)
(*****************************************************************************)

module S2J : sig
  type js = Yojson.Safe.t
  val cv_qc : query_category -> js
  val cv_sset : state_set -> js
  val cv_cache : (cache ref) -> js
end (* module S2J end *)


(*****************************************************************************)
(*****************************************************************************)
(* Utilities - Cache                                                         *)
(*****************************************************************************)
(*****************************************************************************)

val init_cache : unit -> cache ref

val add_entered_loop : cache ref -> Tz.mich_cut_info -> unit
val is_entered_loop : cache ref -> Tz.mich_cut_info -> bool

val add_entered_lmbd : cache ref -> Tz.mich_cut_info -> unit
val is_entered_lmbd : cache ref -> Tz.mich_cut_info -> bool


(*****************************************************************************)
(*****************************************************************************)
(* Utilities - Query                                                         *)
(*****************************************************************************)
(*****************************************************************************)

val state_query_reduce : Tz.sym_state * query_category -> Tz.mich_f


(*****************************************************************************)
(*****************************************************************************)
(* Utilities - State Set Mapping                                             *)
(*****************************************************************************)
(*****************************************************************************)

val map_ss_running : (Tz.sym_state -> Tz.sym_state) -> state_set -> state_set


(*****************************************************************************)
(*****************************************************************************)
(* Run Instruction                                                           *)
(*****************************************************************************)
(*****************************************************************************)

val run_inst : cache ref -> (Tz.mich_i Tz.cc) -> state_set -> state_set
val run_inst_i : cache ref -> (Tz.mich_i Tz.cc) -> Tz.sym_state -> state_set


(*****************************************************************************)
(*****************************************************************************)
(* Utilities - Run Instruction                                               *)
(*****************************************************************************)
(*****************************************************************************)

val run_contract_in_fog : (Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc) -> (Tz.sym_state * (cache ref) * state_set)


(*****************************************************************************)
(*****************************************************************************)
(* Generate Inductiveness & Query Formula                                    *)
(*****************************************************************************)
(*****************************************************************************)

(*****************************************************************************)
(* Coupled mich_cut_info for indicating invariant                            *)
(*****************************************************************************)

val get_normal_mci : invmap -> Tz.mich_cut_info -> Tz.mich_cut_info


(*****************************************************************************)
(* Initial invariant map                                                     *)
(*****************************************************************************)

val true_invmap_of_blocked_sset : Tz.sym_state Tz.PSet.t -> invmap


(*****************************************************************************)
(* Invariant Application form for each mich_cut_category                     *)
(*****************************************************************************)

val extract_typ_stack : Tz.mich_v Tz.cc list -> Tz.mich_t Tz.cc list

val inv_app_guide_vstack : Tz.mich_f Core.Set.Poly.t -> ([`Entry | `Block] * Tz.mich_cut_info option * Tz.mich_v Tz.cc list) -> Tz.mich_f
val inv_app_guide_entry : Tz.mich_f Core.Set.Poly.t -> Tz.sym_state -> Tz.mich_f
val inv_app_guide_block : Tz.mich_f Core.Set.Poly.t -> Tz.sym_state -> Tz.mich_f


(*****************************************************************************)
(* Invariant Map to Michelson Formula                                        *)
(*****************************************************************************)

val find_inv_fmla : invmap -> Tz.mich_cut_info -> Tz.mich_f Core.Set.Poly.t

val inv_induct_fmla_i : Tz.sym_state -> invmap -> Tz.mich_f
val inv_induct_fmla : (Tz.sym_state Tz.PSet.t) -> invmap -> (Tz.mich_f Tz.PSet.t)
val inv_query_fmla : (Tz.sym_state * query_category) -> invmap -> Tz.mich_f
val inv_query_fmla_with_precond : (Tz.sym_state * query_category) -> invmap -> Tz.mich_f -> Tz.mich_f


(*****************************************************************************)
(*****************************************************************************)
(* State Merging Utilities                                                   *)
(*****************************************************************************)
(*****************************************************************************)


(*****************************************************************************)
(* Set Structured Variable Names to Tz/Se Values                             *)
(*****************************************************************************)

val set_stvn_sym_tmpl : (int option * int option) -> (Tz.mich_v Tz.cc -> Tz.mich_v Tz.cc option)
val set_stvn_mv : (int option * int option) -> Tz.mich_v Tz.cc -> Tz.mich_v Tz.cc
val set_stvn_mf : (int option * int option) -> Tz.mich_f -> Tz.mich_f
