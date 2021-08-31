open Tz

(******************************************************************************)
(******************************************************************************)
(* InnerFirst Mapping                                                         *)
(******************************************************************************)
(******************************************************************************)

(* WARNING : It does not map any mich_v in functions (e.g., values in LAMBDA) *)
val mvcc_map_innerfst : mapf:(mich_v -> mich_v) -> mich_v cc -> mich_v cc

val mf_map_innerfst : mapf:(mich_f -> mich_f) -> mich_f -> mich_f

(******************************************************************************)
(******************************************************************************)
(* Utility Functions for Tz                                                   *)
(******************************************************************************)
(******************************************************************************)

(******************************************************************************)
(* Code Component                                                             *)
(******************************************************************************)

val gen_dummy_cc : 'a -> 'a cc

val gen_custom_cc : 'ccbase cc -> 'a -> 'a cc

(******************************************************************************)
(* Value & Formula Optimizations                                              *)
(******************************************************************************)

val opt_mvcc_rules : mich_v -> mich_v

val opt_mvcc : mich_v cc -> mich_v cc

(******************************************************************************)
(* MV_symbol context swap                                                     *)
(******************************************************************************)

val symbol_context_swap_i : mich_sym_ctxt -> mich_v -> mich_v

val symbol_context_swap : mich_sym_ctxt -> mich_v cc -> mich_v cc

val symbol_trx_image_context_swap : mich_sym_ctxt -> trx_image -> trx_image

val symbol_context_swap_recursive : mich_sym_ctxt -> mich_v cc -> mich_v cc

val symbol_context_swap_michf_recursive : mich_sym_ctxt -> mich_f -> mich_f

(******************************************************************************)
(* Tezos Type                                                                 *)
(******************************************************************************)

val typ_of_val : mich_v cc -> mich_t cc

val get_innertyp : mich_t cc -> mich_t cc

val get_innertyp2 : mich_t cc -> mich_t cc * mich_t cc

(******************************************************************************)
(* Michelson Cut Information                                                  *)
(******************************************************************************)

val lb_of_ln_mci : mich_cut_info -> mich_cut_info option

val lb_of_ln_exn : mich_cut_info -> mich_cut_info

val is_ln_mcc : mich_cut_category -> bool

val is_ln_mci : mich_cut_info -> bool

val ln_of_lb_mci : mich_cut_info -> mich_cut_info option

val ln_of_lb_exn : mich_cut_info -> mich_cut_info

val is_lb_mcc : mich_cut_category -> bool

val is_lb_mci : mich_cut_info -> bool

val exit_of_entry_mci : mich_cut_info -> mich_cut_info option

val exit_of_entry_exn : mich_cut_info -> mich_cut_info

val is_exit_mcc : mich_cut_category -> bool

val is_exit_mci : mich_cut_info -> bool

val entry_of_exit_mci : mich_cut_info -> mich_cut_info option

val entry_of_exit_exn : mich_cut_info -> mich_cut_info

val is_entry_mcc : mich_cut_category -> bool

val is_entry_mci : mich_cut_info -> bool

val get_reduced_mcc : mich_cut_category -> r_mich_cut_category

val get_reduced_mci : mich_cut_info -> r_mich_cut_info

(******************************************************************************)
(******************************************************************************)
(* Michelson to Tz                                                            *)
(******************************************************************************)
(******************************************************************************)

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

  val cv_program : Mich.program -> mich_t cc * mich_t cc * mich_i cc
end
(* module M2T end *)
