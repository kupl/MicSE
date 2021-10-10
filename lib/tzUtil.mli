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
(* InnerFirst Folding                                                         *)
(******************************************************************************)
(******************************************************************************)

(* Simple process like context-swapping uses mapping only, but since our
   Integer/NaturalNum/Mutez value requires appropriate constraints
   we need to collect some constraints (e.g., MF_mutez_bound) everytime
   new mutez/nat symbols comes out to the surface by optimization process.
   That's why fold function required.
*)

val mvcc_fold_innerfst :
  f:('a * mich_v cc -> 'a * mich_v cc) -> acc:'a -> mich_v cc -> 'a * mich_v cc

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
(* MV_symbol context swap                                                     *)
(******************************************************************************)

val gen_mich_v_ctx : ctx:mich_sym_ctxt -> mich_v cc -> mich_v_cc_ctx

val symbol_context_swap_michf_recursive : ctx:mich_sym_ctxt -> mich_f -> mich_f

val sym_state_symbol_context_swap : ctx:mich_sym_ctxt -> sym_state -> sym_state

(******************************************************************************)
(* Tezos Type                                                                 *)
(******************************************************************************)

val typ_of_val : mich_v cc -> mich_t cc

val get_innertyp : mich_t cc -> mich_t cc

val get_innertyp2 : mich_t cc -> mich_t cc * mich_t cc

(******************************************************************************)
(* Value & Formula Optimizations                                              *)
(******************************************************************************)

val opt_mf_rules : mich_f -> mich_f

val opt_mf : mich_f -> mich_f

val mvcc_subst_mf_rules :
  mapf:(mich_v_cc_ctx -> mich_v_cc_ctx) -> mich_f -> mich_f

val mvcc_subst_mf : mapf:(mich_v_cc_ctx -> mich_v_cc_ctx) -> mich_f -> mich_f

val mtz_constriant_if_it_is_or_true :
  ctx:mich_sym_ctxt -> tv:mich_t cc * mich_v cc -> mich_f

val nat_constriant_if_it_is_or_true :
  ctx:mich_sym_ctxt -> tv:mich_t cc * mich_v cc -> mich_f

val opt_mvcc_rules :
  ctx:mich_sym_ctxt -> mich_f list * mich_v cc -> mich_f list * mich_v cc

val opt_mvcc : ctx:mich_sym_ctxt -> mich_v cc -> mich_f list * mich_v cc

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

val qid_of_rmci_exn : r_mich_cut_info -> qid

val qid_of_mci_exn : mich_cut_info -> qid

(******************************************************************************)
(* Literals in Tz-Code                                                        *)
(******************************************************************************)

module MVSet : module type of Core.Set.Make (MichVCC_cmp)

val scrap_code_literals : mich_i cc -> MVSet.t

(******************************************************************************)
(* List                                                                       *)
(******************************************************************************)

val v_of_list :
  ctx:mich_sym_ctxt -> mich_v cc -> mich_f list * mich_v cc list * mich_v cc

(******************************************************************************)
(* Operation                                                                  *)
(******************************************************************************)

val mtz_of_op : ctx:mich_sym_ctxt -> mich_v cc -> mich_f list * mich_v cc option

(******************************************************************************)
(* Sigma                                                                      *)
(******************************************************************************)

val is_sigma : mich_v cc -> bool

val sigma_of_cont : mich_v cc -> mich_v cc list

val acc_of_sigma :
  sigma:mich_v cc -> ctx:mich_sym_ctxt -> mich_v cc -> mich_f list * mich_v cc

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
