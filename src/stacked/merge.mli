(* Merge Two States *)


(*****************************************************************************)
(*****************************************************************************)
(* Merge States in Intra-Transaction Situation                               *)
(*****************************************************************************)
(*****************************************************************************)

type ms_iter_info = {
  (* iteration information for the function "intratrx-merge-state" *)
  mii_iter_iv : (Tz.mich_cut_info, Tz.mich_v Tz.cc list) Tz.PMap.t;  (* input-var-info for ITER instruction // MCC_lb_iter *)
  (* "mii_map_iov" : (single-ivar holder) * (single-ovar holder) * io-var-info for MAP instruction // MCC_lb_map *)
  mii_map_iov : (Tz.mich_cut_info, (Tz.mich_v Tz.cc option * Tz.mich_v Tz.cc option * ((Tz.mich_v Tz.cc * Tz.mich_v Tz.cc) list))) Tz.PMap.t; 
  mii_map_accv : (Tz.mich_cut_info, Tz.mich_v Tz.cc) Tz.PMap.t; (* result-var-info for MAP instruction // MCC_ln_map *)
  mii_iter_ef : Tz.mich_cut_info Tz.PSet.t; (* ending-included-flag for ITER instruction // MCC_ln_iter // If exists, then true, else false *)
  mii_map_ef : Tz.mich_cut_info Tz.PSet.t; (* ending-included-flag for MAP instruction // MCC_ln_map // If exists, then true, else false *)
}

val empty_ms_iter_info : ms_iter_info

val intratrx_merge_state : Tz.sym_state -> (Tz.sym_state * ms_iter_info) -> (Tz.sym_state * ms_iter_info)