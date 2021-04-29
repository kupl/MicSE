(* Refuter *)


(*****************************************************************************)
(* Set Structured Variable Names to sym_state and state_set                  *)
(*****************************************************************************)

(* "set_stvn_ss" modifies variables following "Refute Target Properties",
  - ("Tz.ss_entry_mci")
  - "Tz.ss_entry_symstack"
  - ("Tz.ss_block_mci")
  - "Tz.ss_symstack"
  - "Tz.ss_constraints"
*)

val set_stvn_ss : (int option * int option) -> Tz.sym_state -> Tz.sym_state
