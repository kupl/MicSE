(* Refuter *)

(* IMPORTANT NOTE
  Every concepts in "Se" module were designed with forward symbolic execution in mind.
  So, well-constructed "Se.sym_state" and "Se.state_set" contains 
  various Tezos system abstraction such as fixed/dynamic blockchain status
  and internal operation queues (though queue will be changed into stack for later
  Tezos version).
  
  However, this "Refute" module uses state-merging (module "Merge") to construct
  symbolic-executed results from back to forward, it is impossible to form soundly
  blockchain-specific abstractions when merging two different transaction states.

  Therefore, "Merge" and "Refute" modules will not strictly follows the Tezos
  abstraction defined in "Se".
   
  Instead, they will use following Tezos/Michelson abstractions only:
  [ Refute Target Properties ]
  - "Tz.ss_entry_mci", "Tz.ss_entry_symstack", "Tz.ss_block_mci", "Tz.ss_symstack", "Tz.ss_constraints"
  - "Se.query_category"
  - "Jc.Rcfv" to track non-michelson contexts
  - "Jc.Stvn" to avoid variable name conflicts when merging two states
*)


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

let set_stvn_ss : (int option * int option) -> Tz.sym_state -> Tz.sym_state
= let open Tz in
  fun (tnopt, lnopt) ss -> begin
  let stvn_s : mich_v cc -> mich_v cc = Se.set_stvn_mv (tnopt, lnopt) in
  let stvn_f : mich_f -> mich_f = Se.set_stvn_mf (tnopt, lnopt) in
  {ss with
    ss_entry_symstack = List.map stvn_s ss.ss_entry_symstack;
    ss_symstack = List.map stvn_s ss.ss_symstack;
    ss_constraints = List.map stvn_f ss.ss_constraints;
  }
end (* function set_stvn_ss end *)
