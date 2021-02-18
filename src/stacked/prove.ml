(* Prover *)


(*****************************************************************************)
(*****************************************************************************)
(* Utilities                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

let gen_sset : PreLib.Adt.t -> (PreLib.Adt.data option) -> ((Tz.mich_v Tz.cc option) * Tz.sym_state * (Se.cache ref) * Se.state_set)
= fun mich_program mich_init_stg_opt -> begin
  (* Prepare Michelson program and initial-storage if needed *)
  let mich_program : PreLib.Mich.program = 
    mich_program
    |> PreLib.Mich.subst_standard_macro_all_pgm
    |> PreLib.Mich.optm_all_pgm
  in
  (* Convert them to Tz form *)
  let (param_typ, storage_typ, inst) : (Tz.mich_t Tz.cc * Tz.mich_t Tz.cc * Tz.mich_i Tz.cc) =
    TzCvt.M2T.cv_program mich_program in
  let tz_init_stg_opt : Tz.mich_v Tz.cc option =
    Option.bind mich_init_stg_opt (fun x -> Some (TzCvt.M2T.cv_datat mich_program.storage x)) in
  (* Symbolic Execution - Collect Path Conditions & Queries *)
  let (init_ss, cache, sset) = 
    Se.run_contract_in_fog (param_typ, storage_typ, inst) in
  (tz_init_stg_opt, init_ss, cache, sset)
end (* function gen_sset end *)


(*****************************************************************************)
(*****************************************************************************)
(* Small functionalities, returns unit value only                            *)
(*****************************************************************************)
(*****************************************************************************)

let f_count_sset : Se.state_set -> unit
= let sz s = Tz.PSet.length s in
  fun {running; blocked; queries; terminated;} -> begin
  Printf.printf "#running=%d, #blocked=%d, #queries=%d, #terminated=%d\n" (sz running) (sz blocked) (sz queries) (sz terminated)
end (* function f_count_sset end *)

