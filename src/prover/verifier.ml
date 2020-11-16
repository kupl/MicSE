open ProverLib

exception Not_Implemented_f of Vlang.t
exception Not_Implemented_e of Vlang.Expr.t

let rec smtexpr_of_vlangexpr : Vlang.Expr.t -> Smt.z_expr
= let open Vlang.Expr in
  let soe = smtexpr_of_vlangexpr in (* syntax sugar *)
  let 
  fun ve -> begin
  match ve with
  | V_var (t,v) -> Smt.read_var (Smt.create_symbol v) ()
  | _ -> Smt.create_bool_true (* TODO : erase this line and complete function *)
end

let rec smtexpr_of_vlangformula : Vlang.t -> Smt.z_expr
= let open Vlang.Formula in
  let sof = smtexpr_of_vlangformula in  (* syntax sugar *)
  let soe = smtexpr_of_vlangexpr in (* syntax sugar *)
  let err f = Stdlib.raise (Not_Implemented_f f) in (* syntax sugar *)
  fun vf -> begin
  match vf with
  (* logical formula *)
  | VF_true -> Smt.create_bool_true
  | VF_false -> Smt.create_bool_false
  | VF_not f -> Smt.create_bool_not (sof f)
  | VF_and fl -> Smt.create_bool_and (List.map sof fl)
  | VF_or fl -> Smt.create_bool_or (List.map sof fl)
  | VF_eq (e1,e2) -> Smt.create_bool_eq (soe e1) (soe e2)
  | VF_imply (f1, f2) -> Smt.create_bool_imply (sof f1) (sof f2)
  (* micse-cfg specific boolean *)
  | VF_mich_if e -> Smt.create_bool_eq (soe e) (Smt.create_bool_true)
  | VF_mich_if_none e -> Smt.create_bool_option_is_none (soe e)
  | VF_mich_if_left e -> Smt.create_bool_option_is_left (soe e)
  | VF_mich_if_cons e -> Smt.create_bool_list_is_cons (soe e)
  (* NOT USED. belows are not constructed from Prover.converter *)
  | VF_mich_loop e -> Smt.create_bool_eq (soe e) (Smt.create_bool_true)
  | VF_mich_loop_left e -> Smt.create_bool_option_is_left (soe e)
  | VF_mich_map_l e -> Smt.create_bool_option_is_none (soe e)
  | VF_mich_map_m _ -> err vf (* check whether map's size is not 0 *)
  | VF_mich_iter_l e -> Smt.create_bool_option_is_none (soe e)
  | VF_mich_iter_s _ -> err vf (* check whether set's size is not 0 *)
  | VF_mich_iter_m _ -> err vf (* check whether map's size is not 0 *)
  | VF_mich_micse_check_value e -> Smt.create_bool_eq (soe e) (Smt.create_bool_true)
end (* function smtexpr_of_vlangformula end *)
