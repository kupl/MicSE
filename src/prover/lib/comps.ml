(*****************************************************************************)
(*****************************************************************************)
(* Components                                                                *)
(*****************************************************************************)
(*****************************************************************************)

type typ = Vlang.typ
and exp = Vlang.v_exp
and formula = Vlang.v_formula

type t = {
    mutez : component list;
    mutez_map : component list;
  }
and component = exp * typ * approach list
and approach = (formula -> formula)

let empty : t
={ mutez=[]; mutez_map=[] }

let rec serialize : exp -> typ -> t -> t
=fun e ty cur_comps -> begin
  let option_app = (Vlang.create_formula_imply (Vlang.create_formula_is_some e)) in
  let or_left_app = (Vlang.create_formula_imply (Vlang.create_formula_is_left e)) in
  let or_right_app = (Vlang.create_formula_imply (Vlang.create_formula_is_right e)) in
  match ty.d with
  | T_option ty' -> begin
      let e' = Vlang.create_exp_uni_op_un_opt e ty in
      apply_approach (serialize e' ty' cur_comps) option_app
    end
  | T_pair (ty1', ty2') -> begin
      let e1' = Vlang.create_exp_uni_op_un_left e ty in
      let cur_comps' = serialize e1' ty1' cur_comps in
      let e2' = Vlang.create_exp_uni_op_un_right e ty in
      let cur_comps'' = serialize e2' ty2' cur_comps' in
      cur_comps''
    end
  | T_or (ty1', ty2') -> begin
      let e1' = Vlang.create_exp_uni_op_un_left e ty in
      let cur_comps' = apply_approach (serialize e1' ty1' cur_comps) or_left_app in
      let e2' = Vlang.create_exp_uni_op_un_right e ty in
      let cur_comps'' = apply_approach (serialize e2' ty2' cur_comps') or_right_app in
      cur_comps''
    end
  | T_map (_, ty2') -> begin
      match ty2'.d with
      | T_mutez -> { cur_comps with mutez_map=((e, ty, [])::cur_comps.mutez_map)}
      | _ -> cur_comps
    end
  | T_mutez -> { cur_comps with mutez=((e, ty, [])::cur_comps.mutez)}
  | _ -> cur_comps
end

and apply_approach : t -> approach -> t
=fun cur_comps f -> begin
  {
    mutez=(Core.List.map (cur_comps.mutez) ~f:(fun (e, ty, fl) -> (e, ty, (f::fl))));
    mutez_map=(Core.List.map (cur_comps.mutez_map) ~f:(fun (e, ty, fl) -> (e, ty, (f::fl))));
  }
end