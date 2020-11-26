(*****************************************************************************)
(*****************************************************************************)
(* Components                                                                *)
(*****************************************************************************)
(*****************************************************************************)

type typ = Vlang.Ty.t
and formula = Vlang.v_formula
and expr = Vlang.Expr.t

type t = {
    mutez : component list;
    mutez_map : component list;
  }
and component = expr * approach list
and approach = (formula -> formula)

let empty : t
={ mutez=[]; mutez_map=[] }

let rec read_components : expr -> t -> t
=fun e cur_comps -> begin
  let imply_func f1 f2 = Vlang.Formula.VF_imply (f1, f2) in
  let option_app = (imply_func (VF_not (VF_mich_if_none e))) in
  let or_left_app = (imply_func (VF_mich_if_left e)) in
  let or_right_app = (imply_func (VF_not (VF_mich_if_left e))) in
  match (e |> Vlang.TypeUtil.ty_of_expr) with
  | T_option _ -> begin
      let e' = Vlang.Expr.V_unlift_option e in
      append_approach (read_components e' cur_comps) option_app
    end
  | T_pair _ -> begin
      let e1' = Vlang.Expr.V_car e in
      let cur_comps' = read_components e1' cur_comps in
      let e2' = Vlang.Expr.V_cdr e in
      let cur_comps'' = read_components e2' cur_comps' in
      cur_comps''
    end
  | T_or _ -> begin
      let e1' = Vlang.Expr.V_unlift_left e in
      let cur_comps' = append_approach (read_components e1' cur_comps) or_left_app in
      let e2' = Vlang.Expr.V_unlift_right e in
      let cur_comps'' = append_approach (read_components e2' cur_comps') or_right_app in
      cur_comps''
    end
  | T_map (_, ty2') -> begin
      match ty2' with
      | T_mutez -> { cur_comps with mutez_map=((e, [])::cur_comps.mutez_map)}
      | _ -> cur_comps
    end
  | T_mutez -> { cur_comps with mutez=((e, [])::cur_comps.mutez)}
  | _ -> cur_comps
end

and append_approach : t -> approach -> t
=fun cur_comps f -> begin
  {
    mutez=(Core.List.map (cur_comps.mutez) ~f:(fun (o, fl) -> (o, (f::fl))));
    mutez_map=(Core.List.map (cur_comps.mutez_map) ~f:(fun (o, fl) -> (o, (f::fl))));
  }
end
