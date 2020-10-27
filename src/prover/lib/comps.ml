(*****************************************************************************)
(*****************************************************************************)
(* Components                                                                *)
(*****************************************************************************)
(*****************************************************************************)

type typ = Vlang.typ
and formula = Vlang.v_formula
and obj = Vlang.v_obj

type t = {
    mutez : component list;
    mutez_map : component list;
  }
and component = obj * approach list
and approach = (formula -> formula)

let empty : t
={ mutez=[]; mutez_map=[] }

let rec read_components : obj -> t -> t
=fun o cur_comps -> begin
  let option_app = (Vlang.create_formula_imply (Vlang.create_formula_is_some o)) in
  let or_left_app = (Vlang.create_formula_imply (Vlang.create_formula_is_left o)) in
  let or_right_app = (Vlang.create_formula_imply (Vlang.create_formula_is_right o)) in
  match o.typ.d with
  | T_option ty' -> begin
      let o' = Vlang.create_obj_of_exp ~exp:(Vlang.create_exp_uni_op_un_opt o) ~typ:ty' in
      append_approach (read_components o' cur_comps) option_app
    end
  | T_pair (ty1', ty2') -> begin
      let o1' = Vlang.create_obj_of_exp ~exp:(Vlang.create_exp_uni_op_car o) ~typ:ty1' in
      let cur_comps' = read_components o1' cur_comps in
      let o2' = Vlang.create_obj_of_exp ~exp:(Vlang.create_exp_uni_op_cdr o) ~typ:ty2' in
      let cur_comps'' = read_components o2' cur_comps' in
      cur_comps''
    end
  | T_or (ty1', ty2') -> begin
      let o1' = Vlang.create_obj_of_exp ~exp:(Vlang.create_exp_uni_op_un_left o) ~typ:ty1' in
      let cur_comps' = append_approach (read_components o1' cur_comps) or_left_app in
      let o2' = Vlang.create_obj_of_exp ~exp:(Vlang.create_exp_uni_op_un_right o) ~typ:ty2' in
      let cur_comps'' = append_approach (read_components o2' cur_comps') or_right_app in
      cur_comps''
    end
  | T_map (_, ty2') -> begin
      match ty2'.d with
      | T_mutez -> { cur_comps with mutez_map=((o, [])::cur_comps.mutez_map)}
      | _ -> cur_comps
    end
  | T_mutez -> { cur_comps with mutez=((o, [])::cur_comps.mutez)}
  | _ -> cur_comps
end

and append_approach : t -> approach -> t
=fun cur_comps f -> begin
  {
    mutez=(Core.List.map (cur_comps.mutez) ~f:(fun (o, fl) -> (o, (f::fl))));
    mutez_map=(Core.List.map (cur_comps.mutez_map) ~f:(fun (o, fl) -> (o, (f::fl))));
  }
end