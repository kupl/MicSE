module CPSet = Core.Set.Poly (* sugar *)

(* WARNING: "powerset" explodes easily. *)
let powerset : 'a CPSet.t -> ('a CPSet.t) CPSet.t
=fun set -> begin
  CPSet.fold
    set
    ~init:(CPSet.singleton CPSet.empty)
    ~f:(fun accs elem -> CPSet.fold accs ~init:accs ~f:(fun accs subset -> CPSet.add accs (CPSet.add subset elem)))
end (* function powerset end *)

(* "combination_rfl" : combination reflexivity-applied.
  combination_rfl {1,2} === {(1,1); (1,2); (2,2)}
*)
let combination_rfl : 'a CPSet.t -> ('a * 'a) CPSet.t
=fun set -> begin
  let rec foldf : 'a list -> ('a * 'a) CPSet.t -> ('a * 'a) CPSet.t
  =fun lst accset -> begin
    match lst with
    | [] -> accset
    | h :: tl -> 
      let newacc = List.fold_left (fun accs e -> CPSet.add accs (h,e)) accset lst in
      foldf tl newacc
  end in
  foldf (CPSet.to_list set) CPSet.empty
end (* function combination_rfl end *)

(* combination {1;2} {1;2} === {(1,1); (1,2); (2,1); (2,2)} *)
let combination : 'a CPSet.t -> 'b CPSet.t -> ('a * 'b) CPSet.t
=fun aset bset -> begin
  CPSet.fold
    aset
    ~init:CPSet.empty
    ~f:(fun accs a -> CPSet.fold bset ~init:accs ~f:(fun accs' b -> CPSet.add accs' (a,b)))
end (* function combination end *)

(* combination_self_two_diff [1;2;3] === [(1,2); (1,3); (2,3)] (* order can be shuffled *) *)
let combination_self_two_diff : 'a list -> ('a * 'a) list
=fun lst -> begin
  let rec foldf : ('a * 'a) list -> 'a list -> ('a * 'a) list
  =fun accl rl -> begin
    match rl with
    | [] -> accl 
    | _ :: [] -> accl
    | h :: t -> foldf ((List.map (fun x -> (h, x)) t) @ accl) t
  end in
  foldf [] lst
end (* function combination_self_two_diff end *)

(* combination_self_two_diff_rf [1;2;3] === [(1,2); (1,3); (2,3); (2,1); (3,1); (3,2)] (* order can be shuffled *) *)
let combination_self_two_diff_rf : 'a list -> ('a * 'a) list
= fun lst -> begin
  let l = (combination_self_two_diff lst) in
  List.fold_left (fun accl (x,y) -> (y,x) :: accl) l l
end (* function combination_self_two_diff_rf end *)

let combination_self_two_diff_rf_set : 'a CPSet.t -> ('a * 'a) CPSet.t
=fun s -> begin
  combination_self_two_diff_rf (CPSet.to_list s)
  |> CPSet.of_list
end (* function combination_self_two_diff_rf_set end *)


(* "formula_mutez_equal" : every component should be mutez type. *)
let mutez_equal : Vlang.Component.t -> Vlang.Formula.t CPSet.t
= let open Vlang in
  let open Formula in
  fun compset -> begin
  let cb : (Component.comp * Component.comp) CPSet.t = combination_self_two_diff (CPSet.to_list compset) |> CPSet.of_list in
  CPSet.map cb ~f:(fun (c1, c2) -> VF_imply (Component.fold_preconds [c1;c2], VF_eq (c1.body, c2.body)))
end (* function formula_mutez_equal end *)

(* "mutez_ge" : every component should be mutez type. *)
let mutez_ge : Vlang.Component.t -> Vlang.Formula.t CPSet.t
= let open Vlang in
  let open Formula in
  fun compset -> begin
  let cb : (Component.comp * Component.comp) CPSet.t = combination compset compset in
  CPSet.map cb ~f:(fun (c1, c2) -> VF_imply (Component.fold_preconds [c1;c2], VF_sub_mmm_no_underflow (c1.body, c2.body)))
end (* function mutez_ge end *)

(* WARNING : "remain_var_gen" has internal side-effects. *)
let remain_var_gen : Vlang.Expr.t -> Vlang.Expr.t list -> string
= fun e el -> ("__MTZMAP_SUM_R_(" ^ (e |> Vlang.Expr.to_string) ^ ")_(" ^ (el |> Core.List.map ~f:Vlang.Expr.to_string |> Core.String.concat ~sep:")_(") ^ ")")

let mtzmap_partial_sum : Vlang.Component.t -> Vlang.Component.t -> Vlang.Component.t -> Vlang.Formula.t CPSet.t
= let open Vlang in
  let open Formula in
  let open Component in
  fun km_map_set k_set m_set -> begin
  let k_pset = powerset k_set in
  let c_set : ((comp * (comp CPSet.t)) * comp) CPSet.t = combination (combination km_map_set k_pset) m_set in  (* It might explodes *)
  let c_set : ((comp * (comp CPSet.t)) * comp) CPSet.t = CPSet.filter c_set ~f:(fun ((_, k_sset), _) -> Stdlib.not (CPSet.is_empty k_sset)) in (* if middle-key-set is empty, remove it *)
  CPSet.map
    c_set
    ~f:(
      fun ((km, k_sset), m) -> 
      (* get precondition *)
      let prec_lst : Formula.t list = 
        List.flatten [
          km.precond_lst; 
          List.flatten (CPSet.fold k_sset ~init:[] ~f:(fun accl sset -> sset.precond_lst :: accl)); 
          m.precond_lst;
        ] 
      in
      (* generate formula *)
      let k_slist = CPSet.fold k_sset ~init:[] ~f:(fun accl x -> x.body :: accl) in
      VF_imply (fold_precond prec_lst, VF_mtzmap_partial_sum_equal(km.body, k_slist, m.body, remain_var_gen km.body k_slist))
    )
end (* function mtzmap_partial_sum end *)


(* "int_ge" : every component should be integer type. *)
let int_ge : Vlang.Component.t -> Vlang.Formula.t CPSet.t
= let open Vlang in
  let open Formula in
  fun compset -> begin
  let cb : (Component.comp * Component.comp) CPSet.t = combination_self_two_diff_rf_set compset in
  CPSet.map cb ~f:(fun (c1, c2) -> VF_imply (Component.fold_preconds [c1;c2], VF_mich_if (V_geq_ib (V_sub_iii (c1.body, c2.body)))))
end (* function int_ge end *)

(* "nat_ge" : every component should be natural type. *)
let nat_ge : Vlang.Component.t -> Vlang.Formula.t CPSet.t
= let open Vlang in
  let open Formula in
  fun compset -> begin
  let cb : (Component.comp * Component.comp) CPSet.t = combination_self_two_diff_rf_set compset in
  CPSet.map cb ~f:(fun (c1, c2) -> VF_imply (Component.fold_preconds [c1;c2], VF_mich_if (V_geq_ib (V_sub_nni (c1.body, c2.body)))))
end (* function nat_ge end *)


let all_equal : Vlang.Component.t -> Vlang.Formula.t CPSet.t
= let open Vlang in
  let open Formula in
  fun compset -> begin
  let cb : (Component.comp * Component.comp) CPSet.t = combination_self_two_diff (CPSet.to_list compset) |> List.filter (fun (c1, c2) -> c1.Component.typ = c2.Component.typ) |> CPSet.of_list in
  CPSet.map cb ~f:(fun (c1, c2) -> VF_imply (Component.fold_preconds [c1;c2], VF_eq (c1.body, c2.body)))
end (* function all_equal end *)
