(*****************************************************************************)
(*****************************************************************************)
(* SigmaEqualUtils                                                           *)
(*****************************************************************************)
(*****************************************************************************)

module SigmaEqualUtils = struct
  exception Error of string

  module PolySet = Core.Set.Poly

  type idx = Vlang.Expr.t PolySet.t
  type partition = idx PolySet.t

  let read_index : Vlang.Expr.t -> idx
  = let open Vlang in
    let rec collect (e: Expr.t) (idxs: idx) = begin
      match e with
      | V_update_xomm (i, _, m) -> i |> PolySet.add idxs |> collect m (* TODO: handling deletion of entry *)
      | _ -> idxs
    end in
    fun map -> begin
      PolySet.empty |> collect map
  end

  let rec read_origin_map : Vlang.Expr.t -> Vlang.Expr.t
  = fun map -> begin
      match map with
      | V_update_xomm (_, _, m) -> m |> read_origin_map
      | _ -> map
  end

  let read_partition_expr : partition -> map:Vlang.Expr.t -> Vlang.Expr.t list
  = let open Vlang.Expr in
    fun part ~map -> begin
    let part_list = part |> PolySet.to_list in
    let part_idx_list = part_list |> Core.List.map ~f:(fun set -> set |> PolySet.choose |> (function | Some ee -> ee | None -> Error "read_partition_expr: Empty set cannot be appeared" |> raise)) in
    part_idx_list |> Core.List.map ~f:(fun idx -> V_unlift_option (V_get_xmoy (idx, map)))
  end

  let create_remain_var : Vlang.Expr.t -> Vlang.Expr.t
  = fun map -> V_var (T_mutez, ("REMAIN_OF_" ^ (map |> read_origin_map |> Vlang.Expr.to_string)))

  let create_partition : Vlang.Expr.t -> partition list
  = let rec partition (k: int) (idxs: idx) = begin
      let len = PolySet.length idxs in
      if k = 1 then
        (idxs |> PolySet.singleton)::[]
      else if k > len then
        []
      else if k = len then
        (idxs |> PolySet.fold ~init:(PolySet.empty) ~f:(fun acc e -> e |> PolySet.singleton |> PolySet.add acc))::[]
      else begin
        let e = idxs |> PolySet.choose |> (function | Some ee -> ee | None -> Error "create_partition.partition: Empty set cannot reach here" |> raise) in
        let other = e |> PolySet.remove idxs in
        (* Case 1: Singleton e alone *)
        let case1 = other |> partition (k - 1) |> Core.List.map ~f:(fun set -> e |> PolySet.singleton |> PolySet.add set) in
        (* Case 2: e in each partition *)
        let case2 = other |> partition k |> Core.List.fold_left ~init:[] ~f:(fun acc set -> (begin
          set |> PolySet.fold ~init:acc ~f:(fun acc' part -> (begin
            let removed_set = part |> PolySet.remove set in
            let new_set = e |> PolySet.add part |> PolySet.add removed_set in
            new_set::acc'
          end))
        end)) in
        case1@case2
      end
    end in
    let rec create_n_partition (n: int) (idxs: idx) = begin
      if n = 0 then []
      else if n = 1 then (idxs |> partition 1)
      else (idxs |> create_n_partition (n - 1))@(idxs |> partition n)
    end in
    fun map -> begin
    let idx = map |> read_index in
    let len = idx |> PolySet.length in
    idx |> create_n_partition len
  end

  let create_partitioning_formula : partition -> Vlang.t
  = let open Vlang in
    let open Vlang.Formula in
    let rec make_equal (idx_list: Expr.t list) = begin
      match idx_list with
      | h1::h2::[] -> (VF_eq (h1, h2))::[]
      | h1::h2::tl -> (VF_eq (h1, h2))::((h2::tl) |> make_equal)
      | _ -> []
    end in
    let rec make_not_equal (idx_list: Expr.t list) = begin
      match idx_list with
      | h1::tl -> (tl |> Core.List.map ~f:(fun h2 -> (VF_not (VF_eq (h1, h2)))))@(make_not_equal tl)
      | _ -> []
    end in
    fun part -> begin
    let part_list = part |> PolySet.to_list in
    let equal_formula = part_list |> Core.List.fold_left ~f:(fun fs set -> (set |> PolySet.to_list |> make_equal)@fs) ~init:[] in
    let part_idx_list = part_list |> Core.List.map ~f:(fun set -> set |> PolySet.choose |> (function | Some ee -> ee | None -> Error "create_partitioning_formula: Empty set cannot be appeared" |> raise)) in
    let not_equal_formula = part_idx_list |> make_not_equal in
    VF_and (equal_formula @ not_equal_formula)
  end

  let create_idx_exist_formula : Vlang.Expr.t -> Vlang.t
  = let open Vlang.Expr in
    let open Vlang.Formula in
    fun map -> begin
    let origin_map = map |> read_origin_map in
    let idxs = map |> read_index |> PolySet.to_list in
    let pre_exists = idxs |> Core.List.map ~f:(fun idx -> VF_not (VF_mich_if_none (V_get_xmoy (idx, origin_map)))) in
    let post_exists = idxs |> Core.List.map ~f:(fun idx -> VF_not (VF_mich_if_none (V_get_xmoy (idx, map)))) in
    if 0 = (idxs |> Core.List.length) then VF_true else VF_and (pre_exists@post_exists)
  end

  let create_fst_formula : partition -> map:Vlang.Expr.t -> value:Vlang.Expr.t -> Vlang.t
  = let open Vlang.Formula in
    fun part ~map ~value -> begin
    let part_formula = part |> create_partitioning_formula in
    let part_expr_list = part |> read_partition_expr ~map:map in
    let remain_var = map |> create_remain_var in
    let add_expr = part_expr_list |> Core.List.fold_left ~f:(fun acc e -> V_add_mmm (acc, e)) ~init:remain_var in
    let sigma_equal = VF_eq (add_expr, value) in
    VF_imply (part_formula, sigma_equal)
  end

  let create_snd_formula : partition -> map:Vlang.Expr.t -> Vlang.t
  = let open Vlang.Expr in
    let open Vlang.Formula in
    fun part ~map -> begin
    let part_formula = part |> create_partitioning_formula in
    let part_expr_list = part |> read_partition_expr ~map:map in
    let remain_var = map |> create_remain_var in
    let no_overflow_formula, _ = part_expr_list |> Core.List.fold_left ~f:(
        fun (l, acc) e -> (l@[(VF_add_mmm_no_overflow (acc, e))], V_add_mmm (acc, e))
      ) ~init:([], remain_var) in
    VF_imply (part_formula, VF_and (no_overflow_formula))
  end

  let create_pre_formula : map:Vlang.Expr.t -> value:Vlang.Expr.t -> Vlang.t
  = fun ~map ~value -> begin
    let partitions = map |> create_partition in
    let origin_map = map |> read_origin_map in
    let exist_formula = map |> create_idx_exist_formula in
    let first_formulae = partitions |> Core.List.map ~f:(create_fst_formula ~map:origin_map ~value:value) in
    let second_formulae = partitions |> Core.List.map ~f:(create_snd_formula ~map:origin_map) in
    VF_and [exist_formula; VF_and (first_formulae); VF_and (second_formulae)]
  end

  let create_post_formula : map:Vlang.Expr.t -> value:Vlang.Expr.t -> Vlang.t
  = fun ~map ~value -> begin
    let partitions = map |> create_partition in
    let first_formulae = partitions |> Core.List.map ~f:(create_fst_formula ~map:map ~value:value) in 
    let second_formulae = partitions |> Core.List.map ~f:(create_snd_formula ~map:map) in
    VF_and [VF_and (first_formulae); VF_and (second_formulae)] 
  end
end

(*****************************************************************************)
(*****************************************************************************)
(* Templates                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

module Tmp = struct
  exception Error of string

  let sigma_equal_template : Vlang.t -> Vlang.t
  = fun f -> begin
    match f with
    | VF_sigma_equal (`Pre, m, v) -> SigmaEqualUtils.create_pre_formula ~map:m ~value:v
    | VF_sigma_equal (`Post, m, v) -> SigmaEqualUtils.create_post_formula ~map:m ~value:v
    | _ -> Error "sigma_equal_template: Wrong formula for template" |> raise
  end
end