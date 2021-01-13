(* formula templates *)

module PolySet = Core.Set.Poly (* sugar *)

module MtzMapPartialSumEq = struct
  open Vlang

  (* expression-elements in one "idx" treated as they has same value. *)
  type idx = Expr.t PolySet.t
  (* "partiation" separates expressions in several elements *)
  type partition = idx PolySet.t

  (* "const_remain_var_prefix" : constant. magic-string prefix to create a (unique-like) variable name. *)
  let const_remain_var_prefix : string = "__MTZMAP_SUM_REMAIN_("
  let const_remain_var_postfix : string = ")"

  (* "create_remain_var" recieves the map-variable "x" (in vlang expression) and creates the variable-Rx *)
  let create_remain_var : Vlang.Expr.t -> Vlang.Expr.t
  = let open Vlang.Expr in
    fun mv -> begin
    match mv with
    | V_var (_, vname) -> V_var (Ty.T_mutez, const_remain_var_prefix ^ vname ^ const_remain_var_postfix)
    | _ -> V_var (Ty.T_mutez, const_remain_var_prefix ^ (Vlang.Expr.to_string mv) ^ const_remain_var_postfix)
  end (* function create_remain_var end *)

  (* "read_partition_expr {{a;b}; {c}; {d;e;f}} m" returns "[GET_DFT(a,0,m); GET(c,0,m); GET(d,0,m)]" in vlang-expr list form *)
  (* If GET instruction failed, GET(_,m) will return 0-value instead. *)
  let read_partition_expr : partition -> map:Vlang.Expr.t -> Vlang.Expr.t list
  = let open Vlang.Expr in
    fun part ~map -> begin
    let part_list = part |> PolySet.to_list in
    let part_idx_list = part_list |> Core.List.map ~f:(fun set -> set |> PolySet.choose |> (function | Some ee -> ee | None -> Stdlib.failwith "ProverLib.Ftmp.read_partition_expr: Empty set cannot be appeared")) in
    part_idx_list |> Core.List.map ~f:(fun idx -> V_get_default (idx, V_lit_mutez (Z.zero), map))
  end

  (* function "partition" internally used for "create_n_partition". *)
  let rec partition : k:int -> idxs:idx -> partition list = begin
    fun ~k ~idxs ->
    let len = PolySet.length idxs in
    if k = 1 then
      (idxs |> PolySet.singleton)::[]
    else if k > len then
      []
    else if k = len then
      (idxs |> PolySet.fold ~init:(PolySet.empty) ~f:(fun acc e -> e |> PolySet.singleton |> PolySet.add acc))::[]
    else begin
      let e = idxs |> PolySet.choose |> (function | Some ee -> ee | None -> Stdlib.failwith "ProverLib.Ftmp.create_partition.partition: Empty set cannot reach here") in
      let other = e |> PolySet.remove idxs in
      (* Case 1: Singleton e alone *)
      let case1 = partition ~k:(k - 1) ~idxs:other |> Core.List.map ~f:(fun set -> e |> PolySet.singleton |> PolySet.add set) in
      (* Case 2: e in each partition *)
      let case2 = partition ~k:k ~idxs:other |> Core.List.fold_left ~init:[] ~f:(fun acc set -> (begin
        set |> PolySet.fold ~init:acc ~f:(fun acc' part -> (begin
          let removed_set = part |> PolySet.remove set in
          let new_set = e |> PolySet.add part |> PolySet.add removed_set in
          new_set::acc'
        end))
      end)) in
      case1@case2
    end
  end (* function partition end *)

  (* "create_n_partition n idxs" for external use. it returns every partition results for the given expression-set. *)
  let rec create_n_partition : n:int -> idxs:idx -> partition list = begin
    fun ~n ~idxs ->
    if n = 0 then []
      else if n = 1 then (partition ~k:1 ~idxs:idxs)
      else (create_n_partition ~n:(n - 1) ~idxs:idxs)@(partition ~k:n ~idxs:idxs)
  end (* function create_n_partition end *)





  (* "create_partitioning_formula" constructs the common part, equal and not equal,
      of g1 and g2 (the fst_formula and the snd_formula) *)
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
    let part_idx_list = part_list |> Core.List.map ~f:(fun set -> set |> PolySet.choose |> (function | Some ee -> ee | None -> Stdlib.failwith "ProverLib.Ftmp.create_partitioning_formula: Empty set cannot be appeared")) in
    let not_equal_formula = part_idx_list |> make_not_equal in
    VF_and (equal_formula @ not_equal_formula)
  end (* function create_partitioning_formula end *)


  (* G_1 *)
  let create_fst_formula : partition -> map:Vlang.Expr.t -> remain_var:Vlang.Expr.t -> value:Vlang.Expr.t -> Vlang.t
  = let open Vlang.Formula in
    fun part ~map ~remain_var ~value -> begin
    let part_formula = part |> create_partitioning_formula in
    let part_expr_list = part |> read_partition_expr ~map:map in
    let add_expr = part_expr_list |> Core.List.fold_left ~f:(fun acc e -> Expr.V_add_mmm (acc, e)) ~init:remain_var in
    let sigma_equal = VF_eq (add_expr, value) in
    VF_imply (part_formula, sigma_equal)
  end

  (* G_2 *)
  let create_snd_formula : partition -> map:Vlang.Expr.t -> remain_var:Vlang.Expr.t -> Vlang.t
  = let open Vlang.Expr in
    let open Vlang.Formula in
    fun part ~map ~remain_var -> begin
    let part_formula = part |> create_partitioning_formula in
    let part_expr_list = part |> read_partition_expr ~map:map in
    let no_overflow_formula, _ = part_expr_list |> Core.List.fold_left ~f:(
        fun (l, acc) e -> (l@[(VF_add_mmm_no_overflow (acc, e))], V_add_mmm (acc, e))
      ) ~init:([], remain_var) in
    VF_imply (part_formula, VF_and (no_overflow_formula))
  end

  (* "encode_vf_..._sum_equal" encodes VF_mtzmap_partial_sum_equal into Vlang primitives. *)
  let encode_vf_mtzmap_partial_sum_equal : Expr.t * (Expr.t list) * Expr.t -> Vlang.t
  =fun (mmap, keylst, sum) -> begin
    (* make a partition *)
    let keyset : Expr.t PolySet.t = PolySet.of_list keylst in
    let ptt_lst : partition list = create_n_partition ~n:(PolySet.length keyset) ~idxs:keyset in
    (* generate first, second formulas (g1, g2) *)
    let rv : Expr.t = create_remain_var mmap in
    let g1_list : Vlang.t list = List.map (fun p -> create_fst_formula p ~map:mmap ~remain_var:rv ~value:sum) ptt_lst in
    let g2_list : Vlang.t list = List.map (fun p -> create_snd_formula p ~map:mmap ~remain_var:rv) ptt_lst in
    VF_and (g1_list @ g2_list)
  end (* function encode_vf_mtzmap_partial_sum_equal end *)

end (* module MtzMapPartialSumEq end *)
