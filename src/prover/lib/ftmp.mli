(* formula templates *)

module PolySet = Core.Set.Poly (* sugar *)

module MtzMapPartialSumEq : sig
  open Vlang (* sugar *)

  (* expression-elements in one "idx" treated as they has same value. *)
  type idx = Expr.t PolySet.t
  (* "partiation" separates expressions in several elements *)
  type partition = idx PolySet.t

  (* "const_remain_var_prefix" : constant. magic-string prefix to create a (unique-like) variable name. *)
  (* val const_remain_var_prefix : string *)

  (* "create_remain_var" recieves the map-variable "x" (in vlang expression) and creates the variable-Rx *)
  (* val create_remain_var : Vlang.Expr.t -> Vlang.Expr.t *)
  
  (* "create_remain_var" creates the variable-Rx using the given string. *)
  val create_remain_var : string -> Vlang.Expr.t

  (* "read_partition_expr {{a;b}; {c}; {d;e;f}} m" returns "[GET_DFT(a,0,m); GET(c,0,m); GET(d,0,m)]" in vlang-expr list form *)
  (* If GET instruction failed, GET(_,m) will return 0-value instead. *)
  val read_partition_expr : partition -> map:Vlang.Expr.t -> Vlang.Expr.t list

  (* function "partition" internally used for "create_n_partition". *)
  val partition : k:int -> idxs:idx -> partition list

  (* "create_n_partition n idxs" for external use. it returns every partition results for the given expression-set. *)
  val create_n_partition : n:int -> idxs:idx -> partition list


  (* "create_partitioning_formula" constructs the common part, equal and not equal,
      of g1 and g2 (the fst_formula and the snd_formula) *)
  val create_partitioning_formula : partition -> Vlang.t

  (* G_1 *)
  val create_fst_formula : partition -> map:Vlang.Expr.t -> remain_var:Vlang.Expr.t -> value:Vlang.Expr.t -> Vlang.t
  
  (* G_2 *)
  val create_snd_formula : partition -> map:Vlang.Expr.t -> remain_var:Vlang.Expr.t -> Vlang.t
  
  (* "encode_vf_..._sum_equal" encodes VF_mtzmap_partial_sum_equal into Vlang primitives. *)
  val encode_vf_mtzmap_partial_sum_equal : Expr.t * (Expr.t list) * Expr.t * string -> Vlang.t
  
end (* module MtzMapPartialSumEq end *)
