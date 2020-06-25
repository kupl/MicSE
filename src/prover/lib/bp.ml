(*****************************************************************************)
(*****************************************************************************)
(* Formula                                                                   *)
(*****************************************************************************)
(*****************************************************************************)

type var = Cfg.ident
type formula = 
  | F_true  | F_false
  | F_is_true of var
  | F_is_none of var
  | F_is_left of var
  | F_is_cons of var
  | F_and of formula * formula
  | F_or of formula * formula
  | F_not of formula
  | F_imply of formula * formula
  | F_iff of formula * formula
  | F_forall of var list * formula
  | F_exists of var list * formula

let create_formula_is_true : var -> formula
=fun id -> F_is_true id

let create_formula_is_none : var -> formula
=fun id -> F_is_none id

let create_formula_is_left : var -> formula
=fun id -> F_is_left id

let create_formula_is_cons : var -> formula
=fun id -> F_is_cons id

let create_formula_not : formula -> formula
=fun f -> F_not f

let rec string_of_formula : formula -> string
=fun f -> begin
  match f with
  | F_true -> "True"
  | F_false -> "False"
  | F_is_true id -> Cfg.string_of_ident id
  | F_is_none id -> "(" ^ (Cfg.string_of_ident id) ^ " == None)"
  | F_is_left id -> "(" ^ (Cfg.string_of_ident id) ^ " == Left a)"
  | F_is_cons id -> "(" ^ (Cfg.string_of_ident id) ^ " == { a; <rest> })"
  | F_and (f1, f2) -> "(" ^ (string_of_formula f1) ^ " && " ^ (string_of_formula f2) ^ ")"
  | F_or (f1, f2) -> "(" ^ (string_of_formula f1) ^ " && " ^ (string_of_formula f2) ^ ")"
  | F_not f -> "!" ^ (string_of_formula f)
  | F_imply (f1, f2) -> "(" ^ (string_of_formula f1) ^ " -> " ^ (string_of_formula f2) ^ ")"
  | F_iff (f1, f2) -> "(" ^ (string_of_formula f1) ^ " <-> " ^ (string_of_formula f2) ^ ")"
  | F_forall (vl, f) -> "ForAll " ^ (Core.String.concat ~sep:"," vl) ^ ". " ^ (string_of_formula f)
  | F_exists (vl, f) -> "Exists " ^ (Core.String.concat ~sep:"," vl) ^ ". " ^ (string_of_formula f)
end


(*****************************************************************************)
(*****************************************************************************)
(* Instruction                                                               *)
(*****************************************************************************)
(*****************************************************************************)

type exp = Cfg.expr
type inst =
  | I_assume of formula
  | I_assign of var * exp
  | I_skip

let create_inst_assume : formula -> inst
=fun f -> I_assume f

let create_inst_assign : (var * exp) -> inst
=fun (id, e) -> I_assign (id, e)

let create_inst_skip : unit -> inst
=fun () -> I_skip

let string_of_inst : inst -> string
=fun inst -> begin
  match inst with
  | I_assume f -> "Assume " ^ (string_of_formula f) ^ ";"
  | I_assign (id, e) -> (Cfg.string_of_ident id) ^ " := " ^ (Format.flush_str_formatter (Tezla.Pp.expr Format.str_formatter e)) ^ ";"
  | I_skip -> "Skip;"
end

(*****************************************************************************)
(*****************************************************************************)
(* Invariants                                                                *)
(*****************************************************************************)
(*****************************************************************************)

type vertex = Cfg.vertex
type inv = { id: vertex; formula: formula option }
type inv_map = (vertex, formula) Core.Hashtbl.t

let create_dummy_inv : vertex -> inv
=fun vtx -> { id=vtx; formula=None }

let string_of_inv : inv -> string
=fun inv -> begin
  if Option.is_none inv.formula then (Cfg.string_of_vertex inv.id) ^ ": None"
  else (Cfg.string_of_vertex inv.id) ^ ": " ^ (string_of_formula (Option.get inv.formula))
end


(*****************************************************************************)
(*****************************************************************************)
(* Basic path                                                                *)
(*****************************************************************************)
(*****************************************************************************)

type t = { pre: inv; body: inst list; post: inv }

let create_new_bp : vertex -> vertex -> t
=fun pre post -> { pre=(create_dummy_inv pre); body=[]; post=(create_dummy_inv post) }

let create_cut_bp : t -> vertex -> (t * t)
=fun bp loop -> begin
  let loop_inv = create_dummy_inv loop in
  let terminated_bp = { pre=bp.pre; body=bp.body; post=loop_inv } in
  let new_bp = { pre=loop_inv; body=[]; post=bp.post } in
  (terminated_bp, new_bp)
end

let update_body : t -> inst -> t
=fun bp inst -> { pre=bp.pre; body=(bp.body@[inst]); post=bp.post }

let to_string : t -> string
=fun bp -> begin
  let str = "" in
  let str = str ^ (string_of_inv bp.pre) ^ "\n" in
  let str = Core.List.fold_left bp.body ~init:str ~f:(fun str inst -> (
    str ^ (string_of_inst inst) ^ "\n"
  )) in
  let str = str ^ (string_of_inv bp.post) ^ "\n" in
  str
end