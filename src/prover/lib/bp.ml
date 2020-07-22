(*****************************************************************************)
(*****************************************************************************)
(* Condition                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

type typ = Adt.typ
type var = Cfg.ident
type cond = Vlang.v_formula

let create_cond_is_true : var -> typ -> cond
=fun v t -> Vlang.create_formula_is_true (Vlang.create_exp_var v t)

let create_cond_is_none : var -> typ -> cond
=fun v t -> Vlang.create_formula_is_none (Vlang.create_exp_var v t)

let create_cond_is_left : var -> typ -> cond
=fun v t -> Vlang.create_formula_is_left (Vlang.create_exp_var v t)

let create_cond_is_cons : var -> typ -> cond
=fun v t -> Vlang.create_formula_is_cons (Vlang.create_exp_var v t)

let create_cond_not : cond -> cond
=fun f -> Vlang.create_formula_not f

let rec string_of_cond : cond -> string
=fun c -> Vlang.string_of_formula c


(*****************************************************************************)
(*****************************************************************************)
(* Instruction                                                               *)
(*****************************************************************************)
(*****************************************************************************)

type exp = Cfg.expr
type inst =
  | BI_assume of cond
  | BI_assign of var * exp * typ
  | BI_skip

let create_inst_assume : cond -> inst
=fun f -> BI_assume f

let create_inst_assign : (var * exp * typ) -> inst
=fun (id, e, t) -> BI_assign (id, e, t)

let create_inst_skip : unit -> inst
=fun () -> BI_skip

let string_of_inst : inst -> string
=fun inst -> begin
  match inst with
  | BI_assume f -> "Assume " ^ (string_of_cond f) ^ ";"
  | BI_assign (id, e, _) -> (Cfg.string_of_ident id) ^ " := " ^ (Format.flush_str_formatter (Tezla.Pp.expr Format.str_formatter e)) ^ ";"
  | BI_skip -> "Skip;"
end


(*****************************************************************************)
(*****************************************************************************)
(* Invariants                                                                *)
(*****************************************************************************)
(*****************************************************************************)

type vertex = Cfg.vertex
type formula = Vlang.t

type inv = { id: vertex; formula: formula option }
and inv_map = (vertex, formula) Core.Hashtbl.t

let create_dummy_inv : vertex -> inv
=fun vtx -> { id=vtx; formula=None }

let create_inv : vertex -> formula -> inv
=fun vtx f -> { id=vtx; formula=Some f }

let string_of_inv : inv -> string
=fun inv -> begin
  if Option.is_none inv.formula then (Cfg.string_of_vertex inv.id) ^ ": None"
  else (Cfg.string_of_vertex inv.id) ^ ": " ^ (Vlang.string_of_formula (Option.get inv.formula))
end


(*****************************************************************************)
(*****************************************************************************)
(* Basic path                                                                *)
(*****************************************************************************)
(*****************************************************************************)

type t = { pre: inv; body: inst list; post: inv }
and raw_t_list = { bps: t list; trx_inv_vtx: vertex list; loop_inv_vtx: vertex list }

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