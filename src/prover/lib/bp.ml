(*****************************************************************************)
(*****************************************************************************)
(* Condition                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

type typ = Adt.typ

type var = Cfg.ident
and exp = Cfg.expr

type cond =
  | BC_is_true of var
  | BC_is_none of var
  | BC_is_left of var
  | BC_is_cons of var
  | BC_no_overflow of exp * typ
  | BC_no_underflow of exp * typ
  | BC_not of cond

let create_cond_is_true : var -> cond
=fun v -> BC_is_true v

let create_cond_is_none : var -> cond
=fun v -> BC_is_none v

let create_cond_is_left : var -> cond
=fun v -> BC_is_left v

let create_cond_is_cons : var -> cond
=fun v -> BC_is_cons v

let create_cond_no_overflow : exp -> typ -> cond
=fun e t -> BC_no_overflow (e, t)

let create_cond_no_underflow : exp -> typ -> cond
=fun e t -> BC_no_underflow (e, t)

let create_cond_not : cond -> cond
=fun c -> BC_not c

let rec string_of_cond : cond -> string
=fun cond -> begin
  match cond with
  | BC_is_true v -> v ^ "= True"
  | BC_is_none v -> v ^ "= None"
  | BC_is_left v -> v ^ "= Left x"
  | BC_is_cons v -> v ^ "= Cons (hd tl)"
  | BC_no_overflow (e, _) -> "(" ^ (Format.flush_str_formatter (Tezla.Pp.expr Format.str_formatter e)) ^ ")_no_overflow"
  | BC_no_underflow (e, _) -> "(" ^ (Format.flush_str_formatter (Tezla.Pp.expr Format.str_formatter e)) ^ ")_no_underflow"
  | BC_not c -> "!(" ^ (string_of_cond c) ^ ")"
end


(*****************************************************************************)
(*****************************************************************************)
(* Instruction                                                               *)
(*****************************************************************************)
(*****************************************************************************)

type inst =
  | BI_assume of cond
  | BI_assert of cond
  | BI_assign of var * exp
  | BI_skip

let create_inst_assume : cond -> inst
=fun f -> BI_assume f

let create_inst_assert : cond -> inst
=fun f -> BI_assert f

let create_inst_assign : (var * exp) -> inst
=fun (id, e) -> BI_assign (id, e)

let create_inst_skip : inst
=BI_skip

let string_of_inst : inst -> string
=fun inst -> begin
  match inst with
  | BI_assume f -> "Assume " ^ (string_of_cond f) ^ ";"
  | BI_assert f -> "Assert " ^ (string_of_cond f) ^ ";"
  | BI_assign (id, e) -> (Cfg.string_of_ident id) ^ " := " ^ (Format.flush_str_formatter (Tezla.Pp.expr Format.str_formatter e)) ^ ";"
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