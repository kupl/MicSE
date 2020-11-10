(*****************************************************************************)
(*****************************************************************************)
(* Condition                                                                 *)
(*****************************************************************************)
(*****************************************************************************)

type typ = Pre.Lib.Adt.typ

type var = Pre.Lib.Cfg.ident
and exp = Pre.Lib.Cfg.expr
and edge = Pre.Lib.Cfg.edge_label
and vertex = Pre.Lib.Cfg.vertex

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
  | BC_no_overflow (e, _) -> "(" ^ (Pre.Lib.Cfg.expr_to_str e) ^ ")_no_overflow"
  | BC_no_underflow (e, _) -> "(" ^ (Pre.Lib.Cfg.expr_to_str e) ^ ")_no_underflow"
  | BC_not c -> "!(" ^ (string_of_cond c) ^ ")"
end


(*****************************************************************************)
(*****************************************************************************)
(* Instruction                                                               *)
(*****************************************************************************)
(*****************************************************************************)

type inst =
  | BI_assume of cond
  | BI_assert of cond * loc * category
  | BI_assign of var * exp
  | BI_skip

and loc = { entry: vertex; exit: vertex; }

and category = 
  | Q_mutez_overflow
  | Q_shift_overflow
  | Q_assert

let compare_loc : loc -> loc -> int
=fun l1 l2 -> begin
  if l1.entry = l2.entry && l1.exit = l2.exit
  then 0
  else if l1.entry <= l2.entry
       then -1
       else 1
end

let create_inst_assume : cond -> inst
=fun f -> BI_assume f

let create_inst_assert : cond -> loc -> category -> inst
=fun f l c -> BI_assert (f, l, c)

let create_inst_assign : var -> exp -> inst
=fun id e -> BI_assign (id, e)

let create_inst_skip : inst
=BI_skip

let create_loc : vertex -> vertex -> loc
=fun etr ext -> { entry=etr; exit=ext }

let create_category_mutez_overflow : category
=Q_mutez_overflow

let create_category_shift_overflow : category
=Q_shift_overflow

let create_category_assertion : category
=Q_assert

let string_of_inst : inst -> string
=fun inst -> begin
  match inst with
  | BI_assume f -> "Assume " ^ (string_of_cond f) ^ ";"
  | BI_assert (f, _, _) -> "Assert " ^ (string_of_cond f) ^ ";"
  | BI_assign (id, e) -> id ^ " := " ^ (Pre.Lib.Cfg.expr_to_str e) ^ ";"
  | BI_skip -> "Skip;"
end

let string_of_category : category -> string
=fun c -> begin
  match c with
  | Q_mutez_overflow -> "Mutez overflow or underflow"
  | Q_shift_overflow -> "Logical shift overflow and underflow"
  | Q_assert -> "Assertion"
end


(*****************************************************************************)
(*****************************************************************************)
(* Basic path                                                                *)
(*****************************************************************************)
(*****************************************************************************)

type t = { pre: Inv.t; body: (vertex * inst) list; post: Inv.t }

let create_new_bp : vertex -> vertex -> t
=fun pre post -> { pre=(Inv.create ~vtx:pre); body=[]; post=(Inv.create ~vtx:post) }

let create_cut_bp : t -> vertex -> (t * t)
=fun bp loop -> begin
  let loop_inv = Inv.create ~vtx:loop in
  let terminated_bp = { pre=bp.pre; body=bp.body; post=loop_inv } in
  let new_bp = { pre=loop_inv; body=[]; post=bp.post } in
  (terminated_bp, new_bp)
end

let update_body : t -> vertex * inst -> t
=fun bp vtx_inst -> { pre=bp.pre; body=(bp.body@[vtx_inst]); post=bp.post }

let update_inv : t -> pre:Inv.t -> post:Inv.t -> t
=fun bp ~pre ~post -> { pre=pre; body=bp.body; post=post }

let to_string : t -> string
=fun bp -> begin
  let str = "" in
  let str = Core.List.fold_left bp.body ~init:str ~f:(fun str (vtx, inst) -> (
    str ^
    if !Utils.Options.flag_bpopt_rsi && (inst = create_inst_skip)
    then ""
    else (Printf.sprintf "%3d: %s" vtx (string_of_inst inst)) ^
    "\n"
  )) in
  str
end


(*****************************************************************************)
(*****************************************************************************)
(* Basic path list                                                           *)
(*****************************************************************************)
(*****************************************************************************)

type lst = {
  bp_list: t list;
  entry: inv_point;
  exit: inv_point;
  loop: inv_point list;
}
and inv_point = { vtx: vertex; var: var option }

let create_bp_list : bp_list:t list -> entry:inv_point -> exit:inv_point -> loop:inv_point list -> lst
=fun ~bp_list ~entry ~exit ~loop -> { bp_list=bp_list; entry=entry; exit=exit; loop=loop }

let create_inv_point : vtx:vertex -> var_opt:var option -> inv_point
=fun ~vtx ~var_opt -> { vtx=vtx; var=var_opt }