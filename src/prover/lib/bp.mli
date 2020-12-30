(* Basic Path *)

type query_category =
  | Q_mutez_arith_safety
  | Q_int_nat_shift_safety
  | Q_assertion

(* in this representation, we do not consider variable-renaming issue, 
  just follows Cfg's variable representation. *)
type inst =
  | BI_assume of Vlang.t
  | BI_assert of Vlang.t * query_category
  | BI_assign of Pre.Lib.Adt.typ * Pre.Lib.Cfg.ident * Vlang.Expr.t
  | BI_skip

type t = (GlVar.Env.t * Pre.Lib.Cfg.vertex * inst) list
