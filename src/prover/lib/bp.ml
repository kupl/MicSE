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
  | BI_assign of Vlang.Ty.t * PreLib.Cfg.ident * Vlang.Expr.t
  | BI_skip

type basic_node = {
  glenv_ref : GlVar.Env.t ref;
  cfgvtx : PreLib.Cfg.vertex;
  inst : inst;
}

type t = basic_node list

module JsonRep = struct
  exception ParseErr of Yojson.Basic.t

  module Const = struct
    let name_q_mutez_arith_safety = "Q_mutez_arith_safety"
    let name_q_int_nat_shift_safety = "Q_int_nat_shift_safety"
    let name_q_assertion = "Q_assertion"
    let cname_bi_assume   : string = "BI_assume"
    let cname_bi_assert   : string = "BI_assert"
    let cname_bi_assign   : string = "BI_assign"
    let cname_bi_skip        : string = "BI_skip"
    let fname_glenv_ref   : string = "glenv_ref"
    let fname_cfgvtx      : string = "cfgvtx"
    let fname_inst        : string = "inst"
  end (* module JsonRep.Const end *)

  let of_query_category : query_category -> Yojson.Basic.t 
  =function 
    | Q_mutez_arith_safety -> `String Const.name_q_mutez_arith_safety
    | Q_int_nat_shift_safety -> `String Const.name_q_int_nat_shift_safety
    | Q_assertion -> `String Const.name_q_assertion
  let of_inst : inst -> Yojson.Basic.t 
  = function
    | BI_assume v -> `List [`String Const.cname_bi_assume; `String (Vlang.Formula.to_string v)]
    | BI_assert (v, qc) -> `List [`String Const.cname_bi_assert; `String (Vlang.Formula.to_string v); of_query_category qc]
    | BI_assign (vt, id, e) -> `List [`String Const.cname_bi_assign; `String (Vlang.Ty.to_string vt); `String id; `String (Vlang.Expr.to_string e)]
    | BI_skip -> `List [`String Const.cname_bi_skip]
  let of_basic_node : basic_node -> Yojson.Basic.t 
  = fun bn -> begin
    `Assoc [
      Const.fname_glenv_ref, GlVar.Env.JsonRep.of_t !(bn.glenv_ref);
      Const.fname_cfgvtx, `Int bn.cfgvtx;
      Const.fname_inst, of_inst bn.inst;
    ]
  end
  let of_t : t -> Yojson.Basic.t 
  = fun t -> `List (List.map of_basic_node t)

end (* module JsonRep end *)
