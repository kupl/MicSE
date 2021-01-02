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

type t = {
  (* entry_vtx and exit_vtx indicates content's first basic_node and last basic_node's cfgvtx value. *)
  entry_vtx : PreLib.Cfg.vertex;
  exit_vtx : PreLib.Cfg.vertex;
  content : basic_node list;
}

module JsonRep : sig
  exception ParseErr of Yojson.Basic.t

  module Const : sig
    val cname_bi_assume   : string
    val cname_bi_assert   : string
    val cname_bi_assign   : string
    val cname_bi_skip     : string
    val fname_glenv_ref   : string
    val fname_cfgvtx      : string
    val fname_inst        : string
    val fname_entry_vtx   : string
    val fname_exit_vtx    : string
    val fname_content     : string
  end (* module JsonRep.Const end *)

  val of_query_category : query_category -> Yojson.Basic.t
  val of_inst : inst -> Yojson.Basic.t
  val of_basic_node : basic_node -> Yojson.Basic.t
  val of_t : t -> Yojson.Basic.t

end (* module JsonRep end *)
