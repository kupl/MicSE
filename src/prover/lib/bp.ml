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

let remove_skip_inst : t -> t
=fun bp -> begin 
  let skip_foldf = (fun acc x -> if x.inst = BI_skip then acc else x :: acc) in
  {bp with content=(List.fold_left skip_foldf [] bp.content |> List.rev)}
end


module JsonRep = struct
  exception ParseErr of Yojson.Basic.t

  module Const = struct
    let name_q_mutez_arith_safety = "Q_mutez_arith_safety"
    let name_q_int_nat_shift_safety = "Q_int_nat_shift_safety"
    let name_q_assertion = "Q_assertion"
    let cname_bi_assume   : string = "BI_assume"
    let cname_bi_assert   : string = "BI_assert"
    let cname_bi_assign   : string = "BI_assign"
    let cname_bi_skip     : string = "BI_skip"
    let fname_glenv_ref   : string = "glenv_num"
    let fname_cfgvtx      : string = "cfgvtx"
    let fname_inst        : string = "inst"
    let fname_entry_vtx   : string = "entry_vtx"
    let fname_exit_vtx    : string = "exit_vtx"
    let fname_content     : string = "content"
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
  = fun t -> begin
    `Assoc [
      Const.fname_entry_vtx, `Int t.entry_vtx;
      Const.fname_exit_vtx, `Int t.exit_vtx;
      Const.fname_content, `List (List.map of_basic_node t.content);
    ]
  end

end (* module JsonRep end *)


let simple_stringRep_of_tset : ?pretty:bool -> t Core.Set.Poly.t -> string
=fun ?(pretty=true) bps -> begin
  let yj_to_str : Yojson.Basic.t -> string = fun b -> if pretty then Yojson.Basic.pretty_to_string b else Yojson.Basic.to_string b in
  ":: Basic Paths" ^
  Core.List.foldi 
    (Core.Set.Poly.to_list bps)
    ~init:"" 
    ~f:(fun idx str bp ->
      str ^ "\nBasic Path #" ^ (Stdlib.string_of_int idx) ^ "\n" 
      ^ (JsonRep.of_t bp |> yj_to_str)
    )
end
