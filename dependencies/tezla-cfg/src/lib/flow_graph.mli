module Cfg :
  Sig.Flow_graph
    with type vertex = Cfg_node.t
     and type expr = Cfg_node.expr
     and type program = Michelson.Adt.program

(* module Make_inter_cfg
    (F : Sig.Flow
           with type block = Adt.stmt
            and type vertex = Adt.stmt Adt.t) :
  Sig.Inter_flow_graph
    with type vertex = Adt.stmt Adt.t
     and type expr = Adt.expr *)
