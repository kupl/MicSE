(*
  <References>
  
  TCfg : Tezla_cfg.Flow_graph.Cfg
    You can find its interface from https://gitlab.com/releaselab/fresco/tezla-cfg/-/blob/4985c35a5e508f1319ad84d8d18ee6d02abdbc02/src/lib/sig.mli#L19
    Module type is defined at https://gitlab.com/releaselab/fresco/tezla-cfg/-/blob/cb75e7bb56ecba46e3007c7937040a605b288b08/src/lib/flow_graph.mli

  type t = TCfg.t = {
    blocks : (int, vertex) Hashtbl.t;
    flow : G.t;
    functions : (int, string) Hashtbl.t;
    mutable extremals : int list;
    mutable extremalsR : int list;
  }
  module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (V) (E)
    Definition from https://gitlab.com/releaselab/fresco/tezla-cfg/-/blob/cb75e7bb56ecba46e3007c7937040a605b288b08/src/lib/flow_graph.ml#L104
    and https://gitlab.com/releaselab/fresco/tezla-cfg/-/blob/master/src/lib/flow_graph.ml#L33

  type vertex = Tezla_cfg.Cfg_node.t = {id : int; stmt : stmt}
    Definition from https://gitlab.com/releaselab/fresco/tezla-cfg/-/blob/cb75e7bb56ecba46e3007c7937040a605b288b08/src/lib/cfg_node.ml#L31

  type edge_label = Normal | If_true | If_false
    Definition from https://gitlab.com/releaselab/fresco/tezla-cfg/-/blob/4985c35a5e508f1319ad84d8d18ee6d02abdbc02/src/lib/sig.mli#L22

  <Definitions from https://gitlab.com/releaselab/fresco/tezla-cfg/-/blob/cb75e7bb56ecba46e3007c7937040a605b288b08/src/lib/cfg_node.ml>
  type loc = Unknown | Loc of int * int
  type ident = string
  type decl = ident
  type typ = Tezla.Adt.typ
  type expr = Tezla.Adt.expr
  type stmt =
    | Cfg_assign of string * expr
    | Cfg_skip
    | Cfg_drop of string list
    | Cfg_swap
    | Cfg_dig
    | Cfg_dug
    | Cfg_if of string
    | Cfg_if_none of string
    | Cfg_if_left of string
    | Cfg_if_cons of string
    | Cfg_loop of string
    | Cfg_loop_left of string
    | Cfg_map of string
    | Cfg_iter of string
    | Cfg_failwith of string

  type expr = (...)
    Definition from https://gitlab.com/releaselab/fresco/tezla/-/blob/90b842de39ce18e41e3d1cfde684495d6288286d/src/lib/adt.mli#L11
*)

module TCfg :
  Tezla_cfg.Sig.Flow_graph
    with type vertex = Tezla_cfg.Cfg_node.t
     and type expr = Tezla_cfg.Cfg_node.expr
     and type program = Michelson.Adt.program
  
module Node : module type of Tezla_cfg.Cfg_node

type t          = TCfg.t
type vertex     = Node.t
type edge_label = TCfg.edge_label
type vertex_id  = int
type path       = (edge_label * vertex_id) list

val translate : Michelson.Adt.program -> t
val write_dot : t -> string -> unit
val display   : t -> unit
