module Node = Tezla_cfg.Cfg_node

open Batteries
type program = Michelson.Adt.program
type vertex = Node.t
type expr = Node.expr
type edge_label = Tezla_cfg.Flow_graph.Cfg.edge_label

module V = Tezla_cfg.Flow_graph.Cfg.V
module E = Tezla_cfg.Flow_graph.Cfg.E
module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (Tezla_cfg.Flow_graph.Cfg.V) (Tezla_cfg.Flow_graph.Cfg.E)
module Display = Tezla_cfg.Flow_graph.Cfg.Display
module Wrapper = Tezla_cfg.Flow_graph.Cfg.Wrapper

type t = Tezla_cfg.Flow_graph.Cfg.t

let create                = Tezla_cfg.Flow_graph.Cfg.create
let inflow                = Tezla_cfg.Flow_graph.Cfg.inflow
let outflow               = Tezla_cfg.Flow_graph.Cfg.outflow
let is_extremal           = Tezla_cfg.Flow_graph.Cfg.is_extremal
let is_extremalR          = Tezla_cfg.Flow_graph.Cfg.is_extremalR
let add                   = Tezla_cfg.Flow_graph.Cfg.add
let get                   = Tezla_cfg.Flow_graph.Cfg.get
let connect               = Tezla_cfg.Flow_graph.Cfg.connect
let get_blocks            = Tezla_cfg.Flow_graph.Cfg.get_blocks
let get_func_id           = Tezla_cfg.Flow_graph.Cfg.get_func_id
let extremal              = Tezla_cfg.Flow_graph.Cfg.extremal
let extremalR             = Tezla_cfg.Flow_graph.Cfg.extremalR
let labels                = Tezla_cfg.Flow_graph.Cfg.labels
let dot_output            = Tezla_cfg.Flow_graph.Cfg.dot_output
let display_with_gv       = Tezla_cfg.Flow_graph.Cfg.display_with_gv
let show                  = Tezla_cfg.Flow_graph.Cfg.show
let generate_from_program = Tezla_cfg.Flow_graph.Cfg.generate_from_program 