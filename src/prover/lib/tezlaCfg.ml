module TCfg = Tezla_cfg.Flow_graph.Cfg
module Node = Tezla_cfg.Cfg_node

type t          = TCfg.t
type vertex     = Node.t
type edge_label = TCfg.edge_label
type vertex_id  = int
type path       = (edge_label * vertex_id) list

let translate : Adt.t -> t
=fun ast -> begin
  let converted_cfg = Tezla_cfg.Flow_graph.Cfg.generate_from_program ast in
  converted_cfg
end

let write_dot : t -> string -> unit
=fun cfg output_filename -> begin
  let _ = Tezla_cfg.Flow_graph.Cfg.dot_output cfg output_filename in
  ()
end

let display : t -> unit
=fun cfg -> begin
  let _ = Tezla_cfg.Flow_graph.Cfg.show cfg in
  ()
end
