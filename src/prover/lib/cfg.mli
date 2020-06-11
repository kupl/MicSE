type t = Tezla_cfg.Flow_graph.Cfg.t

val translate : Adt.t -> t

val write_dot : t -> string -> unit

val display : t -> unit