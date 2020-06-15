type vertex = int
type edge_label = | Normal | If_true | If_false

module V : Graph.Sig.COMPARABLE with type t = vertex         (* VERTEX *)
module E : Graph.Sig.ORDERED_TYPE_DFT with type t = edge_label   (* EDGE LABLE *)
module G : module type of Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (V) (E)

type t = G.t
