module type Graph =
sig
  type node
  module NodeSet: Ensembles.Ensemble with type elt=node
  module NodeMap: Pmap.S with type key=node
  type graph

  val empty : graph
  val is_empty : graph -> bool
  val add_vertex : node -> graph -> graph
  val add_edge : node -> node -> graph -> graph
  val succs : node -> graph -> NodeSet.set
  val fold : (node -> 'a -> 'a ) -> graph -> 'a -> 'a
end


module GraphAVL =
struct
  type node = string
  module NodeSet = Ensembles.MakeAvl(String)
  module NodeMap = Pmap.AVLMap(String)
  type graph = NodeSet.set NodeMap.pmap

  let empty  = NodeMap.empty

  let is_empty g = g = empty

  let mem v g = NodeMap.mem v g

  let add_vertex v g = NodeMap.add v NodeSet.empty g

  let add_edge v1 v2 g =
    let v1_edge = NodeMap.find v1 g in
    let v2_edge = NodeMap.find v2 g in
    NodeMap.add v1 (NodeSet.add v2 v1_edge) (NodeMap.add v2 (NodeSet.add v1 v2_edge) g)


  let succs v g = NodeMap.find v g

  let fold f g acc = NodeMap.fold f g acc
end
