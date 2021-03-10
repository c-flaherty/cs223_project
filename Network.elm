module Network exposing(..)
import Dict exposing (Dict)

type Network comparable =
  Empty | Dict comparable (Dict comparable (Capacity, Flow))

type alias Capacity = Int
type alias Flow = Int 

empty : Network comparable
empty =
  Empty

addEdge : comparable -> comparable -> Capacity -> Network comparable -> Network comparable
addEdge node1 node2 c network =
  let 
    new_edge = Dict.singleton node1 (Dict.singleton node2 (c, 0))
  in
    case network of
      Empty ->
        new_edge
      _ ->
        Dict.union network new_edge

deleteEdge : comparable -> comparable -> Network comparable -> Network comparable
deleteEdge node1 node2 network =
  if Dict.member node1 network then
    let 
      network_minus_node1 = Dict.remove node1 network
      node1_adj = Dict.get node1 network
      node1_adj_minus_node2 = Dict.remove node2 node1_adj
    in
      Dict.union node1_adj_minus_node2 network_minus_node1
  else
    network

updateCapacity : comparable -> comparable -> Capacity -> Network comparable -> Network comparable
updateCapacity node1 node2 c network =
  addEdge node1 node2 c (deleteEdge node1 node2 network)



