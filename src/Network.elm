module Network exposing(..)
import Dict exposing (Dict)
import Debug

type alias Network =
  {
    source : String, 
    sink : String,
    adj : Dict String (Dict String (Capacity, Flow))
  }
type alias Path = List ({u : String, v : String, c : Capacity, f : Flow})
type alias Capacity = Int
type alias Flow = Int 

fromMaybe : Maybe a -> a
fromMaybe x =
  case x of
    Just y -> y
    Nothing -> Debug.todo "fromJust Nothing"

empty : Network 
empty =
  {source="", sink="", adj=Dict.empty}

{-
addEdge : Network -> String -> String -> Capacity -> Network
addEdge network node1 node2 capacity =
  let 
    new_edge = Dict.singleton node1 (Dict.singleton node2 (c, 0))
  in
    case network of
      Empty ->
        new_edge
      _ ->
        Dict.union network new_edge
-}

{-
deleteEdge : Network -> String -> String -> Network
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
-}

updateCapacity : Network -> String -> String -> Capacity -> Network
updateCapacity network u v capacity =
  let
    u_adj = Maybe.withDefault Dict.empty (Dict.get u network.adj)
    maybeCapacity cf =
      case cf of
        Just (c, f) -> Just (capacity, f)
        Nothing -> Nothing
    upd_u_adj = Dict.update v (maybeCapacity) u_adj
    maybeDict dict =
      case dict of
        Just d -> Just upd_u_adj
        Nothing -> Nothing   
    upd_adj = Dict.update u (maybeDict) network.adj
  in
    {source=network.source, sink=network.sink, adj=upd_adj}

updateFlow : Network -> String -> String -> Flow -> Network
updateFlow network u v flow =
  let
    u_adj = Maybe.withDefault Dict.empty (Dict.get u network.adj)
    maybeFlow cf =
      case cf of
        Just (c, f) -> Just (c, flow)
        Nothing -> Nothing
    upd_u_adj = Dict.update v (maybeFlow) u_adj
    maybeDict dict =
      case dict of
        Just d -> Just upd_u_adj
        Nothing -> Nothing   
    upd_adj = Dict.update u (maybeDict) network.adj
  in
    {source=network.source, sink=network.sink, adj=upd_adj}

getFlow : Network -> String -> String -> Flow
getFlow network u v =
  let
    u_adj = Maybe.withDefault Dict.empty (Dict.get u network.adj)
    (c, f) = Maybe.withDefault (-100, 100) (Dict.get v u_adj)
  in
    f

getCapacity : Network -> String -> String -> Capacity
getCapacity network u v =
  let
    u_adj = Maybe.withDefault Dict.empty (Dict.get u network.adj)
    (c, f) = Maybe.withDefault (-100, -100) (Dict.get v u_adj)
  in
    c

bfs : Network -> String -> String -> Maybe Path
bfs network source sink =
  Debug.todo "bfs"

calcFlow : Network -> Flow
calcFlow network =
  Debug.todo "calcFlow"

augmenting : Network -> Network -> Maybe (Path, Flow)
augmenting network resnet =
  let
    maybe_path = bfs resnet network.source network.sink
  in
    case maybe_path of
      (Just path) ->
        let 
          min_leftover_cap = List.minimum <| List.map (\e -> (getCapacity network e.u e.v) - (getFlow network e.u e.v)) path 
        in 
          Just (path, fromMaybe min_leftover_cap)
      Nothing ->
        Nothing

pushFlow : Network -> Network -> Path -> Flow -> (Network, Network)
pushFlow network resnet aug_path aug_flow =
  case aug_path of
    e :: pathrest ->
      let
        aug_network = updateFlow network e.u e.v (aug_flow + getFlow network e.u e.v)
        aug_resnet = updateCapacity resnet e.u e.v (-aug_flow + getCapacity resnet e.u e.v)
      in
        pushFlow aug_network aug_resnet pathrest aug_flow
    [] ->
      (network, resnet)

ford_fulkerson_helper : Network -> Network -> {network : Network, resnet : Network, aug_path : Maybe Path, flow : Flow}
ford_fulkerson_helper network resnet =
  -- one round of FF --
  let
    maybe_augmenting = augmenting network resnet
  in
    case maybe_augmenting of
      Just (aug_path, aug_flow) ->
        if aug_flow == 0 then
          {network=network, resnet=resnet, aug_path= Just aug_path, flow=calcFlow network}
        else
          let
            (aug_network, aug_resnet) = pushFlow network resnet aug_path aug_flow
          in
            {network=aug_network, resnet=aug_resnet, aug_path= Just aug_path, flow=calcFlow aug_network}
            --ford_fulkerson_helper aug_network aug_resnet
      Nothing ->
        {network=network, resnet=resnet, aug_path=Nothing, flow=calcFlow network}


