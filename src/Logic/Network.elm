module Logic.Network exposing(ford_fulkerson_helper)

import Dict
import Debug
import Set exposing (Set)

import MVC.Types exposing (..)

fromMaybe : Maybe a -> a
fromMaybe x =
  case x of
    Just y -> y
    Nothing -> Debug.todo "fromJust Nothing"

------------------------------------------------- Network API ------------------------------------------------------
empty : Network 
empty =
  {source="", sink="", adj=Dict.empty}

addEdge =
  Debug.todo "addEdge"

deleteEdge =
  Debug.todo "deleteEdge"

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

getNeighbors : Network -> String -> List (String)
getNeighbors network u =
  let
    nbrs_dict_list = Dict.get u network.adj |> Maybe.withDefault Dict.empty |> Dict.toList
  in
    List.map (Tuple.first) nbrs_dict_list

calcFlow : Network -> Flow
calcFlow network =
  let
    foo acc nbrs =
      case nbrs of
        nbr :: nbrs_rest ->
          {-
          let
            nbr = nbr_dict 
              |> Dict.keys
              |> List.head
              |> Maybe.withDefault "DEFAULT"
          in
          -}
          foo (acc + (getFlow network network.source nbr)) nbrs_rest
        [] ->
          acc
  in
    --foo 0 (Dict.get network.source network.adj |> Maybe.withDefault Dict.empty |> Dict.toList) 
    foo 0 (getNeighbors network network.source)
----------------------------------------------------------------------------------------------------------------

constructPath : Network -> List String -> Path
constructPath network path =
-- Construct a List of {u, v, c, f} from a List of vertices
  let 
    foo p =
      case p of
        n1 :: n2 :: rest ->
          (n1, n2) :: (foo (n2::rest))
        _ ->
          []
  in
    List.map (\(n1, n2) -> {u=n1, v=n2, c=getCapacity network n1 n2, f=getFlow network n1 n2}) (foo path)

iterateNbrs : Network -> Set String -> List String -> (Set String, Maybe (List String))
iterateNbrs resnet visited nbrs =
-- Do dfs on all neighors of the vertex from the call
  case nbrs of
    [] -> (visited, Nothing)
    next::nbrs_rest ->
      case dfsHelper resnet next resnet.sink visited of
        (new_visited, Just path) ->
          (new_visited, Just (next::path))
        (new_visited, Nothing) ->
          iterateNbrs resnet visited nbrs_rest

dfsHelper : Network -> String -> String -> Set String -> (Set String, Maybe (List String))
dfsHelper resnet cur sink visited =
  if cur == sink then
    (Set.insert cur visited, Just [cur])
  else if String.isEmpty cur then
    (visited, Nothing)
  else if Set.member cur visited then
    (visited, Nothing)
  else
    let
      cur_visited = Set.insert cur visited

      capIsPositive v =
        (getCapacity resnet cur v) > 0

      notVisited v =
        not (Set.member v cur_visited)

      cur_nbrs = getNeighbors resnet cur 
        |> List.filter capIsPositive 
        |> List.filter notVisited
    in
      case iterateNbrs resnet cur_visited cur_nbrs of
        (new_visited, Just path) ->
          (new_visited, Just (cur::path))
        (new_visited, Nothing) ->
          (new_visited, Nothing)

dfs : Network -> String -> String -> Maybe Path
dfs resnet source sink =
  case dfsHelper resnet source sink Set.empty of
    (_, Just path) ->
      Just (constructPath resnet path)
    (_, Nothing) ->
      Nothing
 
augmenting : Network -> Network -> Maybe (Path, Flow)
augmenting network resnet =
  let
    maybe_path = dfs resnet network.source network.sink
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
        temp_aug_network = updateFlow network e.u e.v (aug_flow + getFlow network e.u e.v)
        aug_network = updateFlow temp_aug_network e.v e.u (-aug_flow + getFlow network e.v e.u)
        temp_aug_resnet = updateCapacity resnet e.u e.v (-aug_flow + getCapacity resnet e.u e.v)
        aug_resnet = updateCapacity temp_aug_resnet e.v e.u (aug_flow + getCapacity resnet e.v e.u)
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
