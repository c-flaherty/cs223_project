module MVC.Utils.StateHelpers exposing (..)

---------------------- Standard Inputs --------------------------
import Browser
import Html exposing (Html, text)
import Html.Attributes
import Debug
import Collage exposing (circle, rectangle, filled, uniform, traced)
import Collage.Layout exposing (at, topLeft, empty)
import Collage.Render exposing (svg)
import Collage.Text
import Color
import Set exposing (Set)
import Dict exposing (Dict)
import Maybe
import Collage.Layout
import Bootstrap.Grid as Grid
import Bootstrap.CDN as CDN
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Maybe
import Collage.Events
import String exposing (fromInt)
import Json.Decode as Decode
import Browser.Events
import Maybe
import Time
-------------------------------------------------------

import Dict exposing (Dict)
import Logic.Network exposing (..)
import MVC.Types exposing (..)

-- Kahn's algorithm, https://en.wikipedia.org/wiki/Topological_sorting
topologicalSortCol : Dict String (Dict String (Capacity, Flow)) -> List String
topologicalSortCol col = 
  let
      startNodes = Set.diff (Set.fromList (Dict.keys col)) (Set.fromList (List.concatMap Dict.keys (Dict.values col)))  

      sorter : List String -> Set String -> List String
      sorter acc start = 
        case Set.toList start of 
          [] -> List.reverse acc 
          x::xs -> 
            let 
              tailStart = Set.remove x start
              nextVerts = Set.intersect (Set.fromList (Dict.keys col)) (Maybe.withDefault Set.empty <| Maybe.andThen (\d -> Just <| Set.fromList <| Dict.keys d) <| Dict.get x col)
            in
              sorter (x::acc) (Set.union tailStart nextVerts)
  in
    sorter [] startNodes

genColPos : Float -> List String -> List Point
genColPos x col = 
  case col of 
    [v] -> [{name = v, x=x, y=toFloat 250}]
    _ -> List.indexedMap (\y_idx -> \v -> {name = v, x=x, y=toFloat (y_idx*(500//((List.length col) - 1)))}) col

genVerts : List (List String) -> Dict String Point
genVerts columns =
  List.indexedMap (\x_idx -> \col -> genColPos ((toFloat x_idx)*(1000.0/(toFloat ((List.length columns) - 1)))) col) columns
  |> List.concat
  |> List.foldr (\p -> \d -> Dict.insert p.name {name=p.name, x=p.x,y=p.y} d) Dict.empty

genEdges : Dict String Point -> AdjacencyList -> List Edge 
genEdges verts adj = 
  Dict.toList adj
  |> List.map (\(k,v) -> (k, Dict.toList v))
  |> List.map (\(k,v) -> List.map (\(o, (capacity, flow)) -> ((Dict.get k verts), (Dict.get o verts), (capacity, flow, Forward))) v)
  |> List.concat 
  |> \tmp ->
    (
      let
        removeMaybe ls = 
          case ls of 
            [] -> []
            (Just t1, Just t2, (capacity, flow, Forward))::lss -> {point1=t1, point2=t2, capacity=capacity, flow=flow, dir=Forward}::(removeMaybe lss)
            _::lss -> removeMaybe lss
      in
        removeMaybe tmp
    )

graphDraftToGraph : List AdjacencyList -> NetworkForViz
graphDraftToGraph draft = 
  let
    columns = List.map topologicalSortCol draft

    verts = genVerts columns
    edges = List.foldr Dict.union Dict.empty draft
      |> genEdges verts

  in
    {points = Dict.values verts, edges = edges}

networkToGraph : Network -> NetworkForViz
networkToGraph g = 
  let
    genModelColumns : Dict String (Dict String (Capacity, Flow)) -> Set String -> List (List String) -> List String -> List (List String)
    genModelColumns adj visited cols current =
      case current of 
        [] -> cols 
        _ ->
          let
            possible_next = List.concatMap (\k -> Dict.get k adj |> (Dict.keys << Maybe.withDefault Dict.empty)) current 
            next = List.filter (\v -> not (Set.member v visited)) possible_next 
              |> Set.toList << Set.fromList
            new_visited = Set.union visited (Set.fromList next)
          in
            genModelColumns adj new_visited (cols++[next]) next
        
    columnVerts = genModelColumns g.adj Set.empty [[g.source]] [g.source]
    columns = List.map (\vs -> Dict.filter (\k -> \v -> List.member k vs) g.adj) columnVerts
      |> List.map topologicalSortCol

    verts = genVerts columns
    edges = genEdges verts g.adj
      
  in
    {points = Dict.values verts, edges = edges}

graphDraftToNetwork : List AdjacencyList -> Network 
graphDraftToNetwork draft = 
  {
    source = "s", 
    sink   = "t",
    adj    = List.foldr Dict.union Dict.empty draft
  }

networkToGraphDraft : Network -> List AdjacencyList -> List AdjacencyList 
networkToGraphDraft {source, sink, adj} current_draft = 
  List.map (\col_verts -> Dict.filter (\vert -> \_ -> Dict.member vert col_verts) adj) current_draft

resetFlowForDraft : Maybe ({ currentNum : Int, points : List AdjacencyList }) -> Maybe ({ currentNum : Int, points : List AdjacencyList })
resetFlowForDraft draft = 
  case draft of 
    Nothing -> Nothing 
    Just {currentNum, points} -> 
      Just {currentNum = currentNum, points = List.map (Dict.map (\k -> \v -> Dict.map (\k1 -> \(c, f) -> (c, 0)) v)) points}