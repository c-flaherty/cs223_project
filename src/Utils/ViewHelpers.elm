module Utils.ViewHelpers exposing (..)

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
--type alias Capacity = Int
--type alias Flow = Int 

networkToModel : Model -> Network -> Model 
networkToModel model g = 
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
        
    columns = genModelColumns g.adj Set.empty [[g.source]] [g.source]
    num_columns = List.length columns 

    genColPos : Float -> List String -> List Point
    genColPos x col = 
      case col of 
        [v] -> [{name = v, x=x, y=toFloat 250}]
        _ -> List.indexedMap (\y_idx -> \v -> {name = v, x=x, y=toFloat (y_idx*(500//((List.length col) - 1)))}) col

    verts = List.indexedMap (\x_idx -> \col -> genColPos ((toFloat x_idx)*(1000.0/(toFloat (num_columns - 1)))) col) columns
      |> List.concat
      |> List.foldr (\p -> \d -> Dict.insert p.name {name=p.name, x=p.x,y=p.y} d) Dict.empty

    edges = 
      Dict.toList g.adj 
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
  in
    {model | graph = Just {points = Dict.values verts, edges = edges}, computationNetwork = Just g, page = Algo, graphDraft = Nothing, focusedVertex = Nothing}
