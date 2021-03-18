module Components.Utils.GraphConstructorHelpers exposing (..)

{-
This file provides helper functions for the graph constructor view.
-}

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

import MVC.Utils.StateHelpers exposing (..)
import MVC.Types exposing (..)

initGraphDraft :  Maybe { currentNum : Int, points : List AdjacencyList }
initGraphDraft = 
  Just {
    currentNum = 0,
    points = [
        Dict.fromList [("s", Dict.fromList [("t", (0,0))])],
        Dict.fromList [("t", Dict.empty)]
      ]
  }

changeColumn : String -> String -> Model -> Model
changeColumn point1 point2 model = 
  case model.graphDraft of 
    Nothing -> model 
    Just {currentNum, points} -> 
      if abs(getIndex point1 points - getIndex point2 points) /= 1 then 
        {model | graphDraft = Just {currentNum = currentNum, points = points}, focusedVertex = Nothing}
      else
        let
            (p1_data, tail_points) = removePoint point1 [] points 
            point2Idx = getIndex point2 tail_points
            new_points = insertPointWithData (Debug.log "point2idx:" point2Idx) False (Debug.log "point1:" point1) (Debug.log "P1Data:" p1_data) [] (Debug.log "tail_points:" tail_points)
        in
          {model | graphDraft = Just {currentNum = currentNum, points = (Debug.log "NewPoints:" new_points)}, focusedVertex = Nothing}

addEdgeToModel : String -> String -> Model -> Model
addEdgeToModel point1 point model = 
  case model.graphDraft of
    Nothing -> model 
    Just {currentNum, points} -> 
      if abs(getIndex point1 points - getIndex point points) > 1 then 
        {model | graphDraft = Just {currentNum = currentNum, points = points}, focusedVertex = Nothing}
      else 
        let
            new_points = addEdge point1 point points
        in
          {model | graphDraft = Just {currentNum = currentNum, points = new_points}, focusedVertex = Nothing}

incrementCapacityOfModel point1 point2 model = 
  case model.graphDraft of 
    Nothing -> model 
    Just {currentNum, points} -> 
      {model | graphDraft = Just {currentNum = currentNum, points = incrementCapacity point1 point2 points}, focusedVertex = Nothing}

insertBetween : String -> String -> Model -> Model 
insertBetween point1 point2 model = 
  case model.graphDraft of 
    Nothing       -> model 
    Just {currentNum, points} ->
      let
          idxOfPoint1 = getIndex point1 points 
          idxOfPoint2 = getIndex point2 points

          new_points = 
            if idxOfPoint1 == idxOfPoint2 then 
              insertPoint idxOfPoint1 False (fromInt currentNum) points 
              |> addEdge point1 (fromInt currentNum)
              |> addEdge (fromInt currentNum) point2
              |> removeEdge point1 point2
            else if idxOfPoint1 == idxOfPoint2 - 1 then 
              insertPoint idxOfPoint1 True (fromInt currentNum) points 
              |> addEdge point1 (fromInt currentNum)
              |> addEdge (fromInt currentNum) point2
              |> removeEdge point1 point2
            else if idxOfPoint1 == idxOfPoint2 + 1 then 
              insertPoint idxOfPoint2 True (fromInt currentNum) points 
              |> addEdge point1 (fromInt currentNum)
              |> addEdge (fromInt currentNum) point2
              |> removeEdge point1 point2
            else if idxOfPoint1 < idxOfPoint2 - 1 then 
              insertPoint (idxOfPoint1+1) False (fromInt currentNum) points 
              |> addEdge point1 (fromInt currentNum)
              |> addEdge (fromInt currentNum) point2
              |> removeEdge point1 point2
            else  
              insertPoint (idxOfPoint2+1) False (fromInt currentNum) points
              |> addEdge point1 (fromInt currentNum)
              |> addEdge (fromInt currentNum) point2
              |> removeEdge point1 point2

      in
      {model | graphDraft = Just {currentNum = currentNum + 1, points = (Debug.log "NewPointsInsertBtwn:" new_points)}, focusedVertex = Nothing}

getIndex : String -> List AdjacencyList -> Int
getIndex p ls = 
  case ls of 
    [] -> Debug.todo "Bad input in insertBetween"
    c::cols -> 
      if List.member p (Dict.keys c) then 
        0 
      else 
        1 + (getIndex p cols)

addEdge : String -> String -> List AdjacencyList -> List AdjacencyList
addEdge p1 p2 ls = 
  case ls of 
    [] -> [] 
    c::cols ->
      if Maybe.andThen (Just << Dict.member p1) (Dict.get p2 c) == Just True then 
        c::cols 
      else if List.member p1 (Dict.keys c) then 
        (Dict.update p1 (Maybe.andThen (\d -> Just <| Dict.insert p2 (0,0) d)) c)::cols
      else 
        c::(addEdge p1 p2 cols)

removeEdge : String -> String -> List AdjacencyList -> List AdjacencyList
removeEdge p1 p2 ls = 
  case ls of 
    [] -> [] 
    c::cols -> 
      if List.member p1 (Dict.keys c) then 
        (Dict.update p1 (Maybe.andThen (\d -> Just <| Dict.remove p2 d)) c)::cols
      else 
        c::(removeEdge p1 p2 cols)


insertPoint : Int -> Bool -> String -> List AdjacencyList -> List AdjacencyList
insertPoint idx new_col name ls = 
  case (idx, ls, new_col) of 
    (0, [], _) -> Debug.todo "Poorly formed input in insertBetween"
    (_, [], _) -> Debug.todo "Poorly formed input in insertBetween"
    (0, c::cols, False) ->
      (Dict.insert name Dict.empty c)::cols
    (0, c::cols, True) ->
      c::(Dict.singleton name Dict.empty)::cols
    (_, c::cols, _) ->
      c::(insertPoint (idx-1) new_col name cols)

insertPointWithData : Int -> Bool -> String -> Dict String (Capacity, Flow) -> List AdjacencyList -> List AdjacencyList -> List AdjacencyList
insertPointWithData idx new_col name data acc ls = 
  case (idx, ls, new_col) of 
    (0, [], _) -> Debug.todo "Poorly formed input in insertBetween"
    (_, [], _) -> Debug.todo "Poorly formed input in insertBetween"
    (0, c::cols, False) ->
      (List.reverse acc) ++ (Dict.insert name data c)::cols
    (0, c::cols, True) ->
      (List.reverse acc) ++ c::(Dict.singleton name data)::cols
    (_, c::cols, _) ->
      (insertPointWithData (idx-1) new_col name data (c::acc) cols)

removePoint : String -> List AdjacencyList -> List AdjacencyList -> (Dict String (Capacity, Flow), List AdjacencyList)
removePoint p acc ls = 
  case ls of 
    [] -> (Dict.empty, [])
    c::cols -> 
      case Dict.get p c of
        Nothing -> removePoint p (c::acc) cols
        Just d ->
          if Dict.isEmpty (Dict.remove p c) then
            (d, (List.reverse acc) ++ cols)
          else 
            (d, (List.reverse acc) ++ [(Dict.remove p c)] ++ cols)

incrementCapacity : String -> String -> List AdjacencyList ->  List AdjacencyList
incrementCapacity p1 p2 ls = 
  case ls of 
    [] -> [] 
    c::cols -> 
      case Dict.get p1 c of 
        Nothing -> c::(incrementCapacity p1 p2 cols)
        Just _  -> 
          (Dict.update p1 (Maybe.andThen (\d -> Just <| Dict.update p2 (Maybe.andThen (\(cap,f) -> Just (cap+1,f))) d)) c)::cols
