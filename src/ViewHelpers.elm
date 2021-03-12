module ViewHelpers exposing (..)

import Dict exposing (Dict)

type alias Capacity = Int
type alias Flow = Int 

getIndex : String -> List (Dict String (Dict String (Capacity, Flow))) -> Int
getIndex p ls = 
  case ls of 
    [] -> Debug.todo "Bad input in insertBetween"
    c::cols -> 
      if List.member p (Dict.keys c) then 
        0 
      else 
        1 + (getIndex p cols)

addEdge : String -> String -> List (Dict String (Dict String (Capacity, Flow))) -> List (Dict String (Dict String (Capacity, Flow)))
addEdge p1 p2 ls = 
  case ls of 
    [] -> [] 
    c::cols -> 
      if List.member p1 (Dict.keys c) then 
        (Dict.update p1 (Maybe.andThen (\d -> Just <| Dict.insert p2 (0,0) d)) c)::cols
      else 
        c::(addEdge p1 p2 cols)

removeEdge : String -> String -> List (Dict String (Dict String (Capacity, Flow))) -> List (Dict String (Dict String (Capacity, Flow)))
removeEdge p1 p2 ls = 
  case ls of 
    [] -> [] 
    c::cols -> 
      if List.member p1 (Dict.keys c) then 
        (Dict.update p1 (Maybe.andThen (\d -> Just <| Dict.remove p2 d)) c)::cols
      else 
        c::(removeEdge p1 p2 cols)


insertPoint : Int -> Bool -> String -> List (Dict String (Dict String (Capacity, Flow))) -> List (Dict String (Dict String (Capacity, Flow)))
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

insertPointWithData : Int -> Bool -> String -> Dict String (Capacity, Flow) -> List (Dict String (Dict String (Capacity, Flow))) -> List (Dict String (Dict String (Capacity, Flow))) -> List (Dict String (Dict String (Capacity, Flow)))
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

removePoint : String -> List (Dict String (Dict String (Capacity, Flow))) -> List (Dict String (Dict String (Capacity, Flow))) -> (Dict String (Capacity, Flow), List (Dict String (Dict String (Capacity, Flow))))
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

incrementCapacity : String -> String -> List (Dict String (Dict String (Capacity, Flow))) ->  List (Dict String (Dict String (Capacity, Flow)))
incrementCapacity p1 p2 ls = 
  case ls of 
    [] -> [] 
    c::cols -> 
      case Dict.get p1 c of 
        Nothing -> c::(incrementCapacity p1 p2 cols)
        Just _  -> 
          (Dict.update p1 (Maybe.andThen (\d -> Just <| Dict.update p2 (Maybe.andThen (\(cap,f) -> Just (cap+1,f))) d)) c)::cols
