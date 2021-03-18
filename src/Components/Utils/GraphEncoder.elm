module Components.Utils.GraphEncoder exposing (..)

{-
This file contains the logic to encode/decode networks to JSON 
for the purposes of downloading/uploading.
-}

import Json.Encode as Mcoder
import Json.Decode as Decoder
import String
import Dict exposing (Dict)
import File

import MVC.Types exposing (..)
import Examples.Example1 exposing (..)

toJSON : { currentNum : Int, points : List AdjacencyList } -> String
toJSON {currentNum, points} = 
  Mcoder.object
    [
      ("currentNum", Mcoder.int currentNum),
      (
        "points", 
        List.map (Dict.map (\v -> \d -> Mcoder.dict (\s -> s) (\(c,f) -> Mcoder.string ((String.fromInt c) ++ "," ++ (String.fromInt f))) d)) points
          |> List.map (Mcoder.dict (\s -> s) (\v -> v))
          |> Mcoder.list (\v -> v)
      )
    ]
    |> Mcoder.encode 0

fromJSON : String -> { currentNum : Int, points : List AdjacencyList }
fromJSON jsonStr = 
  case Decoder.decodeString dr jsonStr of 
    Ok v -> v 
    _    -> Debug.todo "hi"

dr : Decoder.Decoder ({ currentNum : Int, points : List AdjacencyList })
dr = 
  Decoder.map2
    (\currentNum -> \points -> {currentNum=currentNum, points=points})
    (Decoder.field "currentNum" Decoder.int)
    (Decoder.field "points"
      (Decoder.list
      <| Decoder.dict
      <| Decoder.dict 
      <| Decoder.map (\s -> 
        case (String.split "," s) of 
          [c,f] -> 
            case (String.toInt c, String.toInt f) of 
              (Just cInt, Just fInt) -> (cInt, fInt)
              _                      -> Debug.todo "Can't happen"
          _     -> Debug.todo "Can't happen")
        Decoder.string)
    )