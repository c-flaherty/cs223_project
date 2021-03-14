module Examples.Example1 exposing (..)

import Dict

import MVC.Types exposing (..)

example1 : Network
example1 = 
  {
    source = "v1", 
    sink = "v6",
    adj = 
      Dict.empty
      |> Dict.insert "v1" (Dict.fromList [("v3", (4,0)), ("v2", (3,0))])
      |> Dict.insert "v3" (Dict.fromList [("v5", (4,0))])
      |> Dict.insert "v2" (Dict.fromList [("v4", (6,0))])
      |> Dict.insert "v5" (Dict.fromList [("v2", (3,0)), ("v6", (2,0))])
      |> Dict.insert "v4" (Dict.fromList [("v6", (6,0))])
      |> Dict.insert "v6" Dict.empty
  }
