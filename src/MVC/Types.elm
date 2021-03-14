module MVC.Types exposing (Flow, Network, Model, Capacity, Point, Dir(..), Edge, Msg(..), Page(..), Path, Flags)

import Dict exposing (Dict)

type alias Network =
  {
    source : String, 
    sink : String,
    adj : Dict String (Dict String (Capacity, Flow))
  }
type alias Path = List ({u : String, v : String, c : Capacity, f : Flow})
type alias Capacity = Int
type alias Flow = Int 
type alias Flags = ()

type Dir = Forward | Backward | Both
type alias Point = { name:String, x:Float, y:Float }
type alias Edge = { point1:Point, point2:Point, dir:Dir, capacity:Capacity, flow:Flow}

type alias Model = 
  {
    page : Page,
    computationNetwork : Maybe Network,
    resNetwork : Maybe Network,
    graph : Maybe ({
      points : List Point,
      edges : List Edge
    }), 
    graphDraft : Maybe ({
      currentNum : Int,
      points : List (Dict String (Dict String (Capacity, Flow)))
    }),
    focusedVertex : Maybe String,
    shiftDown : Bool
  }

type Msg = ToHome
  | ToExample1
  | BuildGraph
  | ClickedOnEdge {point1 : String, point2 : String}
  | MouseDownOnVertex String 
  | MouseUpOnVertex String
  | MouseUp
  | ShiftDown 
  | ShiftUp
  | Other
  | Tick

type Page = Home 
  | Algo 
  | GraphConstructor