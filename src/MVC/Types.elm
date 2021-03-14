module MVC.Types exposing (Flow, Network, Model, Capacity, Point, Dir(..), Edge, Msg(..), Page(..), Path, Flags, AdjacencyList, NetworkForViz)

import Dict exposing (Dict)

type alias AdjacencyList = Dict String (Dict String (Capacity, Flow))

type alias Network =
  {
    source : String, 
    sink : String,
    adj : AdjacencyList
  }
type alias Path = List ({u : String, v : String, c : Capacity, f : Flow})
type alias Capacity = Int
type alias Flow = Int 
type alias Flags = ()

type Dir = Forward | Backward | Both
type alias Point = { name:String, x:Float, y:Float }
type alias Edge = { point1:Point, point2:Point, dir:Dir, capacity:Capacity, flow:Flow}

type alias NetworkForViz = 
  {
    points : List Point,
    edges : List Edge
  }

type alias Model = 
  {
    -- current page
    page : Page,
    visitingFromConstructor : Bool,

    -- for FF
    computationNetwork : Maybe Network,
    resNetwork : Maybe Network,

    -- for visualization with FF
    graph : Maybe NetworkForViz, 

    -- for viisualization with graph constructor
    graphDraft : Maybe ({
      currentNum : Int,
      points : List AdjacencyList
    }),

    -- I/O for graph constructor
    focusedVertex : Maybe String,
    shiftDown : Bool
  }

type Msg = ToHome
  | ToExample1
  | GraphConstructorToAlgo
  | BuildGraph
  | AlgoToGraphConstructor
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