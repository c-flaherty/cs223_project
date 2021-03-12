module View exposing (main)

-- Add/modify imports if you'd like. ---------------------------------

import Browser
import Html exposing (Html, text)
import Html.Attributes
import Debug
import Collage exposing (circle, rectangle, filled, uniform, traced)
import Collage.Layout exposing (at, topLeft)
import Collage.Render exposing (svg)
import Collage.Text
import Color
import Set exposing (Set)

----------------------------------------------------------------------

import Dict exposing (Dict)
import Maybe
import Collage.Layout
import Bootstrap.Grid as Grid
import Bootstrap.CDN as CDN
import Bootstrap.Button as Button

type alias Network =
  {
    source : String, 
    sink : String,
    adj : Dict String (Dict String (Capacity, Flow)),
    augmenting_path : List String
  }

type alias Capacity = Int
type alias Flow = Int 

type Dir = Forward | Backward | Both
type alias Point = { name:String, x:Float, y:Float }
type alias Edge = { point1:Point, point2:Point, dir:Dir, capacity:Capacity, flow:Flow}

example : Network
example = 
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
      |> Dict.insert "v6" Dict.empty,
    augmenting_path = []
  }

type alias Model = 
  {
    run : Bool,
    points : List Point,
    edges : List Edge
  }

networkToModel : Network -> Model 
networkToModel g = 
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
    {points = Dict.values verts, edges = edges, run=False}

initModel : Model 
initModel = networkToModel example

main : Program Flags Model Msg
main = 
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Flags = ()
init : Flags -> (Model, Cmd Msg)
init () =
  (initModel, Cmd.none)

type Msg = MouseDown 
  | Tick
  | Start

subscriptions : Model -> Sub Msg 
subscriptions model = 
  Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of  
    Start -> ({ model | run=True}, Cmd.none)
    _ -> (model, Cmd.none)

algo_view : Model -> Html Msg 
algo_view model = 
  let 
    createCirc : Point -> Collage.Collage msg
    createCirc p = 
      Collage.Layout.impose
        (Collage.group
          [Collage.circle 25 
            |> Collage.outlined (Collage.solid Collage.thick (uniform Color.blue))
            |> Collage.shift (p.x, p.y),
          Collage.Text.fromString p.name |> Collage.rendered |> Collage.shift (p.x, p.y)])
        (Collage.circle 25
          |> filled (uniform Color.white)
          |> Collage.shift (p.x, p.y))



    createEdge : Edge -> Collage.Collage msg
    createEdge e = 
      let
          midpoint = { x=(e.point1.x + e.point2.x)/2, y=(e.point1.y + e.point2.y)/2 }
          parallel_normed = 
            let
              x = (e.point2.x - e.point1.x)
              y = (e.point2.y - e.point1.y)
              norm = sqrt (x ^ 2 + y ^ 2)

            in 
              {x = x/norm, y =  y/norm}

          rotate deg mag = 
            (midpoint.x + mag*(cos deg)*parallel_normed.x - mag*(sin deg)*parallel_normed.y, midpoint.y + mag*(sin deg)*parallel_normed.x + mag*(cos deg)*parallel_normed.y)
      in
        Collage.group  
          [Collage.segment (e.point1.x, e.point1.y) (e.point2.x, e.point2.y) 
            |> traced (Collage.solid Collage.thick (uniform Color.blue)),
          Collage.segment (midpoint.x, midpoint.y) (rotate 10 20.0)
            |> traced (Collage.solid Collage.thick (uniform Color.blue)),
          Collage.segment (midpoint.x, midpoint.y) (rotate -10 20.0)
            |> traced (Collage.solid Collage.thick (uniform Color.blue)),
          Collage.Text.fromString (String.fromInt e.capacity) 
            |> Collage.Text.color (Color.green)
            |> Collage.rendered 
            |> Collage.shift (rotate -90 50),
          Collage.Text.fromString (String.fromInt e.flow) 
            |> Collage.Text.color (Color.red)
            |> Collage.rendered 
            |> Collage.shift (rotate 90 50)
          ]

    points = List.map createCirc model.points 
    edges = List.map createEdge model.edges 

    foreground = Collage.group (points ++ edges)
      |> Collage.shift (-500, -250)
    background = rectangle 1000 600
      |> Collage.outlined Collage.invisible
      |> Collage.shift (-500, -300)

    scene = Collage.Layout.impose foreground background 

    hspace = Collage.Layout.spacer 0 50 

    text = Collage.Text.fromString "Hi!" |> Collage.rendered

    all = Collage.Layout.vertical <| [scene, hspace, text]
  in
    Grid.container
      [
        Html.Attributes.style "height" "100vh",
        Html.Attributes.style "display" "grid",
        Html.Attributes.style "place-items" "center"
      ]
      [ 
        CDN.stylesheet,
        all
          |> svg
      ]

construct_graph_view : Model -> Html Msg 
construct_graph_view model = 
  let
      a = 1
  in
    Grid.container [
      Html.Attributes.style "height" "100vh",
      Html.Attributes.style "display" "grid",
      Html.Attributes.style "place-items" "center"
    ]
      [
        CDN.stylesheet,
        body model
      ]

body : Model -> Html Msg 
body model = 
  Button.button [Button.primary, Button.onClick Start] [text "Begin"]

view : Model -> Html Msg 
view model =
  if model.run then 
    algo_view model 
  else 
    construct_graph_view model