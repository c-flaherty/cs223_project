module View exposing (..)

-- Add/modify imports if you'd like. ---------------------------------

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

----------------------------------------------------------------------

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
import Collage.Events
import Json.Decode as Decode

import ViewHelpers exposing(..)
import Browser.Events
import Maybe

import Collage.Events

type alias Network =
  {
    source : String, 
    sink : String,
    adj : Dict String (Dict String (Capacity, Flow)),
    augmenting_path : List String
  }

type Dir = Forward | Backward | Both
type alias Point = { name:String, x:Float, y:Float }
type alias Edge = { point1:Point, point2:Point, dir:Dir, capacity:Capacity, flow:Flow}

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
      |> Dict.insert "v6" Dict.empty,
    augmenting_path = []
  }

initGraphDraft :  Maybe { currentNum : Int, points : List (Dict String (Dict String (Capacity, Flow))) }
initGraphDraft = 
  Just {
    currentNum = 0,
    points = [
        Dict.fromList [("s", Dict.fromList [("t", (0,0))])],
        Dict.fromList [("t", Dict.empty)]
      ]
  }

type alias Model = 
  {
    page : Page,
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
    {model | graph = Just {points = Dict.values verts, edges = edges}, page = Algo, graphDraft = Nothing, focusedVertex = Nothing}

graphDraftToGraph : Model -> { points : List Point, edges : List Edge }
graphDraftToGraph model = 
  case model.graphDraft of
    Nothing -> { points = [], edges = []}
    Just draft ->
      let
        {-
        sortColumns acc cols = 
           case (cols, acc) of 
            ([], _ -> List.reverse acc 
            (col::colss, []) -> 
              let
                comparer v1 v2 = 
                  if (Maybe.andThen (\d -> Just (Dict.member v2 d)) (Dict.get v1 col) == Just True) then 
                    LT
                  else if (Maybe.andThen (\d -> Just (Dict.member v1 d)) (Dict.get v2 col) == Just True) then 
                    GT
                  else 
                    LT
              in
                sortColumns [List.sortWith comparer col] colss 
            (col::colss, prev::accTail) -> 
              let 
                comparer v1 v2 = 
                  if (Maybe.andThen (\d -> Just (Dict.member v2 d)) (Dict.get v1 col) == Just True) then 
                    LT
                  else if (Maybe.andThen (\d -> Just (Dict.member v1 d)) (Dict.get v2 col) == Just True) then 
                    GT
                  else if List.filter (\v -> v==v1 || v==v2) prev  == [v2, v1] then 
                    GT 
                  else 
                    LT        
              in
                sortColumns ((List.sortWith comparer col)::acc)
              

        sortedCols = sortColumns [] (List.reverse draft.points)
        -}
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
          
        columns = List.map topologicalSortCol draft.points

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
          List.map Dict.toList draft.points 
          |> List.concat 
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
        {points = Dict.values verts, edges = edges}

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


initModel : Model 
initModel = {
    page = Home,
    graph = Nothing,
    graphDraft = Nothing,
    focusedVertex = Nothing,
    shiftDown = False
  }

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

type Page = Home 
  | Algo 
  | GraphConstructor

subscriptions : Model -> Sub Msg 
subscriptions model = 
  Sub.batch 
    [
      Browser.Events.onMouseUp
        (Decode.succeed MouseUp),
      Browser.Events.onKeyDown
        (Decode.map
          (\key -> if key == "Shift" then ShiftDown else Other)
          (Decode.field "key" Decode.string)
        ),
      Browser.Events.onKeyUp
        (Decode.map
          (\key -> if key == "Shift" then ShiftUp else Other)
          (Decode.field "key" Decode.string)
        )
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of  
    ToExample1 -> (networkToModel model example1, Cmd.none)
    BuildGraph -> ({model | graph = Nothing, graphDraft = initGraphDraft, page = GraphConstructor, focusedVertex = Nothing}, Cmd.none)
    ToHome -> ({model | graph = Nothing, graphDraft = Nothing, page = Home, focusedVertex = Nothing}, Cmd.none)
    ClickedOnEdge {point1, point2} -> 
      case model.graphDraft of 
        Nothing -> (model, Cmd.none)
        Just draft -> 
          if model.shiftDown then 
            (incrementCapacityOfModel point1 point2 model, Cmd.none) 
          else
            (insertBetween point1 point2 model, Cmd.none)
    MouseDownOnVertex point ->
      if point == "t" then 
        (model, Cmd.none)
      else 
        ({model | focusedVertex = Just point}, Cmd.none)
    MouseUpOnVertex point ->
      case model.focusedVertex of 
        Nothing -> (model, Cmd.none)
        Just point1 -> 
          if model.shiftDown then 
            if point == point1 || point == "s" then 
              (model, Cmd.none)
            else 
              (addEdgeToModel point1 point model, Cmd.none)
          else
            if point1 == "s" || point1 == "t"  || point == "s" || point == "t" || point == point1 then 
              (model, Cmd.none)
            else 
              (changeColumn point1 point model, Cmd.none)
    MouseUp ->
      ({model | focusedVertex = Nothing}, Cmd.none)
    ShiftDown -> 
      ({model | shiftDown = True}, Cmd.none)
    ShiftUp -> 
      ({model | shiftDown = False}, Cmd.none)
    Other -> 
      (model, Cmd.none)


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

    points = List.map createCirc (Maybe.withDefault [] (Maybe.andThen (\g -> Just g.points) model.graph))
    edges = List.map createEdge (Maybe.withDefault [] (Maybe.andThen (\g -> Just g.edges) model.graph))

    foreground = Collage.group (points ++ edges)
      |> Collage.shift (-400, -250)
    background = rectangle 1000 600
      |> Collage.outlined Collage.invisible
      
    scene = Collage.Layout.impose foreground background 
  in
    Grid.container
      [
        Html.Attributes.style "height" "100vh",
        Html.Attributes.style "display" "grid",
        Html.Attributes.style "place-items" "center"
      ]
      [ 
        CDN.stylesheet,
        scene
          |> svg,
        Button.button [Button.primary, Button.onClick ToHome] [text "Home"]
      ]

home_view : Model -> Html Msg 
home_view model = 
    Grid.container [
      Html.Attributes.style "height" "100vh",
      Html.Attributes.style "display" "grid",
      Html.Attributes.style "place-items" "center"
    ]
      [
        CDN.stylesheet,
        ButtonGroup.buttonGroup
          []
          [
            ButtonGroup.button [Button.primary, Button.onClick ToExample1] [text "Example 1"],
            ButtonGroup.button [Button.primary, Button.onClick BuildGraph] [text "BuildGraph"]
          ]
      ]

graph_constructor_view : Model -> Html Msg 
graph_constructor_view model = 
  let
    graph = graphDraftToGraph model 

    createCirc : Point -> Collage.Collage Msg
    createCirc p = 
      Collage.Layout.impose
        (Collage.group
          [Collage.circle 25 
            |> 
              (
                if Just p.name == model.focusedVertex then Collage.outlined (Collage.solid Collage.thick (uniform Color.red))
                else Collage.outlined (Collage.solid Collage.thick (uniform Color.blue))
              )
            |> Collage.shift (p.x, p.y),
          Collage.Text.fromString p.name |> Collage.rendered |> Collage.shift (p.x, p.y)])
        (Collage.circle 25
          |> filled (uniform Color.white)
          |> Collage.shift (p.x, p.y))
          |> Collage.Events.onMouseDown (\_ -> MouseDownOnVertex p.name)
          |> Collage.Events.onMouseUp (\_ -> MouseUpOnVertex p.name)



    createEdge : Edge -> Collage.Collage Msg
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
            |> Collage.shift (rotate -90 50)
          ]
        |> Collage.Events.onClick (ClickedOnEdge {point1 = e.point1.name, point2 = e.point2.name}) 

    points = List.map createCirc graph.points
    edges = List.map createEdge graph.edges

    foreground = Collage.group (points ++ edges)
      |> Collage.shift (-500, -250)
    background = rectangle 1200 600
      |> Collage.outlined Collage.invisible
      
    scene = Collage.Layout.impose foreground background


  in
    Grid.container
        [
          Html.Attributes.style "height" "100vh",
          Html.Attributes.style "display" "grid",
          Html.Attributes.style "place-items" "center"
        ]
        [ 
          CDN.stylesheet,
          scene
            |> svg,
          Button.button [Button.primary, Button.onClick ToHome] [text "Home"]
        ]

view : Model -> Html Msg 
view model =
  case model.page of 
    Home -> home_view model
    Algo -> algo_view model
    GraphConstructor -> graph_constructor_view model
    