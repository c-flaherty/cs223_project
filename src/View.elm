module View exposing (main)

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
    graphDraft : Maybe({
      currentNum : Int,
      points : List (Dict String (Dict String (Capacity, Flow)))
    })
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
    {model | graph = Just {points = Dict.values verts, edges = edges}, page = Algo, graphDraft = Nothing}

graphDraftToGraph : Model -> { points : List Point, edges : List Edge }
graphDraftToGraph model = 
  case model.graphDraft of
    Nothing -> { points = [], edges = []}
    Just draft ->
      let
        {-
        graphDraft : Maybe({
          points : List (Dict String (Dict (Capacity, Flow)))
        })
        -}
        
        columns = List.map Dict.keys draft.points
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


insertBetween : String -> String -> Model -> Model 
insertBetween point1 point2 model = 
  case model.graphDraft of 
    Nothing       -> model 
    Just {currentNum, points} ->
      let
            -- points : List (Dict String (Dict (Capacity, Flow)))
          getIndex p ls = 
            case ls of 
              [] -> Debug.todo "Bad input in insertBetween"
              c::cols -> 
                if List.member p (Dict.keys c) then 
                  0 
                else 
                  1 + (getIndex p cols)
          
          idxOfPoint1 = getIndex point1 points 
          idxOfPoint2 = getIndex point2 points

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
                  c::(addEdge p1 p2 cols)

          
          insertPoint : Int -> Bool -> List (Dict String (Dict String (Capacity, Flow))) -> List (Dict String (Dict String (Capacity, Flow)))
          insertPoint idx new_col ls = 
            case (idx, ls, new_col) of 
              (0, [], _) -> Debug.todo "Poorly formed input in insertBetween"
              (_, [], _) -> Debug.todo "Poorly formed input in insertBetween"
              (0, c::cols, False) ->
                (Dict.insert (fromInt currentNum) Dict.empty c)::cols
              (0, c::cols, True) ->
                c::(Dict.singleton (fromInt currentNum) Dict.empty)::cols
              (_, c::cols, _) ->
                c::(insertPoint (idx-1) new_col cols)

          new_points = 
            if idxOfPoint1 == idxOfPoint2 then 
              insertPoint idxOfPoint1 False points 
              |> addEdge point1 (fromInt currentNum)
              |> addEdge (fromInt currentNum) point2
              |> removeEdge point1 point2
            else if idxOfPoint1 == idxOfPoint2 - 1 then 
              insertPoint idxOfPoint1 True points 
              |> addEdge point1 (fromInt currentNum)
              |> addEdge (fromInt currentNum) point2
              |> removeEdge point1 point2
            else if idxOfPoint1 == idxOfPoint2 + 1 then 
              insertPoint idxOfPoint2 True points 
              |> addEdge point1 (fromInt currentNum)
              |> addEdge (fromInt currentNum) point2
              |> removeEdge point1 point2
            else if idxOfPoint1 < idxOfPoint2 - 1 then 
              insertPoint (idxOfPoint1+1) False points 
              |> addEdge point1 (fromInt currentNum)
              |> addEdge (fromInt currentNum) point2
              |> removeEdge point1 point2
            else  
              insertPoint (idxOfPoint2+1) False points
              |> addEdge point1 (fromInt currentNum)
              |> addEdge (fromInt currentNum) point2
              |> removeEdge point1 point2

      in
       {model | graphDraft = Just {currentNum = currentNum + 1, points = new_points}}


initModel : Model 
initModel = {
    page = Home,
    graph = Nothing,
    graphDraft = Nothing
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
  | ClickedOn {point1 : String, point2 : String}

type Page = Home 
  | Algo 
  | GraphConstructor

subscriptions : Model -> Sub Msg 
subscriptions model = 
  Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of  
    ToExample1 -> (networkToModel model example1, Cmd.none)
    BuildGraph -> ({model | graph = Nothing, graphDraft = initGraphDraft, page = GraphConstructor}, Cmd.none)
    ToHome -> ({model | graph = Nothing, graphDraft = Nothing, page = Home}, Cmd.none)
    ClickedOn {point1, point2} -> 
      case model.graphDraft of 
        Nothing -> (model, Cmd.none)
        Just draft -> 
          (insertBetween point1 point2 model, Cmd.none)

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
            |> Collage.shift (rotate -90 50),
          Collage.Text.fromString (String.fromInt e.flow) 
            |> Collage.Text.color (Color.red)
            |> Collage.rendered 
            |> Collage.shift (rotate 90 50)
          ]
        |> Collage.Events.onClick (ClickedOn {point1 = e.point1.name, point2 = e.point2.name})

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
    