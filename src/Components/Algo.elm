module Components.Algo exposing (..)

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

import MVC.Types exposing (..)
import MVC.Utils.StateHelpers exposing (..)

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
            |> traced (Collage.solid Collage.thick (uniform (if e.flow > 0 then Color.red else Color.blue) )),
          Collage.segment (midpoint.x, midpoint.y) (rotate 10 20.0)
            |> traced (Collage.solid Collage.thick (uniform (if e.flow > 0 then Color.red else Color.blue)  )),
          Collage.segment (midpoint.x, midpoint.y) (rotate -10 20.0)
            |> traced (Collage.solid Collage.thick (uniform (if e.flow > 0 then Color.red else Color.blue) )),
          Collage.Text.fromString (String.fromInt e.capacity) 
            |> Collage.Text.color (Color.green)
            |> Collage.rendered 
            |> Collage.shift (rotate -90 50),
          Collage.Text.fromString (String.fromInt e.flow) 
            |> Collage.Text.color (Color.red)
            |> Collage.rendered 
            |> Collage.shift (rotate 90 50)
          ]

    graph = 
      if model.visitingFromConstructor then 
        case model.graphDraft of 
          Nothing -> Debug.todo "Bad input in GraphConstructor"
          Just {points} -> graphDraftToGraph points 
      else 
        case model.graph of 
          Nothing -> Debug.todo "Bad input in GraphConstructor"
          Just g -> g
    pointsViz = List.map createCirc graph.points
    edgesViz = List.map createEdge graph.edges

    foreground = Collage.group (pointsViz ++ edgesViz)
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
        if model.visitingFromConstructor then
          ButtonGroup.buttonGroup
            []
            [
              ButtonGroup.button [Button.primary, Button.onClick AlgoToGraphConstructor] [text "Return to Constructor"],
              ButtonGroup.button [Button.primary, Button.onClick ToHome] [text "Home"]
            ]
        else 
          Button.button [Button.primary, Button.onClick ToHome] [text "Home"]
        
      ]
