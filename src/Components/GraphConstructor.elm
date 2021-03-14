module Components.GraphConstructor exposing (..)

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

import Utils.ViewHelpers exposing (..)
import MVC.Types exposing (..)
import Components.Utils.GraphConstructorHelpers exposing (..)

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