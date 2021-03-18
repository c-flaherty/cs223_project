module Components.GraphConstructor exposing (..)

{-
This file corresponds to the graph constructor view.
-}

---------------------- Standard Inputs --------------------------
import Browser
import Html exposing (..)
import Html.Attributes exposing (href)
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
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Maybe
import Collage.Events
import String exposing (fromInt)
import Json.Decode as Decode
import Browser.Events
import Maybe
import Time
-------------------------------------------------------

import MVC.Utils.StateHelpers exposing (..)
import MVC.Types exposing (..)
import Components.Utils.GraphConstructorHelpers exposing (..)
import MVC.Utils.StateHelpers exposing (..)

graph_constructor_view : Model -> Html Msg 
graph_constructor_view model = 
  let
    graph = 
      case model.graphDraft of 
        Nothing -> Debug.todo "Bad input in GraphConstructor"
        Just {points} -> graphDraftToGraph points 

    createCirc : Point -> Collage.Collage Msg
    createCirc p = 
      Collage.Layout.impose
        (Collage.group
          [Collage.circle 25 
            |> 
              (
                if Just p.name == model.focusedVertex then Collage.outlined (Collage.solid Collage.ultrathick (uniform Color.red))
                else Collage.outlined (Collage.solid Collage.ultrathick (uniform Color.blue))
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
            |> traced (Collage.solid Collage.ultrathick (uniform Color.blue)),
          Collage.segment (midpoint.x, midpoint.y) (rotate 10 20.0)
            |> traced (Collage.solid Collage.ultrathick (uniform Color.blue)),
          Collage.segment (midpoint.x, midpoint.y) (rotate -10 20.0)
            |> traced (Collage.solid Collage.ultrathick (uniform Color.blue)),
          Collage.Text.fromString (String.fromInt e.capacity) 
            |> Collage.Text.color (Color.green)
            |> Collage.rendered 
            |> Collage.shift (rotate -90 50)
          ]
        |> Collage.Events.onClick (ClickedOnEdge {point1 = e.point1.name, point2 = e.point2.name}) 

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
          ButtonGroup.buttonGroup
          []
          [
            ButtonGroup.button [Button.primary, Button.onClick GraphConstructorToAlgo] [text "Compute Max Flow"],
            ButtonGroup.button [Button.primary, Button.onClick ToHome] [text "Home"]
          ],
          div [Spacing.pt4, Spacing.pb4] [instructions]
        ]


instructions =
  ListGroup.custom
    [ ListGroup.anchor
        [
          ListGroup.disabled,
          ListGroup.attrs [ href "#", Flex.col, Flex.alignItemsStart ]
        ]
        [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
            [ h5 [ Spacing.mb1 ] [ text "Add vertex along edge" ] ]
        , p [ Spacing.mb1 ] [ text "Clicking on edge will add another vertex along that edge." ]
        ]
    , ListGroup.anchor
        [ 
          ListGroup.disabled,
          ListGroup.attrs [ href "#", Flex.col, Flex.alignItemsStart ] ]
        [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
            [ h5 [ Spacing.mb1 ] [ text "Move vertex to new column" ] ]
        , p [ Spacing.mb1 ] [ text "Holding shift and clicking on an edge will increment the capacity along that edge in the direction of the arrow by 1." ]
        ]
    , ListGroup.anchor
        [ 
          ListGroup.disabled,
          ListGroup.attrs [ href "#", Flex.col, Flex.alignItemsStart ] ]
        [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
            [ h5 [ Spacing.mb1 ] [ text "Add edge between vertices at most one column apart" ] ]
        , p [ Spacing.mb1 ] [ text "Dragging from vertex A to vertex B will move vertex A into the column of vertex B." ]
        ]
    , ListGroup.anchor
        [ 
          ListGroup.disabled,
          ListGroup.attrs [ href "#", Flex.col, Flex.alignItemsStart ] ]
        [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
            [ h5 [ Spacing.mb1 ] [ text "Increment capacity along edge" ] ]
        , p [ Spacing.mb1 ] [ text "Holding shift and dragging from vertex A to vertex B will add an edge from vertex A to vertex B, provided theses vertices are at most one column apart." ]
        ]
    ]