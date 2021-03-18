module Components.Home exposing (..)

{-
This file corresponds to the home view.
-}

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