module MVC.View exposing (..)

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

import MVC.Types exposing (Flow, Network, Model, Capacity, Point, Dir(..), Edge, Msg(..), Page(..), Path, Flags)
import Components.GraphConstructor exposing (..)
import Components.Algo exposing (..) 
import Components.Home exposing (..)

view : Model -> Html Msg 
view model =
  case model.page of 
    Home -> home_view model
    Algo -> algo_view model
    GraphConstructor -> graph_constructor_view model
    