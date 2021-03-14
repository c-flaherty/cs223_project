module MVC.State exposing (..)

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
import Logic.Network exposing (..)
import Utils.ViewHelpers exposing (..)
import Components.Utils.GraphConstructorHelpers exposing (..)
import Examples.Example1 exposing (..)

initModel : Model 
initModel = {
    page = Home,
    computationNetwork = Nothing,
    resNetwork = Nothing, 
    graph = Nothing,
    graphDraft = Nothing,
    focusedVertex = Nothing,
    shiftDown = False
  }


init : Flags -> (Model, Cmd Msg)
init () =
  (initModel, Cmd.none)

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
        ),
      Time.every 2000 (\_ -> Debug.log "Tick:" Tick)
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of  
    ToExample1 -> 
      let newModel = networkToModel model example1 
      in ({newModel | resNetwork = newModel.computationNetwork }, Cmd.none)
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
    Tick -> 
      if model.page == Algo then 
        case (model.computationNetwork, model.resNetwork) of 
          (Just cNet, Just rNet) ->
            let 
              {network, resnet, aug_path, flow} = Logic.Network.ford_fulkerson_helper cNet rNet 
              newModel = networkToModel model (Debug.log "network:" network)
            in 
              ({newModel | resNetwork = Just resnet}, Cmd.none)
          (_, _) -> (model, Cmd.none)
      else 
        (model, Cmd.none)
    Other -> 
      (model, Cmd.none)