module Index exposing (..)

{-
Compiling this file produces the index.html page for the app.
-}

import Html 
import Browser

import MVC.State exposing (..) 
import MVC.View exposing (..)
import MVC.Types exposing (..)

main : Program Flags Model Msg
main = 
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }