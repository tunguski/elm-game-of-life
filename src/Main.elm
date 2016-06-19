module GameOfLife exposing (main)

import Html.App as Html

import Time exposing (Time, every)

import Model exposing (..)
import View exposing (..)
import Update exposing (..)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.running then
    Time.every (1000.0 / toFloat model.speed) Tick
  else
    Sub.none

