module GameOfLife exposing (main)

import Browser
import Model exposing (..)
import Time exposing (Posix, every)
import Update exposing (..)
import View exposing (..)


main =
    Browser.element
        { init = \() -> init
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
