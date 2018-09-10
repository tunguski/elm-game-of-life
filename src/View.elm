module View exposing (printLifeTable, printRow, selectStructure, showCell, showSelectGroup, showSelectOption, view)

import Array exposing (..)
import Css exposing (..)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onClick, onInput)
import Model exposing (..)
import Patterns exposing (..)
import String exposing (concat)
import Update exposing (..)


showCell : Int -> Int -> Bool -> Html Msg
showCell row col alive =
    td
        [ class
            (if alive then
                "alive"

             else
                "dead"
            )
        , onClick (ToggleCell row col)
        ]
        []


printRow : Model -> Int -> Html Msg
printRow model row =
    tr []
        (toList <|
            Array.map (\col -> showCell row col (isCellAlive row col model.grid)) <|
                initialize model.width identity
        )


printLifeTable : Model -> Html Msg
printLifeTable model =
    table []
        [ tbody []
            (toList <|
                Array.map (printRow model) <|
                    initialize model.height identity
            )
        ]


selectStructure : List ( String, List Structure ) -> Html Msg
selectStructure list =
    select
        [ class "form-control input-sm"

        {-
           on
             "change"
             (Json.Decode.at ["target", "selectedIndex"] Json.Decode.int)
             Selected
        -}
        ]
        (List.map showSelectGroup list)


showSelectGroup : ( String, List Structure ) -> Html Msg
showSelectGroup ( name, structures ) =
    optgroup [ attribute "label" name ] (List.map showSelectOption structures)


showSelectOption : Structure -> Html Msg
showSelectOption ( name, list ) =
    option [] [ text name ]


view : Model -> Html Msg
view model =
    div [ class "container-fluid" ]
        ([ node "link" [ rel "stylesheet", href "https://bootswatch.com/4/darkly/bootstrap.css" ] []
         , node "style" [] [ text cssStyle ]
         ]
            ++ innerBody model
        )


innerBody model =
    [ div [ class "row" ]
        [ div [ class "col-xs-3" ]
            [ h1 [] [ text <| Debug.log "test" "Game of Life" ]
            ]
        ]
    , div [ class "row" ]
        [ div [ class "col-xs-3" ]
            [ div [ class "form-group" ]
                [ label [] [ text "Height" ]
                , input
                    [ class "form-control input-sm"
                    , type_ "number"
                    , value (toString model.height)
                    , onInput <| parseInt <| \int -> Resize int model.width
                    ]
                    []
                ]
            ]
        , div [ class "col-xs-3" ]
            [ div [ class "form-group" ]
                [ label [] [ text "Width" ]
                , input
                    [ class "form-control input-sm"
                    , type_ "number"
                    , value (toString model.width)
                    , onInput <| parseInt <| Resize model.height
                    ]
                    []
                ]
            ]
        , div [ class "col-xs-3" ]
            [ div [ class "form-group" ]
                [ label []
                    [ text "Speed "
                    , span [ class "text-muted" ] [ text (toString model.speed) ]
                    ]
                , input
                    [ class "form-control input-sm"
                    , type_ "range"
                    , Attr.min "1"
                    , Attr.max "100"
                    , value (toString model.speed)
                    , onInput <| parseInt Speed
                    ]
                    []
                ]
            ]
        , div [ class "col-xs-3" ]
            [ div [ class "form-group" ]
                [ label [] [ text "Select" ]
                , selectStructure definedStructures
                ]
            ]
        ]
    , div [ class "btn-group" ]
        [ button
            [ class "btn btn-primary btn-sm"
            , onClick (Running (not model.running))
            ]
            [ text
                (if model.running then
                    "Stop"

                 else
                    "Start"
                )
            ]
        , button
            [ class "btn btn-primary btn-sm"
            , onClick OneStep
            ]
            [ text "One step" ]
        , button
            [ class "btn btn-warning btn-sm"
            , onClick (Setup [])
            ]
            [ text "Clear" ]
        ]
    , div [ class "btn-group" ]
        (List.map
            (\( name, list ) ->
                button
                    [ class "btn btn-default btn-sm"
                    , onClick (Setup list)
                    ]
                    [ text name ]
            )
            methuselahs
        )
    , printLifeTable model
    , text
        (concat
            [ "Speed: "
            , toString model.speed
            , " Round: "
            , toString model.round
            , " Interval: "
            , toString model.intervalLog
            , " Frames: "
            , toString <| round <| 1000 / model.intervalLog
            ]
        )
    ]
