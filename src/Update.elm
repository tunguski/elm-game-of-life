module Update exposing (aliveCondition, andMap, lookupFields, oks, parseInt, potentialFields, shouldBeAlive, step, sumCoordinates, update, updateLiving)

import Array exposing (..)
import Model exposing (..)
import Set
import String exposing (toInt)
import Time exposing (Posix, posixToMillis)


step model =
    let
        updatedGrid =
            updateLiving model

        updatedModel =
            { model
                | grid = updatedGrid
                , history = List.take 20 (model.grid :: model.history)
            }
    in
    case isEmpty updatedGrid of
        -- || List.member model.grid model.history of
        True ->
            ( { updatedModel | running = False }
            , Cmd.none
            )

        False ->
            ( { updatedModel | round = updatedModel.round + 1 }
            , Cmd.none
            )



{- -}


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        None ->
            ( model
            , Cmd.none
            )

        Tick newTime ->
            let
                interval =
                    posixToMillis newTime - posixToMillis model.time
            in
            if (toFloat interval * toFloat model.speed) > 1000 && model.running then
                let
                    ( updatedModel, cmd ) =
                        step model
                in
                ( { updatedModel
                    | time = newTime
                    , intervalLog = toFloat interval
                  }
                , Cmd.none
                )

            else
                ( model
                , Cmd.none
                )

        OneStep ->
            step model

        ToggleCell row col ->
            ( { model | grid = updateGrid row col model.grid }
            , Cmd.none
            )

        Running start ->
            ( { model | running = start }
            , Cmd.none
            )

        Setup pattern ->
            ( { model
                | running = False
                , grid = Model.fromList model pattern
                , round = 0
                , history = []
              }
            , Cmd.none
            )

        Resize height width ->
            ( { model
                | height = height
                , width = width
              }
            , Cmd.none
            )

        Speed speed ->
            ( { model | speed = speed }
            , Cmd.none
            )



{- cartesian join -}


andMap :
    List a
    -> List (a -> b)
    -> List b -- like (<*>) in Haskell, specialised to lists
andMap listOfAs listOfFuncs =
    List.concatMap (\f -> List.map f listOfAs) listOfFuncs



{- -}


lookupFields : List ( Int, Int )
lookupFields =
    List.map (\a b -> ( a, b )) (List.range -1 1)
        |> andMap (List.range -1 1)



{- -}


potentialFields : Grid -> List ( Int, Int )
potentialFields grid =
    let
        alive =
            toIndexedList grid
                |> List.map
                    (\( row, list ) ->
                        toIndexedList list
                            |> List.map
                                (\( col, elem ) ->
                                    elem |> Maybe.map (\int -> ( row, col ))
                                )
                    )
                |> List.foldr (\elem list -> List.append elem list) []
                |> List.foldr
                    (\elem list ->
                        case elem of
                            Just a ->
                                a :: list

                            Nothing ->
                                list
                    )
                    []
    in
    List.map sumCoordinates lookupFields
        |> andMap alive
        |> Set.fromList
        |> Set.toList



{- -}


sumCoordinates : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
sumCoordinates ( a, b ) ( c, d ) =
    ( a + c, b + d )



{- Is nearby field alive and should be counted -}


aliveCondition : Model -> Int -> Int -> ( Int, Int ) -> Bool
aliveCondition model row col ( r, c ) =
    (r /= 0 || c /= 0)
        && (row + r)
        >= 0
        && (row + r)
        < model.height
        && (col + c)
        >= 0
        && (col + c)
        < model.width
        && isCellAlive (row + r) (col + c) model.grid



{- Count living neighbours -}


oks : Model -> ( Int, Int ) -> Int
oks model ( row, col ) =
    List.length
        (List.filter identity
            (List.map (aliveCondition model row col) lookupFields)
        )



{- Should cell be alive in next iteration -}


shouldBeAlive : Grid -> ( Int, Int ) -> Int -> Maybe ( Int, Int )
shouldBeAlive grid ( row, col ) value =
    case isCellAlive row col grid of
        True ->
            case value == 2 || value == 3 of
                True ->
                    Just ( row, col )

                False ->
                    Nothing

        False ->
            case value == 3 of
                True ->
                    Just ( row, col )

                False ->
                    Nothing



{- Create new grid with aliveness for next iteration -}


updateLiving : Model -> Grid
updateLiving model =
    Model.fromList model <|
        List.filterMap
            (\s ->
                shouldBeAlive model.grid s (oks model s)
            )
            (potentialFields model.grid)


parseInt : (Int -> Msg) -> String -> Msg
parseInt fn text =
    case toInt text of
        Just int ->
            fn int

        _ ->
            None
