module Model exposing (..)

import Array exposing (..)
import Time exposing (Time)

import Platform.Cmd exposing (..)


type Msg
  = None
  | Tick Time
  | OneStep
  | ToggleCell Int Int
  | Running Bool
  | Setup (List (Int, Int))
  | Resize Int Int
  | Speed Int


type alias Grid = Array (Array (Maybe Int))


type alias Model =
  { time : Time
  , round : Int
  , grid : Grid
  , width : Int
  , height : Int
  , running : Bool
  , speed : Int
  , intervalLog : Float
  , history : List Grid
  }


defaultSize : Int
defaultSize = 50


init : (Model, Cmd Msg)
init =
  let
    baseModel =
      { time = 0
      , round = 0
      , grid = empty
      , width = defaultSize
      , height = defaultSize
      , running = False
      , speed = 5
      , intervalLog = 0
      , history = []
      }
  in
    { baseModel | grid = fromList baseModel [] } ![]


fromList : Model -> List (Int, Int) -> Grid
fromList model list =
  let
    empty = initialize model.height
      (\row -> initialize model.width (\col -> Nothing))
  in
    List.foldr (\(row, col) grid ->
      updateGrid row col grid
    ) empty list


{-  -}
updateGrid : Int -> Int -> Grid -> Grid
updateGrid row col grid =
  get row grid |> Maybe.map (\r ->
    set row
      (set col (if isCellAlive row col grid then Nothing else Just 1) r)
      grid
  ) |> Maybe.withDefault grid


{- Is cell alive -}
isCellAlive : Int -> Int -> Grid -> Bool
isCellAlive row col grid =
  get row grid
    |> Maybe.map (\r -> get col r)
    |> (flip Maybe.andThen) identity
    |> Maybe.map (\c ->
      case c of
        Just val -> True
        Nothing -> False
    )
    |> Maybe.withDefault False


