module Patterns exposing (RLE, Structure, acorn, copperHead, definedStructures, dieHard, firstPattern, gliderString, gosperGliderGun, gosperGliderGunDestructionString, gosperGliderGunString, mapWithDefault, methuselahs, oscillators, parseCellMultiplier, parsePattern, parseRle, parseRleInternal, processAliveCell, processDeadCell, processEndOfLine, rPentomino, theSmallestKnownPeriod60gun)

import Array exposing (..)
import Regex exposing (..)
import String


dieHard : List ( Int, Int )
dieHard =
    [ ( 20, 20 )
    , ( 20, 21 )
    , ( 21, 21 )
    , ( 21, 25 )
    , ( 21, 26 )
    , ( 21, 27 )
    , ( 19, 26 )
    ]


acorn : List ( Int, Int )
acorn =
    [ ( 60, 60 )
    , ( 60, 61 )
    , ( 58, 61 )
    , ( 59, 63 )
    , ( 60, 64 )
    , ( 60, 65 )
    , ( 60, 66 )
    ]


rPentomino : List ( Int, Int )
rPentomino =
    [ ( 20, 20 )
    , ( 20, 21 )
    , ( 20, 22 )
    , ( 19, 21 )
    , ( 21, 20 )
    ]


gosperGliderGun : List ( Int, Int )
gosperGliderGun =
    [ ( 21, 11 )
    , ( 22, 11 )
    , ( 21, 12 )
    , ( 22, 12 )
    , ( 21, 21 )
    , ( 22, 21 )
    , ( 23, 21 )
    , ( 20, 22 )
    , ( 24, 22 )
    , ( 19, 23 )
    , ( 25, 23 )
    , ( 19, 24 )
    , ( 25, 24 )
    , ( 22, 25 )
    , ( 20, 26 )
    , ( 24, 26 )
    , ( 21, 27 )
    , ( 22, 27 )
    , ( 23, 27 )
    , ( 22, 28 )
    , ( 19, 31 )
    , ( 20, 31 )
    , ( 21, 31 )
    , ( 19, 32 )
    , ( 20, 32 )
    , ( 21, 32 )
    , ( 18, 33 )
    , ( 22, 33 )
    , ( 18, 35 )
    , ( 22, 35 )
    , ( 17, 35 )
    , ( 23, 35 )
    , ( 19, 45 )
    , ( 20, 45 )
    , ( 19, 46 )
    , ( 20, 46 )
    ]


gosperGliderGunDestructionString =
    "15bo31b$16bo30b$14b3o30b6$44bo2b$44bobo$obo11bo29b2ob$b2o11b4o29b$bo13b4o10b2o16b$4b2o9bo2bo9bobo16b$4b2o9b4o8b3o8b2o7b$14b4o8b3o9b2o7b$14bo12b3o17b$28bobo16b$6b2o21b2o16b$7b2o38b$6bo40b$37b2o8b$37bobo7b$37bo!"


gliderString =
    "bo$2bo$3o!"


gosperGliderGunString =
    "24bo$22bobo$12b2o6b2o12b2o$11bo3bo4b2o12b2o$2o8bo5bo3b2o$2o8bo3bob2o4bobo$10bo5bo7bo$11bo3bo$12b2o!"


theSmallestKnownPeriod60gun =
    "27bo11b$25bobo11b$15b2o6b2o12b2o$14bo3bo4b2o12b2o$3b2o8bo5bo3b2o14b$3b2o8bo3bob2o4bobo11b$13bo5bo7bo11b$14bo3bo20b$15b2o22b$26bo12b$27b2o10b$26b2o11b4$21b2o16b$9bobo10b2o15b$9bo2bo8bo17b$2o10b2o11b2o12b$2o8bo3b2o8bobo12b$5b2o5b2o9bo6b2o7b$4bo4bo2bo10bo2bo2bo2bo6b$9bobo11bo6b3o6b$24bobo5b3o4b$25b2o6bobo3b$35bo3b$35b2o!"


copperHead =
    "4b2o$3b4o2$2b6o$3b4o2$2b2o2b2o$2obo2bob2o$3bo2bo3$4b2o$4b2o!"


type alias RLE =
    { comment : Maybe String
    , name : Maybe String
    , author : Maybe String
    , coordinates : Maybe ( Int, Int )
    , pattern : List ( Int, Int )
    }


parseRle : String -> Maybe RLE
parseRle input =
    Maybe.map (\( row, col, rle ) -> rle) <|
        parseRleInternal input 0 ( 0, 0, RLE Nothing Nothing Nothing Nothing [] )


parseRleInternal : String -> Int -> ( Int, Int, RLE ) -> Maybe ( Int, Int, RLE )
parseRleInternal input processed ( row, col, rle ) =
    let
        parser pattern =
            case Regex.fromString pattern of
                Just r ->
                    List.head (find r input)

                _ ->
                    Nothing

        deadCell =
            parser "(\\d+)?b"

        aliveCell =
            parser "(\\d+)?o"

        endOfLine =
            parser "\\$"

        executionMapper =
            [ ( deadCell, processDeadCell )
            , ( aliveCell, processAliveCell )
            , ( endOfLine, processEndOfLine )
            ]
    in
    if String.length input == 0 then
        Just ( row, col, rle )

    else if processed >= 1000 then
        Debug.log "Too long pattern" (Just ( row, col, rle ))

    else
        case List.head <| List.filter (\( pattern, processor ) -> firstPattern pattern) executionMapper of
            Just ( Just pattern, processor ) ->
                parseRleInternal (String.dropLeft (String.length pattern.match) input)
                    (processed + 1)
                    (processor pattern ( row, col, rle ))

            _ ->
                let
                    x =
                        Debug.log "No match" input
                in
                parseRleInternal (String.dropLeft 1 input)
                    (processed + 1)
                    ( row, col, rle )


firstPattern : Maybe Match -> Bool
firstPattern maybeMatch =
    mapWithDefault False (\match -> match.index == 0) maybeMatch


mapWithDefault : a -> (b -> a) -> Maybe b -> a
mapWithDefault default processor maybe =
    case maybe of
        Just value ->
            processor value

        Nothing ->
            default


parseCellMultiplier : Match -> Int
parseCellMultiplier match =
    case List.head match.submatches of
        Just (Just submatch) ->
            case String.toInt submatch of
                Just int ->
                    int

                _ ->
                    1

        _ ->
            1


processDeadCell : Match -> ( Int, Int, RLE ) -> ( Int, Int, RLE )
processDeadCell match ( row, col, rle ) =
    ( row, col + parseCellMultiplier match, rle )


processAliveCell : Match -> ( Int, Int, RLE ) -> ( Int, Int, RLE )
processAliveCell match ( row, col, rle ) =
    let
        times =
            parseCellMultiplier match
    in
    ( row
    , col + times
    , { rle
        | pattern =
            List.append rle.pattern
                (toList <| map (\s -> ( row, col + s )) <| initialize times identity)
      }
    )


processEndOfLine : Match -> ( Int, Int, RLE ) -> ( Int, Int, RLE )
processEndOfLine match ( row, col, rle ) =
    ( row + 1, 0, rle )


type alias Structure =
    ( String, List ( Int, Int ) )


parsePattern : String -> String -> ( String, List ( Int, Int ) )
parsePattern name pattern =
    ( name, mapWithDefault [] (\rle -> rle.pattern) <| parseRle pattern )


methuselahs =
    [ ( "Die hard", dieHard )
    , ( "Acorn", acorn )
    , ( "R-pentomino", rPentomino )
    , ( "Gosper glider gun", gosperGliderGun )
    , parsePattern "gosper Glider Gun" gosperGliderGunString
    , parsePattern "glider" gliderString
    , parsePattern "gosper Glider Gun Destruction" gosperGliderGunDestructionString
    , parsePattern "the Smallest Known Period 60 gun" theSmallestKnownPeriod60gun
    , parsePattern "CopperHead" copperHead
    ]


oscillators =
    [ ( "", [] )
    , ( "", [] )
    , ( "", [] )
    , ( "", [] )
    ]


definedStructures : List ( String, List Structure )
definedStructures =
    [ ( "Methuselahs", methuselahs )
    , ( "Oscillators", oscillators )
    ]
