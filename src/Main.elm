module Main exposing (Cell(..), LifeGame, Point, getCell, next)

import Array exposing (Array)


type Cell
    = Live
    | Dead


type alias LifeGame =
    { field : Array (Array Cell)
    , height : Int
    , width : Int
    }


type alias Point =
    ( Int, Int )


getCell : Point -> LifeGame -> Maybe Cell
getCell ( x, y ) lg =
    Array.get y lg.field
        |> Maybe.andThen (Array.get x)


next : LifeGame -> LifeGame
next lg =
    let
        around : Point -> List Point
        around ( x, y ) =
            [ ( x - 1, y - 1 )
            , ( x, y - 1 )
            , ( x + 1, y - 1 )
            , ( x - 1, y )
            , ( x + 1, y )
            , ( x - 1, y + 1 )
            , ( x, y + 1 )
            , ( x + 1, y + 1 )
            ]

        mapCellFunc : Int -> Int -> Cell -> Cell
        mapCellFunc y x cell =
            let
                arounds =
                    around ( x, y )

                livedAround =
                    List.map (\p -> getCell p lg) arounds
                        -- list-extra使えばcountで一発で行ける
                        |> List.filter ((==) (Just Live))
                        |> List.length
            in
            case cell of
                Live ->
                    if livedAround == 2 || livedAround == 3 then
                        Live

                    else
                        Dead

                Dead ->
                    if livedAround == 3 then
                        Live

                    else
                        Dead

        mapLineFunc : Int -> Array Cell -> Array Cell
        mapLineFunc y line =
            Array.indexedMap (mapCellFunc y) line

        nextField =
            Array.indexedMap mapLineFunc lg.field
    in
    { lg | field = nextField }
