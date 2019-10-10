module Effect.Reader exposing (Reader, andThen, map, map2, map3)

import GameBoy exposing (GameBoy)


type alias Reader a =
    GameBoy -> ( a, GameBoy )


map : (a -> b) -> Reader a -> Reader b
map f readerA gameBoy =
    let
        ( a, gameBoyA ) =
            readerA gameBoy
    in
    ( f a, gameBoyA )


map2 : (a -> b -> c) -> Reader a -> Reader b -> Reader c
map2 f readerA readerB gameBoy =
    let
        ( a, gameBoyA ) =
            readerA gameBoy

        ( b, gameBoyB ) =
            readerB gameBoyA
    in
    ( f a b, gameBoyB )


map3 : (a -> b -> c -> d) -> Reader a -> Reader b -> Reader c -> Reader d
map3 f readerA readerB readerC gameBoy =
    let
        ( a, gameBoyA ) =
            readerA gameBoy

        ( b, gameBoyB ) =
            readerB gameBoyA

        ( c, gameBoyC ) =
            readerC gameBoyB
    in
    ( f a b c, gameBoyC )


andThen : (a -> Reader b) -> Reader a -> Reader b
andThen f readerA gameBoy =
    let
        ( a, gameBoyA ) =
            readerA gameBoy
    in
    f a gameBoyA
