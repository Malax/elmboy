module Effect exposing
    ( Effect
    , Reader
    , Writer
    , join
    , join2
    , join3
    , mapReader
    , mapReader2
    , mapReader3
    )

import GameBoy exposing (GameBoy)


type alias Reader a =
    GameBoy -> ( a, GameBoy )


type alias Writer a =
    a -> Effect


type alias Effect =
    GameBoy -> GameBoy


join : Reader a -> (a -> Effect) -> Effect
join reader f gameBoy =
    let
        ( value, modifiedGameBoy ) =
            reader gameBoy
    in
    f value modifiedGameBoy


join2 : Reader a -> Reader b -> (a -> b -> Effect) -> Effect
join2 readerA readerB f gameBoy =
    let
        ( effect, modifiedGameBoy ) =
            mapReader2 f readerA readerB gameBoy
    in
    effect modifiedGameBoy


join3 : Reader a -> Reader b -> Reader c -> (a -> b -> c -> Effect) -> Effect
join3 readerA readerB readerC f gameBoy =
    let
        ( effect, modifiedGameBoy ) =
            mapReader3 f readerA readerB readerC gameBoy
    in
    effect modifiedGameBoy


mapReader : (a -> b) -> Reader a -> Reader b
mapReader f reader =
    reader >> Tuple.mapFirst f


mapReader2 : (a -> b -> c) -> Reader a -> Reader b -> Reader c
mapReader2 f readerA readerB gameBoy =
    let
        ( a, aGameBoy ) =
            readerA gameBoy

        ( b, bGameBoy ) =
            readerB aGameBoy
    in
    ( f a b, bGameBoy )


mapReader3 : (a -> b -> c -> d) -> Reader a -> Reader b -> Reader c -> Reader d
mapReader3 f readerA readerB readerC gameBoy =
    let
        ( a, aGameBoy ) =
            readerA gameBoy

        ( b, bGameBoy ) =
            readerB aGameBoy

        ( c, cGameBoy ) =
            readerC bGameBoy
    in
    ( f a b c, cGameBoy )
