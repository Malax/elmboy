module Effect exposing
    ( Effect
    , join
    , join2
    , join3
    )

import Effect.Reader exposing (Reader)
import Effect.Writer exposing (Writer)
import GameBoy exposing (GameBoy)


type alias Effect =
    GameBoy -> GameBoy


join : Reader a -> Writer a -> Effect
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
            Effect.Reader.map2 f readerA readerB gameBoy
    in
    effect modifiedGameBoy


join3 : Reader a -> Reader b -> Reader c -> (a -> b -> c -> Effect) -> Effect
join3 readerA readerB readerC f gameBoy =
    let
        ( effect, modifiedGameBoy ) =
            Effect.Reader.map3 f readerA readerB readerC gameBoy
    in
    effect modifiedGameBoy
