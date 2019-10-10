module Effect.Writer exposing (Writer)

import GameBoy exposing (GameBoy)


type alias Writer a =
    a -> GameBoy -> GameBoy
