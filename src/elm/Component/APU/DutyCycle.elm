module Component.APU.DutyCycle exposing (DutyCycle(..), sample)


type DutyCycle
    = Zero
    | One
    | Two
    | Three


sample : DutyCycle -> Int -> Float
sample duty index =
    let
        normalizedIndex =
            remainderBy 8 index
    in
    case duty of
        Zero ->
            case normalizedIndex of
                0 ->
                    -1

                1 ->
                    -1

                2 ->
                    -1

                3 ->
                    -1

                4 ->
                    -1

                5 ->
                    -1

                6 ->
                    -1

                _ ->
                    1

        One ->
            case normalizedIndex of
                0 ->
                    1

                1 ->
                    -1

                2 ->
                    -1

                3 ->
                    -1

                4 ->
                    -1

                5 ->
                    -1

                6 ->
                    -1

                _ ->
                    1

        Two ->
            case normalizedIndex of
                0 ->
                    1

                1 ->
                    -1

                2 ->
                    -1

                3 ->
                    -1

                4 ->
                    -1

                5 ->
                    1

                6 ->
                    1

                _ ->
                    1

        Three ->
            case normalizedIndex of
                0 ->
                    -1

                1 ->
                    1

                2 ->
                    1

                3 ->
                    1

                4 ->
                    1

                5 ->
                    1

                6 ->
                    1

                _ ->
                    -1
