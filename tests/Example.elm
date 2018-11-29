module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    test "Bogus Test" <|
        \_ -> Expect.equal "Elmboy" "Elmboy"
