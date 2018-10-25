module Main exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Component.RAM as RAM



-- Benchmarking skeleton for ad-hoc benchmarking while development


suite : Benchmark
suite =
    describe "thing"
        [ describe "category"
            [ benchmark "a" <|
                \_ -> RAM.init 0x0136
            , benchmark "b" <|
                \_ -> 2 * 2
            ]
        , Benchmark.compare "functionality"
            "x"
            (\_ -> 1 + 1)
            "y"
            (\_ -> 1 + 1)
        ]


main : BenchmarkProgram
main =
    program suite
