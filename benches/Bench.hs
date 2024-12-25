module Main where

import Criterion.Main

import Days.D1 qualified as D1
import Days.D10 qualified as D10
import Days.D11 qualified as D11
import Days.D12 qualified as D12
import Days.D13 qualified as D13
import Days.D14 qualified as D14
import Days.D15 qualified as D15
import Days.D16 qualified as D16
import Days.D17 qualified as D17
import Days.D18 qualified as D18
import Days.D19 qualified as D19
import Days.D2 qualified as D2
import Days.D20 qualified as D20
import Days.D21 qualified as D21
import Days.D22 qualified as D22
import Days.D23 qualified as D23
import Days.D24 qualified as D24
import Days.D25 qualified as D25
import Days.D3 qualified as D3
import Days.D4 qualified as D4
import Days.D5 qualified as D5
import Days.D6 qualified as D6
import Days.D7 qualified as D7
import Days.D8 qualified as D8
import Days.D9 qualified as D9

main :: IO ()
main = do
    inputD1 <- readFile "input/d1.txt"
    inputD2 <- readFile "input/d2.txt"
    inputD3 <- readFile "input/d3.txt"
    inputD4 <- readFile "input/d4.txt"
    inputD5 <- readFile "input/d5.txt"
    inputD6 <- readFile "input/d6.txt"
    inputD7 <- readFile "input/d7.txt"
    inputD8 <- readFile "input/d8.txt"
    inputD9 <- readFile "input/d9.txt"
    inputD10 <- readFile "input/d10.txt"
    inputD11 <- readFile "input/d11.txt"
    inputD12 <- readFile "input/d12.txt"
    inputD13 <- readFile "input/d13.txt"
    inputD14 <- readFile "input/d14.txt"
    inputD15 <- readFile "input/d15.txt"
    inputD16 <- readFile "input/d16.txt"
    inputD17 <- readFile "input/d17.txt"
    inputD18 <- readFile "input/d18.txt"
    inputD19 <- readFile "input/d19.txt"
    inputD20 <- readFile "input/d20.txt"
    inputD21 <- readFile "input/d21.txt"
    inputD22 <- readFile "input/d22.txt"
    inputD23 <- readFile "input/d23.txt"
    inputD24 <- readFile "input/d24.txt"
    inputD25 <- readFile "input/d25.txt"

    defaultMain
        [ bgroup
            "day 1"
            [ bench "part 1" $ whnf D1.part1 inputD1
            , bench "part 2" $ whnf D1.part2 inputD1
            ]
        , bgroup
            "day 2"
            [ bench "part 1" $ whnf D2.part1 inputD2
            , bench "part 2" $ whnf D2.part2 inputD2
            ]
        , bgroup
            "day 3"
            [ bench "part 1" $ whnf D3.part1 inputD3
            , bench "part 2" $ whnf D3.part2 inputD3
            ]
        , bgroup
            "day 4"
            [ bench "part 1" $ whnf D4.part1 inputD4
            , bench "part 2" $ whnf D4.part2 inputD4
            ]
        , bgroup
            "day 5"
            [ bench "part 1" $ whnf D5.part1 inputD5
            , bench "part 2" $ whnf D5.part2 inputD5
            ]
        , bgroup
            "day 6"
            [ bench "part 1" $ whnf D6.part1 inputD6
            , bench "part 2" $ whnf D6.part2 inputD6
            ]
        , bgroup
            "day 7"
            [ bench "part 1" $ whnf D7.part1 inputD7
            , bench "part 2" $ whnf D7.part2 inputD7
            ]
        , bgroup
            "day 8"
            [ bench "part 1" $ whnf D8.part1 inputD8
            , bench "part 2" $ whnf D8.part2 inputD8
            ]
        , bgroup
            "day 9"
            [ bench "part 1" $ whnf D9.part1 inputD9
            , bench "part 2" $ whnf D9.part2 inputD9
            ]
        , bgroup
            "day 10"
            [ bench "part 1" $ whnf D10.part1 inputD10
            , bench "part 2" $ whnf D10.part2 inputD10
            ]
        , bgroup
            "day 11"
            [ bench "part 1" $ whnf D11.part1 inputD11
            , bench "part 2" $ whnf D11.part2 inputD11
            ]
        , bgroup
            "day 12"
            [ bench "part 1" $ whnf D12.part1 inputD12
            , bench "part 2" $ whnf D12.part2 inputD12
            ]
        , bgroup
            "day 13"
            [ bench "part 1" $ whnf D13.part1 inputD13
            , bench "part 2" $ whnf D13.part2 inputD13
            ]
        , bgroup
            "day 14"
            [ bench "part 1" $ whnf D14.part1 inputD14
            , bench "part 2" $ whnf D14.part2 inputD14
            ]
        , bgroup
            "day 15"
            [ bench "part 1" $ whnf D15.part1 inputD15
            , bench "part 2" $ whnf D15.part2 inputD15
            ]
        , bgroup
            "day 16"
            [ bench "part 1" $ whnf D16.part1 inputD16
            , bench "part 2" $ whnf D16.part2 inputD16
            ]
        , bgroup
            "day 17"
            [ bench "part 1" $ whnf D17.part1 inputD17
            , bench "part 2" $ whnf D17.part2 inputD17
            ]
        , bgroup
            "day 18"
            [ bench "part 1" $ whnf D18.part1 inputD18
            , bench "part 2" $ whnf D18.part2 inputD18
            ]
        , bgroup
            "day 19"
            [ bench "part 1" $ whnf D19.part1 inputD19
            , bench "part 2" $ whnf D19.part2 inputD19
            ]
        , bgroup
            "day 20"
            [ bench "part 1" $ whnf D20.part1 inputD20
            , bench "part 2" $ whnf D20.part2 inputD20
            ]
        , bgroup
            "day 21"
            [ bench "part 1" $ whnf D21.part1 inputD21
            , bench "part 2" $ whnf D21.part2 inputD21
            ]
        , bgroup
            "day 22"
            [ bench "part 1" $ whnf D22.part1 inputD22
            , bench "part 2" $ whnf D22.part2 inputD22
            ]
        , bgroup
            "day 23"
            [ bench "part 1" $ whnf D23.part1 inputD23
            , bench "part 2" $ whnf D23.part2 inputD23
            ]
        , bgroup
            "day 24"
            [ bench "part 1" $ whnf D24.part1 inputD24
            , bench "part 2" $ whnf D24.part2 inputD24
            ]
        , bgroup
            "day 25"
            [ bench "part 1" $ nf D25.part1 inputD25
            , bench "part 2" $ nf D25.part2 inputD25
            ]
        ]