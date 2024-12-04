module Main where

import Criterion.Main

import Days.D1 qualified as D1
import Days.D2 qualified as D2
import Days.D3 qualified as D3
import Days.D4 qualified as D4

main :: IO ()
main = do
    input <- readFile "input/d4.txt"
    defaultMain
        [ bgroup
            "day 4"
            [ bench "part 1" $ whnf D4.part1 input
            , bench "part2" $ whnf D4.part2 input
            ]
        ]