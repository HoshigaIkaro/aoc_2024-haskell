module Main where

import Criterion.Main

import Days.D1 qualified as D1
import Days.D2 qualified as D2
import Days.D3 qualified as D3
import Days.D4 qualified as D4
import Days.D5 qualified as D5
import Days.D7 qualified as D7
import Days.D8 qualified as D8


main :: IO ()
main = do
    inputD4 <- readFile "input/d4.txt"
    inputD5 <- readFile "input/d5.txt"
    defaultMain
        [ 
        --     bgroup
        --     "day 4"
        --     [ bench "part 1" $ whnf D4.part1 inputD4
        --     , bench "part 2" $ whnf D4.part2 inputD4
        --     ]
        -- , bgroup
        --     "day 5"
        --     [ bench "part 1" $ whnf D5.part1 inputD5
        --     , bench "part 2" $ whnf D5.part2 inputD5
        --     ]
        -- , 
        bgroup
            "day 8"
            [ bench "part 1" $ whnf D8.part1 inputD5
            , bench "part 2" $ whnf D8.part2 inputD5
            ]
        ]