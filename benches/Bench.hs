module Main where

import Criterion.Main

import Days.D1 qualified as D1
import Days.D2 qualified as D2
import Days.D3 qualified as D3
import Days.D4 qualified as D4
import Days.D5 qualified as D5
import Days.D7 qualified as D7
import Days.D8 qualified as D8
import Days.D10 qualified as D10
import Days.D11 qualified as D11
import Days.D12 qualified as D12
import Days.D19 qualified as D19
import Days.D23 qualified as D23

main :: IO ()
main = do
    inputD4 <- readFile "input/d4.txt"
    inputD5 <- readFile "input/d5.txt"
    inputD10 <- readFile "input/d10.txt"
    inputD11 <- readFile "input/d11.txt"
    inputD12 <- readFile "input/d12.txt"
    inputD19 <- readFile "input/d19.txt"
    inputD23 <- readFile "input/d23.txt"
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
        -- bgroup
        --     "day 8"
        --     [ bench "part 1" $ whnf D8.part1 inputD5
        --     , bench "part 2" $ whnf D8.part2 inputD5
        --     ]
        -- bgroup
        --     "day 10"
        --     [ bench "part 1" $ whnf D10.part1 inputD10
        --     , bench "part 2" $ whnf D10.part2 inputD10
        --     ]
        --     ,
        -- bgroup
        --     "day 11"
        --     [ bench "part 1" $ nf D11.part1 inputD11
        --     , bench "part 2" $ nf D11.part2 inputD11
        --     ],
        -- bgroup
        --     "day 12"
        --     [ bench "part 1" $ nf D12.part1 inputD12
        --     , bench "part 2" $ nf D12.part2 inputD12
        --     ]
        bgroup
            "day 19"
            [ bench "part 1" $ nf D19.part1 inputD19
            , bench "part 2" $ nf D19.part2 inputD19
            ]
        -- bgroup
        --     "day 23"
        --     [ bench "part 1" $ nf D23.part1 inputD23
        --     , bench "part 2" $ nf D23.part2 inputD23
        --     ]
        ]