module Days.D11 (run, part1, part2) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as M
import GHC.Float (double2Int, int2Double)

run :: IO ()
run = do
    input <- readFile "input/d11.txt"
    print $ part1 input
    print $ part2 input

type Count = Int

type Frequencies = IntMap Count

evolve :: Frequencies -> Frequencies
evolve = foldr f M.empty . M.toList
  where
    alterFunc key numX = M.alter (Just . maybe numX (numX +)) key
    f (x, numX) acc
        | x == 0 = alterFunc 1 numX acc
        | even lb =
            let one = alterFunc (x `div` (10 ^ mid)) numX acc
             in alterFunc (x `mod` (10 ^ mid)) numX one
        | otherwise = alterFunc (2024 * x) numX acc
      where
        lb = 1 + double2Int (logBase 10 (int2Double x))
        mid = lb `div` 2

evolveTimes :: Int -> Frequencies -> Frequencies
evolveTimes 0 res = res
evolveTimes n res = evolveTimes (n - 1) (evolve res)

pInput :: String -> Frequencies
pInput = foldr (f . read) M.empty . words
  where
    f = M.alter (Just . maybe 1 succ)

part1 :: String -> Int
part1 = sum . M.elems . evolveTimes 25 . pInput

part2 :: String -> Int
part2 = sum . M.elems . evolveTimes 75 . pInput