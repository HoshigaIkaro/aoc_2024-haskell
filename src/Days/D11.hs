module Days.D11 (run, part1, part2) where

import Data.Bits
import Data.Map (Map)
import Data.Map qualified as M
import GHC.Float (double2Int, int2Double)

run :: IO ()
run = do
    input <- readFile "input/d11.txt"
    print $ part1 input
    print $ part2 input

evolve :: [Int] -> [Int]
evolve [] = []
evolve (x : xs)
    | x == 0 = 1 : evolve xs
    | even lb = x `div` (10 ^ mid) : (mod x (10 ^ mid)) : evolve xs
    | otherwise = x * 2024 : evolve xs
  where
    lb = 1 + double2Int (logBase 10 (int2Double x))
    mid = lb `div` 2

evolveTimes :: Int -> [Int] -> [Int]
evolveTimes 0 res = res
evolveTimes n res = evolveTimes (n - 1) (evolve res)

part1 :: String -> Int
part1 = length . evolveTimes 25 . map read . words

evolveV2 :: Map Int Int -> Map Int Int
evolveV2 = foldr f M.empty . M.toList
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

evolveTimesV2 :: Int -> Map Int Int -> Map Int Int
evolveTimesV2 0 res = res
evolveTimesV2 n res = evolveTimesV2 (n - 1) (evolveV2 res)

pMap :: String -> Map Int Int
pMap = foldr f M.empty . map read . words
  where
    f n acc = M.alter (Just . maybe 1 succ) n acc

part2 :: String -> Int
part2 = sum . M.elems . evolveTimesV2 75 . pMap