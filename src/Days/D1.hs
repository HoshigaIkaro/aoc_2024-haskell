{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.D1 (run, part1, part2) where

import Control.Arrow
import Data.List (sort, transpose)

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as M
import Data.Maybe (fromMaybe)

run :: IO ()
run = do
  input <- readFile "input/d1.txt"
  print $ part1 input
  print $ part2 input

pToList :: String -> [[Int]]
pToList = transpose . map (map read . words) . lines

listPairToTuple :: [[Int]] -> ([Int], [Int])
listPairToTuple = liftA2 (,) head (!! 1)

part1 :: String -> Int
part1 = sum . map (abs . uncurry (-)) . uncurry zip . (sort *** sort) . listPairToTuple . pToList

frequencies :: [Int] -> IntMap Int
frequencies = foldr (M.alter f) mempty
 where
  f = Just . maybe 1 succ

part2 :: String -> Int
part2 = sum . f . second frequencies . listPairToTuple . pToList
 where
  f :: ([Int], IntMap Int) -> [Int]
  f (a, b) = map (\i -> i * fromMaybe 0 (M.lookup i b)) a