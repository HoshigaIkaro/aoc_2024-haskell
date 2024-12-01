{-# OPTIONS_GHC -Wno-x-partial #-}

module MyLib (part1, part2) where

import Control.Arrow ((***))
import Data.List (sort, transpose)

import Data.Map (Map)
import Data.Map qualified as M

pToList :: String -> [[Int]]
pToList = transpose . map (map read . words) . lines

listPairToTuple :: [[Int]] -> ([Int], [Int])
listPairToTuple = liftA2 (,) head (!! 1)

part1 :: String -> Int
part1 = sum . map (abs . uncurry (-)) . uncurry zip . (sort *** sort) . listPairToTuple . pToList

frequencies :: [Int] -> Map Int Int
frequencies = foldr (\a -> M.alter f a) mempty
  where
    f = Just . maybe 1 succ

part2 :: String -> Int
part2 = sum . f . (sort *** frequencies) . listPairToTuple . pToList
  where
    f :: ([Int], Map Int Int) -> [Int]
    f (a, b) = map (\i -> i * maybe 0 id (M.lookup i b)) a