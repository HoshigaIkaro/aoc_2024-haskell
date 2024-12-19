{-# LANGUAGE OverloadedStrings #-}

module Days.D19 (run, part1, part2) where

import Control.Arrow
import Control.Concurrent (setNumCapabilities)
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T

run :: IO ()
run = do
    setNumCapabilities 32
    input <- readFile "input/d19.txt"
    print $ part1 input
    print $ part2 input

pInput :: String -> ([String], [String])
pInput s = (a, b)
  where
    hs = lines s
    a = map T.unpack $ T.splitOn ", " $ T.pack $ head hs
    b = drop 2 hs

isPossible :: [String] -> String -> Bool
isPossible _ [] = True
isPossible patterns target
    | null prefixes = False
    | otherwise = any (isPossible patterns) newTargets
  where
    prefixes = filter (flip isPrefixOf target) patterns
    newTargets = map (flip drop target . length) prefixes

part1 :: String -> Int
part1 s = length $ filter (isPossible patterns) towels
  where
    (patterns, towels) = pInput s


findPaths :: [String] -> Map String Int -> String -> (Int, Map String Int)
findPaths patterns cache target
    | target == "" = (1, cache)
    | otherwise =
        case M.lookup target cache of
            Just v -> (v, cache)
            Nothing ->
                let (v, oCache) = go (map (flip drop target . length) $ filter (flip isPrefixOf target) patterns) cache
                 in (v, M.insert target v oCache)
  where
    go [] zCache = (0, zCache)
    go (x : xs) wCache =
        let (y, vCache) = findPaths patterns wCache x
         in first (y +) $ go xs vCache

part2 :: String -> Int
part2 s = fst $ foldr f (0, mempty) towels
  where
    (patterns, towels) = pInput s
    f towel (v, acc) =
        let (y, oAcc) = findPaths patterns acc towel
         in (v + y, oAcc)