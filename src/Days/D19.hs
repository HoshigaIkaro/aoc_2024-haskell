{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.D19 (run, part1, part2) where

import Control.Arrow
import Control.Monad.State.Strict
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text qualified as T

run :: IO ()
run = do
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
    prefixes = filter (`isPrefixOf` target) patterns
    newTargets = map (flip drop target . length) prefixes

part1 :: String -> Int
part1 s = length $ filter (isPossible patterns) towels
  where
    (patterns, towels) = pInput s

-- findPaths :: [String] -> Map String Int -> String -> (Int, Map String Int)
-- findPaths patterns cache target
--     | target == "" = (1, cache)
--     | otherwise =
--         case M.lookup target cache of
--             Just v -> (v, cache)
--             Nothing -> updateCacheInResult $ go (map dropPrefix validPrefixes) cache
--   where
--     validPrefixes = filter (`isPrefixOf` target) patterns
--     dropPrefix = flip drop target . length
--     -- updateCacheInResult = fst &&& (M.insert target <$> fst <*> snd)
--     updateCacheInResult (value, rCache) = (value, M.insert target value rCache)
--     go [] wCache = (0, wCache)
--     go (x : xs) wCache =
--         let (y, vCache) = findPaths patterns wCache x
--          in first (y +) $ go xs vCache

type Cache = Map String Int

findPaths :: [String] -> String -> State Cache Int
findPaths patterns target
    | target == "" = pure 1
    | otherwise = do
        cache <- get
        case M.lookup target cache of
            Just v -> pure v
            Nothing -> do
                let validPrefixes = filter (`isPrefixOf` target) patterns
                    newTargets = map (flip drop target . length) $ validPrefixes
                v <- sum <$> mapM (findPaths patterns) newTargets
                modify (M.insert target v)
                pure v

part2 :: String -> Int
part2 s = sum $ evalState (sequenceA $ map (findPaths patterns) towels) M.empty
  where
    (patterns, towels) = pInput s

-- part2 s = fst $ foldr f (0, mempty) towels
--     where
--     f towel (v, acc) =
--         let (y, oAcc) = findPaths patterns acc towel
--          in (v + y, oAcc)