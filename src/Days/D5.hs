{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial -Wno-unused-top-binds #-}

module Days.D5 (run, part1, part2) where

import Data.List ( sortBy )
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe (fromMaybe)

run :: IO ()
run = do
    input <- readFile "input/d5.txt"
    print $ part1 input
    print $ part2 input

pRules :: Text -> Map (Int, Int) Ordering
pRules = foldr processLine M.empty . T.lines
  where
    toTuple lst = (head lst, lst !! 1)
    updateRules = flip M.insert LT
    processLine = updateRules . toTuple . map (read . T.unpack) . T.split (== '|')

pUpdate :: Text -> [Int]
pUpdate = map (read . T.unpack) . T.split (== ',')

pInput :: String -> (Map (Int, Int) Ordering, [[Int]])
pInput s = (rules, updates)
  where
    t = T.pack s
    halves = T.splitOn "\n\n" t
    rules = pRules (head halves)
    updates = map pUpdate (T.lines $ last halves)

updateIsCorrect :: Map (Int, Int) Ordering -> [Int] -> Bool
updateIsCorrect mapping lst = all f $ zip lst (drop 1 lst)
  where
    f (a, b) = M.notMember (b, a) mapping

middleNumber :: [Int] -> Int
middleNumber lst = lst !! mid
  where
    mid = length lst `div` 2

part1 :: String -> Int
part1 s = sum . map middleNumber . filter (updateIsCorrect rules) $ updates
  where
    (rules, updates) = pInput s

fixUpdate :: Map (Int, Int) Ordering -> [Int] -> [Int]
fixUpdate mapping = sortBy sortFunc
  where
    sortFunc a b = fromMaybe GT (M.lookup (a, b) mapping)

part2 :: String -> Int
part2 s = sum . map (middleNumber . fixUpdate rules) . filter (not . updateIsCorrect rules) $ updates
  where
    (rules, updates) = pInput s
