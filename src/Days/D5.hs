{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.D5 (run, part1, part2) where

import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T

run :: IO ()
run = do
    input <- readFile "input/d5.txt"
    print $ part1 input
    print $ part2 input

pRules :: Text -> Map Int [Int]
pRules = foldr processLine M.empty . T.lines
  where
    toTuple lst = (head lst, lst !! 1)
    updateRules (smaller, larger) = M.alter (maybe (Just [smaller]) (Just . (smaller :))) larger
    processLine = updateRules . toTuple . map (read . T.unpack) . T.split (== '|')

pUpdate :: Text -> [Int]
pUpdate = map (read . T.unpack) . T.split (== ',')

pInput :: String -> (Map Int [Int], [[Int]])
pInput s = (rules, updates)
  where
    t = T.pack s
    halves = T.splitOn "\n\n" t
    rules = pRules (head halves)
    updates = map pUpdate (T.lines $ last halves)

updateIsCorrect :: Map Int [Int] -> [Int] -> Bool
updateIsCorrect _ [] = True
updateIsCorrect mapping (x : xs) = noInvalidInRemaining && updateIsCorrect mapping xs
  where
    noInvalidInRemaining = all (`notElem` xs) (fromMaybe [] $ M.lookup x mapping)

middleNumber :: [Int] -> Int
middleNumber lst = lst !! mid
  where
    mid = length lst `div` 2

part1 :: String -> Int
part1 s = sum . map middleNumber . filter (updateIsCorrect rules) $ updates
  where
    (rules, updates) = pInput s

fixUpdate :: Map Int [Int] -> [Int] -> [Int]
fixUpdate _ [] = []
fixUpdate mapping (x : xs)
    | not (null smallerThanX) = fixUpdate mapping updatedList
    | otherwise = x : fixUpdate mapping xs
  where
    smallerThanX = filter (`elem` xs) . fromMaybe [] $ M.lookup x mapping
    greatestIndex = maximum $ mapMaybe (`elemIndex` xs) smallerThanX
    updatedList = take (greatestIndex + 1) xs <> [x] <> drop (greatestIndex + 1) xs

part2 :: String -> Int
part2 s = sum . map (middleNumber . fixUpdate rules) . filter (not . updateIsCorrect rules) $ updates
  where
    (rules, updates) = pInput s
