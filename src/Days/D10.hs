{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.D10 (run, part1, part2) where

import Data.Ix (Ix (inRange))
import Data.List (singleton)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S

run :: IO ()
run = do
    input <- readFile "input/d10.txt"
    print $ part1 input
    print $ part2 input

type Point = (Int, Int)

data Board = Board
    { bMap :: Map Point Char
    , bWidth :: Int
    , bHeight :: Int
    }
    deriving (Show)

pBoard :: String -> Board
pBoard s = Board{bMap = mapping, bWidth = width, bHeight = height}
  where
    ls = lines s
    height = length ls
    width = length (head ls)
    allPointsInRow row = [(row, col) | col <- [0 .. width - 1]]
    f row = zip (allPointsInRow row)
    mapping = M.fromList . concat $ zipWith f [0 .. height - 1] ls

adjacentPoints :: Point -> [Point]
adjacentPoints (r, c) = [(r - 1, c), (r, c - 1), (r, c + 1), (r + 1, c)]

validAdjacentPoints :: Board -> Set Point -> Point -> [Point]
validAdjacentPoints b visited p = filter f $ adjacentPoints p
  where
    mapping = bMap b
    width = bWidth b
    height = bHeight b
    inBounds (r, c) = inRange (0, height - 1) r && inRange (0, width - 1) c
    originalValue = mapping M.! p
    f other = other `S.notMember` visited && inBounds other && mapping M.! other == succ originalValue

getScore :: Board -> Point -> Int
getScore b = go S.empty . singleton
  where
    mapping = bMap b
    go _ [] = 0
    go visited (x : xs)
        | x `elem` visited = go visited xs
        | mapping M.! x == '9' = 1 + rest
        | otherwise = rest
      where
        newVisited = S.insert x visited
        rest = go newVisited (xs ++ validAdjacentPoints b newVisited x)

getZeroPoints :: Board -> [Point]
getZeroPoints = map fst . filter ((== '0') . snd) . M.toList . bMap

part1 :: String -> Int
part1 s = sum $ map (getScore b) (getZeroPoints b)
  where
    b = pBoard s

getRating :: Board -> Point -> Int
getRating b = go S.empty . singleton
  where
    mapping = bMap b
    go _ [] = 0
    go visited (x : xs)
        | mapping M.! x == '9' = 1 + rest
        | otherwise = rest
      where
        newVisited = S.insert x visited
        rest = go newVisited (xs ++ validAdjacentPoints b newVisited x)

part2 :: String -> Int
part2 s = sum $ map (getRating b) (getZeroPoints b)
  where
    b = pBoard s
