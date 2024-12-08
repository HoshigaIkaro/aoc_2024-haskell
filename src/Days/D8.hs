{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.D8 (run, part1, part2) where

import Control.Arrow
import Data.Function
import Data.Ix (Ix (inRange))
import Data.List (groupBy, sortBy)
import Data.Set qualified as S

run :: IO ()
run = do
    input <- readFile "input/d8.txt"
    print $ part1 input
    print $ part2 input

type Point = (Int, Int)

type Antennas = [Point]

type InBoundsFunc = (Point -> Bool)

pInput :: String -> ([Antennas], InBoundsFunc)
pInput s = (map (map snd) allAntennas, inBounds)
  where
    ls = lines s
    height = length ls
    width = length $ head ls
    inBounds (row, col) = inRange (0, height - 1) row && inRange (0, width - 1) col
    allPointsInRow row = [(row, col) | col <- [0 .. width - 1]]
    f row = filter ((/= '.') . fst) . flip zip (allPointsInRow row)
    allAntennas = groupBy (\a b -> fst a == fst b) . sortBy (compare `on` fst) . concat $ zipWith f [0 .. height - 1] ls

type FindAntinodeFunc = InBoundsFunc -> (Point, Point) -> [Point]

findAntinodesBetween :: FindAntinodeFunc
findAntinodesBetween inBounds (pA, pB) = filter inBounds [possiblePoint1, possiblePoint2]
  where
    deltaX = snd pB - snd pA
    deltaY = fst pB - fst pA
    possiblePoint1 = (subtract deltaY *** subtract deltaX) pA
    possiblePoint2 = ((+ deltaY) *** (+ deltaX)) pB

findAllForFreqWith :: InBoundsFunc -> FindAntinodeFunc -> Antennas -> [Point]
findAllForFreqWith inBounds findAntinodeFunc antennas = concatMap (findAntinodeFunc inBounds) $ antennaCombos antennas
  where
    antennaCombos [] = []
    antennaCombos (x : xs) = [(x, y) | y <- xs] <> antennaCombos xs

part1 :: String -> Int
part1 s = S.size $ S.fromList $ concatMap (findAllForFreqWith inBounds findAntinodesBetween) antennasByFreq
  where
    (antennasByFreq, inBounds) = pInput s

findAntinodesBetweenV2 :: FindAntinodeFunc
findAntinodesBetweenV2 inBounds (pA, pB) = possiblePoints1 <> possiblePoints2
  where
    deltaX = snd pB - snd pA
    deltaY = fst pB - fst pA
    possiblePoints1 = takeWhile inBounds $ iterate (subtract deltaY *** subtract deltaX) pA
    possiblePoints2 = takeWhile inBounds $ iterate ((+ deltaY) *** (+ deltaX)) pB

part2 :: String -> Int
part2 s = S.size $ S.fromList $ concatMap (findAllForFreqWith inBounds findAntinodesBetweenV2) antennasByFreq
  where
    (antennasByFreq, inBounds) = pInput s
