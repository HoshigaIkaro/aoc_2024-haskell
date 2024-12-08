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

data Board = Board {aBF :: [Antennas], bWidth :: Int, bHeight :: Int} deriving (Show, Eq, Ord)

-- pInput :: String -> Board
pInput s = Board{aBF = map (map snd) allAntennas, bWidth = width, bHeight = height}
  where
    ls = lines s
    height = length ls
    width = length $ head ls
    allPointsInRow row = [(row, col) | col <- [0 .. width - 1]]
    f row = filter ((/= '.') . fst) . flip zip (allPointsInRow row)
    allAntennas = groupBy (\a b -> fst a == fst b) . sortBy (compare `on` fst) . concat $ zipWith f [0 .. height - 1] ls

findAntinodesBetween :: Board -> Point -> Point -> [Point]
findAntinodesBetween b pA pB = filter inBounds [possiblePoint1, possiblePoint2]
  where
    width = bWidth b
    height = bHeight b
    inBounds (row, col) = inRange (0, height - 1) row && inRange (0, width - 1) col
    deltaX = snd pB - snd pA
    deltaY = fst pB - fst pA
    possiblePoint1 = (subtract deltaY *** subtract deltaX) pA
    possiblePoint2 = ((+ deltaY) *** (+ deltaX)) pB

findAllAntinodesForFreq :: Board -> Antennas -> [Point]
findAllAntinodesForFreq b antennas = concatMap (uncurry (findAntinodesBetween b)) $ antennaCombos antennas
  where
    antennaCombos [] = []
    antennaCombos (x : xs) = [(x, y) | y <- xs] <> antennaCombos xs

part1 :: String -> Int
part1 s = length $ S.toList $ S.fromList $ concatMap (findAllAntinodesForFreq b) (aBF b)
  where
    b = pInput s

findAntinodesBetween' :: Board -> Point -> Point -> [Point]
findAntinodesBetween' b pA pB = filter inBounds possiblePoints1 <> possiblePoints2
  where
    width = bWidth b
    height = bHeight b
    inBounds (row, col) = inRange (0, height - 1) row && inRange (0, width - 1) col
    deltaX = snd pB - snd pA
    deltaY = fst pB - fst pA
    possiblePoints1 = takeWhile inBounds $ iterate (subtract deltaY *** subtract deltaX) pA
    possiblePoints2 = takeWhile inBounds $ iterate ((+ deltaY) *** (+ deltaX)) pB

findAllAntinodesForFreq' :: Board -> Antennas -> [Point]
findAllAntinodesForFreq' b antennas = concatMap (uncurry (findAntinodesBetween' b)) $ antennaCombos antennas
  where
    antennaCombos [] = []
    antennaCombos (x : xs) = [(x, y) | y <- xs] <> antennaCombos xs

part2 :: String -> Int
part2 s =  length $ S.toList $ S.fromList $ concatMap (findAllAntinodesForFreq' b) (aBF b)
  where
    b = pInput s