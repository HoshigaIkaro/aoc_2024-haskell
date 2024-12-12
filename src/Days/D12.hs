{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.D12 (run, part1, part2) where

import Control.Arrow
import Data.List (nub, sort)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S

run :: IO ()
run = do
    input <- readFile "input/d12.txt"
    print $ part1 input
    print $ part2 input

type Point = (Int, Int)

pInput :: String -> Map Point Char
pInput s = mapping
  where
    ls = lines s
    allPointsInRow row = [(row, col) | col <- [0 ..]]
    f row = zip (allPointsInRow row)
    mapping = M.fromList . concat $ zipWith f [0 ..] ls

adjacentPoints :: Point -> [Point]
adjacentPoints (r, c) = [(r - 1, c), (r, c - 1), (r, c + 1), (r + 1, c)]

validAdjacentPoints :: Map Point Char -> Set Point -> Point -> [Point]
validAdjacentPoints mapping visited p = filter f $ adjacentPoints p
  where
    f other = other `S.notMember` visited && other `M.member` mapping

groupFarms :: Map Point Char -> [[Point]]
groupFarms mapping
    | M.null mapping = []
    | otherwise = farmPoints : groupFarms newMapping
  where
    (point, farm) = head $ M.toList $ M.take 1 mapping
    farmPoints = connectFarm mapping point farm
    newMapping = foldr M.delete mapping farmPoints

connectFarm :: Map Point Char -> Point -> Char -> [Point]
connectFarm mapping start farm = go [start] S.empty
  where
    go [] _ = []
    go (x : xs) visited
        | x `S.member` visited = go xs visited
        | currentFarm /= farm = go xs visited
        | otherwise = x : go (xs <> newPoints) newVisited
      where
        currentFarm = mapping M.! x
        newPoints = validAdjacentPoints mapping visited x
        newVisited = S.insert x visited

farmArea :: [Point] -> Int
farmArea = length

farmPerimeter :: [Point] -> Int
farmPerimeter points = go points
  where
    pointSet = S.fromList points
    go [] = 0
    go (x : xs) = 4 - length adjacent + go xs
      where
        adjacent = filter (`S.member` pointSet) $ adjacentPoints x

part1 :: String -> Int
part1 = sum . map ((*) <$> farmArea <*> farmPerimeter) . groupFarms . pInput

horizontalSides :: [Point] -> [Point] -> Int
horizontalSides farmPoints points =
    sum (map (((+) <$> topSides <*> bottomSides) . (`getRow` points)) rows)
  where
    rows = nub $ map fst points
    getRow r = sort . filter ((== r) . fst)
    go [] = []
    go (x : xs) = (x : tailSide) : go (drop (length tailSide) xs)
      where
        tailSide = takeWhile (`elem` xs) $ drop 1 $ iterate (second succ) x
    topSides = length . go . filter (\p -> first pred p `elem` farmPoints)
    bottomSides = length . go . filter (\p -> first succ p `elem` farmPoints)

verticalSides :: [Point] -> [Point] -> Int
verticalSides farmPoints encasementPoints =
    sum (map (((+) <$> leftSides <*> rightSides) . (`getColumn` encasementPoints)) columns)
  where
    columns = nub $ map snd encasementPoints
    getColumn c = sort . filter ((== c) . snd)
    go [] = []
    go (x : xs) = (x : tailSide) : go (drop (length tailSide) xs)
      where
        tailSide = takeWhile (`elem` xs) $ drop 1 $ iterate (first succ) x
    leftSides = length . go . filter (\p -> second pred p `elem` farmPoints)
    rightSides = length . go . filter (\p -> second succ p `elem` farmPoints)

encase :: [Point] -> [Point]
encase points = S.toList $ go points S.empty
  where
    go [] _ = S.empty
    go (x : xs) visited
        | x `S.member` visited = go xs visited
        | length adjacentInFarm == 4 = go xs newVisited
        | otherwise = S.fromList adjacentNotInFarm <> go xs newVisited
      where
        adjacent = adjacentPoints x
        adjacentInFarm = filter (`elem` points) adjacent
        adjacentNotInFarm = filter (`notElem` points) adjacent
        newVisited = S.insert x visited

part2 :: String -> Int
part2 = sum . map ((*) <$> farmArea <*> getSides) . groupFarms . pInput
  where
    getSides farm =
        let encased = encase farm
         in verticalSides farm encased + horizontalSides farm encased
