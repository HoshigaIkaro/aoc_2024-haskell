{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.D20 (run, part1, part2) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S

run :: IO ()
run = do
    input <- readFile "input/d20.txt"
    print $ part1 input
    print $ part2 input

type Point = (Int, Int)

data Board = Board
    { bWalls :: Set Point
    , bMap :: Map Point Char
    , bStart :: Point
    , bEnd :: Point
    , bWidth :: Int
    , bHeight :: Int
    }
    deriving (Show)

pBoard :: String -> Board
pBoard s =
    Board
        { bWalls = walls
        , bMap = newMap
        , bStart = start
        , bEnd = end
        , bWidth = width
        , bHeight = height
        }
  where
    ls = lines s
    allPointsInRow row = [(row, col) | col <- [0 ..]]
    f row = zip (allPointsInRow row)
    mapping = M.fromList . concat $ zipWith f [0 ..] ls
    walls = S.fromList . M.keys $ M.filter (== '#') mapping
    start = head $ M.keys $ M.filter (== 'S') mapping
    end = head $ M.keys $ M.filter (== 'E') mapping
    height = length ls
    width = length $ head ls
    newMap = M.filter (`notElem` "#") mapping

adjacentPoints :: Point -> [Point]
adjacentPoints (r, c) = [(r - 1, c), (r, c - 1), (r, c + 1), (r + 1, c)]

validPoints :: Map Point a -> Point -> [Point]
validPoints mapping = filter (`M.member` mapping) . adjacentPoints

fPathLen :: Map Point Char -> Point -> Point -> [Point]
fPathLen mapping start end = reverse $ go [[start]] S.empty
  where
    go [] _ = []
    go (stateL : xs) visited
        | point `S.member` visited = go xs visited
        | point == end = stateL
        | otherwise = go (xs <> newStates) (S.insert point visited)
      where
        point = head stateL
        adjacent = filter (`S.notMember` visited) $ validPoints mapping point
        newStates = map (: stateL) adjacent

dist :: Point -> Point -> Int
dist (rA, cA) (rB, cB) = abs (rA - rB) + abs (cA - cB)

validDistV1 :: Point -> Point -> Bool
validDistV1 a b = dist a b == 2

validDistV2 :: Point -> Point -> Bool
validDistV2 a b = dist a b <= 20

zipTimeLeft :: [Point] -> [(Int, Point)]
zipTimeLeft = reverse . zip [0 ..] . reverse

type ValidDistFunc = Point -> Point -> Bool

cheatPairssOver :: ValidDistFunc -> Int -> Int -> [Point] -> [(Point, Point)]
cheatPairssOver func originalTime n = go 0 . zipTimeLeft
  where
    go _ [] = []
    go time ((_, x) : xs) =
        map
            (\(_, y) -> (x, y))
            (filter (\(remaining, y) -> originalTime - (time + dist x y + remaining) >= n && func x y) (drop n xs))
            <> go (time + 1) xs

part1 :: String -> Int
part1 s = length $ cheatPairssOver validDistV1 originalTime 100 original
  where
    b = pBoard s
    start = bStart b
    end = bEnd b
    mapping = bMap b
    original = fPathLen mapping start end
    originalTime = length original

part2 :: String -> Int
part2 s = length $ cheatPairssOver validDistV2 originalTime 100 original
  where
    b = pBoard s
    start = bStart b
    end = bEnd b
    mapping = bMap b
    original = fPathLen mapping start end
    originalTime = length original
