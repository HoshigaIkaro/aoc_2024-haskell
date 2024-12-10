module Days.D10 (run, part1, part2) where

import Data.Ix (Ix (inRange))
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
adjacentPoints (r, c) =
    [ (r - 1, c)
    , (r, c - 1)
    , (r, c + 1)
    , (r + 1, c)
    ]

validAdjacentPoints :: Board -> Set Point -> Point -> [Point]
validAdjacentPoints b visited p = filter f $ adjacentPoints p
  where
    mapping = bMap b
    width = bWidth b
    height = bHeight b
    inBounds (r, c) = inRange (0, height - 1) r && inRange (0, width - 1) c
    originalValue = mapping M.! p
    f other = other `S.notMember` visited && inBounds other && mapping M.! other == succ originalValue

numTrails :: Board -> Point -> Int
numTrails b start = go [start] S.empty
  where
    mapping = bMap b
    go [] _ = 0
    go (x : xs) visited
        | x `elem` visited = go xs visited
        | mapping M.! x == '9' = 1 + go (xs ++ validAdjacentPoints b newVisited x) newVisited
        | otherwise = go (xs ++ validAdjacentPoints b newVisited x) newVisited
      where
        newVisited = S.insert x visited

-- part1 :: String -> Int
part1 s = sum $ map (numTrails b) possibleStarts
  where
    b = pBoard s
    possibleStarts = map fst $ filter ((== '0') . snd) (M.toList $ bMap b)

numTrails' :: Board -> Point -> Int
numTrails' b start = go [start] S.empty
  where
    mapping = bMap b
    go [] _ = 0
    go (x : xs) visited
        | mapping M.! x == '9' = 1 + go (xs ++ validAdjacentPoints b newVisited x) newVisited
        | otherwise = go (xs ++ validAdjacentPoints b newVisited x) newVisited
      where
        newVisited = S.insert x visited

part2 :: String -> Int
part2 s = sum $ map (numTrails' b) possibleStarts
  where
    b = pBoard s
    possibleStarts = map fst $ filter ((== '0') . snd) (M.toList $ bMap b)
