module Days.D20 (run, part1, part2) where

import Control.Concurrent (setNumCapabilities)
import Control.Parallel.Strategies (parListChunk, parMap, rdeepseq, rpar, using)
import Data.Ix (Ix (inRange))
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S

run :: IO ()
run = do
    setNumCapabilities 24
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

validPoints mapping = filter (`M.member` mapping) . adjacentPoints

allCheatStartLocations :: Board -> Set Point
allCheatStartLocations b = S.filter inBounds $ bWalls b
  where
    width = bWidth b
    height = bHeight b
    inBounds (r, c) = inRange (1, height - 2) r && inRange (1, width - 2) c

allCheatPairsFromWalls :: Board -> Set (Point, Point)
allCheatPairsFromWalls b = S.fromList $ concatMap f $ S.toList $ allCheatStartLocations b
  where
    mapping = bMap b
    walls = bMap b
    isWall = (`M.member` walls)
    isPath = (`M.member` mapping)
    upWorks (r, c)
        | isPath (r + 1, c) && (isPath (r - 1, c) || (isWall (r - 1, c) && isPath (r - 2, c))) = Just $ ((r, c), (r - 1, c))
        | otherwise = Nothing
    downWorks (r, c)
        | isPath (r - 1, c) && (isPath (r + 1, c) || (isWall (r + 1, c) && isPath (r + 2, c))) = Just $ ((r, c), (r + 1, c))
        | otherwise = Nothing
    leftWorks (r, c)
        | isPath (r, c + 1) && (isPath (r, c - 1) || (isWall (r, c - 1) && isPath (r, c - 2))) = Just $ ((r, c), (r, c - 1))
        | otherwise = Nothing
    rightWorks (r, c)
        | isPath (r, c - 1) && (isPath (r, c + 1) || (isWall (r, c + 1) && isPath (r, c + 2))) = Just $ ((r, c), (r, c + 1))
        | otherwise = Nothing
    f p = mapMaybe ($ p) [upWorks, downWorks, leftWorks, rightWorks]

-- allCheatPairsFromEmpty :: Board -> Set (Point, Point)

mPathLen :: Map Point Char -> Point -> Point -> Maybe Int
mPathLen mapping start end = go [(start, False, 0)] S.empty
  where
    go [] _ = Nothing
    go ((point, cheatEnabled, steps) : xs) visited
        | point `S.member` visited = go xs visited
        | point == end = pure steps
        | otherwise = go (xs <> newStates) (S.insert point visited)
      where
        f p
            | mapping M.! p == '2' && not cheatEnabled = False
            | otherwise = True
        mEnable p = cheatEnabled || mapping M.! p == '1'
        adjacent = filter f $ filter (`S.notMember` visited) $ filter (`M.member` mapping) $ adjacentPoints point
        newStates = map (\p -> (p, mEnable p, steps + 1)) adjacent

addCheatToMapping :: Map Point Char -> (Point, Point) -> Map Point Char
addCheatToMapping mapping (start, end) = M.insert start '1' (M.insert end '2' mapping)

-- pathLen :: Map Point Char -> Set Point -> Point -> Point -> Maybe Int
-- pathLen mapping oVisited start end = go [(start, 0)] oVisited
--   where
--     go [] _ = Nothing
--     go ((point, steps) : xs) visited
--         | point `S.member` visited = go xs visited
--         | point == end = Just steps
--         | otherwise = go (xs <> newStates) (S.insert point visited)
--       where
--         adjacent = filter (`S.notMember` visited) $ filter (`M.member` mapping) $ adjacentPoints point
--         newStates = map (,steps + 1) adjacent

-- pathLenC :: Map Point Char -> Point -> Point -> Int -> [Int]
-- pathLenC mapping start end originalTime = go [(start, 0)] S.empty
--   where
--     -- isWall = (`M.member` walls)
--     isPath = (`M.member` mapping)
--     go [] _ = [0]
--     go ((point@(r, c), steps) : xs) visited
--         | point `S.member` visited = go xs visited
--         | point == end = [steps]
--         | otherwise = lenFromCheat <> go (xs <> newStates) (S.insert point visited)
--       where
--         adjacent = adjacentPoints point
--         wallAdjacent = filter (`M.notMember` mapping) adjacent
--         f (bR, bC)
--             | bR == r - 1 && isPath (bR - 1, bC) = Just (1, (bR - 1, bC))
--             | bR == r - 1 && isPath (bR - 2, bC) = Just (2, (bR - 2, bC))
--             | bR == r + 1 && isPath (bR + 1, bC) = Just (1, (bR + 1, bC))
--             | bR == r + 1 && isPath (bR + 2, bC) = Just (2, (bR + 2, bC))
--             | bC == c - 1 && isPath (bR, bC - 1) = Just (1, (bR, bC - 1))
--             | bC == c - 1 && isPath (bR, bC - 2) = Just (2, (bR, bC - 2))
--             | bC == c + 1 && isPath (bR, bC + 1) = Just (1, (bR, bC + 1))
--             | bC == c + 1 && isPath (bR, bC + 2) = Just (2, (bR, bC + 2))
--             | otherwise = Nothing
--         cheatAdjacent = mapMaybe f wallAdjacent
--         g (extra, newPoint) = (steps + extra +) <$> pathLen mapping visited newPoint end
--         lenFromCheat = mapMaybe g cheatAdjacent
--         newStates = map (,steps + 1) adjacent

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
        adjacent = filter (`S.notMember` visited) $ filter (`M.member` mapping) $ adjacentPoints point
        newStates = map (: stateL) adjacent
dist :: Point -> Point -> Int
dist (rA, cA) (rB, cB) = abs (rA - rB) + abs (cA - cB)

validDistV1 :: Point -> Point -> Bool
validDistV1 a b = dist a b == 2

validDistV2 :: Point -> Point -> Bool
validDistV2 a b = dist a b <= 20

-- validDistV2 a b = dist a b == 6

type ValidDistFunc = Point -> Point -> Bool

cheatPairssOver :: ValidDistFunc -> Int -> [Point] -> [(Point, Point)]
cheatPairssOver _ _ [] = []
cheatPairssOver f n (x : xs)
    | n > length xs = []
    | otherwise = (map (x,) $ filter (f x) (drop n xs)) <> cheatPairssOver f n xs

zipTimeLeft :: [Point] -> [(Int, Point)]
zipTimeLeft = reverse . zip [0 ..] . reverse

cheatPairssOverV :: ValidDistFunc -> Int -> Int -> [Point] -> [(Point, Point)]
cheatPairssOverV func originalTime n = go 0 . zipTimeLeft
  where
    go time [] = []
    go time ((d, x) : xs) = map (\(_, y) -> (x, y)) (filter (\(remaining, y) -> originalTime - (time + dist x y + remaining) >= n && func x y) (drop n xs)) <> go (time + 1) xs

part1 :: String -> Int
part1 s = length $ cheatPairssOver validDistV1 100 $ fPathLen mapping start end
  where
    b = pBoard s
    start = bStart b
    end = bEnd b
    mapping = bMap b

-- part2 :: String -> Int
part2 s = S.size $ S.fromList $ cheatPairssOverV validDistV2 originalTime 100 $ fPathLen mapping start end
  where
    b = pBoard s
    start = bStart b
    end = bEnd b
    mapping = bMap b
    originalTime = length $ fPathLen mapping start end
