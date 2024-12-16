{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.D16 (run, part1, part2) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Control.Arrow
import Data.Heap (Heap)
import Data.Heap qualified as H

run :: IO ()
run = do
    input <- readFile "input/d16.txt"
    print $ part1 input
    print $ part2 input

type Point = (Int, Int)

data Board = Board
    { bWalls :: Set Point
    , bMap :: Map Point Char
    , bStart :: Point
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
    height = length ls
    width = length $ head ls
    newMap = M.filter (`notElem` "#") mapping

data Direction = UP | DOWN | LEFT | RIGHT deriving (Show, Eq, Ord)

adjacentPoints :: Point -> [(Point, Direction)]
adjacentPoints (r, c) = [((r - 1, c), UP), ((r, c - 1), LEFT), ((r, c + 1), RIGHT), ((r + 1, c), DOWN)]

validDirChange :: Direction -> [Direction]
validDirChange dir
    | dir `elem` [UP, DOWN] = [dir, LEFT, RIGHT]
    | otherwise = [dir, UP, DOWN]

validAdjacentStates :: Map Point Char -> Set Point -> (Point, Direction) -> [(Point, Direction)]
validAdjacentStates mapping visited (p, dir) = filter f $ adjacentPoints p
  where
    f (other, newDir) = other `S.notMember` visited && other `M.member` mapping && newDir `elem` validDirChange dir

findBestPathAndScore :: Board -> (Int, [(Point, Direction)])
findBestPathAndScore b = go initialHeap S.empty
  where
    start = bStart b
    mapping = bMap b
    initialHeap = H.singleton (H.Entry 0 [(start, RIGHT)])
    go heap visited
        | H.null heap = (0, [])
        | point `S.member` visited = go newHeap visited
        | mapping M.! point == 'E' = (score, stateL)
        | otherwise = go (newHeap <> newStates) newVisited
      where
        (H.Entry{priority = score, payload = stateL}) = H.minimum heap
        state@(point, dir) = head stateL
        newHeap = H.deleteMin heap
        f s = incScorFunc dir s score
        newStates = H.fromList $ map (H.Entry <$> f <*> (: stateL)) $ validAdjacentStates mapping visited state
        incScorFunc originalDir (_, newDir)
            | originalDir == newDir = succ
            | otherwise = (+ 1001)
        newVisited = S.insert point visited

part1 :: String -> Int
part1 s = fst $ findBestPathAndScore b
  where
    b = pBoard s

findBestPathPoints :: Board -> Point -> Set Point
findBestPathPoints b target = S.fromList $ map fst $ go initialHeap M.empty
  where
    allTarget = map (target,) [UP, DOWN, LEFT, RIGHT]
    start = bStart b
    mapping = bMap b
    initialHeap :: Heap (H.Entry Int [((Point, Direction), (Point, Direction))])
    initialHeap = H.singleton (H.Entry 0 [((start, RIGHT), (start, RIGHT))])
    go ::
        Heap (H.Entry Int [((Point, Direction), (Point, Direction))]) ->
        Map (Point, Direction) (Int, [(Point, Direction)]) ->
        [(Point, Direction)]
    go heap visited
        | H.null heap = []
        | state `M.member` visited = g
        | point == target = final allTarget $ M.map snd newVisited
        | otherwise = go (newHeap <> newStates) newVisited
      where
        (H.Entry{priority = score, payload = stateL}) = H.minimum heap
        (state@(point, dir), old) = head stateL
        newHeap = H.deleteMin heap
        f s = incScorFunc dir s score
        newStates = H.fromList $ map (H.Entry <$> f <*> ((: stateL) . (,state))) $ validAdjacentStates mapping S.empty state
        incScorFunc originalDir (_, newDir)
            | originalDir == newDir = succ
            | otherwise = (+ 1001)
        newVisited = M.alter (Just . maybe (score, [old]) (second (old :))) state visited
        g
            | fst sG == score = go newHeap newVisited
            | fst sG > score = go newHeap (M.singleton state (score, [old]))
            | otherwise = go newHeap visited
          where
            sG = visited M.! state
    final :: [(Point, Direction)] -> Map (Point, Direction) [(Point, Direction)] -> [(Point, Direction)]
    final [] _ = []
    final (p : ps) m
        | p == (start, RIGHT) = p : final ps m
        | p `M.member` m = p : final (ps <> m M.! p) m
        | otherwise = p : final ps m

part2 :: String -> Int
part2 s = S.size $ findBestPathPoints b target
  where
    b = pBoard s
    target = head $ M.keys $ M.filter (== 'E') $ bMap b
