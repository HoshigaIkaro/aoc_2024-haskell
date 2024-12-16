module Days.D16 (run, part1, part2) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S

-- import Data.Heap (Heap)

import Control.Arrow
import Data.Heap (Heap)
import Data.Heap qualified as H
import Data.Maybe

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

createCostToEnd :: [(Point, Direction)] -> Map (Point, Direction) Int
createCostToEnd path = snd $ foldr f ((0, snd start), initialMap) rest
  where
    start = head path
    rest = reverse (drop 1 path)
    initialMap = M.singleton start 0
    f (point, newDir) ((score, oldDir), acc)
        | newDir == oldDir = ((score + 1, newDir), M.insert (point, newDir) (score + 1) acc)
        | otherwise = ((newScoreChangeDir, newDir), M.insert (point, newDir) (newScoreChangeDir) acc)
      where
        newScoreChangeDir = score + 1001

addPathToKnownWith ::
    ([(Point, Direction)] -> Map (Point, Direction) Int) ->
    [(Point, Direction)] ->
    Map (Point, Direction) Int ->
    Map (Point, Direction) Int
addPathToKnownWith f path known = f path <> known

-- findBestPathPoints :: Board -> Point -> Int -> Int -> Set Point
-- findBestPathPoints b target minScore maxLen = go initialHeap S.empty
--   where
--     start = bStart b
--     mapping = bMap b
--     initialHeap = H.singleton (H.Entry 0 [(start, RIGHT)])
--     go heap visited
--         | H.null heap = S.empty
--         | score > minScore || length stateL > maxLen = go newHeap visited
--         | point == target = if score == minScore then S.fromList (map fst stateL) <> go newHeap visited else go newHeap visited
--         | otherwise = go (newHeap <> newStates) visited
--       where
--         (H.Entry{priority = score, payload = stateL}) = H.minimum heap
--         state@(point, dir) = head stateL
--         newHeap = H.deleteMin heap
--         f s = incScorFunc dir s score
--         newStates = H.fromList $ map (H.Entry <$> f <*> (: stateL)) $ filter (`notElem` stateL) $ validAdjacentStates mapping S.empty state
--         incScorFunc originalDir (_, newDir)
--             | originalDir == newDir = succ
--             | otherwise = (+ 1001)
--         newVisited = S.insert point visited

findBestPathPoints :: Board -> Point -> Int -> Set Point
findBestPathPoints b target minScore = S.fromList $ map fst $ go initialHeap M.empty
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
        | H.null heap = final allTarget $ M.map snd visited
        -- | score > minScore = go newHeap visited
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

-- findBestPathPoints :: Board -> Point -> Set Point
-- findBestPathPoints b target = go initialHeap S.empty Nothing Nothing
--   where
--     start = bStart b
--     mapping = bMap b
--     initialHeap = H.singleton (H.Entry 0 [(0, (start, RIGHT))])
--     newInitialheap = H.fromList . map (H.Entry <$> fst <*> (: []))
--     go heap visited minScore known
--         | H.null heap = S.empty
--         | isJust known && state `M.member` (fromJust known) =
--             if score + (fromJust known M.! state) == fromJust minScore
--                 then S.fromList statePoints <> go (newHeap <> newStates) visited minScore (addPathToKnownWith createCostToEnd stateL <$> known)
--                 else go (newHeap <> newStates) visited minScore known
--         | point == target = case minScore of
--             Nothing -> S.fromList statePoints <> go (newInitialheap stateLS) S.empty (Just score) (Just $ createCostToEnd stateL)
--             Just s ->
--                 if score == s
--                     then
--                         S.fromList statePoints <> go newHeap visited minScore (addPathToKnownWith createCostToEnd stateL <$> known)
--                     else go newHeap visited minScore known
--         | stateA `S.member` visited = go newHeap visited minScore known
--         | otherwise = case minScore of
--             Nothing -> go (newHeap <> newStates) newVisited minScore known
--             Just s ->
--                 if score > s
--                     then S.empty
--                     else go (newHeap <> newStates) newVisited minScore known
--       where
--         (H.Entry{priority = _, payload = stateLS}) = H.minimum heap
--         stateL = map snd stateLS
--         statePoints = map fst stateL
--         stateA@(score, (point, dir)) = head stateLS
--         state = (point, dir)
--         newHeap = H.deleteMin heap
--         f s = incScorFunc dir s score
--         g s = (f s, s) : stateLS
--         newStates = H.fromList $ map (H.Entry <$> f <*> g) $ filter (`notElem` stateL) $ validAdjacentStates mapping S.empty state
--         incScorFunc originalDir (_, newDir)
--             | originalDir == newDir = succ
--             | otherwise = (+ 1001)
--         newVisited = S.insert stateA visited

-- newVisited = visited

-- part2 :: String -> Int
part2 s = S.size $ findBestPathPoints b target minScore
  where
    -- part2 s = createKnownPath [((7, 5), RIGHT), ((7, 4), RIGHT), ((7, 3), UP),((8, 3), UP), ((9, 3), UP), ((10, 3), UP), ((11, 3), RIGHT), ((11, 2), RIGHT), ((11, 1), UP), ((12, 1), UP), ((13, 1), RIGHT)]
    (minScore, p) = findBestPathAndScore b
    maxLen = length p
    b = pBoard s
    target = head $ M.keys $ M.filter (== 'E') $ bMap b

-- findBestPathPointsV :: Board -> (Int, Map (Point, Direction) Int) -> Point -> Set Point
-- findBestPathPointsV b (initialMin, initialKnown) target = S.fromList $ map fst $ M.keys $ go initialHeap S.empty initialMin initialKnown
--   where
--     start = bStart b
--     mapping = bMap b
--     initialHeap = H.singleton (H.Entry 0 [(start, RIGHT)])
--     go heap visited minScore known
--         | H.null heap = known
--         | state `M.member` known =
--             if score + (known M.! state) == minScore
--                 then go newHeap visited minScore (addPathToKnownWith stateL known)
--                 else go newHeap visited minScore known
--         | point == target =
--             if score == minScore
--                 then
--                     go newHeap visited minScore (addPathToKnownWith stateL known)
--                 else go newHeap visited minScore known
--         | state `S.member` visited = go newHeap visited minScore known
--         | otherwise =
--             if score > minScore
--                 then known
--                 else go (newHeap <> newStates) newVisited minScore known
--       where
--         (H.Entry{priority = score, payload = stateL}) = H.minimum heap
--         statePoints = map fst stateL
--         state@(point, dir) = head stateL
--         newHeap = H.deleteMin heap
--         f s = incScorFunc dir s score
--         newStates = H.fromList $ map (H.Entry <$> f <*> (: stateL)) $ filter (`notElem` stateL) $ validAdjacentStates mapping S.empty state
--         incScorFunc originalDir (_, newDir)
--             | originalDir == newDir = succ
--             | otherwise = (+ 1001)
--         newVisited = S.insert state visited
