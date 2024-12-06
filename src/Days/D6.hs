{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.D6 (run, part1, part2) where

import Control.Arrow
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Ix (Ix (inRange))
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S

run :: IO ()
run = do
    input <- readFile "input/d6.txt"
    print $ part1 input
    print $ part2 input

type Point = (Int, Int)

type Walls = Set Point

data Direction = UP | DOWN | LEFT | RIGHT deriving (Show, Ord, Eq)

data Board = Board
    { bWalls :: Walls
    , bGuard :: (Point, Direction)
    , bHeight :: Int
    , bWidth :: Int
    }
    deriving (Show, Eq, Ord)

pInput :: String -> Board
pInput s =
    Board
        { bWalls = walls
        , bGuard = fromJust dir
        , bHeight = height
        , bWidth = width
        }
  where
    ls = lines s
    height = length ls
    width = length $ head ls
    pDirection '^' = UP
    pDirection 'v' = DOWN
    pDirection '>' = RIGHT
    pDirection _ = LEFT
    f (char, point) (acc, mGuard)
        | char == '.' = (acc, mGuard)
        | char == '#' = (S.insert point acc, mGuard)
        | otherwise = (acc, Just (point, pDirection char))
    foldRow (row, rowNum) (acc, mGuard) = foldr (f . second (rowNum,)) (acc, mGuard) $ zip row [0 ..]
    (walls, dir) = foldr foldRow (S.empty, Nothing) $ zip ls [0 ..]

getMove :: Direction -> ((Int, Int) -> (Int, Int))
getMove UP = first pred
getMove DOWN = first succ
getMove LEFT = second pred
getMove RIGHT = second succ

getOppositeMove :: Direction -> ((Int, Int) -> (Int, Int))
getOppositeMove DOWN = first pred
getOppositeMove UP = first succ
getOppositeMove RIGHT = second pred
getOppositeMove LEFT = second succ

nextDir :: Direction -> Direction
nextDir UP = RIGHT
nextDir RIGHT = DOWN
nextDir DOWN = LEFT
nextDir LEFT = UP

moveGuardUntilLeave :: Board -> Set (Point, Direction)
moveGuardUntilLeave b = go direction position S.empty
  where
    width = bWidth b
    height = bHeight b
    walls = bWalls b
    (position, direction) = bGuard b
    inBounds (row, col) = inRange (0, height - 1) row && inRange (0, width - 1) col
    go dir point visited
        | not (inBounds point) = visited
        | newPoint `elem` walls = go (nextDir dir) point (S.insert (point, dir) visited)
        | otherwise = go dir newPoint (S.insert (point, dir) visited)
      where
        newPoint = getMove dir point

part1 :: String -> Int
part1 = S.size . moveGuardUntilLeave . pInput

isCycle :: Board -> Bool
isCycle b = go direction position S.empty
  where
    width = bWidth b
    height = bHeight b
    walls = bWalls b
    (position, direction) = bGuard b
    inBounds (row, col) = inRange (0, height - 1) row && inRange (0, width - 1) col
    go dir point visited
        | (point, dir) `elem` visited = True
        | not (inBounds point) = False
        | newPoint `elem` walls = go (nextDir dir) point (S.insert (point, dir) visited)
        | otherwise = go dir newPoint (S.insert (point, dir) visited)
      where
        newPoint = getMove dir point

nextWall :: Set Point -> Point -> Direction -> Maybe Point
nextWall walls (row, col) dir
    | dir == UP = fmap snd $ unsnoc $ sort (S.toList $ S.filter (\(newRow, newCol) -> newRow < row && newCol == col) walls)
    | dir == DOWN = fmap fst $ uncons $ sort (S.toList $ S.filter (\(newRow, newCol) -> newRow > row && newCol == col) walls)
    | dir == LEFT = fmap snd $ unsnoc $ sort (S.toList $ S.filter (\(newRow, newCol) -> newRow == row && newCol < col) walls)
    | dir == RIGHT = fmap fst $ uncons $ sort (S.toList $ S.filter (\(newRow, newCol) -> newRow == row && newCol > col) walls)
    | otherwise = Nothing

findCycles :: Board -> Set Point
findCycles b = go direction position
  where
    width = bWidth b
    height = bHeight b
    walls = bWalls b
    (position, direction) = bGuard b
    inBounds (row, col) = inRange (0, height - 1) row && inRange (0, width - 1) col
    go dir point
        | not (inBounds point) = S.empty
        | newPoint `elem` walls = go (nextDir dir) point
        | otherwise =
            if not (inBounds newPoint)
                then S.empty
                else case canMakeCycleInFront (S.singleton (newPoint, dir)) newPoint dir of
                    Nothing -> go dir newPoint
                    Just True -> S.insert newPoint $ go dir newPoint
                    Just False -> go dir newPoint
      where
        newPoint = getMove dir point
        wallsUpdated = S.insert newPoint walls
        canMakeCycleInFront vWall wallPoint cDir = do
            let newDir = nextDir cDir
            newWall <- nextWall wallsUpdated (getOppositeMove cDir wallPoint) newDir
            let newState = (newWall, newDir)
            if newState `S.member` vWall
                then pure True
                else canMakeCycleInFront (S.insert newState vWall) newWall newDir

-- canMakeCycleInFront = do
--     let newDir1 = nextDir dir
--     newWall1 <- nextWall wallsUpdated point newDir1
--     let newDir2 = nextDir newDir1
--     newWall2 <- nextWall wallsUpdated (getOppositeMove newDir1 newWall1) newDir2
--     let newDir3 = nextDir newDir2
--     newWall3 <- nextWall wallsUpdated (getOppositeMove newDir2 newWall2) newDir3
--     let newDir4 = nextDir newDir3
--     newWall4 <- nextWall wallsUpdated (getOppositeMove newDir3 newWall3) newDir4
--     pure (newPoint == newWall4)

-- isCycleAlt :: Board -> Bool
-- isCycleAlt b = go direction position False S.empty
--   where
--     width = bWidth b
--     height = bHeight b
--     walls = bWalls b
--     (position, direction) = bGuard b
--     inBounds (row, col) = inRange (0, height - 1) row && inRange (0, width - 1) col
--     go dir point previousWasTurn visited
--         | not previousWasTurn && (point, dir) `elem` visited = True
--         | not (inBounds point) = False
--         | newPoint `elem` walls = go (nextDir dir) point True (S.insert (point, dir) visited)
--         | not previousWasTurn && (point, dir) `S.member` visited = True
--         | otherwise = go dir newPoint False (S.insert (point, dir) visited)
--       where
--         newPoint = getMove dir point

part2 :: String -> Int
-- part2 s = S.size $ findCycles b
-- part2 s = go newBoards
part2 s = sum $ map (f . isCycle) newBoards
  where
    -- part2 s = [b] <> newBoards

    b = pInput s
    -- newBoards = (\p -> b{bWalls = S.insert p $ bWalls b})
    movementPoints = moveGuardUntilLeave b
    height = bHeight b
    width = bWidth b
    inBounds (row, col) = inRange (0, height - 1) row && inRange (0, width - 1) col
    possibleBlockPoints = S.toList $ S.filter inBounds $ S.map (\(point, dir) -> getMove dir point) movementPoints
    newBoards = map (\p -> b{bWalls = S.insert p $ bWalls b}) possibleBlockPoints
    f True = 1
    f False = 0
    go [] = 0
    go lst = force (sum (parMap rdeepseq (f . isCycle) (take 100 lst))) + go (drop 100 lst)
