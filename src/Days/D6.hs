{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -feager-blackholing #-}

module Days.D6 (run, part1, part2) where

import Control.Arrow
import Control.Concurrent (setNumCapabilities)
import Control.Parallel.Strategies
import Data.Containers.ListUtils
import Data.Ix (Ix (inRange))
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S

run :: IO ()
run = do
    setNumCapabilities 32
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

moveGuardUntilLeave :: Board -> [(Point, Direction)]
moveGuardUntilLeave b = go direction position
  where
    width = bWidth b
    height = bHeight b
    walls = bWalls b
    (position, direction) = bGuard b
    inBounds (row, col) = inRange (0, height - 1) row && inRange (0, width - 1) col
    go dir point
        | not (inBounds point) = []
        | otherwise = (point, dir) : go newDir newPoint
      where
        afterMove = getMove dir point
        (newPoint, newDir) =
            if afterMove `elem` walls
                then (point, nextDir dir)
                else (afterMove, dir)

part1 :: String -> Int
part1 = S.size . S.fromList . map fst . moveGuardUntilLeave . pInput

nextWall :: Walls -> Point -> Direction -> Maybe Point
nextWall walls (row, col) dir
    | dir == UP = fmap snd $ unsnoc $ sort (S.toList $ S.filter (\(newRow, newCol) -> newRow < row && newCol == col) walls)
    | dir == DOWN = fmap fst $ uncons $ sort (S.toList $ S.filter (\(newRow, newCol) -> newRow > row && newCol == col) walls)
    | dir == LEFT = fmap snd $ unsnoc $ sort (S.toList $ S.filter (\(newRow, newCol) -> newRow == row && newCol < col) walls)
    | dir == RIGHT = fmap fst $ uncons $ sort (S.toList $ S.filter (\(newRow, newCol) -> newRow == row && newCol > col) walls)
    | otherwise = Nothing

nextPoint :: Walls -> Point -> Direction -> Maybe Point
nextPoint walls point dir = getOppositeMove dir <$> nextWall walls point dir

isLoop :: Set (Point, Direction) -> Point -> Direction -> Walls -> Bool
isLoop visited point dir walls = do
    let newDir = nextDir dir
    case nextPoint walls point dir of
        Nothing -> False
        Just newPoint ->
            let newState = (newPoint, newDir)
             in if newState `S.member` visited
                    then True
                    else isLoop (S.insert newState visited) newPoint newDir walls

part2 :: String -> Int
part2 s = length . filter id $ parMap rpar (isLoop S.empty point dir) newWallSets
  where
    b = pInput s
    (point, dir) = bGuard b
    movementPointsAndDir = moveGuardUntilLeave b
    newWallSets = map (`S.insert` (bWalls b)) . nubOrd $ map (uncurry (flip getMove)) movementPointsAndDir
