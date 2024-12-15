{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.D15 (run, part1, part2) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S

import Control.Arrow
import Data.Containers.ListUtils (nubOrd)

run :: IO ()
run = do
    input <- readFile "input/d15.txt"
    print $ part1 input
    print $ part2 input

type Point = (Int, Int)

data Board = Board
    { bWalls :: Set Point
    , bMap :: Map Point Char
    , bRobot :: Point
    , bWidth :: Int
    , bHeight :: Int
    }
    deriving (Show)

pBoard :: String -> Board
pBoard s =
    Board
        { bWalls = walls
        , bMap = newMap
        , bRobot = robot
        , bWidth = width
        , bHeight = height
        }
  where
    ls = lines s
    allPointsInRow row = [(row, col) | col <- [0 ..]]
    f row = zip (allPointsInRow row)
    mapping = M.fromList . concat $ zipWith f [0 ..] ls
    walls = S.fromList . M.keys $ M.filter (== '#') mapping
    robot = head $ M.keys $ M.filter (== '@') mapping
    newMap = M.filter (`notElem` "#@.") mapping
    height = length ls
    width = length $ head ls

pInput :: String -> (Board, [Char])
pInput s = (pBoard $ unlines a, b)
  where
    hs = lines s
    a = takeWhile ((&&) <$> (not . null) <*> (/= "\r")) hs
    b = concat $ drop (length a + 1) hs

moveDir :: Char -> Point -> Point
moveDir '^' = first pred
moveDir 'v' = first succ
moveDir '<' = second pred
moveDir _ = second succ

updateMap :: (Point -> Point) -> Map Point Char -> [Point] -> Map Point Char
updateMap moveFunc mapping = newMap
  where
    deletedOld = foldr M.delete mapping
    f (k, v) = M.insert k v
    newMap lst = foldr f (deletedOld lst) (zip (map moveFunc lst) (map (mapping M.!) lst))

tryMove :: Char -> Board -> Maybe Board
tryMove dir b = case lineUntilFree of
    Nothing -> Nothing
    Just lst -> Just b{bMap = newMap lst, bRobot = newRobot}
  where
    robot = bRobot b
    mapping = bMap b
    walls = bWalls b
    moveFunc = moveDir dir
    newRobot = moveFunc robot
    lineUntilFree =
        case takeWhile ((&&) <$> (`S.notMember` walls) <*> (`M.member` mapping)) $ iterate moveFunc newRobot of
            [] -> if newRobot `S.member` walls then Nothing else Just []
            lst -> if moveFunc (last lst) `S.member` walls then Nothing else Just lst
    newMap = updateMap moveFunc mapping

moveAll :: (Char -> Board -> Maybe Board) -> [Char] -> Board -> Board
moveAll _ [] b = b
moveAll f (x : xs) b = case f x b of
    Nothing -> moveAll f xs b
    Just newB -> moveAll f xs newB

sumCoord :: [Point] -> Int
sumCoord = sum . map f
  where
    f (a, b) = 100 * a + b

part1 :: String -> Int
part1 s = sumCoord $ M.keys $ bMap $ moveAll tryMove ops b
  where
    (b, ops) = pInput s

double :: Board -> Board
double b = b{bWalls = newWalls, bMap = mapB, bWidth = bWidth b * 2, bRobot = newRobot}
  where
    walls = bWalls b
    wallsA = S.map (second (* 2)) walls
    newWalls = S.union wallsA (S.map (second succ) wallsA)
    mapA = M.fromList $ map ((,'[') . second (* 2)) (M.keys (bMap b))
    f p = M.insert (second succ p) ']'
    mapB = foldr f mapA (M.keys mapA)
    newRobot = second (* 2) $ bRobot b

shiftFunc :: Char -> Char -> Point -> Point
shiftFunc op c = if op `elem` "^v" then f else id
  where
    f
        | c == '[' = second succ
        | otherwise = second pred

tryMoveV2 :: Char -> Board -> Maybe Board
tryMoveV2 dir b
    | dir `elem` "<>" = tryMove dir b
    | newRobot `M.notMember` mapping && newRobot `S.notMember` walls = Just b{bRobot = newRobot}
    | otherwise = case lineUntilFree of
        Nothing -> Nothing
        Just lst -> Just b{bMap = newMap lst, bRobot = newRobot}
  where
    robot = bRobot b
    mapping = bMap b
    walls = bWalls b
    moveFunc = moveDir dir
    newRobot = moveFunc robot
    lineUntilFree = case getConnected [newRobot] S.empty of
        Nothing -> Nothing
        Just lst ->
            if any (\p -> moveFunc p `S.member` walls) (nubOrd lst)
                then Nothing
                else Just lst
    newMap = updateMap moveFunc mapping
    getConnected [] _ = Just []
    getConnected (p : rest) visited
        | p `S.member` visited = getConnected rest visited
        | p `S.member` walls = Nothing
        | p `M.member` mapping = (p :) <$> getConnected (newPoint : adjPoint : rest) newVisited
        | otherwise = getConnected rest newVisited
      where
        currentChar = mapping M.! p
        adjPoint = shiftFunc dir currentChar p
        newPoint = moveFunc p
        newVisited = S.insert p visited

showBoard :: Board -> String
showBoard b = unlines $ map processRow [0 .. height - 1]
  where
    walls = bWalls b
    mapping = bMap b
    width = bWidth b
    height = bHeight b
    robot = bRobot b
    processRow r = [f (r, c) | c <- [0 .. width - 1]]
    f p
        | p `M.member` mapping = mapping M.! p
        | p `S.member` walls = '#'
        | p == robot = '@'
        | otherwise = '.'

part2 :: String -> Int
part2 s = sumCoord . M.keys . M.filter (== '[') . bMap . moveAll tryMoveV2 ops . double $ b
  where
    (b, ops) = pInput s
