{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.D18 (run, part1, part2) where

import Data.Char
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S

run :: IO ()
run = do
    input <- readFile "input/d18.txt"
    print $ part1 input
    print $ part2 input

type Point = (Int, Int)

pInput :: String -> [Point]
pInput = map f . lines
  where
    f s =
        let x = takeWhile isDigit s
            y = drop (length x + 1) s
         in (read y, read x)

initialMapSet :: Point -> Set Point
initialMapSet (height, width) = S.fromList $ concatMap f [0 .. height - 1]
  where
    f r = [(r, c) | c <- [0 .. width - 1]]

adjacentPoints :: Point -> [Point]
adjacentPoints (r, c) = [(r - 1, c), (r, c - 1), (r, c + 1), (r + 1, c)]

findWaySimp :: Point -> Set Point -> Maybe Int
findWaySimp (height, width) mapping = go [initial] S.empty
  where
    initial = ((0, 0), 0)
    target = (height - 1, width - 1)
    go [] _ = Nothing
    go ((point, steps) : ps) visited
        | point `S.member` visited = go ps visited
        | point == target = Just steps
        | otherwise = go (ps <> validAdjacent) newVisited
      where
        newVisited = S.insert point visited
        adjacent = adjacentPoints point
        f p = p `S.notMember` visited && p `S.member` mapping
        validAdjacent = map (,succ steps) $ filter f adjacent

applyTime :: Int -> [Point] -> Set Point -> Set Point
applyTime time fallList points = points S.\\ fallen
  where
    fallen = S.fromList $ take time fallList

part1 :: String -> Int
part1 s = fromJust $ findWaySimp size $ applyTime 1024 fallMap initialMap
  where
    size = (71, 71)
    -- size = (7, 7)
    fallMap = pInput s
    initialMap = initialMapSet size

findFirstBlocking :: Point -> [Point] -> Set Point -> Point
findFirstBlocking size fallList mapping = go (1, length fallList)
  where
    go (l, r)
        | l == r = currentRemoved
        | isNothing $ findWaySimp size currentMapping =
            case findWaySimp size (S.insert currentRemoved currentMapping) of
                Nothing -> go (l, time)
                _ -> currentRemoved
        | otherwise = go (time, r)
      where
        time = (l + r) `div` 2
        toRemove = take time fallList
        currentRemoved = last toRemove
        currentMapping = mapping S.\\ S.fromList toRemove

toExpectedCoordinateFormat :: Point -> Point
toExpectedCoordinateFormat = (,) <$> snd <*> fst

part2 :: String -> Point
part2 s = toExpectedCoordinateFormat $ findFirstBlocking size fallList mapping
  where
    size = (71, 71)
    -- size = (7, 7)
    fallList = pInput s
    mapping = initialMapSet size
