{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.D4 (run, part1, part2) where

import Control.Arrow (Arrow (first, (***)))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T

run :: IO ()
run = do
    input <- readFile "input/d4.txt"
    print $ part1 input
    print $ part2 input

type Point = (Int, Int)

data Board = Board
    { bMap :: Map Point Text
    , bWidth :: Int
    , bHeight :: Int
    }
    deriving (Show)

rows :: Board -> [Text]
rows b = map f [0 .. height - 1]
  where
    mapping = bMap b
    height = bHeight b
    f row = T.concat . M.elems $ M.filterWithKey (\(r, _) _ -> row == r) mapping

cols :: Board -> [Text]
cols b = map f [0 .. width - 1]
  where
    mapping = bMap b
    width = bWidth b
    f col = T.concat . M.elems $ M.filterWithKey (\(_, c) _ -> col == c) mapping

diagonals :: Board -> [Text]
diagonals b = combinedLeft <> combinedRight
  where
    mapping = bMap b
    width = bWidth b
    height = bHeight b
    lMostPoints = [(row, 0) | row <- [0 .. height - 1]]
    rMostPoints = [(row, width - 1) | row <- [0 .. height - 1]]
    bMostPoints = [(height - 1, col) | col <- [0 .. width - 1]]
    upRightFrom = takeWhile (\(r, c) -> r >= 0 && c < width) . iterate (pred *** succ)
    upLeftFrom = takeWhile (\(r, c) -> r >= 0 && c >= 0) . iterate (pred *** pred)
    combinedLeft = map (T.concat . map (mapping M.!) . upRightFrom) (lMostPoints <> drop 1 bMostPoints)
    combinedRight = map (T.concat . map (mapping M.!) . upLeftFrom) (rMostPoints <> init bMostPoints)

allLines :: Board -> [Text]
allLines b = combined <> map T.reverse combined
  where
    combined = rows b <> cols b <> diagonals b

pBoard :: String -> Board
pBoard s = Board{bMap = mapping, bWidth = width, bHeight = height}
  where
    ls = lines s
    height = length ls
    width = length (head ls)
    allPointsInRow row = [(row, col) | col <- [0 .. width - 1]]
    f row = zip (allPointsInRow row) . map T.singleton
    mapping = M.fromList . concat $ zipWith f [0 .. height - 1] ls

part1 :: String -> Int
part1 = sum . map (T.count "XMAS") . allLines . pBoard

orientations :: [[(Point -> Point, Text)]]
orientations = [one, two, three, four]
  where
    upLeft = pred *** pred
    upRight = pred *** succ
    downLeft = succ *** pred
    downRight = succ *** succ
    one = [(upLeft, "M"), (upRight, "S"), (id, "A"), (downLeft, "M"), (downRight, "S")]
    two = [(upLeft, "M"), (upRight, "M"), (id, "A"), (downLeft, "S"), (downRight, "S")]
    three = [(upLeft, "S"), (upRight, "M"), (id, "A"), (downLeft, "S"), (downRight, "M")]
    four = [(upLeft, "S"), (upRight, "S"), (id, "A"), (downLeft, "M"), (downRight, "M")]

xmasHere :: Board -> Point -> Bool
xmasHere board center
    | mapping M.! center == "A" = any works orientations
    | otherwise = False
  where
    mapping = bMap board
    matches = (==) . (mapping M.!)
    works = all (uncurry matches . first ($ center))

part2 :: String -> Int
part2 s = length . filter id $ map (xmasHere board) possiblePoints
  where
    board = pBoard s
    width = bWidth board
    height = bWidth board
    possiblePoints = [(row, col) | row <- [1 .. height - 2], col <- [1 .. width - 2]]