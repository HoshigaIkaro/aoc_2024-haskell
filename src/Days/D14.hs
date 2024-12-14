{-# LANGUAGE OverloadedStrings #-}

module Days.D14 (run, part1, part2) where

import Control.Arrow
import Control.Monad
import Data.Ix (Ix (inRange))
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

run :: IO ()
run = do
    input <- readFile "input/d14.txt"
    print $ part1 input
    print $ part2 input

-- simulate input (101, 103)

type Parser = Parsec Void Text

type Point = (Int, Int)

pSInt :: Parser Int
pSInt = L.signed space L.decimal

pPair :: Parser Point
pPair = do
    x <- pSInt
    void $ char ','
    y <- pSInt
    pure (x, y)

pLine :: Parser (Point, Point)
pLine = do
    void $ string "p="
    p <- pPair
    void $ string " v="
    v <- pPair
    pure (p, v)

pInput :: String -> [(Point, Point)]
pInput = fromJust . parseMaybe go . T.pack
  where
    go = sepEndBy pLine eol

displacementAfter :: Int -> Point -> Point
displacementAfter time = f *** f
  where
    f = (* time)

positionAfter :: Int -> (Point, Point) -> Point
positionAfter time ((x, y), velocity) = (x + deltaX, y + deltaY)
  where
    (deltaX, deltaY) = displacementAfter time velocity

wrap :: Point -> Point -> Point
wrap (width, height) (x, y) = (x `mod` width, y `mod` height)

downLeftQuadrant :: Point -> [Point] -> [Point]
downLeftQuadrant (width, height) = filter f
  where
    f (x, y) = inRange (0, width `div` 2 - 1) x && inRange (height `div` 2 + 1, height - 1) y

upLeftQuadrant :: Point -> [Point] -> [Point]
upLeftQuadrant (width, height) = filter f
  where
    f (x, y) = inRange (0, width `div` 2 - 1) x && inRange (0, height `div` 2 - 1) y

downRightQuadrant :: Point -> [Point] -> [Point]
downRightQuadrant (width, height) = filter f
  where
    f (x, y) = inRange (width `div` 2 + 1, width - 1) x && inRange (height `div` 2 + 1, height - 1) y

upRightQuadrant :: Point -> [Point] -> [Point]
upRightQuadrant (width, height) = filter f
  where
    f (x, y) = inRange (width `div` 2 + 1, width - 1) x && inRange (0, height `div` 2 - 1) y

part1 :: String -> Int
part1 = product . map length . f . map (wrap size . positionAfter 100) . pInput
  where
    size = (101, 103)
    f a = map ($ a) [upLeftQuadrant size, downLeftQuadrant size, upRightQuadrant size, downRightQuadrant size]

displayPoints :: Point -> [Point] -> String
displayPoints (width, height) points = unlines $ map processRow [0 .. height - 1]
  where
    processRow y = map (f . (,y)) [0 .. width - 1]
    pointSet = S.fromList points
    f p
        | p `S.member` pointSet = '#'
        | otherwise = '.'

numUntilCycle :: Point -> [(Point, Point)] -> Int
numUntilCycle size points = go 0 pointSet S.empty
  where
    pointSet = S.fromList points
    go time currentPoints visited
        | currentPoints `S.member` visited = time
        | otherwise = go (time + 1) (S.map ((,) <$> (wrap size . positionAfter 1) <*> snd) currentPoints) (S.insert currentPoints visited)

simulate :: String -> Point -> IO ()
simulate s size = go 0 $ pInput s
  where
    go time lst = do
        appendFile "./output/d14.txt" (show time <> "\n")
        appendFile "./output/d14.txt" (displayPoints size (map fst lst))
        appendFile "./output/d14.txt" "\n"
        let new = map ((,) <$> (wrap size . positionAfter 1) <*> snd) lst
        if time < 12000 then go (time + 1) new else pure ()

rowInPoints :: Int -> Point -> [Point] -> Bool
rowInPoints t (width, height) points = any (`S.isSubsetOf` pointSet) allSubSet
  where
    pointSet = S.fromList points
    f y = [S.fromList [(x, y) | x <- [s .. s + t]] | s <- [0 .. width - 1 - t]]
    allSubSet = concatMap f [0 .. height - 1]

part2 :: String -> Int
-- part2 s = displayPoints size $ map (wrap size . positionAfter 6771) $ pInput s
part2 s = go 0 $ pInput s
  where
    size = (101, 103)
    go time lst
        | rowInPoints 7 size points = time
        | otherwise = go (time + 1) new
      where
        points = map fst lst
        new = map ((,) <$> (wrap size . positionAfter 1) <*> snd) lst