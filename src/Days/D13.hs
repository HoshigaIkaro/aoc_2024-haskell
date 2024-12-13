{-# LANGUAGE OverloadedStrings #-}

module Days.D13 (run, part1, part2) where

import Control.Monad
import Data.Function
import Data.List
import Data.Maybe (fromJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

type Point = (Int, Int)

pGroup :: Parser (Point, Point, Point)
pGroup = do
    void $ string "Button A: X+"
    xA <- L.decimal
    void $ string ", Y+"
    yA <- L.decimal
    void eol
    void $ string "Button B: X+"
    xB <- L.decimal
    void $ string ", Y+"
    yB <- L.decimal
    void eol
    void $ string "Prize: X="
    xP <- L.decimal
    void $ string ", Y="
    yP <- L.decimal
    let a = (xA, yA)
        b = (xB, yB)
        p = (xP, yP)
    void eol -- Expect every group to end in an end of line sequence
    pure (a, b, p)

pInput :: String -> [(Point, Point, Point)]
pInput = fromJust . parseMaybe (sepBy pGroup eol) . T.pack

run :: IO ()
run = do
    input <- readFile "input/d13.txt"
    print $ part1 input
    print $ part2 input

possible :: Int -> Int -> Int -> Bool
possible a b c = c `mod` d == 0
  where
    d = gcd a b

findPairings :: Point -> Point -> Point -> Maybe [Point]
findPairings (xA, yA) (xB, yB) (xP, yP) = case go 0 of
    [] -> Nothing
    lst -> Just lst
  where
    go numB
        | current > xP || numB > 100 = []
        | diff `mod` xA == 0 && numA <= 100 && worksForY = (numA, numB) : rest
        | otherwise = rest
      where
        current = numB * xB
        diff = xP - current
        numA = diff `div` xA
        rest = go (numB + 1)
        worksForY = numA * yA + numB * yB == yP

cost :: Point -> Int
cost = (+) <$> ((* 3) . fst) <*> snd

processGroup :: (Point, Point, Point) -> Maybe Int
processGroup (a@(xA, yA), b@(xB, yB), p@(xP, yP))
    | possible xA xB xP && possible yA yB yP = one
    | otherwise = Nothing
  where
    f = minimum . map cost
    one = f <$> findPairings a b p

part1 :: String -> Int
part1 = sum . mapMaybe processGroup . pInput

findPair :: Point -> Point -> Point -> Maybe Point
findPair (x1, y1) (x2, y2) (x3, y3)
    | a * x1 + b * x2 == x3 && a * y1 + b * y2 == y3 = Just (a, b)
    | otherwise = Nothing
  where
    a = (x2 * y3 - x3 * y2) `div` (x2 * y1 - x1 * y2)
    b = (x1 * y3 - y1 * x3) `div` (x1 * y2 - x2 * y1)

processGroupV2 :: (Point, Point, Point) -> Maybe Int
processGroupV2 (a@(xA, yA), b@(xB, yB), p@(xP, yP))
    | possible xA xB xP && possible yA yB yP = cost <$> findPair a b p
    | otherwise = Nothing

part2 :: String -> Int
part2 = sum . mapMaybe processGroupV2 . map f . pInput
  where
    n = 10000000000000
    f (a, b, (x, y)) = (a, b, (x + n, y + n))
