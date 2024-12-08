{-# LANGUAGE OverloadedStrings #-}

module Days.D7 (run, part1, part2) where

import Control.Monad
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Float.RealFracMethods (ceilingDoubleInt, floorDoubleInt)
import Text.Megaparsec
import Text.Megaparsec.Char (space, string)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

run :: IO ()
run = do
  input <- readFile "input/d7.txt"
  print $ part1 input
  print $ part2 input

pLine :: Parser (Int, [Int])
pLine = do
  num <- L.decimal
  void $ string ": "
  values <- sepBy L.decimal space
  pure (num, values)

pInput :: Text -> [(Int, [Int])]
pInput = map (fromJust . parseMaybe pLine) . T.lines

isPossible :: [Int -> Int -> Int] -> Int -> Int -> [Int] -> Bool
isPossible _ current target [] = target == current
isPossible ops current target (x : xs) = any (\op -> isPossible ops (current `op` x) target xs) ops

part1 :: String -> Int
part1 = sum . map fst . filter (uncurry (isPossible operators 0)) . pInput . T.pack
 where
  operators = [(+), (*)]

part2 :: String -> Int
part2 = sum . map fst . filter (uncurry (isPossible operators 0)) . pInput . T.pack
 where
  l = succ . floorDoubleInt . logBase 10 . fromIntegral
  f a b = a * (10 ^ l b) + b
  operators = [(+), (*), f]