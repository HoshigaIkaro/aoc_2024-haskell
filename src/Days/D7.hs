{-# LANGUAGE OverloadedStrings #-}

module Days.D7 (run, part1, part2) where

import Control.Monad
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol, newline, space, string)
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

operatorsPossible :: Int -> [Int] -> [Int -> Int -> Int] -> Int -> Bool
operatorsPossible target [] _ current = target == current
operatorsPossible target (x : xs) ops current =
    any (\op -> operatorsPossible target xs ops (current `op` x)) ops

part1 :: String -> Int
part1 s = sum . map fst . filter (\(n, vals) -> operatorsPossible n vals [(+), (*)] 0) . pInput $ T.pack s

operatorsWithConcatPossible :: Int -> [Int] -> [Int -> Int -> Int] -> Int -> Bool
operatorsWithConcatPossible target [] _ current = target == current
operatorsWithConcatPossible target (x : xs) ops current =
    any (\op -> operatorsWithConcatPossible target xs ops (current `op` x)) ops

part2 :: String -> Int
part2 s = sum . map fst . filter (\(n, vals) -> operatorsWithConcatPossible n vals [(+), (*), f] 0) . pInput $ T.pack s
    where
        f a b = read (show a <> show b)