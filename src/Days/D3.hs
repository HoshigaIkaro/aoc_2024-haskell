{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.D3 (run, part1, part2) where

import Control.Monad (void)
import Data.Either (fromRight, rights)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Replace.Megaparsec (sepCap)
import Text.Megaparsec (Parsec, runParser, (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

run :: IO ()
run = do
    input <- readFile "input/d3.txt"
    print $ part1 input
    print $ part2 input

pPair :: Parser (Int, Int)
pPair = do
    void $ char '('
    a <- L.decimal
    void $ char ','
    b <- L.decimal
    void $ char ')'
    pure (a, b)

pMulPair :: Parser (Int, Int)
pMulPair = string "mul" *> pPair

pAllPairs :: Parser [Either Text (Int, Int)]
pAllPairs = sepCap pMulPair

part1 :: String -> Int
part1 = sum . map (uncurry (*)) . rights . fromRight [] . runParser pAllPairs "" . T.pack

pDo :: Parser ()
pDo = void $ string "do()"
pDont :: Parser ()
pDont = void $ string "don't()"

data PResult = Pair (Int, Int) | Do | Dont deriving (Show)

pAllPairsToggleable :: Parser [Either Text PResult]
pAllPairsToggleable =
    sepCap $
        (Pair <$> pMulPair)
            <|> (Dont <$ pDont)
            <|> (Do <$ pDo)

part2 :: String -> Int
part2 = snd . foldr go (True, 0) . reverse . rights . fromRight [] . runParser pAllPairsToggleable "" . T.pack
  where
    go Dont (_, acc) = (False, acc)
    go Do (_, acc) = (True, acc)
    go (Pair (a, b)) (enabled, acc)
        | enabled = (enabled, acc + a * b)
        | otherwise = (enabled, acc)