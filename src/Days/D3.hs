{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.D3 (run, part1, part2) where

import Control.Monad (void)
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

run :: IO ()
run = do
    input <- readFile "input/d3.txt"
    print $ part1 input
    print $ part2 input

pPair :: Parser (Int, Int)
pPair = do
    a <- L.decimal
    void $ char ','
    b <- L.decimal
    pure (a, b)

pMulPair :: Parser (Int, Int)
pMulPair = lookAhead (string "mul" *> parentheses pPair) *> (string "mul" *> parentheses pPair) <* (void (many anyChar) <|> eof)

anyChar :: Parser ()
anyChar = void asciiChar <|> void (char ' ') <|> void symbolChar

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (void $ many anyChar)

betweenFiller :: Parser a -> Parser a
betweenFiller = between (many anyChar) (many anyChar)

parentheses :: Parser a -> Parser a
parentheses = between (char '(') (char ')')

-- pAllPairs :: Parser [(Int, Int)]
pAllPairs = many (recover (many (anySingleBut 'm') *> (string "mul" *> parentheses pPair)))
  where
    recover = withRecovery $ \e -> do
        registerParseError e
        _ <- void (many (anySingleBut 'm')) <|> eof
        pure (0, 0)

findPairs :: String -> [(Int, Int)]
findPairs "" = []
findPairs str = case parseMaybe pMulPair (T.pack str) of
    Nothing -> findPairs (drop 1 str)
    Just pair -> pair : findPairs (drop 1 str)

part1 :: String -> Int
part1 = sum . map (uncurry (*)) . findPairs

pDo :: Parser ()
pDo = void $ string "do()" <* (void (many anyChar) <|> eof)

pDont :: Parser ()
pDont = void $ string "don't()" <* (void (many anyChar) <|> eof)

part2 :: String -> Int
part2 = sum . map (uncurry (*)) . findPairsToggleable True . T.pack

findPairsToggleable :: Bool -> Text -> [(Int, Int)]
findPairsToggleable _ "" = []
findPairsToggleable False str
    | "do()" `T.isPrefixOf` str = findPairsToggleable True (T.drop 4 str)
    | otherwise = findPairsToggleable False (T.drop 1 str)
findPairsToggleable True str
    | "don't()" `T.isPrefixOf` str = findPairsToggleable False (T.drop 7 str)
    | otherwise = case parseMaybe pMulPair str of
        Nothing -> findPairsToggleable True (T.drop 1 str)
        Just pair -> pair : findPairsToggleable True (T.drop 1 str)