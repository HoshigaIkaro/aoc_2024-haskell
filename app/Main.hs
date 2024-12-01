module Main where

import MyLib (part1, part2)

main :: IO ()
main = do
    input1 <- readFile "input/d1/p1.txt"
    print $ part1 input1
    print $ part2 input1
