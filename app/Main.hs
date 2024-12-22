module Main where

import Days (runDay)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if length args == 0
        then runDay 7
        else runDay $ read $ args !! 0
    -- testInteractive