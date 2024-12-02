{-# OPTIONS_GHC -Wno-x-partial #-}

module Days (runDay) where

import Days.D1 qualified as D1
import Days.D2 qualified as D2

runDay :: Int -> IO ()
runDay day = case day of
    1 -> D1.run
    2 -> D2.run
    _ -> pure ()