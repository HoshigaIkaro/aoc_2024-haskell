{-# OPTIONS_GHC -Wno-x-partial #-}

module Days (
    module Days,
) where

import Days.D1 qualified as D1
import Days.D2 qualified as D2
import Days.D3 qualified as D3
import Days.D4 qualified as D4

runDay :: Int -> IO ()
runDay day = case day of
    1 -> D1.run
    2 -> D2.run
    3 -> D3.run
    4 -> D4.run
    _ -> pure ()