{-# OPTIONS_GHC -Wno-x-partial #-}

module Days (
    module Days,
) where

import Days.D1 qualified as D1
import Days.D2 qualified as D2
import Days.D3 qualified as D3
import Days.D4 qualified as D4
import Days.D5 qualified as D5
import Days.D6 qualified as D6

runDay :: Int -> IO ()
runDay day = case day of
    1 -> D1.run
    2 -> D2.run
    3 -> D3.run
    4 -> D4.run
    5 -> D5.run
    6 -> D6.run
    _ -> pure ()