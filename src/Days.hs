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
import Days.D7 qualified as D7
import Days.D8 qualified as D8
import Days.D9 qualified as D9
import Days.D10 qualified as D10
import Days.D11 qualified as D11
import Days.D12 qualified as D12
import Days.D13 qualified as D13

runDay :: Int -> IO ()
runDay day = case day of
    1 -> D1.run
    2 -> D2.run
    3 -> D3.run
    4 -> D4.run
    5 -> D5.run
    6 -> D6.run
    7 -> D7.run
    8 -> D8.run
    9 -> D9.run
    10 -> D10.run
    11 -> D11.run
    12 -> D12.run
    13 -> D13.run
    _ -> pure ()