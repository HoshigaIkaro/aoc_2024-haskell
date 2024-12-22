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
import Days.D14 qualified as D14
import Days.D15 qualified as D15
import Days.D16 qualified as D16
import Days.D17 qualified as D17
import Days.D18 qualified as D18
import Days.D19 qualified as D19
import Days.D20 qualified as D20
import Days.D21 qualified as D21
import Days.D22 qualified as D22

-- import Graphics.Vty (Vty (nextEvent), VtyUserConfig (VtyUserConfig), defaultConfig)
-- import Graphics.Vty.CrossPlatform (mkVty)
-- import Graphics.Vty.Input

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
    14 -> D14.run
    15 -> D15.run
    16 -> D16.run
    17 -> D17.run
    18 -> D18.run
    19 -> D19.run
    20 -> D20.run
    21 -> D21.run
    22 -> D22.run
    _ -> pure ()

-- testInteractive :: IO ()
-- testInteractive = do
--     vty <- mkVty defaultConfig
--     e <- nextEvent vty
--     print e
--     case e of
--         EvKey key _ -> print key
--         _ -> testInteractive
--     pure ()