{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
import Clash.Prelude
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Clock
import Data.Maybe
import System.IO.Unsafe
import Data.List as L
import Control.Monad.Fix

import SimIO

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

{-# ANN topEntity
  (Synthesize
    { t_name   = "SimTest"
    , t_inputs =
          [ PortName "CLK_25MHZ"
          , PortName "RESET"
          , PortName "NEXT"
          ]
    , t_output = PortName "SUM"
    }) #-}
topEntity
    :: Clock Dom25
    -> Reset Dom25
    -> Signal Dom25 (Unsigned 8)
    -> Signal Dom25 (Unsigned 8)
topEntity = withEnableGen board
  where
    board next = r
      where
        r = register 0 $ r + next

foo = simulate_lazy $ topEntity clockGen resetGen

main :: IO ()
main = do
    simulateIO (topEntity clockGen resetGen) $ \x -> do
        putStrLn $ unwords ["The current value is", showX x]
        readLn
