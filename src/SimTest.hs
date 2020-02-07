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

dialogue :: ([i] -> [o]) -> (o -> IO i) -> IO [o]
dialogue circuit act = mfix $ fmap circuit . go
  where
    go ~(o:os) = unsafeInterleaveIO $ (:) <$> act o <*> go os

simulateIO
    :: (KnownDomain dom)
    => (HiddenClockResetEnable dom => Signal dom i -> Signal dom o)
    -> (o -> IO i)
    -> IO [o]
simulateIO circuit act = dialogue (simulate_lazy circuit) act

foo = simulate_lazy $ topEntity clockGen resetGen

main :: IO ()
main = do
    simulateIO (topEntity clockGen resetGen) $ \x -> do
        putStrLn $ unwords ["The current value is", showX x]
        readLn
    mapM_ print $ L.take 10 ys
    return ()
