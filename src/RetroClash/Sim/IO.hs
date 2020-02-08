{-# LANGUAGE RankNTypes #-}
module RetroClash.Sim.IO where

import Clash.Prelude
import System.IO.Unsafe
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent

simulateIO
    :: (KnownDomain dom, NFDataX i, NFDataX o)
    => (HiddenClockResetEnable dom => Signal dom i -> Signal dom o)
    -> (o -> IO i)
    -> IO ()
simulateIO circuit world = do
    ys <- mfix $ fmap circuitFun . go
    consume ys
  where
    circuitFun = simulate circuit

    go ~(o:os) = unsafeInterleaveIO $ (:) <$> world o <*> go os
    consume (o:os) = consume os

simulateIO2
    :: (KnownDomain dom, NFDataX i, NFDataX o)
    => (HiddenClockResetEnable dom => Signal dom i -> Signal dom o)
    -> (o -> IO i)
    -> IO ()
simulateIO2 circuit world = do
    inChan <- newChan
    outChan <- newChan
    forkIO $ forever $ do
        readChan outChan >>= world >>= writeChan inChan
    is <- getChanContents inChan
    mapM_ (writeChan outChan) $ circuitFun is
  where
    circuitFun = simulate circuit
