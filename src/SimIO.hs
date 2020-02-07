{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module SimIO where

import Clash.Prelude
import System.IO.Unsafe
import Control.Monad.Fix
import Control.Concurrent
import Control.Monad
import Data.List as L
import Debug.Trace

dialogue' :: ([i] -> [o]) -> (o -> IO i) -> IO ()
dialogue' circuit act = do
    ys <- mfix $ fmap circuit . go
    consume ys
  where
    go ~(o:os) = unsafeInterleaveIO $ (:) <$> act o <*> go os
    consume (o:os) = consume os
    consume [] = return ()

dialogue :: ([i] -> [o]) -> (o -> IO i) -> IO ()
dialogue circuit act = do
    inChan <- newChan
    outChan <- newChan
    forkIO $ forever $ do
        readChan outChan >>= act >>= writeChan inChan
    is <- getChanContents inChan
    mapM_ (writeChan outChan) $ circuit is

simulateIO
    :: (KnownDomain dom)
    => (HiddenClockResetEnable dom => Signal dom i -> Signal dom o)
    -> (o -> IO i)
    -> IO ()
simulateIO circuit act = dialogue (simulate_lazy circuit) act
