{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Pong
import RetroClash.Sim.IO

import RetroClash.Utils
import RetroClash.VGA
import Control.Monad.State
import Data.Foldable
import Control.Lens hiding (Index)
import RetroClash.Sim.SDL
import Data.Array.IO
import Control.Concurrent
import Data.IORef

import Clash.Prelude hiding (lift)
import Data.Word
import Debug.Trace
import System.IO
import SDL hiding (get)

import System.Clock

vgaRetrace :: VGATiming visible -> (Int, Bit)
vgaRetrace VGATiming{..} = (snatToNum pulseWidth + snatToNum postWidth, toActiveDyn polarity True)

data SinkState
    = Visible Int
    | WaitSync Bool
    | Retrace Int
    deriving (Show)

vgaSink
    :: forall w h r g b m ps. (KnownNat w, KnownNat h, Monad m, m ~ IO)
    => VGATimings ps w h
    -> (Int -> Int -> (r, g, b) -> m ())
    -> (Bit, Bit, (r, g, b))
    -> StateT (SinkState, SinkState, TimeSpec) m ()
vgaSink VGATimings{..} paint (hsync0, vsync0, color) = do
    (x, endLine) <- zoom _1 $ direction w horizRetrace hsync
    (y, endFrame) <- zoom _2 $ (if endLine then id else undo) $ direction h vertRetrace vsync
    when (endLine && endFrame) $ zoom _3 $ do
        t <- lift $ getTime Monotonic
        t0 <- get
        put t
        lift $ print $ millisec t - millisec t0
    for_ (liftA2 (,) x y) $ \(x, y) -> lift $ paint x y color
  where
    (horizRetrace, hsyncTarget) = vgaRetrace vgaHorizTiming
    (vertRetrace, vsyncTarget) = vgaRetrace vgaVertTiming

    millisec (TimeSpec sec nsec) = sec * 1_000 + nsec `div` 1_000_000

    vsync = vsync0 == vsyncTarget
    hsync = hsync0 == hsyncTarget

    w = fromIntegral (maxBound :: Index w)
    h = fromIntegral (maxBound :: Index h)

    undo act = do
        s <- get
        x <- act
        put s
        return x

    -- direction :: (KnownNat vis, Monad m) => Int -> Bool -> StateT (SinkState vis) m (Maybe (Index vis), Bool)
    direction vis retrace sync = do
        s <- get
        case s of
            Retrace n -> do
                put $ if n == retrace then Visible 0 else Retrace (n + 1)
                return (Nothing, False)
            Visible i -> do
                put $ if i == vis then WaitSync sync else Visible (i + 1)
                return (Just i, False)
            WaitSync b -> do
                let end = not b && sync
                put $ if end then Retrace 0 else WaitSync sync
                return (Nothing, end)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    arr <- newArray @IOUArray ((0, 0, 0), (639, 479, 2)) 0
    ray <- newIORef (0, 0)

    let writeBuf x y (r, g, b) = do
            writeIORef ray (x, y)
            writeArray arr (x, y, 0) r
            writeArray arr (x, y, 1) g
            writeArray arr (x, y, 2) b

    keyStateRef <- newIORef (const False)

    forkIO $ do
        ref <- newIORef (WaitSync False, WaitSync False, TimeSpec 0 0)
        cnt <- newIORef (0 :: Int)
        simulateIO2 topEntity' $ \vgaOut@(vgaHSync, vgaVSync, (vgaR, vgaG, vgaB)) -> do
            keyState <- readIORef keyStateRef
            let sw = repeat low
                up = keyState ScancodeUp
                dn = keyState ScancodeDown

            s <- readIORef ref
            s' <- execStateT (vgaSink vga640x480at60 writeBuf vgaOut) s
            writeIORef ref s'

            return (sw, toActive up, toActive dn)

    withMainWindow "VGA" 2 () $ \events keyState () -> do
        writeIORef keyStateRef keyState
        return $ Just (rasterizeRay ray $ rasterizeBuffer (SNat @640) (SNat @480) arr, ())
  where
    topEntity' i =
        let VGAOut{ vgaSync = VGASync{..}, ..} = topEntity clockGen resetGen sw up dn
        in bundle (vgaHSync, vgaVSync, bundle (vgaR, vgaG, vgaB))
      where
        (sw, up, dn) = unbundle i
