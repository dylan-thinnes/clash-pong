{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import Clash.Prelude hiding (lift)

import Pong
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Sim.SDL
import RetroClash.Sim.VGA
import RetroClash.Sim.VGASDL
import Control.Monad.State
import Control.Arrow.Transformer.Automaton
import Control.Lens

main :: IO ()
main = do
    buf <- newBufferArray

    let board (up, dn) =
            let VGAOut{ vgaSync = VGASync{..}, ..} = topEntity clockGen resetGen up dn
            in bundle (vgaHSync, vgaVSync, bitCoerce <$> bundle (vgaR, vgaG, vgaB))
        sim = signalAutomaton (board . unbundle)

    flip evalStateT (sim, initSink) $ withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        let up = toActive $ keyDown ScancodeUp
            down = toActive $ keyDown ScancodeDown
        let sinkVGA = zoom _2 . vgaSinkBuf vga640x480at60 buf

        let go (Automaton step) = do
                let (vga, next) = step (up, down)
                frameDone <- sinkVGA vga
                if frameDone then return next else go next
        _1 <~ (go =<< use _1)

        return $ rasterizeBuffer buf
  where
    videoParams = MkVideoParams
        { windowTitle = "Pong"
        , screenScale = 2
        , screenRefreshRate = 60
        , reportFPS = True
        }
