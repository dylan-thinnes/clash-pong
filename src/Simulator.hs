{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude

import Pong.Game
import Pong.Video
import RetroClash.Sim.SDL.PatternGenerator

import SDL hiding (get)
import Control.Monad.State

main :: IO ()
main = withMainWindow "Pong" 2 initState $ \events keyState s -> fmap Just $ flip runStateT s $ do
    modify $ updateState defaultParams $ MkInputState
        { paddleUp = keyState ScancodeUp
        , paddleDown = keyState ScancodeDown
        }
    gets $ draw defaultParams
