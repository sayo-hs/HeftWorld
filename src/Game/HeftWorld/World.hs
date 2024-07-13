-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Game.HeftWorld.World where

import Control.Effect.Handler.Heftia.ShiftReset (runShift_)
import Control.Effect.Handler.Heftia.State (evalState)
import Control.Monad (forM_, unless, when)
import Data.Effect.Coroutine (yield)
import Data.Effect.State (get, modify)
import Data.Function (fix)
import Formatting (sformat, shown)
import Game.HeftWorld.IO (Game, KeyState, drawImage, renderText, withFont, withImage)
import Linear (V2 (V2), V4 (V4))
import Web.KeyCode (Key (..))

heftWorldUpdate :: Game image surface font
heftWorldUpdate =
    evalState @(V2 Double) (V2 0 0)
        . runShift_
        $ do
            blockGreen <- withImage "assets/block_green.bmp"
            font <- withFont "/usr/share/fonts/ja-ipafonts/ipag.ttf" 20
            fix \next -> do
                pos@(V2 px py) <- get @(V2 Double)
                forM_ [-4 .. 4] \(i :: Int) -> do
                    forM_ [-4 .. 4] \(j :: Int) -> do
                        Right () <- drawImage blockGreen (V2 (px + fromIntegral i * 64) (py + fromIntegral j * 64))
                        pure ()

                isKeyPressed :: KeyState <- yield ()

                when (isKeyPressed ArrowLeft) $ modify @(V2 Double) \(V2 x y) -> V2 (x - 1) y
                when (isKeyPressed ArrowRight) $ modify @(V2 Double) \(V2 x y) -> V2 (x + 1) y
                when (isKeyPressed ArrowDown) $ modify @(V2 Double) \(V2 x y) -> V2 x (y + 1)
                when (isKeyPressed ArrowUp) $ modify @(V2 Double) \(V2 x y) -> V2 x (y - 1)

                Right text <- renderText font (V4 0 0 0 255) (sformat shown pos)
                Right () <- drawImage text (V2 10 10)

                unless (isKeyPressed Space) next
