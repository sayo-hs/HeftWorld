-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Game.HeftWorld.World where

import Control.Effect.Handler.Heftia.ShiftReset (runShift_)
import Control.Effect.Handler.Heftia.State (evalState)
import Control.Monad (unless, when)
import Data.Effect.Coroutine (yield)
import Data.Effect.State (get, modify)
import Data.Function (fix)
import Game.HeftWorld.IO (Game, KeyState, drawImage, withImage)
import Linear (V2 (V2))
import Web.KeyCode (Key (..))

heftWorldUpdate :: Game image surface font
heftWorldUpdate =
    evalState @(V2 Double) (V2 0 0)
        . runShift_
        $ do
            blockGreen <- withImage "assets/block_green.bmp"
            fix \next -> do
                pos <- get @(V2 Double)
                Right () <- drawImage blockGreen pos

                isKeyPressed :: KeyState <- yield ()

                when (isKeyPressed ArrowLeft) $ modify @(V2 Double) \(V2 x y) -> V2 (x - 1) y
                when (isKeyPressed ArrowRight) $ modify @(V2 Double) \(V2 x y) -> V2 (x + 1) y
                when (isKeyPressed ArrowDown) $ modify @(V2 Double) \(V2 x y) -> V2 x (y + 1)
                when (isKeyPressed ArrowUp) $ modify @(V2 Double) \(V2 x y) -> V2 x (y - 1)

                unless (isKeyPressed Space) next
