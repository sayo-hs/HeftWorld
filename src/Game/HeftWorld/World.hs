{-# LANGUAGE OverloadedStrings #-}

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
import Formatting (fixed, sformat, shown, (%))
import Game.HeftWorld.IO (
    ExternalState (ExternalState, deltaTime, elapsedTime, isKeyPressed),
    Game,
    drawImage,
    drawText,
    withFont,
    withImage,
 )
import Linear (V2 (V2), V4 (V4))
import Web.KeyCode (Key (..))

heftWorldUpdate :: Game image surface font
heftWorldUpdate =
    evalState @(V2 Double) (V2 400 400)
        . runShift_
        $ do
            blockGreen <- withImage "assets/block_green.bmp"

            font <- withFont "assets/font/Monoid/Monoid-Regular.ttf" 16
            let drawBlackText = drawText font (V4 0 0 0 255)

            fix \next -> do
                pos <- get @(V2 Double)
                Right () <- drawImage blockGreen pos

                ExternalState {..} <- yield ()

                when (isKeyPressed ArrowLeft) $ modify @(V2 Double) \(V2 x y) -> V2 (x - speed) y
                when (isKeyPressed ArrowRight) $ modify @(V2 Double) \(V2 x y) -> V2 (x + speed) y
                when (isKeyPressed ArrowDown) $ modify @(V2 Double) \(V2 x y) -> V2 x (y + speed)
                when (isKeyPressed ArrowUp) $ modify @(V2 Double) \(V2 x y) -> V2 x (y - speed)

                let debugTexts =
                        [ sformat (fixed 2 % " FPS") (1 / deltaTime)
                        , sformat ("elapsed time: " % fixed 2 % " s") elapsedTime
                        , sformat ("block position: " % shown) pos
                        , ""
                        , "control:"
                        , "    arrow keys: move"
                        , "    space key: quit"
                        ]

                forM_ (zip [0 ..] debugTexts) \(i, text) ->
                    drawBlackText (V2 10 (10 + i * 24)) text

                unless (isKeyPressed Space) next

speed :: Double
speed = 16
