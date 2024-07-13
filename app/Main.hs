{-# LANGUAGE OverloadedStrings #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect (type (~>))
import Control.Effect.ExtensibleChurch ((:!!), type (!!))
import Control.Effect.Handler.Heftia.Coroutine (runCoroutine)
import Control.Effect.Handler.Heftia.Fail (runFailAsIO)
import Control.Effect.Handler.Heftia.State (runStateIORef)
import Control.Effect.Hefty (interpret, runEff, transformAll, unkeyEff)
import Control.Exception (catch, throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Effect (LNop)
import Data.Effect.Coroutine (Status (Coroutine, Done))
import Data.Effect.Key (type (#>))
import Data.Effect.State (State)
import Data.Free.Sum (type (+))
import Data.Function ((&))
import Data.Hefty.Union (Union (exhaust, inject0, weaken, weaken2, weaken3, (|+:)))
import Data.Map (Map)
import Game.HeftWorld.IO (
    Game,
    Graphics' (
        DrawImage,
        LoadFont,
        LoadImage,
        RenderText,
        TextSize,
        UnloadFont,
        UnloadImage
    ),
    KeyState,
    LoadError (NoSuchFile),
    SpriteState,
    drawSprites,
    runSpriteAsState,
 )
import Game.HeftWorld.World (heftWorldUpdate)
import Numeric.Natural (Natural)
import SDL
import SDL.Font (Font)
import SDL.Font qualified as Font
import SDL.Image (loadTexture)
import Web.KeyCode qualified as Web

main :: IO ()
main = do
    initializeAll
    Font.initialize
    window <- createWindow "HeftWorld" defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer
    appLoop window renderer
    destroyWindow window

-- | メインループ
appLoop :: Window -> Renderer -> IO ()
appLoop window renderer =
    runEff
        . (fmap snd . runStateIORef 0)
        . (fmap snd . runStateIORef mempty . unkeyEff)
        $ do
            void $ runLoop window renderer =<< runGame renderer heftWorldUpdate

type GameLoopM =
    LNop !! "sprites" #> State (Map Natural (SpriteState Texture)) + State Natural + IO

runLoop :: Window -> Renderer -> Status GameLoopM () KeyState a -> GameLoopM (Maybe a)
runLoop window renderer = \case
    Done a -> pure $ Just a
    Coroutine () k -> do
        events <- pollEvents
        if QuitEvent `elem` map eventPayload events
            then pure Nothing
            else do
                ks <- getKeyboardState

                rendererDrawColor renderer $= V4 255 255 255 255
                clear renderer

                next <- k $ maybe False ks . toSdlScancode

                present renderer

                runLoop window renderer next
  where
    toSdlScancode :: Web.Key -> Maybe Scancode
    toSdlScancode = \case
        Web.ArrowDown -> Just ScancodeDown
        Web.ArrowUp -> Just ScancodeUp
        Web.ArrowLeft -> Just ScancodeLeft
        Web.ArrowRight -> Just ScancodeRight
        Web.Space -> Just ScancodeSpace
        _ -> Nothing

-- | Graphicsエフェクト群をSDLの関数へと解釈
runGame ::
    Renderer ->
    Game Texture Natural Font ->
    GameLoopM (Status GameLoopM () KeyState ())
runGame renderer update =
    ( ( update
            & raise3Under4
            & runSpriteAsState
      )
        <* drawSprites
    )
        & unkeyEff
        & interpret \case
            LoadImage path ->
                liftIO $
                    do
                        tex <- loadTexture renderer path
                        -- surface <- convertSurface image screenPixelFormat
                        pure $ Right tex
                        `catch` \case
                            SDLCallFailed _ _ "Parameter 'src' is invalid" ->
                                pure $ Left NoSuchFile
                            e -> throwIO e
            UnloadImage tex -> destroyTexture tex
            DrawImage tex (V2 x y) -> do
                TextureInfo {textureWidth, textureHeight} <- queryTexture tex
                let destRect =
                        Rectangle
                            (P $ V2 (round x) (round y))
                            (V2 textureWidth textureHeight)
                copy renderer tex Nothing (Just destRect)
                pure $ Right ()
            LoadFont path fontSize ->
                -- fixme: error handling
                Right <$> Font.load path fontSize
            UnloadFont font -> Font.free font
            RenderText font color text -> do
                -- fixme: error handling
                surface <- Font.blended font color text
                Right <$> createTextureFromSurface renderer surface
            TextSize font text -> do
                (x, y) <- Font.size font text
                pure $ V2 (fromIntegral x) (fromIntegral y)
        & runFailAsIO
        & runCoroutine

raise3Under4 :: '[] :!! e7 ': e6 ': e5 ': e4 ': '[] ~> '[] :!! e7 ': e6 ': e5 ': e4 ': e3 ': e2 ': e1 ': '[]
raise3Under4 =
    transformAll $
        inject0
            |+: weaken . inject0
            |+: weaken2 . inject0
            |+: weaken3 . inject0
            |+: exhaust
