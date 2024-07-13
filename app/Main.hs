{-# LANGUAGE OverloadedStrings #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect (type (~>))
import Control.Effect.ExtensibleFinal ((:!!), type (!!))
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
import Web.KeyCode qualified as Web

main :: IO ()
main = do
    initializeAll
    window <- createWindow "HeftWorld" defaultWindow
    appLoop window
    destroyWindow window

-- | メインループ
appLoop :: Window -> IO ()
appLoop window =
    runEff
        . (fmap snd . runStateIORef 0)
        . (fmap snd . runStateIORef mempty . unkeyEff)
        $ do
            screen <- getWindowSurface window
            pixelFormat <- surfaceFormat screen
            void $ runLoop window =<< runGame screen pixelFormat heftWorldUpdate

type GameLoopM =
    LNop !! "sprites" #> State (Map Natural (SpriteState Surface)) + State Natural + IO

runLoop :: Window -> Status GameLoopM () KeyState a -> GameLoopM (Maybe a)
runLoop window = \case
    Done a -> pure $ Just a
    Coroutine () k -> do
        events <- pollEvents
        if QuitEvent `elem` map eventPayload events
            then pure Nothing
            else do
                updateWindowSurface window
                ks <- getKeyboardState
                next <- k $ maybe False ks . toSdlScancode
                runLoop window next
  where
    {-
    keyStateFromEvents :: [Event] -> KeyState
    keyStateFromEvents events =
        maybe False (`Set.member` pressedKeys) . toSdlKeycode
      where
        pressedKeys = flip foldMap events \event ->
            case eventPayload event of
                KeyboardEvent keyev ->
                    if keyboardEventKeyMotion keyev == Pressed
                        then Set.singleton $ keysymKeycode $ keyboardEventKeysym keyev
                        else mempty
                _ -> mempty
    -}

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
    Surface ->
    SurfacePixelFormat ->
    Game Surface Natural Font ->
    GameLoopM (Status GameLoopM () KeyState ())
runGame screen screenPixelFormat update =
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
                        image <- loadBMP path
                        surface <- convertSurface image screenPixelFormat
                        pure $ Right surface
                        `catch` \case
                            SDLCallFailed _ _ "Parameter 'src' is invalid" ->
                                pure $ Left NoSuchFile
                            e -> throwIO e
            UnloadImage surface -> freeSurface surface
            DrawImage surface (V2 x y) -> do
                _ <- surfaceBlit surface Nothing screen (Just $ P $ V2 (round x) (round y))
                pure $ Right ()
            LoadFont path fontSize -> error "not implemented yet"
            UnloadFont font -> Font.free font
            RenderText font color text -> error "not implemented yet"
            TextSize font text -> error "not implemented yet"
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
