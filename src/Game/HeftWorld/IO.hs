{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Game.HeftWorld.IO where

import Control.Arrow ((>>>))
import Control.Effect (type (<<:), type (~>))
import Control.Effect.ExtensibleChurch ((:!!), type (!!))
import Control.Effect.Handler.Heftia.Fresh (runFreshNaturalAsState)
import Control.Effect.Handler.Heftia.KVStore (runKVStoreAsState)
import Control.Effect.Hefty (interpret, keySubsume, raiseUnder, unkeyEff, type ($))
import Control.Effect.Key (SendInsBy)
import Control.Exception (Exception)
import Control.Monad (forM_)
import Control.Monad.Cont (ContT (ContT))
import Data.Effect (LNop, LiftIns)
import Data.Effect.Coroutine (Yield)
import Data.Effect.Fail (Fail)
import Data.Effect.Fresh (Fresh, fresh)
import Data.Effect.KVStore (KVStore, deleteKV, lookupKV, writeKV)
import Data.Effect.Key.TH (makeKeyedEffect)
import Data.Effect.ShiftReset (Shift_, shift_)
import Data.Effect.State (State, get'')
import Data.Free.Sum (type (+))
import Data.Functor ((<&>))
import Data.Hefty.Extensible (MemberBy, type (<|))
import Data.Kind (Type)
import Data.Map (Map)
import Data.NonEmptyText (NonEmptyText)
import Data.NonEmptyText qualified as NE
import Data.Text (Text)
import Data.Word (Word8)
import Linear (V2, V4)
import Numeric.Natural (Natural)
import Web.KeyCode qualified as Keycode

data Graphics' (image :: Type) (font :: Type) a where
    LoadImage :: FilePath -> Graphics' image font (Either LoadError image)
    UnloadImage :: image -> Graphics' image font ()
    DrawImage :: image -> V2 Double -> Graphics' image font (Either NoSuchImage ())
    LoadFont :: FilePath -> Int -> Graphics' image font (Either LoadError font)
    UnloadFont :: font -> Graphics' image font ()
    RenderText :: font -> V4 Word8 -> NonEmptyText -> Graphics' image font (Either NoSuchFont image)
    TextSize :: font -> Text -> Graphics' image font (V2 Double)

data LoadError = NoSuchFile | InvalidFileFormat
    deriving stock (Show)
    deriving anyclass (Exception)

data NoSuchImage = NoSuchImage
    deriving stock (Show)
    deriving anyclass (Exception)

data NoSuchSprite = NoSuchSprite
    deriving stock (Show)
    deriving anyclass (Exception)

data NoSuchFont = NoSuchFont
    deriving stock (Show)
    deriving anyclass (Exception)

makeKeyedEffect [''Graphics'] []

withImage :: (SendInsBy GraphicsKey (Graphics' image font) m, Shift_ <<: m, MonadFail m) => FilePath -> m image
withImage path = shift_ \k -> do
    Right i <- loadImage path
    k i <* unloadImage i

withFont :: (SendInsBy GraphicsKey (Graphics' image font) m, Shift_ <<: m, MonadFail m) => FilePath -> Int -> m font
withFont path fontSize = shift_ \k -> do
    Right f <- loadFont path fontSize
    k f <* unloadFont f

drawText :: (SendInsBy GraphicsKey (Graphics' image font) m, MonadFail m) => font -> V4 Word8 -> V2 Double -> Text -> m ()
drawText font color position text =
    forM_ (NE.fromText text) \neText -> do
        Right img <- renderText font color neText
        Right () <- drawImage img position
        pure ()

data Sprite' (image :: Type) (sprite :: Type) a where
    CreateSprite :: Sprite' image sprite sprite
    DeleteSprite :: sprite -> Sprite' image sprite ()
    SetSpriteImage :: sprite -> image -> Sprite' image sprite (Either NoSuchSprite ())
    UnsetSpriteImage :: sprite -> Sprite' image sprite (Either NoSuchSprite ())
    GetSpriteImage :: sprite -> Sprite' image sprite (Either NoSuchSprite (Maybe image))
    SetSpritePosition :: sprite -> Maybe (V2 Double) -> Sprite' image sprite (Either NoSuchSprite ())
    GetSpritePosition :: sprite -> Sprite' image sprite (Either NoSuchSprite (Maybe (V2 Double)))

makeKeyedEffect [''Sprite'] []

withSprite :: (SendInsBy SpriteKey (Sprite' image sprite) m, MonadFail m) => ContT r m sprite
withSprite = ContT \k -> do
    s <- createSprite
    k s <* deleteSprite s

runSpriteAsState ::
    forall image r.
    (MemberBy "sprites" (State (Map Natural (SpriteState image))) r, State Natural <| r) =>
    '[] :!! LiftIns (Sprite image Natural) ': r ~> '[] :!! r
runSpriteAsState =
    raiseUnder >>> raiseUnder
        >>> runSprite @image
        >>> raiseUnder
        >>> runKVStoreAsState @Natural @(SpriteState image)
        >>> keySubsume @"sprites"
        >>> runFreshNaturalAsState

runSprite ::
    forall image r.
    (KVStore Natural (SpriteState image) <| r, Fresh Natural <| r) =>
    '[] :!! LiftIns (Sprite image Natural) ': r ~> '[] :!! r
runSprite =
    unkeyEff >>> interpret \case
        CreateSprite -> do
            n <- fresh
            writeKV n $ SpriteState @image Nothing Nothing
            pure n
        DeleteSprite n ->
            deleteKV @(SpriteState image) n
        SetSpriteImage n img ->
            lookupKV @_ @(SpriteState image) n >>= \case
                Just (SpriteState {position}) -> do
                    writeKV n $ SpriteState @image (Just img) position
                    pure $ Right ()
                Nothing -> pure $ Left NoSuchSprite
        UnsetSpriteImage n ->
            lookupKV @_ @(SpriteState image) n >>= \case
                Just (SpriteState {position}) -> do
                    writeKV n $ SpriteState @image Nothing position
                    pure $ Right ()
                Nothing -> pure $ Left NoSuchSprite
        GetSpriteImage n ->
            lookupKV n <&> \case
                Just (SpriteState {spriteImage}) -> Right spriteImage
                Nothing -> Left NoSuchSprite
        SetSpritePosition n pos ->
            lookupKV n >>= \case
                Just (SpriteState {spriteImage}) -> do
                    writeKV n $ SpriteState @image spriteImage pos
                    pure $ Right ()
                Nothing -> pure $ Left NoSuchSprite
        GetSpritePosition n ->
            lookupKV @_ @(SpriteState image) n <&> \case
                Just (SpriteState {position}) -> Right position
                Nothing -> Left NoSuchSprite

data SpriteState image = SpriteState {spriteImage :: Maybe image, position :: Maybe (V2 Double)}

drawSprites ::
    forall image sprite m.
    ( SendInsBy "sprites" (State (Map Natural (SpriteState image))) m
    , SendInsBy GraphicsKey (Graphics' image sprite) m
    , Monad m
    ) =>
    m ()
drawSprites =
    get'' @"sprites" >>= mapM_ \SpriteState {..} ->
        forM_ spriteImage \img -> forM_ position \pos -> drawImage img pos

eitherToFail :: (Show e, MonadFail m) => Either e a -> m a
eitherToFail = either (fail . show) pure

type Game image sprite font =
    LNop !! Sprite image sprite + Graphics image font + Fail + Yield () ExternalState $ ()

data ExternalState = ExternalState
    { isKeyPressed :: Keycode.Key -> Bool
    , deltaTime :: Double
    , elapsedTime :: Double
    }
