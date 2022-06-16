{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Godot.Extension.Core where

import Control.Monad
import Data.IORef (IORef, newIORef, readIORef)
import Foreign hiding (void)
import Foreign.C.Types
-- import GHC.Base
import GHC.IO (unsafePerformIO, bracket)
import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits
import Godot.Extension.Extension qualified as E
import Witch
import Data.Coerce
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Lens hiding (from)
import qualified Data.Vector as V
import Foreign.C (withCString)

interface :: IORef E.GdnativeInterface
interface = unsafePerformIO $ newIORef $ error "Attempted to access E.GdnativeInterface!"

class a <: b where
  upcast :: a -> b

instance {-# OVERLAPPABLE #-} x <: x where
  upcast = id

memAlloc' :: forall a. Storable a => IO (Ptr a)
memAlloc' =
  coerce @(Ptr ()) @(Ptr a)
    <$> E.memAlloc (fromIntegral $ sizeOf (undefined :: a))

memFree' :: forall a. Ptr a -> IO ()
memFree' ptr = E.memFree $ coerce @_ @(Ptr ()) ptr

withGdnativeCallError :: (E.GdnativeCallErrorPtr -> IO a) -> IO (Either E.GdnativeCallError a)
withGdnativeCallError f =
  bracket
    (memAlloc' @E.GdnativeCallError)
    memFree'
    \err -> do
      ret <- f err
      err' <- peek err
      case err'.error' of
        E.GdnativeCallOk -> pure $ Right ret
        e -> pure $ Left err'

withGdnativeCallError' :: (E.GdnativeCallErrorPtr -> IO a) -> IO a
withGdnativeCallError' f = do
  res <- withGdnativeCallError f
  case res of
    Left e -> error $ show e.error'
    Right r -> pure r

class Construct f where
  construct :: f

-- Vector2 is a builtin, so it'll be a pointered item.
newtype Vector2 = MkVector2 {unVector2 :: Ptr Vector2}

instance Storable Vector2 where
  sizeOf _ = 16 -- from builtin sizes defines
  alignment _ = 16 -- also from builtin
  peek = pure . MkVector2
  poke _ _ = error "Don't poke Vector2"

-- instance HasField "x" Vector2 (IO Double) where
getX v = realToFrac @CDouble @Double <$> peekByteOff v.unVector2 0

getY v = realToFrac @CDouble @Double <$> peekByteOff v.unVector2 8

instance MonadIO m => Construct (m Vector2)

instance MonadIO m => Construct (Vector2 -> m Vector2)

instance MonadIO m => Construct (Double -> Double -> m Vector2)

-- newtype GdVariant = MkGdVariant { unGdVariant :: Ptr GdVariant }

withVariantArray :: V.Vector E.GdnativeVariantPtr -> (Ptr E.GdnativeVariantPtr -> IO a) -> IO a
withVariantArray vs f = allocaArray @() (length vs) \arr -> f (coerce arr)

_AXIS_X :: Int
_AXIS_X = 0

_ZERO :: Vector2
_ZERO = unsafePerformIO $ construct
{-# NOINLINE _ZERO #-}

_ONE :: Vector2
_ONE = unsafePerformIO $ construct (0.0 :: Double) (0.0 :: Double)
{-# NOINLINE _ONE #-}

vector2Eq :: Vector2 -> Vector2 -> IO Bool
vector2Eq = undefined

vector2Neg :: Vector2 -> IO ()
vector2Neg = undefined

angle :: Vector2 -> IO Double
angle v =
  withCString "angle" \cstr -> do
    f <- E.mkGdnativePtrBuiltInMethod <$> E.variantGetPtrBuiltinMethod (from E.GdnativeVariantTypeVector2) cstr 0
    dPtr <- memAlloc' @CFloat
    f (coerce v.unVector2) nullPtr (coerce dPtr) 0
    realToFrac <$> peek dPtr
 where
  angleBind = unsafePerformIO do
    withCString "angle" \cstr -> do
      E.mkGdnativePtrBuiltInMethod <$> E.variantGetPtrBuiltinMethod (from E.GdnativeVariantTypeVector2) cstr 0

-- playerProcess :: Float -> Godot Player ()
-- playerProcess delta = do
--   animSprite <- getNode @"AnimatedSprite" self
--   screenSize <- self >>= getViewportRect <&> view #y
--   [left, right, up, down] <- do
--     Just inp <- getSingleton @Input
--     mapM (isActionPressed inp) ["ui_left", "ui_right", "ui_up", "ui_down"]
--   let velocity =
--         let bVal b v = if b then v else 0
--          in V2 (bVal left -1 + bVal right 1) (bVal up -1 + bVal down 1)
--   if velocity /= zero
--     then do
--       speed <- self.speed
--       pos   <- from =<< self.position
--       let velocity' = normalize velocity * speed
--           pos'      = pos + velocity' * delta
--           clamp v a b = max a (min b v)
--       setPosition <$> self <*> pure newPos
--       play (Just "") Nothing animSprite
--       if velocity'.x /= 0
--         then do
--           setAnimation "right" animSprite
--           setFlipV (velocity'.y > 0) animSprite
--           setFlipH (velocity'.x < 0) animSprite
--           play "animName" True animSprite
--         else when (velocity'.y /= 0) do
--           setAnimation "up" animSprite
--           setFlipV (velocity'.y > 0) animSprite
--    else stop animSprite