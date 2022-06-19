{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Godot.Extension.Core where

import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Coerce
import Data.IORef (IORef, newIORef, readIORef)
import Data.Vector qualified as V
import Foreign hiding (void)
import Foreign.C (withCString)
import Foreign.C.Types
import GHC.ForeignPtr (unsafeWithForeignPtr)
import GHC.IO (bracket, unsafePerformIO)
import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits
import Godot.Extension.Extension qualified as E
import Godot.Extension.Generate.Utils (withUtf8)
import Witch

interface :: IORef E.GdnativeInterface
interface = unsafePerformIO $ newIORef $ error "Attempted to access E.GdnativeInterface!"

class a <: b where
  upcast :: a -> b

instance {-# OVERLAPPABLE #-} x <: x where
  upcast = id

-- memAlloc' :: forall a. Storable a => IO (Ptr a)
-- memAlloc' =
--   coerce @(Ptr ()) @(Ptr a)
--     <$> E.memAlloc (fromIntegral $ sizeOf (undefined :: a))

-- memFree' :: forall a. Ptr a -> IO ()
-- memFree' ptr = E.memFree $ coerce @_ @(Ptr ()) ptr

-- withGdnativeCallError :: (E.GdnativeCallErrorPtr -> IO a) -> IO (Either E.GdnativeCallError a)
-- withGdnativeCallError f =
--   bracket
--     (memAlloc' @E.GdnativeCallError)
--     memFree'
--     \err -> do
--       ret <- f err
--       err' <- peek err
--       case err'.error' of
--         E.GdnativeCallOk -> pure $ Right ret
--         e -> pure $ Left err'

-- withGdnativeCallError' :: (E.GdnativeCallErrorPtr -> IO a) -> IO a
-- withGdnativeCallError' f = do
--   res <- withGdnativeCallError f
--   case res of
--     Left e -> error $ show e.error'
--     Right r -> pure r

class Construct f where
  constructNoFinal :: f

-- Vector2 is a builtin, so it'll be a pointered item.
newtype Vector2 = MkVector2 {unVector2 :: ForeignPtr ()}

-- instance Storable Vector2 where
--   sizeOf _ = 16 -- from builtin sizes defines
--   alignment _ = 16 -- also from builtin
--   peek = pure . MkVector2
--   poke _ _ = error "Don't poke Vector2"

-- instance HasField "x" Vector2 (IO Double) where
getX v = realToFrac @CDouble @Double <$> peekByteOff v.unVector2 0

getY v = realToFrac @CDouble @Double <$> peekByteOff v.unVector2 8

construct :: m Vector2
construct = do
  undefined

instance MonadIO m => Construct (m Vector2)

instance MonadIO m => Construct (Vector2 -> m Vector2)

instance MonadIO m => Construct (Double -> Double -> m Vector2)

-- newtype GdVariant = MkGdVariant { unGdVariant :: Ptr GdVariant }

-- withVariantArray :: V.Vector E.GdnativeVariantPtr -> (Ptr E.GdnativeVariantPtr -> IO a) -> IO a
-- withVariantArray vs f = allocaArray @() (length vs) \arr -> f (coerce arr)

{- |
Class that overloads getting Godot constants.

Sometimes you want to return a Haskell-native type, rather than a Godot type,
for convenience. Otherwise,
if you care about performance, you can simply perform your operations on the
Godot-native types and pass them to Godot functions freely.
-}
class Constant x s f where
  constant :: f

-- instance MonadIO m => Constant Vector2 "AXIS_X" (m Vector2) where
--   constant = undefined

instance Applicative m => From t (m t) where
  from = pure

instance MonadIO m => From Vector2 (m (Double, Double)) where
  from = undefined

instance (MonadIO m, From Vector2 (m t)) => Constant Vector2 "_AXIS_X" (m t) where
  {-# INLINE constant #-}
  constant = from @Vector2 undefined

instance Constant Vector2 "_AXIS_X" Int where
  {-# INLINE constant #-}
  constant = 0

{-# INLINE _AXIS_X #-}
_AXIS_X :: forall f. Constant Vector2 "_AXIS_X" f => f
_AXIS_X = constant @Vector2 @"_AXIS_X"

test :: IO Int
test = do
  (v :: Vector2) <- _AXIS_X
  undefined

-- let a = _AXIS_X
--  in a + 1

_ZERO :: Vector2
_ZERO = unsafePerformIO $ constructNoFinal
{-# NOINLINE _ZERO #-}

_ONE :: Vector2
_ONE = unsafePerformIO $ constructNoFinal (0.0 :: Double) (0.0 :: Double)
{-# NOINLINE _ONE #-}

vector2Eq :: Vector2 -> Vector2 -> IO Bool
vector2Eq = undefined

vector2Neg :: Vector2 -> IO ()
vector2Neg = undefined

instance From CFloat Double where
  from = undefined

-- angle :: MonadIO m => Vector2 -> m Double
-- angle v = liftIO $
--   withUtf8 "angle" \cstr -> do
--     r <- memAlloc' @CFloat
--     angleBind (coerce v.unVector2) nullPtr (coerce r) 0
--     from <$> peek r
--  where
--   angleBind = unsafePerformIO $
--     withCString "angle" \cstr -> do
--       E.mkGdnativePtrBuiltInMethod <$> E.variantGetPtrBuiltinMethod (from E.GdnativeVariantTypeVector2) cstr 0

-- angleTo :: MonadIO m => Vector2 -> Vector2 -> m Double angleTo v1 v = liftIO do
--   allocaBytes 8 \ptr -> do
--     E.objectMethodBindCall
--     undefined
--   -- v1Ptr <- coerce @_ @(Ptr E.GdnativeVariantPtr) <$> malloc @(Ptr ())
--   undefined
--  where
--   angleToBind = unsafePerformIO $
--     withUtf8 "Vector2" \clsStr ->
--       withUtf8 "angle_to" \mtdStr ->
--         E.classdbGetMethodBind clsStr mtdStr 0

-- setNormal :: MonadIO m => Vector2 -> Vector2 -> m ()
-- setNormal a v = liftIO $ unsafeWithForeignPtr (unVector2 v) (\v' -> coerce v' `_setNormalBind` coerce (unVector2 a))
-- -- setNormal a v = liftIO $ _setNormalBind (coerce $ unVector2 v) (coerce $ unVector2 a)
--  where
--   _setNormalBind = unsafePerformIO $
--     withUtf8 "normal" \str ->
--       E.mkGdnativePtrSetter <$> E.variantGetPtrSetter (from E.GdnativeVariantTypeVector2) str

-- When allocated for end-user use
--
-- r <- memAlloc 16
-- vecConstructor r nullPtr
-- MkVector2 <$> newForeignPtr memFree r
--
-- When used in a scoped loop
--
-- r <- mallocPlainForeignPtrBytes @() 16
-- unsafeWithForeignPtr r \r' -> vecConstructor r' nullPtr
-- pure $ MkVector2 r

-- FromInternal
--   returns unmanaged void pointer to thing

-- {-# INLINABLE _set #-}
-- _set :: (MonadIO m, Object :< a1, FromInternal a2 GdString) => a1 -> a2 -> Variant -> m Bool
-- _set a1 a2 a3 = liftIO $ do
--   let b1 = upcast a1
--   ba <- newPinnedByteArray 17
--   b3 <- fromInternal @_ @GdString a2
--   unsafeWithForeignPtr (unVariant a3) \b4 ->
--     writeByteArray ba 0 (unGdString b3)
--     writeByteArray b3 8 b4
--   objectMethodBindPtrcall _setBind (unObject b1) (coerce $ mutableByteArrayContents ba) (coerce $ mutableByteArrayContents b2 `plusPtr` 16)
--   from @CBool <$> peek (coerce $ mutableByteArrayContents ba `plusPtr` 16)
--  where
--   {-# NOINLINE _setBind -#}
--   _setBind = unsafePerformIO $
--     withUtf8 "Object" \str1 ->
--       withUtf8 "_set" \str2 ->
--         mkGdnativeMethodBindPtr <$> classDbGetMethodBind str1 str2 00000

newtype Object = MkObject {unObject :: ForeignPtr ()}

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