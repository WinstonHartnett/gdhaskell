{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MagicHash #-}
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
import Control.Monad.Primitive (PrimMonad (PrimState), RealWorld, touch)
import Data.Coerce
import Data.IORef (IORef, newIORef, readIORef)
import Data.Kind (Type)
import Data.Primitive (ByteArray, MutableArray, MutableByteArray, mutableByteArrayContents, newByteArray, newPinnedByteArray, writeByteArray)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Vector qualified as V
import Foreign hiding (void)
import Foreign.C (withCString)
import Foreign.C.Types
import GHC.Exts (Int#, (+#))
import GHC.ForeignPtr (ForeignPtr (ForeignPtr), unsafeForeignPtrToPtr, unsafeWithForeignPtr)
import GHC.IO (bracket, unsafePerformIO)
import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits
import Godot.Extension.Extension (GdnativeInterface (objectMethodBindPtrcall))
import Godot.Extension.Extension qualified as E
import Godot.Extension.Generate.Utils (withUtf8)
import Witch

-- interface :: IORef E.GdnativeInterface
-- interface = unsafePerformIO $ newIORef $ error "Attempted to access E.GdnativeInterface!"

-- class a <: b where
--   upcast :: a -> b

-- instance {-# OVERLAPPABLE #-} x <: x where
--   upcast = id

-- -- memAlloc' :: forall a. Storable a => IO (Ptr a)
-- -- memAlloc' =
-- --   coerce @(Ptr ()) @(Ptr a)
-- --     <$> E.memAlloc (fromIntegral $ sizeOf (undefined :: a))

-- -- memFree' :: forall a. Ptr a -> IO ()
-- -- memFree' ptr = E.memFree $ coerce @_ @(Ptr ()) ptr

-- -- withGdnativeCallError :: (E.GdnativeCallErrorPtr -> IO a) -> IO (Either E.GdnativeCallError a)
-- -- withGdnativeCallError f =
-- --   bracket
-- --     (memAlloc' @E.GdnativeCallError)
-- --     memFree'
-- --     \err -> do
-- --       ret <- f err
-- --       err' <- peek err
-- --       case err'.error' of
-- --         E.GdnativeCallOk -> pure $ Right ret
-- --         e -> pure $ Left err'

-- -- withGdnativeCallError' :: (E.GdnativeCallErrorPtr -> IO a) -> IO a
-- -- withGdnativeCallError' f = do
-- --   res <- withGdnativeCallError f
-- --   case res of
-- --     Left e -> error $ show e.error'
-- --     Right r -> pure r

-- class Construct f where
--   construct :: f

-- -- Vector2 is a builtin, so it'll be a pointered item.
newtype Vector2 = MkVector2 {unVector2 :: ForeignPtr ()}

instance MonadIO m => Construct (m Vector2)

-- instance MonadIO m => Construct (m (InternalAlloc Vector2))

instance MonadIO m => Construct (Vector2 -> m Vector2)

instance MonadIO m => Construct (Double -> Double -> m Vector2)

-- {- |
-- Class that overloads getting Godot constants.

-- Sometimes you want to return a Haskell-native type, rather than a Godot type,
-- for convenience. Otherwise,
-- if you care about performance, you can simply perform your operations on the
-- Godot-native types and pass them to Godot functions freely.
-- -}
-- class Constant x s f where
--   constant :: f

newtype Object = MkObject {unObject :: ForeignPtr ()}

newtype GdString = MkGdString {unGdString :: ForeignPtr ()}

type Variant = E.GdnativeVariantPtr

{- |
Lifted version of 'unsafeWithForeignPtr'. Note that we're ignoring all
the usual caveats about stateful monads and exceptions, because
'unsafeWithForeignPtr' warns about them as well.
-}
unsafeWithForeignPtr' :: (PrimMonad m, MonadIO m) => ForeignPtr a -> (Ptr a -> m b) -> m b
unsafeWithForeignPtr' fo f = do
  r <- f (unsafeForeignPtrToPtr fo)
  touchForeignPtr' fo
  pure r
 where
  touchForeignPtr' (ForeignPtr _ r) = touch r

class Construct f where
  construct :: f

{- |
Class for converting marshalling data for use in Godot calls. There are two
cases for which instances are provided:

  1. The given data is backed by a 'ForeignPtr', which is provided to the
     interior function.
  2. The data is Haskell-native and requires conversion to a Variant before
     being passed to Godot. It will be allocated into a pinned 'ByteArray'.
-}
class MarshalTo m a b where
  marshal :: a -> (Ptr () -> m c) -> m c

-- -- The type requires marshalling into a temporary allocation.
-- instance {-# OVERLAPPABLE #-} (MonadIO m, From a (m (InternalAlloc b))) => Marshal m a b where
--   marshal a f = do
--     (MkInternalAlloc c) <- from @_ @(m (InternalAlloc b)) a
--     f c

-- 'a' is backed by a 'ForeignPtr'.
instance (PrimMonad m, MonadIO m, Coercible a (ForeignPtr ())) => MarshalTo m a a where
  marshal = unsafeWithForeignPtr' . coerce

-- 'a' requires conversion into 'b'.
instance {-# INCOHERENT #-} (MonadIO m, From a (m b), Coercible b (ForeignPtr ())) => MarshalTo m a b where
  marshal a =
    undefined

type family MarshalSize x :: Nat

type instance MarshalSize Vector2 = 16
type instance MarshalSize Object = 8

class MonadIO m => MarshalFrom m b c where
  marshalFrom :: (Ptr () -> m ()) -> m c

instance
  (MonadIO m, n ~ MarshalSize s, KnownNat n, From (ForeignPtr ()) s) =>
  MarshalFrom m s s
  where
  marshalFrom f = do
    m <- liftIO $ E.memAlloc $ fromIntegral $ natVal (Proxy @n)
    p <- liftIO $ newForeignPtr undefined m
    f m
    pure $ from p

-- | Temporary allocation used for construction.
instance {-# INCOHERENT #-} (MonadIO m, PrimMonad m, From Vector2 (m s)) => MarshalFrom m Vector2 s where
  marshalFrom f = do
    m <- liftIO $ mallocForeignPtrBytes 16
    unsafeWithForeignPtr' m (f . coerce)
    from $ MkVector2 m

instance From CDouble Double

instance MonadIO m => From (Ptr ()) (m Vector2) where
  from p = liftIO do
    x <- peekByteOff @CDouble p 0
    y <- peekByteOff @CDouble p 8
    construct (into @Double x) (into @Double y)

marshal4 ::
  forall m a1 b1 a2 b2 a3 b3 r s.
  ( MonadIO m
  , PrimMonad m
  , MarshalTo m a1 b1
  , MarshalTo m a2 b2
  , MarshalTo m a3 b3
  , MarshalFrom m r s
  ) =>
  E.GdnativePtrBuiltInMethod ->
  a1 ->
  a2 ->
  a3 ->
  m s
marshal4 bPtr a1 a2 a3 =
  marshal @_ @_ @b1 a1 \b1 ->
    marshal @_ @_ @b2 a2 \b2 ->
      marshal @_ @_ @b3 a3 \b3 -> do
        ba <- newPinnedByteArray 16
        writeByteArray ba 1 b2
        writeByteArray ba 2 b3
        marshalFrom @_ @r @s \p1 ->
          liftIO $
            bPtr
              (coerce b1)
              (coerce $ mutableByteArrayContents ba)
              (coerce p1)
              3

instance From (ForeignPtr ()) Vector2

instance From Vector2 (ForeignPtr ())

instance MonadIO m => From Vector2 (m (Double, Double)) where
  from = undefined

angleTo ::
  forall s m a1 a2.
  (MonadIO m, PrimMonad m, MarshalTo m a1 Vector2, MarshalTo m a2 Vector2, MarshalFrom m Vector2 s) =>
  a1 ->
  a2 ->
  a1 ->
  m s
angleTo a1 a2 a3 =
  marshal4
    @_
    @a1
    @Vector2
    @a2
    @Vector2
    @a1
    @Vector2
    @Vector2
    @s
    undefined
    a1
    a2
    a3

testF :: IO ()
testF = do
  v <- angleTo @(Double, Double) (undefined :: Vector2) (undefined :: Vector2) (undefined :: Vector2)
  undefined

-- Notes:
--
-- 1. You don't need to use a custom constructor for the BA marshal. Either you
-- already have a FPtr or you have something that can be converted into a BA Ptr.
-- But if the conversion func uses a constructor, what then?

-- End goal
-- 1. Take either a Haskell-native obj or a Godot-native obj and return a pointer
-- to something that corresponds to a Godot structure.
-- 2. Should require no type annotations by the user.
-- 3. Should have no performance penalty (with INLINE and INLINABLE as appropriate).
-- 4. Should reduce code duplication as far as possible.
--
-- TODO
--
-- - Consider making enums just ints so that you can AND/OR them C-style
--   or provide a separate non-overloaded constant corresponding to the enum val.
-- - Better way to handle marshalling (is inlining array operations a good idea?)
--   - Most fcns are very small so maybe not?

-- playerProcess :: Double -> Gd Player ()
-- playerProcess delta = do
--   base'      <- base
--   self'      <- self
--   animSprite <- getNode @"AnimatedSprite" base'
--   screenSize <- getSize @Vec2 =<< getViewportRect @Rect2 base'
--   inp        <- getSingleton @Input
--
--   velocity'  <- do
--     [left, right, up, down] <-
--       forM ["left", "right", "up", "down"] \a -> isActionPressed a False inp
--     let velocity = V2 (bVal left -1 + bVal right 1) (bVal up -1 + bVal down 1)
--     if length velocity > 0.0
--       then do
--         play Nothing animSprite
--         pure $ normalized velocity * self'.speed
--       else stop animSprite *> pure vPos
--
--   let p  = velocity' * delta
--       p' = V2 (clamp p.x 0.0 screenSize.x) (clamp p.y 0.0 screenSize.y)
--   setPosition base'
--
--   if velocity' /= _ZERO
--     then animSprite & do
--       setAnimation "right" 
--       setFlipV False
--       setFlipH (velocity'.x < 0)
--     else when (velocity'.y /= _ZERO) $ animSprite & do
--       setAnimation "up"
--       setFlipV (velocity'.y > 0)
--  where
--   bVal b v = if b then v else 0