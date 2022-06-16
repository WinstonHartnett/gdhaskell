{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Godot.Extension.Unsafe where

import GHC.IO (IO(IO))
import GHC.Exts (realWorld#)

-- | From 'bytestring'. It's unsafe. It's really unsafe.
-- This is just used to guarantee all the global interface accessors are inlined.
accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE accursedUnutterablePerformIO #-}