{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}

module FLib where

import Godot.Extension.Extension
import Foreign
import Witch
import Data.Coerce
import Foreign.C
import Control.Monad
import System.IO

printCString :: Ptr CChar -> IO ()
printCString ptr = do
  val <- castCCharToChar <$> peek ptr
  if (val /= '\0') 
    then putChar val >> printCString (ptr `plusPtr` 1)
    else putChar '\n'

godot_library_init :: GdnativeInterfacePtr -> GdnativeExtensionClassLibraryPtr -> GdnativeInitializationPtr -> IO ()
godot_library_init p_interface p_library r_initialization = do
  putStrLn "Initialized."
  interface <- peek p_interface

  -- !err <- malloc @GdnativeCallError
  !err <- coerce @_ @GdnativeCallErrorPtr <$> interface.memAlloc 12

  !arrPtr <- coerce @_ @GdnativeVariantPtr <$> interface.memAlloc 40

  interface.variantNewNil arrPtr
  interface.variantConstruct 
    (from GdnativeVariantTypeArray)
    arrPtr
    nullPtr
    0
    err

  -- interface.variantDestroy arrPtr

  -- !strPtr <- coerce @_ @GdnativeStringPtr <$> mallocBytes @() 8
  -- withCString "I love Godot strings.\0" \cstr -> do
  --   interface.stringNewWithLatin1Chars
  --     strPtr
  --     cstr

  print "Got here"
  undefined

foreign export ccall godot_library_init :: GdnativeInterfacePtr -> GdnativeExtensionClassLibraryPtr -> GdnativeInitializationPtr -> IO ()