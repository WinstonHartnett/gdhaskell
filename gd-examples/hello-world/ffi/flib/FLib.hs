{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module FLib where

import Godot.Extension.Extension
import Foreign
import Witch
import Data.Coerce
import Foreign.C
import Control.Monad
import System.IO
import GHC.IO (unsafePerformIO)
import Data.IORef
import GHC.Records
import Data.String (IsString)
import qualified Data.Text as T

printCString :: Ptr CChar -> IO ()
printCString ptr = do
  val <- castCCharToChar <$> peek ptr
  if (val /= '\0') 
    then putChar val >> printCString (ptr `plusPtr` 1)
    else putChar '\n'

interface :: IORef GdnativeInterface
interface = unsafePerformIO $ newIORef (error "GdnativeInterface is unitialized!")
{-# NOINLINE interface #-}

getInterface :: IO GdnativeInterface
getInterface = readIORef interface

godot_library_init :: GdnativeInterfacePtr -> GdnativeExtensionClassLibraryPtr -> GdnativeInitializationPtr -> IO ()
godot_library_init p_interface p_library r_initialization = do
  putStrLn ">>>> Initialized."
  interface' <- peek p_interface
  writeIORef interface interface'
  
  !err <- coerce @_ @GdnativeCallErrorPtr <$> interface'.memAlloc 12
  !quatPtr <- coerce @_ @GdnativeVariantPtr <$> interface'.memAlloc 16

  interface'.variantNewNil quatPtr
  interface'.variantConstruct 
    (from GdnativeVariantTypeQuaternion)
    quatPtr
    nullPtr
    0
    err
  
  stringPtr <- coerce @_ @GdnativeVariantPtr <$> interface'.memAlloc 8
  
  withCString "x" \cstr -> 
    interface'.stringNewWithLatin1Chars (coerce stringPtr) cstr
  
  floatPtr <- coerce @_ @GdnativeVariantPtr <$> interface'.memAlloc 40
  interface'.variantConstruct 
    (from GdnativeVariantTypeFloat)
    floatPtr
    nullPtr
    0
    err

  boolPtr <- coerce @_ @(Ptr GdnativeBool) <$> interface'.memAlloc 1

  putStrLn ">>>> Got here."
  undefined

foreign export ccall godot_library_init :: GdnativeInterfacePtr -> GdnativeExtensionClassLibraryPtr -> GdnativeInitializationPtr -> IO ()