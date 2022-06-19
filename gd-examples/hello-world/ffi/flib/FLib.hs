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
import GHC.IO (unsafePerformIO, bracket)
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

instance From CDouble Double where
  from = realToFrac

getX :: GdnativeVariantPtr -> IO Double
getX v = do
  bracket
    (malloc @CDouble)
    free
    \r -> __getXBind (coerce v) (coerce r) >> fmap (from @CDouble @Double) (peek r)
 where
  __getXBind :: GdnativePtrGetter
  __getXBind = unsafePerformIO $
    withCString "x" \ptr -> do
      mkGdnativePtrGetter <$> variantGetPtrGetter (from GdnativeVariantTypeVector2) ptr
  {-# NOINLINE __getXBind #-}

godot_library_init :: GdnativeInterfacePtr -> GdnativeExtensionClassLibraryPtr -> GdnativeInitializationPtr -> IO ()
godot_library_init p_interface p_library r_initialization = do
  putStrLn ">>>> Initialized."
  initInterface p_interface
  
  !err <- coerce @_ @GdnativeCallErrorPtr <$> memAlloc 12
  !dPtr <- coerce @_ @GdnativeVariantPtr <$> memAlloc 8

  !dCstr <- mkGdnativePtrConstructor <$> variantGetPtrConstructor (from GdnativeVariantTypeDictionary) 0
  dCstr (coerce dPtr) nullPtr

  !vPtr <- coerce @_ @GdnativeVariantPtr <$> memAlloc 24
  !vCstr <- mkGdnativePtrConstructor <$> variantGetPtrConstructor (from GdnativeVariantTypeVector3) 0
  vCstr (coerce vPtr) nullPtr
  
  !vKey <- coerce @_ @GdnativeVariantPtr <$> memAlloc 50
  withCString "test_key" \str ->
    stringNewWithLatin1Chars (coerce vKey) str
  vKeyCstr <- variantConstruct (from GdnativeVariantTypeString) (coerce vKey) nullPtr 0 err
  -- !vKeyCstr <- mkGdnativePtrConstructor <$> variantGetPtrConstructor (from GdnativeVariantTypeString) 0
  -- vKeyCstr (coerce vKey) nullPtr

  print . (.error') =<< peek err
  !keyPtr <- dictionaryOperatorIndex (coerce dPtr) (coerce vKey)

  !retStr <- coerce @_ @GdnativeStringPtr <$> memAlloc 50

  withCString "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" \cstr -> do
    variantStringify (coerce keyPtr) retStr
    l <- stringToLatin1Chars retStr cstr 20
    print =<< peekCString cstr
    

  -- print =<< peek (coerce @_ @CString retStr)
  
  -- !vPtr <- coerce @_ @GdnativeVariantPtr <$> memAlloc 8

  -- vCstr (coerce vPtr) nullPtr
  -- flt5 <- coerce @_ @(Ptr CDouble) <$> memAlloc 8 
  -- poke flt5 (realToFrac $ (5.0 :: Double))
  -- setXBind (coerce vPtr) (coerce flt5)
  
  -- print =<< getX vPtr
  -- !vStr <- withCString "x" \cstr ->
  --   mkGdnativePtrSetter <$> variantGetPtrSetter (from GdnativeVariantTypeVector2) cstr

  -- vStr (coerce vPtr) (coerce flt5)

  -- print =<< peek flt5
  -- memFree (coerce flt5)
  
  -- print =<< peekByteOff @CFloat (coerce vPtr) 0

  putStrLn ">>>> Got here."
  undefined
 where
  setXBind :: GdnativePtrSetter
  setXBind = 
    unsafePerformIO do
      putStrLn ">>>> SET <<<<"
      withCString "x" \cstr ->
        mkGdnativePtrSetter <$> variantGetPtrSetter (from GdnativeVariantTypeVector2) cstr
  {-# NOINLINE setXBind #-}

foreign export ccall godot_library_init :: GdnativeInterfacePtr -> GdnativeExtensionClassLibraryPtr -> GdnativeInitializationPtr -> IO ()