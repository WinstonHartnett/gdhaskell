{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Godot.Extension.Extension where

import Witch
import Foreign.C
import Foreign.Storable
import Foreign.Ptr

#include <godot/gdnative_interface.h>

instance From CInt Int where
  from = fromIntegral

-- Variant Types
{#enum GDNativeVariantType as GdnativeVariantType {underscoreToCase}
  deriving (Show, Eq, Ord, Bounded) #}

{#enum GDNativeVariantOperator as GdnativeVariantOperator {underscoreToCase}
  deriving (Show, Eq, Ord, Bounded) #}

{#pointer GDNativeVariantPtr as GdnativeVariantPtr newtype #}
deriving newtype instance Show GdnativeVariantPtr
{#pointer GDNativeStringNamePtr as GdnativeStringNamePtr newtype #}
deriving newtype instance Show GdnativeStringNamePtr
{#pointer GDNativeStringPtr as GdnativeStringPtr newtype #}
deriving newtype instance Show GdnativeStringPtr
{#pointer GDNativeObjectPtr as GdnativeObjectPtr newtype #}
deriving newtype instance Show GdnativeObjectPtr
{#pointer GDNativeTypePtr as GdnativeTypePtr newtype #}
deriving newtype instance Show GdnativeTypePtr
{#pointer GDNativeExtensionPtr as GdnativeExtensionPtr newtype #}
deriving newtype instance Show GdnativeExtensionPtr
{#pointer GDNativeMethodBindPtr as GdnativeMethodBindPtr newtype #}
deriving newtype instance Show GdnativeMethodBindPtr
type GdnativeInt = {#type GDNativeInt #}
type GdnativeBool = {#type GDNativeBool #}
type GdObjectInstanceId = {#type GDObjectInstanceID #}

-- Variant Data IO
{#enum GDNativeCallErrorType as GdNativeCallErrorType {underscoreToCase}
  deriving (Show, Eq, Ord, Bounded) #}

data GdnativeCallError =
    GdnativeCallErrorOk
  | GdnativeCallErrorErrorInvalidMethod
  | GdnativeCallErrorErrorInvalidArgument
      { argumentVariantType :: GdnativeVariantType
      , expectedVariantType :: GdnativeVariantType
      }
  | GdnativeCallErrorErrorTooManyArguments
      { argumentNumArgs :: Int
      , expectedNumArgs :: Int
      }
  | GdnativeCallErrorErrorTooFewArguments
      { argumentNumArgs :: Int
      , expectedNumArgs :: Int
      }
  | GdnativeCallErrorErrorInstanceIsNull
  deriving (Show, Eq, Ord)

instance Storable GdnativeCallError where
  sizeOf _ = {#sizeof GDNativeCallError #}
  alignment _ = {#alignof GDNativeCallError #}
  peek ptr = do
    err <- toEnum . from <$> {#get GDNativeCallError->error #} ptr
    arg <- from <$> {#get GDNativeCallError->argument #} ptr
    exp <- from <$> {#get GDNativeCallError->expected #} ptr
    pure $ case err of
      GdnativeCallOk -> GdnativeCallErrorOk
      GdnativeCallErrorInvalidMethod -> GdnativeCallErrorErrorInvalidMethod
      GdnativeCallErrorInvalidArgument -> 
        GdnativeCallErrorErrorInvalidArgument (toEnum arg) (toEnum exp)
      GdnativeCallErrorTooManyArguments ->
        GdnativeCallErrorErrorTooManyArguments arg exp
      GdnativeCallErrorTooFewArguments ->
        GdnativeCallErrorErrorTooFewArguments arg exp
      GdnativeCallErrorInstanceIsNull -> GdnativeCallErrorErrorInstanceIsNull
  poke _ _ = error "don't poke GdnativeCallError"

{#pointer *GDNativeCallError as GdnativeCallErrorPtr -> GdnativeCallError #}

type GdnativeVariantFromTypeConstructorFunc 
  =  GdnativeVariantPtr 
  -> GdnativeTypePtr 
  -> IO ()
foreign import ccall "dynamic" mkVariantFromTypeConstructorFunc
  :: FunPtr GdnativeVariantFromTypeConstructorFunc
  -> GdnativeVariantFromTypeConstructorFunc
type GdnativeTypeFromVariantConstructorFunc
  =  GdnativeTypePtr
  -> GdnativeVariantPtr
  -> IO ()
foreign import ccall "dynamic" mkTypeFromVariantConstructorFunc
  :: FunPtr GdnativeTypeFromVariantConstructorFunc
  -> GdnativeTypeFromVariantConstructorFunc
type GdnativePtrOperatorEvaluator = GdnativeTypePtr -> GdnativeTypePtr -> GdnativeTypePtr -> IO ()
foreign import ccall "dynamic" mkPtrOperatorEvaluator
  :: FunPtr GdnativePtrOperatorEvaluator
  -> GdnativePtrOperatorEvaluator
type GdnativePtrBuiltInMethod = GdnativeTypePtr -> Ptr GdnativeTypePtr -> GdnativeTypePtr -> CInt -> IO ()
foreign import ccall "dynamic" mkPtrBuiltInMethod
  :: FunPtr GdnativePtrBuiltInMethod
  -> GdnativePtrBuiltInMethod
type GdnativeGdnativePtrConstructor = GdnativeTypePtr -> Ptr GdnativeTypePtr -> IO ()
foreign import ccall "dynamic" mkGdnativePtrConstructor
  :: FunPtr GdnativeGdnativePtrConstructor
  -> GdnativeGdnativePtrConstructor
type GdnativePtrDestructor = GdnativeTypePtr -> IO ()
foreign import ccall "dynamic" mkPtrDestructor
  :: FunPtr GdnativePtrDestructor
  -> GdnativePtrDestructor
type GdnativePtrSetter = GdnativeTypePtr -> GdnativeTypePtr -> IO ()
foreign import ccall "dynamic" mkPtrSetter
  :: FunPtr GdnativePtrSetter
  -> GdnativePtrSetter
type GdnativePtrGetter = GdnativeTypePtr -> GdnativeTypePtr -> IO ()
foreign import ccall "dynamic" mkPtrGetter
  :: FunPtr GdnativePtrGetter
  -> GdnativePtrGetter
type GdnativePtrIndexedSetter = GdnativeTypePtr -> GdnativeInt -> GdnativeTypePtr -> IO ()
foreign import ccall "dynamic" mkPtrIndexedSetter
  :: FunPtr GdnativePtrIndexedSetter
  -> GdnativePtrIndexedSetter
type GdnativePtrIndexedGetter = GdnativeTypePtr -> GdnativeInt -> GdnativeTypePtr -> IO ()
foreign import ccall "dynamic" mkPtrIndexedGetter
  :: FunPtr GdnativePtrIndexedGetter
  -> GdnativePtrIndexedGetter
type GdnativePtrKeyedSetter = GdnativeTypePtr -> GdnativeTypePtr -> GdnativeTypePtr -> IO ()
foreign import ccall "dynamic" mkPtrKeyedSetter
  :: FunPtr GdnativePtrKeyedSetter
  -> GdnativePtrKeyedSetter
type GdnativePtrKeyedGetter = GdnativeTypePtr -> GdnativeTypePtr -> GdnativeTypePtr -> IO ()
foreign import ccall "dynamic" mkPtrKeyedGetter
  :: FunPtr GdnativePtrKeyedGetter
  -> GdnativePtrKeyedGetter
type GdnativePtrKeyedChecker = GdnativeVariantPtr -> GdnativeVariantPtr -> IO ()
foreign import ccall "dynamic" mkPtrKeyedChecker
  :: FunPtr GdnativePtrKeyedChecker
  -> GdnativePtrKeyedChecker
type GdnativePtrUtilityFunction = GdnativeTypePtr -> Ptr GdnativeTypePtr -> CInt -> IO ()
foreign import ccall "dynamic" mkPtrUtilityFunction
  :: FunPtr GdnativePtrUtilityFunction
  -> GdnativePtrUtilityFunction

type GdnativeClassConstructor = {#type GDNativeObjectPtr #}

type GdnativeInstanceBindingCreateCallback = FunPtr (Ptr () -> Ptr () -> IO (Ptr ()))
foreign import ccall "dynamic" mkInstanceBindingCreateCallback
  :: FunPtr GdnativeInstanceBindingCreateCallback
  -> GdnativeInstanceBindingCreateCallback
type GdnativeInstanceBindingFreeCallback = Ptr () -> Ptr () -> Ptr () -> IO ()
foreign import ccall "dynamic" mkInstanceBindingFreeCallback
  :: FunPtr GdnativeInstanceBindingFreeCallback
  -> GdnativeInstanceBindingFreeCallback
type GdnativeInstanceBindingReferenceCallback = Ptr () -> Ptr () -> GdnativeBool -> IO CUChar
foreign import ccall "dynamic" mkInstanceBindingReferenceCallback
  :: FunPtr GdnativeInstanceBindingReferenceCallback
  -> GdnativeInstanceBindingReferenceCallback

data GdnativeInstanceBindingCallbacks = GdnativeInstanceBindingCallbacks
  { createCallback :: GdnativeInstanceBindingCreateCallback
  , freeCallback   :: GdnativeInstanceBindingFreeCallback
  , referenceCallback :: GdnativeInstanceBindingReferenceCallback
  }

instance Storable GdnativeInstanceBindingCallbacks where
  sizeOf _ = {#sizeof GDNativeInstanceBindingCallbacks #}
  alignment _ = {#alignof GDNativeInstanceBindingCallbacks #}
  peek ptr =
    GdnativeInstanceBindingCallbacks
      <$> {#get GDNativeInstanceBindingCallbacks->create_callback #} ptr
      <*> (mkInstanceBindingFreeCallback
            <$> {#get GDNativeInstanceBindingCallbacks->free_callback #} ptr)
      <*> (mkInstanceBindingReferenceCallback 
            <$> {#get GDNativeInstanceBindingCallbacks->reference_callback #} ptr)
  poke _ = error "don't poke GdnativeInstanceBindingCallbacks"

{#pointer *GDNativeInstanceBindingCallbacks as 
  GdnativeInstanceBindingCallbacksPtr -> GdnativeInstanceBindingCallbacks #}

type GdextensionClassInstancePtr = {#type GDExtensionClassInstancePtr #}

type GdnativeExtensionClassSet
  = GdextensionClassInstancePtr
  -> GdnativeStringNamePtr
  -> GdnativeVariantPtr
  -> IO ()
foreign import ccall "dynamic" mkGdnativeExtensionClassSet
  :: FunPtr GdnativeExtensionClassSet
  -> GdnativeExtensionClassSet
type GdnativeExtensionClassGet 
  = GdextensionClassInstancePtr
  -> GdnativeStringNamePtr
  -> GdnativeVariantPtr
  -> IO ()
foreign import ccall "dynamic" mkGdnativeExtensionClassGet
  :: FunPtr GdnativeExtensionClassGet
  -> GdnativeExtensionClassGet

data GdnativePropertyInfo = PropertyInfo
  { type' :: CUInt
  , name  :: CString
  , className :: CString
  , hint :: CUInt
  , hintString :: CString
  , usage :: CUInt
  }

{#pointer *GDNativePropertyInfo as GdnativePropertyInfoPtr -> GdnativePropertyInfo #}

type GdnativeExtensionClassGetPropertyList = FunPtr (GdextensionClassInstancePtr -> Ptr CUInt -> IO GdnativePropertyInfoPtr)
foreign import ccall "dynamic" mkExtensionClassGetPropertyList
  :: FunPtr GdnativeExtensionClassGetPropertyList
  -> GdnativeExtensionClassGetPropertyList
type GdnativeExtensionClassFreePropertyList = GdextensionClassInstancePtr -> GdnativePropertyInfoPtr -> IO ()
foreign import ccall "dynamic" mkExtensionClassFreePropertyList
  :: FunPtr GdnativeExtensionClassFreePropertyList
  -> GdnativeExtensionClassFreePropertyList
type GdnativeExtensionClassNotification = GdextensionClassInstancePtr -> CInt -> IO ()
foreign import ccall "dynamic" mkExtensionClassNotification
  :: FunPtr GdnativeExtensionClassNotification
  -> GdnativeExtensionClassNotification
type GdnativeExtensionClassToString = GdextensionClassInstancePtr -> IO CString
foreign import ccall "dynamic" mkExtensionClassClassToString
  :: FunPtr GdnativeExtensionClassToString
  -> GdnativeExtensionClassToString
type GdnativeExtensionClassReference = GdextensionClassInstancePtr -> IO ()
foreign import ccall "dynamic" mkExtensionClassClassReference
  :: FunPtr GdnativeExtensionClassReference
  -> GdnativeExtensionClassReference
type GdnativeExtensionClassUnreference = GdextensionClassInstancePtr -> IO ()
foreign import ccall "dynamic" mkExtensionClassClassUnreference
  :: FunPtr GdnativeExtensionClassUnreference
  -> GdnativeExtensionClassUnreference
type GdnativeExtensionClassCallVirtual = GdextensionClassInstancePtr -> Ptr GdnativeTypePtr -> GdnativeTypePtr -> IO ()
foreign import ccall "dynamic" mkExtensionClassCallVirtual
  :: FunPtr GdnativeExtensionClassCallVirtual
  -> GdnativeExtensionClassCallVirtual
type GdnativeExtensionClassCreateInstance = Ptr () -> IO GdnativeObjectPtr
foreign import ccall "dynamic" mkExtensionClassCreateInstance
  :: FunPtr GdnativeExtensionClassCreateInstance
  -> GdnativeExtensionClassCreateInstance
type GdnativeExtensionClassFreeInstance = Ptr () -> GdextensionClassInstancePtr -> IO ()
foreign import ccall "dynamic" mkExtensionClassFreeInstance
  :: FunPtr GdnativeExtensionClassFreeInstance
  -> GdnativeExtensionClassFreeInstance
type GdnativeExtensionClassObjectInstance = GdextensionClassInstancePtr -> GdnativeObjectPtr -> IO ()
foreign import ccall "dynamic" mkExtensionClassObjectInstance
  :: FunPtr GdnativeExtensionClassObjectInstance
  -> GdnativeExtensionClassObjectInstance
type GdnativeExtensionClassGetVirtual = Ptr () -> CString -> IO (FunPtr GdnativeExtensionClassCallVirtual)
foreign import ccall "dynamic" mkExtensionClassGetVirtual
  :: FunPtr GdnativeExtensionClassGetVirtual
  -> GdnativeExtensionClassGetVirtual

data GdnativeExtensionClassCreationInfo = GdnativeExtensionClassCreationInfo
  { setFunc :: GdnativeExtensionClassSet
  , getFunc :: GdnativeExtensionClassGet
  , getPropertyListFunc :: GdnativeExtensionClassGetPropertyList
  , freePropertyListFunc :: GdnativeExtensionClassFreePropertyList
  , notificationFunc :: GdnativeExtensionClassNotification
  , toStringFunc :: GdnativeExtensionClassToString
  , referenceFunc :: GdnativeExtensionClassReference
  , unreferenceFunc :: GdnativeExtensionClassUnreference
  , createInstanceFunc :: GdnativeExtensionClassCreateInstance
  , freeInstanceFunc :: GdnativeExtensionClassFreeInstance
  , getVirtualFunc :: GdnativeExtensionClassGetVirtual
  , classUserdata :: Ptr ()
  }

{#pointer *GDNativeExtensionClassCreationInfo as GdnativeExtensionClassCreationInfoPtr -> GdnativeExtensionClassCreationInfo #}

type GdnativeExtensionClassLibraryPtr = {#type GDNativeExtensionClassLibraryPtr #}

{#enum GDNativeExtensionClassMethodFlags as GdnativeExtensionClassMethodFlags {underscoreToCase}
  deriving (Show, Eq, Ord, Bounded) #}
{#enum GDNativeExtensionClassMethodArgumentMetadata as GdnativeExtensionClassMethodArgumentMetadata {underscoreToCase}
  deriving (Show, Eq, Ord, Bounded) #}

type GdnativeExtensionClassMethodCall = Ptr () -> GdextensionClassInstancePtr -> GdnativeVariantPtr -> GdnativeInt -> GdnativeVariantPtr -> GdnativeCallError -> IO ()

-- {#pointer GDNativeVariantFromTypeConstructorFunc as GdnativeVariantFromTypeConstructorFunc newtype #}
-- deriving newtype instance Show GdnativeVariantFromTypeConstructorFunc
-- {#pointer GDNativeTypeFromVariantConstructorFunc as GdnativeTypeFromVariantConstructorFunc newtype #}
-- deriving newtype instance Show GdnativeTypeFromVariantConstructorFunc
-- {#pointer GDNativePtrOperatorEvaluator as GdnativePtrOperatorEvaluator newtype #}
-- deriving newtype instance Show GdnativePtrOperatorEvaluator
-- {#pointer GDNativePtrBuiltInMethod as GdnativePtrBuiltInMethod newtype #}
-- deriving newtype instance Show GdnativePtrBuiltInMethod
-- {#pointer GDNativePtrConstructor as GdnativePtrConstructor newtype #}
-- deriving newtype instance Show GdnativePtrConstructor
-- {#pointer GDNativeV as GdnativeV newtype #}
-- deriving newtype instance Show GdnativeV
-- {#pointer GDNativeV as GdnativeV newtype #}
-- deriving newtype instance Show GdnativeV
-- {#pointer GDNativeV as GdnativeV newtype #}
-- deriving newtype instance Show GdnativeV
-- {#pointer GDNativeV as GdnativeV newtype #}
-- deriving newtype instance Show GdnativeV
-- {#pointer GDNativeV as GdnativeV newtype #}
-- deriving newtype instance Show GdnativeV
-- {#pointer GDNativeV as GdnativeV newtype #}
-- deriving newtype instance Show GdnativeV
-- {#pointer GDNativeV as GdnativeV newtype #}
-- deriving newtype instance Show GdnativeV
-- {#pointer GDNativeV as GdnativeV newtype #}
-- deriving newtype instance Show GdnativeV
-- {#pointer GDNativeV as GdnativeV newtype #}
-- deriving newtype instance Show GdnativeV
-- {#pointer GDNativeV as GdnativeV newtype #}
-- deriving newtype instance Show GdnativeV
