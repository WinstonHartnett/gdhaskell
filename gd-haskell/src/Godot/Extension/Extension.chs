{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Godot.Extension.Extension where

import Witch
import Foreign.C
import Foreign.Storable
import Foreign.Ptr

#include <godot/gdnative_interface.h>

instance From CInt Int where
  from = fromIntegral

-- | Enums aren't marshallable via foreign imports, so this is a placeholder.
newtype CEnum e = MkCEnum CInt

instance Enum e => From (CEnum e) e where
  from (MkCEnum i) = toEnum $ from i

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

data GdnativePropertyInfo = GdnativePropertyInfo
  { type' :: CUInt
  , name  :: CString
  , className :: CString
  , hint :: CUInt
  , hintString :: CString
  , usage :: CUInt
  }

instance Storable GdnativePropertyInfo where
  sizeOf _ = {#sizeof GDNativePropertyInfo #}
  alignment _ = {#alignof GDNativePropertyInfo #}
  peek ptr =
    GdnativePropertyInfo
      <$> {#get GDNativePropertyInfo->type #} ptr
      <*> {#get GDNativePropertyInfo->name #} ptr
      <*> {#get GDNativePropertyInfo->class_name #} ptr
      <*> {#get GDNativePropertyInfo->hint #} ptr
      <*> {#get GDNativePropertyInfo->hint_string #} ptr
      <*> {#get GDNativePropertyInfo->usage #} ptr

{#pointer *GDNativePropertyInfo as GdnativePropertyInfoPtr -> GdnativePropertyInfo #}

type GdnativeExtensionClassGetPropertyList = FunPtr (GdextensionClassInstancePtr -> Ptr CUInt -> IO GdnativePropertyInfoPtr)
foreign import ccall "dynamic" mkGdnativeExtensionClassGetPropertyList
  :: FunPtr GdnativeExtensionClassGetPropertyList
  -> GdnativeExtensionClassGetPropertyList
type GdnativeExtensionClassFreePropertyList = GdextensionClassInstancePtr -> GdnativePropertyInfoPtr -> IO ()
foreign import ccall "dynamic" mkGdnativeExtensionClassFreePropertyList
  :: FunPtr GdnativeExtensionClassFreePropertyList
  -> GdnativeExtensionClassFreePropertyList
type GdnativeExtensionClassNotification = GdextensionClassInstancePtr -> CInt -> IO ()
foreign import ccall "dynamic" mkGdnativeExtensionClassNotification
  :: FunPtr GdnativeExtensionClassNotification
  -> GdnativeExtensionClassNotification
type GdnativeExtensionClassToString = GdextensionClassInstancePtr -> IO CString
foreign import ccall "dynamic" mkGdnativeExtensionClassClassToString
  :: FunPtr GdnativeExtensionClassToString
  -> GdnativeExtensionClassToString
type GdnativeExtensionClassReference = GdextensionClassInstancePtr -> IO ()
foreign import ccall "dynamic" mkGdnativeExtensionClassClassReference
  :: FunPtr GdnativeExtensionClassReference
  -> GdnativeExtensionClassReference
type GdnativeExtensionClassUnreference = GdextensionClassInstancePtr -> IO ()
foreign import ccall "dynamic" mkGdnativeExtensionClassClassUnreference
  :: FunPtr GdnativeExtensionClassUnreference
  -> GdnativeExtensionClassUnreference
type GdnativeExtensionClassCallVirtual = GdextensionClassInstancePtr -> Ptr GdnativeTypePtr -> GdnativeTypePtr -> IO ()
foreign import ccall "dynamic" mkGdnativeExtensionClassCallVirtual
  :: FunPtr GdnativeExtensionClassCallVirtual
  -> GdnativeExtensionClassCallVirtual
type GdnativeExtensionClassCreateInstance = Ptr () -> IO GdnativeObjectPtr
foreign import ccall "dynamic" mkGdnativeExtensionClassCreateInstance
  :: FunPtr GdnativeExtensionClassCreateInstance
  -> GdnativeExtensionClassCreateInstance
type GdnativeExtensionClassFreeInstance = Ptr () -> GdextensionClassInstancePtr -> IO ()
foreign import ccall "dynamic" mkGdnativeExtensionClassFreeInstance
  :: FunPtr GdnativeExtensionClassFreeInstance
  -> GdnativeExtensionClassFreeInstance
type GdnativeExtensionClassObjectInstance = GdextensionClassInstancePtr -> GdnativeObjectPtr -> IO ()
foreign import ccall "dynamic" mkGdnativeExtensionClassObjectInstance
  :: FunPtr GdnativeExtensionClassObjectInstance
  -> GdnativeExtensionClassObjectInstance
type GdnativeExtensionClassGetVirtual = Ptr () -> CString -> IO (FunPtr GdnativeExtensionClassCallVirtual)
foreign import ccall "dynamic" mkGdnativeExtensionClassGetVirtual
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

type GdnativeExtensionClassMethodCall = Ptr () -> GdextensionClassInstancePtr -> GdnativeVariantPtr -> GdnativeInt -> GdnativeVariantPtr -> Ptr GdnativeCallError -> IO ()
foreign import ccall "dynamic" mkGdnativeExtensionClassMethodCall
  :: FunPtr GdnativeExtensionClassMethodCall
  -> GdnativeExtensionClassMethodCall
type GdnativeExtensionClassMethodPtrCall = Ptr () -> GdextensionClassInstancePtr -> GdnativeTypePtr -> GdnativeTypePtr -> IO ()
foreign import ccall "dynamic" mkGdnativeExtensionClassMethodPtrCall
  :: FunPtr GdnativeExtensionClassMethodPtrCall
  -> GdnativeExtensionClassMethodPtrCall

type GdnativeExtensionClassMethodGetArgumentType = Ptr () -> CUInt -> IO (CEnum GdnativeVariantType)
foreign import ccall "dynamic" mkGdnativeExtensionClassMethodGetArgumentType
  :: FunPtr GdnativeExtensionClassMethodGetArgumentType
  -> GdnativeExtensionClassMethodGetArgumentType
type GdnativeExtensionClassMethodGetArgumentInfo = Ptr () -> CInt -> GdnativePropertyInfoPtr -> IO ()
foreign import ccall "dynamic" mkGdnativeExtensionClassMethodGetArgumentInfo
  :: FunPtr GdnativeExtensionClassMethodGetArgumentInfo
  -> GdnativeExtensionClassMethodGetArgumentInfo
type GdnativeExtensionClassMethodGetArgumentMetadata = Ptr () -> CInt -> IO (CEnum GdnativeExtensionClassMethodArgumentMetadata)
foreign import ccall "dynamic" mkGdnativeExtensionClassMethodGetArgumentMetadata
  :: FunPtr GdnativeExtensionClassMethodGetArgumentMetadata
  -> GdnativeExtensionClassMethodGetArgumentMetadata

data GdnativeExtensionClassMethodInfo = GdnativeExtensionClassMethodInfo
  { name :: CString
  , methodUserdata :: Ptr ()
  , callFunc :: GdnativeExtensionClassMethodCall
  , ptrcallFunc :: GdnativeExtensionClassMethodPtrCall
  , methodFlags :: CUInt
  , argumentCountr :: CUInt
  , hasReturnValue :: GdnativeBool
  , getArgumentTypeFunc :: GdnativeExtensionClassMethodGetArgumentType
  , getArgumentInfoFunc :: GdnativeExtensionClassMethodGetArgumentInfo
  , getArgumentMetadataFunc :: GdnativeExtensionClassMethodGetArgumentMetadata
  , defaultArgumentCount :: CUInt
  , defaultArguments :: Ptr GdnativeVariantPtr
  }

data GdnativeInterface = GdnativeInterface
  { versionMajor  :: CUInt
  , versionMinor  :: CUInt
  , versionPatch  :: CUInt
  , versionString :: CString

  , memAlloc   :: FunPtr (CSize -> IO (Ptr ()))
  , memRealloc :: FunPtr (Ptr () -> CSize -> IO (Ptr ()))
  , memFree    :: FunPtr (Ptr () -> IO ())

  , printError       :: FunPtr (CString -> CString -> CString -> CInt -> IO ())
  , printWarning     :: FunPtr (CString -> CString -> CString -> CInt -> IO ())
  , printScriptError :: FunPtr (CString -> CString -> CString -> CInt -> IO ())
  
  , variantNewCopy :: FunPtr (GdnativeVariantPtr -> GdnativeVariantPtr -> IO ())
  , variantNewNil  :: FunPtr (GdnativeVariantPtr -> IO ())
  , variantDestroy :: FunPtr (GdnativeVariantPtr -> IO ())
  
  , variantCall       :: FunPtr (GdnativeVariantPtr -> GdnativeStringNamePtr -> Ptr GdnativeVariantPtr -> GdnativeInt -> GdnativeVariantPtr -> Ptr GdnativeCallError -> IO ())
  , variantCallStatic :: FunPtr (GdnativeVariantPtr -> GdnativeStringNamePtr -> Ptr GdnativeVariantPtr -> GdnativeInt -> GdnativeVariantPtr -> Ptr GdnativeCallError -> IO ())
  , variantEvaluate   :: FunPtr (GdnativeVariantOperator -> GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr GdnativeBool -> IO ())
  , variantSet        :: FunPtr (GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr GdnativeBool -> IO ())
  , variantSetNamed   :: FunPtr (GdnativeVariantPtr -> GdnativeStringNamePtr -> GdnativeVariantPtr -> Ptr GdnativeBool -> IO ())
  , variantSetKeyed   :: FunPtr (GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr GdnativeBool -> IO ())
  , variantSetIndexed :: FunPtr (GdnativeVariantPtr -> GdnativeInt -> GdnativeVariantPtr -> Ptr GdnativeBool -> Ptr GdnativeBool -> IO ())
  }










{#enum GDNativeInitializationLevel as GdnativeInitializationLevel {underscoreToCase}
  deriving (Show, Eq, Ord, Bounded) #}

data GdnativeInitialization = GdnativeInitialization
  { minimumInitializationLevel :: GdnativeInitializationLevel
  , userdata :: Ptr ()
  , initialize :: FunPtr (Ptr () -> GdnativeInitializationLevel -> IO ())
  , deinitialize :: FunPtr (Ptr () -> GdnativeInitializationLevel -> IO ())
  }

type GdnativeInitializationFunction = Ptr GdnativeInterface -> GdnativeExtensionClassLibraryPtr -> CEnum GdnativeInitialization -> IO ()
foreign import ccall "dynamic" mkGdnativeInitializationFunction
  :: FunPtr GdnativeInitializationFunction
  -> GdnativeInitializationFunction
