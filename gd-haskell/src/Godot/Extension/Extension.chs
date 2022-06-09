{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Godot.Extension.Extension where

import Witch
import Foreign.C
import Foreign.Storable
import Foreign.Ptr
import Data.Coerce

#include <godot/gdnative_interface.h>

instance From CInt Int where
  from = fromIntegral

instance From Int CInt where
  from = fromIntegral

-- | Enums aren't marshallable via foreign imports, so this is a placeholder.
newtype CEnum e = MkCEnum CInt

instance Enum e => From (CEnum e) e where
  from (MkCEnum i) = toEnum $ from i

instance Enum e => From e (CEnum e) where
  from = MkCEnum . from . fromEnum

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
type GdnativePtrConstructor = GdnativeTypePtr -> Ptr GdnativeTypePtr -> IO ()
foreign import ccall "dynamic" mkGdnativePtrConstructor
  :: FunPtr GdnativePtrConstructor
  -> GdnativePtrConstructor
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
  -> IO CUChar
foreign import ccall "dynamic" mkGdnativeExtensionClassSet
  :: FunPtr GdnativeExtensionClassSet
  -> GdnativeExtensionClassSet
type GdnativeExtensionClassGet 
  = GdextensionClassInstancePtr
  -> GdnativeStringNamePtr
  -> GdnativeVariantPtr
  -> IO CUChar
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

type GdnativeExtensionClassGetPropertyList = GdextensionClassInstancePtr -> Ptr CUInt -> IO GdnativePropertyInfoPtr
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
foreign import ccall "dynamic" mkGdnativeExtensionClassToString
  :: FunPtr GdnativeExtensionClassToString
  -> GdnativeExtensionClassToString
type GdnativeExtensionClassReference = GdextensionClassInstancePtr -> IO ()
foreign import ccall "dynamic" mkGdnativeExtensionClassReference
  :: FunPtr GdnativeExtensionClassReference
  -> GdnativeExtensionClassReference
type GdnativeExtensionClassUnreference = GdextensionClassInstancePtr -> IO ()
foreign import ccall "dynamic" mkGdnativeExtensionClassUnreference
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

instance Storable GdnativeExtensionClassCreationInfo where
  sizeOf _ = {#sizeof GDNativeExtensionClassCreationInfo #}
  alignment _ = {#alignof GDNativeExtensionClassCreationInfo #}
  peek ptr =
    GdnativeExtensionClassCreationInfo
      <$> (mkGdnativeExtensionClassSet <$> {#get GDNativeExtensionClassCreationInfo->set_func #} ptr)
      <*> (mkGdnativeExtensionClassGet <$> {#get GDNativeExtensionClassCreationInfo->get_func #} ptr)
      <*> (mkGdnativeExtensionClassGetPropertyList <$> {#get GDNativeExtensionClassCreationInfo->get_property_list_func #} ptr)
      <*> (mkGdnativeExtensionClassFreePropertyList <$> {#get GDNativeExtensionClassCreationInfo->free_property_list_func #} ptr)
      <*> (mkGdnativeExtensionClassNotification <$> {#get GDNativeExtensionClassCreationInfo->notification_func #} ptr)
      <*> (mkGdnativeExtensionClassToString <$> {#get GDNativeExtensionClassCreationInfo->to_string_func #} ptr)
      <*> (mkGdnativeExtensionClassReference <$> {#get GDNativeExtensionClassCreationInfo->reference_func #} ptr)
      <*> (mkGdnativeExtensionClassUnreference <$> {#get GDNativeExtensionClassCreationInfo->unreference_func #} ptr)
      <*> (mkGdnativeExtensionClassCreateInstance <$> {#get GDNativeExtensionClassCreationInfo->create_instance_func #} ptr)
      <*> (mkGdnativeExtensionClassFreeInstance <$> {#get GDNativeExtensionClassCreationInfo->free_instance_func #} ptr)
      <*> (mkGdnativeExtensionClassGetVirtual <$> {#get GDNativeExtensionClassCreationInfo->get_virtual_func #} ptr)
      <*> ({#get GDNativeExtensionClassCreationInfo->class_userdata #} ptr)

{#pointer *GDNativeExtensionClassCreationInfo as GdnativeExtensionClassCreationInfoPtr -> GdnativeExtensionClassCreationInfo #}

type GdnativeExtensionClassLibraryPtr = {#type GDNativeExtensionClassLibraryPtr #}

{#enum GDNativeExtensionClassMethodFlags as GdnativeExtensionClassMethodFlags {underscoreToCase}
  deriving (Show, Eq, Ord, Bounded) #}
{#enum GDNativeExtensionClassMethodArgumentMetadata as GdnativeExtensionClassMethodArgumentMetadata {underscoreToCase}
  deriving (Show, Eq, Ord, Bounded) #}

type GdnativeExtensionClassMethodCall = Ptr () -> GdextensionClassInstancePtr -> Ptr GdnativeVariantPtr -> GdnativeInt -> GdnativeVariantPtr -> Ptr GdnativeCallError -> IO ()
foreign import ccall "dynamic" mkGdnativeExtensionClassMethodCall
  :: FunPtr GdnativeExtensionClassMethodCall
  -> GdnativeExtensionClassMethodCall
type GdnativeExtensionClassMethodPtrCall = Ptr () -> GdextensionClassInstancePtr -> Ptr GdnativeTypePtr -> GdnativeTypePtr -> IO ()
foreign import ccall "dynamic" mkGdnativeExtensionClassMethodPtrCall
  :: FunPtr GdnativeExtensionClassMethodPtrCall
  -> GdnativeExtensionClassMethodPtrCall

type GdnativeExtensionClassMethodGetArgumentType = Ptr () -> CInt -> IO (CEnum GdnativeVariantType)
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
  { name                    :: CString
  , methodUserdata          :: Ptr ()
  , callFunc                :: GdnativeExtensionClassMethodCall
  , ptrcallFunc             :: GdnativeExtensionClassMethodPtrCall
  , methodFlags             :: CUInt
  , argumentCount           :: CUInt
  , hasReturnValue          :: GdnativeBool
  , getArgumentTypeFunc     :: GdnativeExtensionClassMethodGetArgumentType
  , getArgumentInfoFunc     :: GdnativeExtensionClassMethodGetArgumentInfo
  , getArgumentMetadataFunc :: GdnativeExtensionClassMethodGetArgumentMetadata
  , defaultArgumentCount    :: CUInt
  , defaultArguments        :: Ptr GdnativeVariantPtr
  }

instance Storable GdnativeExtensionClassMethodInfo where
  sizeOf _ = {#sizeof GDNativeExtensionClassMethodInfo #}
  alignment _ = {#alignof GDNativeExtensionClassMethodInfo #}
  peek ptr =
    GdnativeExtensionClassMethodInfo
      <$> {#get GDNativeExtensionClassMethodInfo->name #} ptr
      <*> {#get GDNativeExtensionClassMethodInfo->method_userdata #} ptr
      <*> (mkGdnativeExtensionClassMethodCall <$> {#get GDNativeExtensionClassMethodInfo->call_func #} ptr)
      <*> (mkGdnativeExtensionClassMethodPtrCall <$> {#get GDNativeExtensionClassMethodInfo->ptrcall_func #} ptr)
      <*> {#get GDNativeExtensionClassMethodInfo->method_flags #} ptr
      <*> {#get GDNativeExtensionClassMethodInfo->argument_count #} ptr
      <*> {#get GDNativeExtensionClassMethodInfo->has_return_value #} ptr
      <*> (mkGdnativeExtensionClassMethodGetArgumentType . coerce <$> {#get GDNativeExtensionClassMethodInfo->get_argument_type_func #} ptr)
      <*> (mkGdnativeExtensionClassMethodGetArgumentInfo <$> {#get GDNativeExtensionClassMethodInfo->get_argument_info_func #} ptr)
      <*> (mkGdnativeExtensionClassMethodGetArgumentMetadata . coerce <$> {#get GDNativeExtensionClassMethodInfo->get_argument_metadata_func #} ptr)
      <*> {#get GDNativeExtensionClassMethodInfo->default_argument_count #} ptr
      <*> {#get GDNativeExtensionClassMethodInfo->default_arguments #} ptr

{#pointer *GDNativeExtensionClassMethodInfo as GdnativeExtensionClassMethodInfoPtr -> GdnativeExtensionClassMethodInfo #}

type MemAlloc = CULong -> IO (Ptr ())
foreign import ccall "dynamic" mkMemAlloc :: FunPtr MemAlloc -> MemAlloc
type MemRealloc = Ptr () -> CULong -> IO (Ptr ())
foreign import ccall "dynamic" mkMemRealloc :: FunPtr MemRealloc -> MemRealloc
type MemFree = Ptr () -> IO ()

foreign import ccall "dynamic" mkMemFree :: FunPtr MemFree -> MemFree
type PrintError = CString -> CString -> CString -> CInt -> IO ()
foreign import ccall "dynamic" mkPrintError :: FunPtr PrintError -> PrintError
type PrintWarning = CString -> CString -> CString -> CInt -> IO ()
foreign import ccall "dynamic" mkPrintWarning :: FunPtr PrintWarning -> PrintWarning
type PrintScriptError = CString -> CString -> CString -> CInt -> IO ()

foreign import ccall "dynamic" mkPrintScriptError :: FunPtr PrintScriptError -> PrintScriptError
type VariantNewCopy = GdnativeVariantPtr -> GdnativeVariantPtr -> IO ()
foreign import ccall "dynamic" mkVariantNewCopy :: FunPtr VariantNewCopy -> VariantNewCopy
type VariantNewNil = GdnativeVariantPtr -> IO ()
foreign import ccall "dynamic" mkVariantNewNil :: FunPtr VariantNewNil -> VariantNewNil
type VariantDestroy = GdnativeVariantPtr -> IO ()
foreign import ccall "dynamic" mkVariantDestroy :: FunPtr VariantDestroy -> VariantDestroy

type VariantCall = GdnativeVariantPtr -> GdnativeStringNamePtr -> Ptr GdnativeVariantPtr -> GdnativeInt -> GdnativeVariantPtr -> Ptr GdnativeCallError -> IO ()
foreign import ccall "dynamic" mkVariantCall :: FunPtr VariantCall -> VariantCall
type VariantCallStatic = CEnum GdnativeVariantType -> GdnativeStringNamePtr -> Ptr GdnativeVariantPtr -> GdnativeInt -> GdnativeVariantPtr -> Ptr GdnativeCallError -> IO ()
foreign import ccall "dynamic" mkVariantCallStatic :: FunPtr VariantCallStatic -> VariantCallStatic
type VariantEvaluate = CEnum GdnativeVariantOperator -> GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr GdnativeBool -> IO ()
foreign import ccall "dynamic" mkVariantEvaluate :: FunPtr VariantEvaluate -> VariantEvaluate
type VariantSet = GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr GdnativeBool -> IO ()
foreign import ccall "dynamic" mkVariantSet :: FunPtr VariantSet -> VariantSet
type VariantSetNamed = GdnativeVariantPtr -> GdnativeStringNamePtr -> GdnativeVariantPtr -> Ptr GdnativeBool -> IO ()
foreign import ccall "dynamic" mkVariantSetNamed :: FunPtr VariantSetNamed -> VariantSetNamed
type VariantSetKeyed = GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr GdnativeBool -> IO ()
foreign import ccall "dynamic" mkVariantSetKeyed :: FunPtr VariantSetKeyed -> VariantSetKeyed
type VariantSetIndexed = GdnativeVariantPtr -> GdnativeInt -> GdnativeVariantPtr -> Ptr GdnativeBool -> Ptr GdnativeBool -> IO ()
foreign import ccall "dynamic" mkVariantSetIndexed :: FunPtr VariantSetIndexed -> VariantSetIndexed
type VariantGet = GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr GdnativeBool -> IO ()
foreign import ccall "dynamic" mkVariantGet :: FunPtr VariantGet -> VariantGet
type VariantGetNamed = GdnativeVariantPtr -> GdnativeStringNamePtr -> GdnativeVariantPtr -> Ptr GdnativeBool -> IO ()
foreign import ccall "dynamic" mkVariantGetNamed :: FunPtr VariantGetNamed -> VariantGetNamed
type VariantGetKeyed = GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr GdnativeBool -> IO ()
foreign import ccall "dynamic" mkVariantGetKeyed :: FunPtr VariantGetKeyed -> VariantGetKeyed
type VariantGetIndexed = GdnativeVariantPtr -> GdnativeInt -> GdnativeVariantPtr -> Ptr GdnativeBool -> Ptr GdnativeBool -> IO ()
foreign import ccall "dynamic" mkVariantGetIndexed :: FunPtr VariantGetIndexed -> VariantGetIndexed
type VariantIterInit = GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr GdnativeBool -> IO GdnativeBool
foreign import ccall "dynamic" mkVariantIterInit :: FunPtr VariantIterInit -> VariantIterInit
type VariantIterNext = GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr GdnativeBool -> IO GdnativeBool
foreign import ccall "dynamic" mkVariantIterNext :: FunPtr VariantIterNext -> VariantIterNext
type VariantIterGet = GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr GdnativeBool -> IO ()
foreign import ccall "dynamic" mkVariantIterGet :: FunPtr VariantIterGet -> VariantIterGet
type VariantHashCompare = GdnativeVariantPtr -> GdnativeVariantPtr -> IO GdnativeBool
foreign import ccall "dynamic" mkVariantHashCompare :: FunPtr VariantHashCompare -> VariantHashCompare
type VariantBooleanize = GdnativeVariantPtr -> IO GdnativeBool
foreign import ccall "dynamic" mkVariantBooleanize :: FunPtr VariantBooleanize -> VariantBooleanize
type VariantBlend = GdnativeVariantPtr -> GdnativeVariantPtr -> CFloat -> GdnativeVariantPtr -> IO ()
foreign import ccall "dynamic" mkVariantBlend :: FunPtr VariantBlend -> VariantBlend
type VariantInterpolate = GdnativeVariantPtr -> GdnativeVariantPtr -> CFloat -> GdnativeVariantPtr -> IO ()
foreign import ccall "dynamic" mkVariantInterpolate :: FunPtr VariantInterpolate -> VariantInterpolate
type VariantDuplicate = GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeBool -> IO ()
foreign import ccall "dynamic" mkVariantDuplicate :: FunPtr VariantDuplicate -> VariantDuplicate
type VariantStringify = GdnativeVariantPtr -> GdnativeStringPtr -> IO ()
foreign import ccall "dynamic" mkVariantStringify :: FunPtr VariantStringify -> VariantStringify

type VariantGetType = GdnativeVariantPtr -> IO (CEnum GdnativeVariantType)
foreign import ccall "dynamic" mkVariantGetType :: FunPtr VariantGetType -> VariantGetType
type VariantHasMethod = GdnativeVariantPtr -> GdnativeStringNamePtr -> IO GdnativeBool
foreign import ccall "dynamic" mkVariantHasMethod :: FunPtr VariantHasMethod -> VariantHasMethod
type VariantHasMember = CEnum GdnativeVariantType -> GdnativeStringNamePtr -> IO GdnativeBool
foreign import ccall "dynamic" mkVariantHasMember :: FunPtr VariantHasMember -> VariantHasMember
type VariantHasKey = GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr GdnativeBool -> IO GdnativeBool
foreign import ccall "dynamic" mkVariantHasKey :: FunPtr VariantHasKey -> VariantHasKey
type VariantGetTypeName = CEnum GdnativeVariantType -> GdnativeStringPtr -> IO ()
foreign import ccall "dynamic" mkVariantGetTypeName :: FunPtr VariantGetTypeName -> VariantGetTypeName
type VariantCanConvert = CEnum GdnativeVariantType -> CEnum GdnativeVariantType -> IO GdnativeBool
foreign import ccall "dynamic" mkVariantCanConvert :: FunPtr VariantCanConvert -> VariantCanConvert
type VariantCanConvertStrict = CEnum GdnativeVariantType -> CEnum GdnativeVariantType -> IO GdnativeBool
foreign import ccall "dynamic" mkVariantCanConvertStrict :: FunPtr VariantCanConvertStrict -> VariantCanConvertStrict

type GetVariantFromTypeConstructor = CEnum GdnativeVariantType -> IO (FunPtr GdnativeVariantFromTypeConstructorFunc)
foreign import ccall "dynamic" mkGetVariantFromTypeConstructor :: FunPtr GetVariantFromTypeConstructor -> GetVariantFromTypeConstructor
type GetVariantToTypeConstructor = CEnum GdnativeVariantType -> IO (FunPtr GdnativeTypeFromVariantConstructorFunc)
foreign import ccall "dynamic" mkGetVariantToTypeConstructor :: FunPtr GetVariantToTypeConstructor -> GetVariantToTypeConstructor
type VariantGetPtrOperatorEvaluator = CEnum GdnativeVariantOperator -> CEnum GdnativeVariantType -> CEnum GdnativeVariantType -> IO (FunPtr GdnativePtrOperatorEvaluator)
foreign import ccall "dynamic" mkVariantGetPtrOperatorEvaluator :: FunPtr VariantGetPtrOperatorEvaluator -> VariantGetPtrOperatorEvaluator
type VariantGetPtrBuiltinMethod = CEnum GdnativeVariantType -> CString -> GdnativeInt -> IO (FunPtr GdnativePtrBuiltInMethod)
foreign import ccall "dynamic" mkVariantGetPtrBuiltinMethod :: FunPtr VariantGetPtrBuiltinMethod -> VariantGetPtrBuiltinMethod
type VariantGetPtrConstructor = CEnum GdnativeVariantType -> CInt -> IO (FunPtr GdnativePtrConstructor)
foreign import ccall "dynamic" mkVariantGetPtrConstructor :: FunPtr VariantGetPtrConstructor -> VariantGetPtrConstructor
type VariantGetPtrDestructor = CEnum GdnativeVariantType -> IO (FunPtr GdnativePtrDestructor)
foreign import ccall "dynamic" mkVariantGetPtrDestructor :: FunPtr VariantGetPtrDestructor -> VariantGetPtrDestructor
type VariantConstruct = CEnum GdnativeVariantType -> GdnativeVariantPtr -> Ptr GdnativeVariantPtr -> CInt -> Ptr GdnativeCallError -> IO ()
foreign import ccall "dynamic" mkVariantConstruct :: FunPtr VariantConstruct -> VariantConstruct
type VariantGetPtrSetter = CEnum GdnativeVariantType -> CString -> IO (FunPtr GdnativePtrSetter)
foreign import ccall "dynamic" mkVariantGetPtrSetter :: FunPtr VariantGetPtrSetter -> VariantGetPtrSetter
type VariantGetPtrGetter = CEnum GdnativeVariantType -> CString -> IO (FunPtr GdnativePtrGetter)
foreign import ccall "dynamic" mkVariantGetPtrGetter :: FunPtr VariantGetPtrGetter -> VariantGetPtrGetter
type VariantGetPtrIndexedSetter = CEnum GdnativeVariantType -> IO (FunPtr GdnativePtrIndexedSetter)
foreign import ccall "dynamic" mkVariantGetPtrIndexedSetter :: FunPtr VariantGetPtrIndexedSetter -> VariantGetPtrIndexedSetter
type VariantGetPtrIndexedGetter = CEnum GdnativeVariantType -> IO (FunPtr GdnativePtrIndexedGetter)
foreign import ccall "dynamic" mkVariantGetPtrIndexedGetter :: FunPtr VariantGetPtrIndexedGetter -> VariantGetPtrIndexedGetter
type VariantGetPtrKeyedSetter = CEnum GdnativeVariantType -> IO (FunPtr GdnativePtrKeyedSetter)
foreign import ccall "dynamic" mkVariantGetPtrKeyedSetter :: FunPtr VariantGetPtrKeyedSetter -> VariantGetPtrKeyedSetter
type VariantGetPtrKeyedGetter = CEnum GdnativeVariantType -> IO (FunPtr GdnativePtrKeyedGetter)
foreign import ccall "dynamic" mkVariantGetPtrKeyedGetter :: FunPtr VariantGetPtrKeyedGetter -> VariantGetPtrKeyedGetter
type VariantGetPtrKeyedChecker = CEnum GdnativeVariantType -> IO (FunPtr GdnativePtrKeyedChecker)
foreign import ccall "dynamic" mkVariantGetPtrKeyedChecker :: FunPtr VariantGetPtrKeyedChecker -> VariantGetPtrKeyedChecker
type VariantGetConstantValue = CEnum GdnativeVariantType -> CString -> GdnativeVariantPtr -> IO ()
foreign import ccall "dynamic" mkVariantGetConstantValue :: FunPtr VariantGetConstantValue -> VariantGetConstantValue
type VariantGetPtrUtilityFunction = CString -> GdnativeInt -> IO (FunPtr GdnativePtrUtilityFunction)
foreign import ccall "dynamic" mkVariantGetPtrUtilityFunction :: FunPtr VariantGetPtrUtilityFunction -> VariantGetPtrUtilityFunction

type StringNewWithLatin1Chars = GdnativeStringPtr -> CString -> IO ()
foreign import ccall "dynamic" mkStringNewWithLatin1Chars :: FunPtr StringNewWithLatin1Chars -> StringNewWithLatin1Chars
type StringNewWithUtf8Chars = GdnativeStringPtr -> CString -> IO ()
foreign import ccall "dynamic" mkStringNewWithUtf8Chars :: FunPtr StringNewWithUtf8Chars -> StringNewWithUtf8Chars
type StringNewWithUtf16Chars = GdnativeStringPtr -> Ptr CUShort -> IO ()
foreign import ccall "dynamic" mkStringNewWithUtf16Chars :: FunPtr StringNewWithUtf16Chars -> StringNewWithUtf16Chars
type StringNewWithUtf32Chars = GdnativeStringPtr -> Ptr CUInt -> IO ()
foreign import ccall "dynamic" mkStringNewWithUtf32Chars :: FunPtr StringNewWithUtf32Chars -> StringNewWithUtf32Chars
type StringNewWithWideChars = GdnativeStringPtr -> Ptr CInt -> IO ()
foreign import ccall "dynamic" mkStringNewWithWideChars :: FunPtr StringNewWithWideChars -> StringNewWithWideChars
type StringNewWithLatin1CharsAndLen = GdnativeStringPtr -> CString -> GdnativeInt -> IO ()
foreign import ccall "dynamic" mkStringNewWithLatin1CharsAndLen :: FunPtr StringNewWithLatin1CharsAndLen -> StringNewWithLatin1CharsAndLen
type StringNewWithUtf8CharsAndLen = GdnativeStringPtr -> CString -> GdnativeInt -> IO ()
foreign import ccall "dynamic" mkStringNewWithUtf8CharsAndLen :: FunPtr StringNewWithUtf8CharsAndLen -> StringNewWithUtf8CharsAndLen
type StringNewWithUtf16CharsAndLen = GdnativeStringPtr -> Ptr CUShort -> GdnativeInt -> IO ()
foreign import ccall "dynamic" mkStringNewWithUtf16CharsAndLen :: FunPtr StringNewWithUtf16CharsAndLen -> StringNewWithUtf16CharsAndLen
type StringNewWithUtf32CharsAndLen = GdnativeStringPtr -> Ptr CUInt -> GdnativeInt -> IO ()
foreign import ccall "dynamic" mkStringNewWithUtf32CharsAndLen :: FunPtr StringNewWithUtf32CharsAndLen -> StringNewWithUtf32CharsAndLen
type StringNewWithWideCharsAndLen = GdnativeStringPtr -> Ptr CInt -> GdnativeInt -> IO ()
foreign import ccall "dynamic" mkStringNewWithWideCharsAndLen :: FunPtr StringNewWithWideCharsAndLen -> StringNewWithWideCharsAndLen

type StringToLatin1Chars = GdnativeStringPtr -> CString -> GdnativeInt -> IO GdnativeInt
foreign import ccall "dynamic" mkStringToLatin1Chars :: FunPtr StringToLatin1Chars -> StringToLatin1Chars
type StringToUtf8Chars = GdnativeStringPtr -> CString -> GdnativeInt -> IO GdnativeInt
foreign import ccall "dynamic" mkStringToUtf8Chars :: FunPtr StringToUtf8Chars -> StringToUtf8Chars
type StringToUtf16Chars = GdnativeStringPtr -> Ptr CUShort -> GdnativeInt -> IO GdnativeInt
foreign import ccall "dynamic" mkStringToUtf16Chars :: FunPtr StringToUtf16Chars -> StringToUtf16Chars
type StringToUtf32Chars = GdnativeStringPtr -> Ptr CUInt -> GdnativeInt -> IO GdnativeInt
foreign import ccall "dynamic" mkStringToUtf32Chars :: FunPtr StringToUtf32Chars -> StringToUtf32Chars
type StringToWideChars = GdnativeStringPtr -> Ptr CInt -> GdnativeInt -> IO GdnativeInt
foreign import ccall "dynamic" mkStringToWideChars :: FunPtr StringToWideChars -> StringToWideChars
type StringOperatorIndex = GdnativeStringPtr -> GdnativeInt -> IO (Ptr CUInt)
foreign import ccall "dynamic" mkStringOperatorIndex :: FunPtr StringOperatorIndex -> StringOperatorIndex
type StringOperatorIndexConst = GdnativeStringPtr -> GdnativeInt -> IO (Ptr CUInt)
foreign import ccall "dynamic" mkStringOperatorIndexConst :: FunPtr StringOperatorIndexConst -> StringOperatorIndexConst

type PackedByteArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO (Ptr CUChar)
foreign import ccall "dynamic" mkPackedByteArrayOperatorIndex :: FunPtr PackedByteArrayOperatorIndex -> PackedByteArrayOperatorIndex
type PackedByteArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO (Ptr CUChar)
foreign import ccall "dynamic" mkPackedByteArrayOperatorIndexConst :: FunPtr PackedByteArrayOperatorIndexConst -> PackedByteArrayOperatorIndexConst

type PackedColorArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO GdnativeTypePtr
foreign import ccall "dynamic" mkPackedColorArrayOperatorIndex :: FunPtr PackedColorArrayOperatorIndex -> PackedColorArrayOperatorIndex
type PackedColorArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO GdnativeTypePtr
foreign import ccall "dynamic" mkPackedColorArrayOperatorIndexConst :: FunPtr PackedColorArrayOperatorIndexConst -> PackedColorArrayOperatorIndexConst

type PackedFloat32ArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO (Ptr CFloat)
foreign import ccall "dynamic" mkPackedFloat32ArrayOperatorIndex :: FunPtr PackedFloat32ArrayOperatorIndex -> PackedFloat32ArrayOperatorIndex
type PackedFloat32ArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO (Ptr CFloat)
foreign import ccall "dynamic" mkPackedFloat32ArrayOperatorIndexConst :: FunPtr PackedFloat32ArrayOperatorIndexConst -> PackedFloat32ArrayOperatorIndexConst
type PackedFloat64ArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO (Ptr CDouble)
foreign import ccall "dynamic" mkPackedFloat64ArrayOperatorIndex :: FunPtr PackedFloat64ArrayOperatorIndex -> PackedFloat64ArrayOperatorIndex
type PackedFloat64ArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO (Ptr CDouble)
foreign import ccall "dynamic" mkPackedFloat64ArrayOperatorIndexConst :: FunPtr PackedFloat64ArrayOperatorIndexConst -> PackedFloat64ArrayOperatorIndexConst

type PackedInt32ArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO (Ptr CInt)
foreign import ccall "dynamic" mkPackedInt32ArrayOperatorIndex :: FunPtr PackedInt32ArrayOperatorIndex -> PackedInt32ArrayOperatorIndex
type PackedInt32ArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO (Ptr CInt)
foreign import ccall "dynamic" mkPackedInt32ArrayOperatorIndexConst :: FunPtr PackedInt32ArrayOperatorIndexConst -> PackedInt32ArrayOperatorIndexConst
type PackedInt64ArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO (Ptr CLong)
foreign import ccall "dynamic" mkPackedInt64ArrayOperatorIndex :: FunPtr PackedInt64ArrayOperatorIndex -> PackedInt64ArrayOperatorIndex
type PackedInt64ArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO (Ptr CLong)
foreign import ccall "dynamic" mkPackedInt64ArrayOperatorIndexConst :: FunPtr PackedInt64ArrayOperatorIndexConst -> PackedInt64ArrayOperatorIndexConst

type PackedStringArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO GdnativeStringPtr
foreign import ccall "dynamic" mkPackedStringArrayOperatorIndex :: FunPtr PackedStringArrayOperatorIndex -> PackedStringArrayOperatorIndex
type PackedStringArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO GdnativeStringPtr
foreign import ccall "dynamic" mkPackedStringArrayOperatorIndexConst :: FunPtr PackedStringArrayOperatorIndexConst -> PackedStringArrayOperatorIndexConst

type PackedVector2ArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO GdnativeTypePtr
foreign import ccall "dynamic" mkPackedVector2ArrayOperatorIndex :: FunPtr PackedVector2ArrayOperatorIndex -> PackedVector2ArrayOperatorIndex
type PackedVector2ArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO GdnativeTypePtr
foreign import ccall "dynamic" mkPackedVector2ArrayOperatorIndexConst :: FunPtr PackedVector2ArrayOperatorIndexConst -> PackedVector2ArrayOperatorIndexConst
type PackedVector3ArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO GdnativeTypePtr
foreign import ccall "dynamic" mkPackedVector3ArrayOperatorIndex :: FunPtr PackedVector3ArrayOperatorIndex -> PackedVector3ArrayOperatorIndex
type PackedVector3ArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO GdnativeTypePtr
foreign import ccall "dynamic" mkPackedVector3ArrayOperatorIndexConst :: FunPtr PackedVector3ArrayOperatorIndexConst -> PackedVector3ArrayOperatorIndexConst

type ArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO GdnativeVariantPtr
foreign import ccall "dynamic" mkArrayOperatorIndex :: FunPtr ArrayOperatorIndex -> ArrayOperatorIndex
type ArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO GdnativeVariantPtr
foreign import ccall "dynamic" mkArrayOperatorIndexConst :: FunPtr ArrayOperatorIndexConst -> ArrayOperatorIndexConst

type DictionaryOperatorIndex = GdnativeTypePtr -> GdnativeVariantPtr -> IO GdnativeVariantPtr
foreign import ccall "dynamic" mkDictionaryOperatorIndex :: FunPtr DictionaryOperatorIndex -> DictionaryOperatorIndex
type DictionaryOperatorIndexConst = GdnativeTypePtr -> GdnativeVariantPtr -> IO GdnativeVariantPtr
foreign import ccall "dynamic" mkDictionaryOperatorIndexConst :: FunPtr DictionaryOperatorIndexConst -> DictionaryOperatorIndexConst

type ObjectMethodBindCall = GdnativeMethodBindPtr -> GdnativeObjectPtr -> Ptr GdnativeVariantPtr -> GdnativeInt -> GdnativeVariantPtr -> Ptr GdnativeCallError -> IO ()
foreign import ccall "dynamic" mkObjectMethodBindCall :: FunPtr ObjectMethodBindCall -> ObjectMethodBindCall
type ObjectMethodBindPtrcall = GdnativeMethodBindPtr -> GdnativeObjectPtr -> Ptr GdnativeTypePtr -> GdnativeTypePtr -> IO ()
foreign import ccall "dynamic" mkObjectMethodBindPtrcall :: FunPtr ObjectMethodBindPtrcall -> ObjectMethodBindPtrcall
type ObjectDestroy = GdnativeObjectPtr -> IO ()
foreign import ccall "dynamic" mkObjectDestroy :: FunPtr ObjectDestroy -> ObjectDestroy
type GlobalGetSingleton = CString -> IO GdnativeObjectPtr
foreign import ccall "dynamic" mkGlobalGetSingleton :: FunPtr GlobalGetSingleton -> GlobalGetSingleton

type ObjectGetInstanceBinding = GdnativeObjectPtr -> Ptr () -> Ptr GdnativeInstanceBindingCallbacks -> IO (Ptr ())
foreign import ccall "dynamic" mkObjectGetInstanceBinding :: FunPtr ObjectGetInstanceBinding -> ObjectGetInstanceBinding
type ObjectSetInstanceBinding = GdnativeObjectPtr -> Ptr () -> Ptr () -> Ptr GdnativeInstanceBindingCallbacks -> IO ()
foreign import ccall "dynamic" mkObjectSetInstanceBinding :: FunPtr ObjectSetInstanceBinding -> ObjectSetInstanceBinding

type ObjectSetInstance = GdnativeObjectPtr -> CString -> GdextensionClassInstancePtr -> IO ()
foreign import ccall "dynamic" mkObjectSetInstance :: FunPtr ObjectSetInstance -> ObjectSetInstance

type ObjectCastTo = GdnativeObjectPtr -> Ptr () -> IO GdnativeObjectPtr
foreign import ccall "dynamic" mkObjectCastTo :: FunPtr ObjectCastTo -> ObjectCastTo
type ObjectGetInstanceFromId = GdObjectInstanceId -> IO GdnativeObjectPtr
foreign import ccall "dynamic" mkObjectGetInstanceFromId :: FunPtr ObjectGetInstanceFromId -> ObjectGetInstanceFromId
type GetInstanceId = GdnativeObjectPtr -> IO GdObjectInstanceId
foreign import ccall "dynamic" mkGetInstanceId :: FunPtr GetInstanceId -> GetInstanceId

type ClassdbConstructObject = CString -> IO GdnativeObjectPtr
foreign import ccall "dynamic" mkClassdbConstructObject :: FunPtr ClassdbConstructObject -> ClassdbConstructObject
type ClassdbGetMethodBind = CString -> CString -> GdnativeInt -> IO GdnativeMethodBindPtr
foreign import ccall "dynamic" mkClassdbGetMethodBind :: FunPtr ClassdbGetMethodBind -> ClassdbGetMethodBind
type ClassdbGetClassTag = CString -> IO (Ptr ())
foreign import ccall "dynamic" mkClassdbGetClassTag :: FunPtr ClassdbGetClassTag -> ClassdbGetClassTag

type ClassdbRegisterExtensionClass = GdnativeExtensionClassLibraryPtr -> CString -> CString -> Ptr GdnativeExtensionClassCreationInfo -> IO ()
foreign import ccall "dynamic" mkClassdbRegisterExtensionClass :: FunPtr ClassdbRegisterExtensionClass -> ClassdbRegisterExtensionClass
type ClassdbRegisterExtensionClassMethod = GdnativeExtensionClassLibraryPtr -> CString -> Ptr GdnativeExtensionClassMethodInfo -> IO ()
foreign import ccall "dynamic" mkClassdbRegisterExtensionClassMethod :: FunPtr ClassdbRegisterExtensionClassMethod -> ClassdbRegisterExtensionClassMethod
type ClassdbRegisterExtensionClassIntegerConstant = GdnativeExtensionClassLibraryPtr -> CString -> CString -> CString -> GdnativeInt -> IO ()
foreign import ccall "dynamic" mkClassdbRegisterExtensionClassIntegerConstant :: FunPtr ClassdbRegisterExtensionClassIntegerConstant -> ClassdbRegisterExtensionClassIntegerConstant
type ClassdbRegisterExtensionClassProperty = GdnativeExtensionClassLibraryPtr -> CString -> Ptr GdnativePropertyInfo -> CString -> CString -> IO ()
foreign import ccall "dynamic" mkClassdbRegisterExtensionClassProperty :: FunPtr ClassdbRegisterExtensionClassProperty -> ClassdbRegisterExtensionClassProperty
type ClassdbRegisterExtensionClassPropertyGroup = GdnativeExtensionClassLibraryPtr -> CString -> CString -> CString -> IO ()
foreign import ccall "dynamic" mkClassdbRegisterExtensionClassPropertyGroup :: FunPtr ClassdbRegisterExtensionClassPropertyGroup -> ClassdbRegisterExtensionClassPropertyGroup
type ClassdbRegisterExtensionClassPropertySubgroup = GdnativeExtensionClassLibraryPtr -> CString -> CString -> CString -> IO ()
foreign import ccall "dynamic" mkClassdbRegisterExtensionClassPropertySubgroup :: FunPtr ClassdbRegisterExtensionClassPropertySubgroup -> ClassdbRegisterExtensionClassPropertySubgroup
type ClassdbRegisterExtensionClassSignal = GdnativeExtensionClassLibraryPtr -> CString -> CString -> Ptr GdnativePropertyInfo -> GdnativeInt -> IO ()
foreign import ccall "dynamic" mkClassdbRegisterExtensionClassSignal :: FunPtr ClassdbRegisterExtensionClassSignal -> ClassdbRegisterExtensionClassSignal
type ClassdbUnregisterExtensionClass = GdnativeExtensionClassLibraryPtr -> CString -> IO ()
foreign import ccall "dynamic" mkClassdbUnregisterExtensionClass :: FunPtr ClassdbUnregisterExtensionClass -> ClassdbUnregisterExtensionClass

data GdnativeInterface = GdnativeInterface
  { versionMajor                                      :: CUInt
  , versionMinor                                      :: CUInt
  , versionPatch                                      :: CUInt
  , versionString                                     :: CString

  , memAlloc                                          :: MemAlloc
  , memRealloc                                        :: MemRealloc
  , memFree                                           :: MemFree

  , printError                                        :: PrintError
  , printWarning                                      :: PrintWarning
  , printScriptError                                  :: PrintScriptError
  
  , variantNewCopy                                    :: VariantNewCopy
  , variantNewNil                                     :: VariantNewNil
  , variantDestroy                                    :: VariantDestroy
  
  , variantCall                                       :: VariantCall
  , variantCallStatic                                 :: VariantCallStatic
  , variantEvaluate                                   :: VariantEvaluate
  , variantSet                                        :: VariantSet
  , variantSetNamed                                   :: VariantSetNamed
  , variantSetKeyed                                   :: VariantSetKeyed
  , variantSetIndexed                                 :: VariantSetIndexed
  , variantGet                                        :: VariantGet
  , variantGetNamed                                   :: VariantGetNamed
  , variantGetKeyed                                   :: VariantGetKeyed
  , variantGetIndexed                                 :: VariantGetIndexed
  , variantIterInit                                   :: VariantIterInit
  , variantIterNext                                   :: VariantIterNext
  , variantIterGet                                    :: VariantIterGet
  , variantHashCompare                                :: VariantHashCompare
  , variantBooleanize                                 :: VariantBooleanize
  , variantBlend                                      :: VariantBlend
  , variantInterpolate                                :: VariantInterpolate
  , variantDuplicate                                  :: VariantDuplicate
  , variantStringify                                  :: VariantStringify
  
  , variantGetType                                    :: VariantGetType
  , variantHasMethod                                  :: VariantHasMethod
  , variantHasMember                                  :: VariantHasMember
  , variantHasKey                                     :: VariantHasKey
  , variantGetTypeName                                :: VariantGetTypeName
  , variantCanConvert                                 :: VariantCanConvert
  , variantCanConvertStrict                           :: VariantCanConvertStrict
  
  , getVariantFromTypeConstructor                     :: GetVariantFromTypeConstructor
  , getVariantToTypeConstructor                       :: GetVariantToTypeConstructor
  , variantGetPtrOperatorEvaluator                    :: VariantGetPtrOperatorEvaluator
  , variantGetPtrBuiltinMethod                        :: VariantGetPtrBuiltinMethod
  , variantGetPtrConstructor                          :: VariantGetPtrConstructor
  , variantGetPtrDestructor                           :: VariantGetPtrDestructor
  , variantConstruct                                  :: VariantConstruct
  , variantGetPtrSetter                               :: VariantGetPtrSetter
  , variantGetPtrGetter                               :: VariantGetPtrGetter
  , variantGetPtrIndexedSetter                        :: VariantGetPtrIndexedSetter
  , variantGetPtrIndexedGetter                        :: VariantGetPtrIndexedGetter
  , variantGetPtrKeyedSetter                          :: VariantGetPtrKeyedSetter
  , variantGetPtrKeyedGetter                          :: VariantGetPtrKeyedGetter
  , variantGetPtrKeyedChecker                         :: VariantGetPtrKeyedChecker
  , variantGetConstantValue                           :: VariantGetConstantValue
  , variantGetPtrUtilityFunction                      :: VariantGetPtrUtilityFunction

  , stringNewWithLatin1Chars                          :: StringNewWithLatin1Chars
  , stringNewWithUtf8Chars                            :: StringNewWithUtf8Chars
  , stringNewWithUtf16Chars                           :: StringNewWithUtf16Chars
  , stringNewWithUtf32Chars                           :: StringNewWithUtf32Chars
  , stringNewWithWideChars                            :: StringNewWithWideChars
  , stringNewWithLatin1CharsAndLen                    :: StringNewWithLatin1CharsAndLen
  , stringNewWithUtf8CharsAndLen                      :: StringNewWithUtf8CharsAndLen
  , stringNewWithUtf16CharsAndLen                     :: StringNewWithUtf16CharsAndLen
  , stringNewWithUtf32CharsAndLen                     :: StringNewWithUtf32CharsAndLen
  , stringNewWithWideCharsAndLen                      :: StringNewWithWideCharsAndLen
    
  , stringToLatin1Chars                               :: StringToLatin1Chars
  , stringToUtf8Chars                                 :: StringToUtf8Chars
  , stringToUtf16Chars                                :: StringToUtf16Chars
  , stringToUtf32Chars                                :: StringToUtf32Chars
  , stringToWideChars                                 :: StringToWideChars
  , stringOperatorIndex                               :: StringOperatorIndex
  , stringOperatorIndexConst                          :: StringOperatorIndexConst

  , packedByteArrayOperatorIndex                      :: PackedByteArrayOperatorIndex
  , packedByteArrayOperatorIndexConst                 :: PackedByteArrayOperatorIndexConst

  , packedColorArrayOperatorIndex                     :: PackedColorArrayOperatorIndex
  , packedColorArrayOperatorIndexConst                :: PackedColorArrayOperatorIndexConst

  , packedFloat32ArrayOperatorIndex                   :: PackedFloat32ArrayOperatorIndex
  , packedFloat32ArrayOperatorIndexConst              :: PackedFloat32ArrayOperatorIndexConst
  , packedFloat64ArrayOperatorIndex                   :: PackedFloat64ArrayOperatorIndex
  , packedFloat64ArrayOperatorIndexConst              :: PackedFloat64ArrayOperatorIndexConst
  
  , packedInt32ArrayOperatorIndex                     :: PackedInt32ArrayOperatorIndex
  , packedInt32ArrayOperatorIndexConst                :: PackedInt32ArrayOperatorIndexConst
  , packedInt64ArrayOperatorIndex                     :: PackedInt64ArrayOperatorIndex
  , packedInt64ArrayOperatorIndexConst                :: PackedInt64ArrayOperatorIndexConst
  
  , packedStringArrayOperatorIndex                    :: PackedStringArrayOperatorIndex
  , packedStringArrayOperatorIndexConst               :: PackedStringArrayOperatorIndexConst
  
  , packedVector2ArrayOperatorIndex                   :: PackedVector2ArrayOperatorIndex
  , packedVector2ArrayOperatorIndexConst              :: PackedVector2ArrayOperatorIndexConst
  , packedVector3ArrayOperatorIndex                   :: PackedVector3ArrayOperatorIndex
  , packedVector3ArrayOperatorIndexConst              :: PackedVector3ArrayOperatorIndexConst
  
  , arrayOperatorIndex                                :: ArrayOperatorIndex
  , arrayOperatorIndexConst                           :: ArrayOperatorIndexConst
  
  , dictionaryOperatorIndex                           :: DictionaryOperatorIndex
  , dictionaryOperatorIndexConst                      :: DictionaryOperatorIndexConst
    
  , objectMethodBindCall                              :: ObjectMethodBindCall
  , objectMethodBindPtrcall                           :: ObjectMethodBindPtrcall
  , objectDestroy                                     :: ObjectDestroy
  , globalGetSingleton                                :: GlobalGetSingleton

  , objectGetInstanceBinding                          :: ObjectGetInstanceBinding
  , objectSetInstanceBinding                          :: ObjectSetInstanceBinding

  , objectSetInstance                                 :: ObjectSetInstance
  
  , objectCastTo                                      :: ObjectCastTo
  , objectGetInstanceFromId                           :: ObjectGetInstanceFromId

  , classdbConstructObject                            :: ClassdbConstructObject
  , classdbGetMethodBind                              :: ClassdbGetMethodBind
  , classdbGetClassTag                                :: ClassdbGetClassTag
  
  , classdbRegisterExtensionClass                     :: ClassdbRegisterExtensionClass
  , classdbRegisterExtensionClassMethod               :: ClassdbRegisterExtensionClassMethod
  , classdbRegisterExtensionClassIntegerConstant      :: ClassdbRegisterExtensionClassIntegerConstant
  , classdbRegisterExtensionClassProperty             :: ClassdbRegisterExtensionClassProperty
  , classdbRegisterExtensionClassPropertyGroup        :: ClassdbRegisterExtensionClassPropertyGroup
  , classdbRegisterExtensionClassPropertySubgroup     :: ClassdbRegisterExtensionClassPropertySubgroup
  , classdbRegisterExtensionClassSignal               :: ClassdbRegisterExtensionClassSignal
  , classdbUnregisterExtensionClass                   :: ClassdbUnregisterExtensionClass
  }

instance Storable GdnativeInterface where
  sizeOf _ = {#sizeof GDNativeInterface #}
  alignment _ = {#alignof GDNativeInterface #}
  peek ptr = do
    versionMajor                                      <-                                                      {#get GDNativeInterface->version_major #} ptr
    versionMinor                                      <-                                                      {#get GDNativeInterface->version_minor #} ptr
    versionString                                     <-                                                      {#get GDNativeInterface->version_string #} ptr
    versionPatch                                      <-                                                      {#get GDNativeInterface->version_patch #} ptr

    memAlloc                                          <- (mkMemAlloc                                      <$> {#get GDNativeInterface->mem_alloc #} ptr)
    memRealloc                                        <- (mkMemRealloc                                    <$> {#get GDNativeInterface->mem_realloc #} ptr)
    memFree                                           <- (mkMemFree                                       <$> {#get GDNativeInterface->mem_free #} ptr)

    printError                                        <- (mkPrintError                                    <$> {#get GDNativeInterface->print_error #} ptr)
    printWarning                                      <- (mkPrintWarning                                  <$> {#get GDNativeInterface->print_warning #} ptr)
    printScriptError                                  <- (mkPrintScriptError                              <$> {#get GDNativeInterface->print_script_error #} ptr)
  
    variantNewCopy                                    <- (mkVariantNewCopy                                <$> {#get GDNativeInterface->variant_new_copy #} ptr)
    variantNewNil                                     <- (mkVariantNewNil                                 <$> {#get GDNativeInterface->variant_new_nil #} ptr)
    variantDestroy                                    <- (mkVariantDestroy                                <$> {#get GDNativeInterface->variant_destroy #} ptr)
  
    variantCall                                       <- (mkVariantCall                                   <$> {#get GDNativeInterface->variant_call #} ptr)
    variantCallStatic                                 <- (mkVariantCallStatic              . coerce       <$> {#get GDNativeInterface->variant_call_static #} ptr)
    variantEvaluate                                   <- (mkVariantEvaluate                . coerce       <$> {#get GDNativeInterface->variant_evaluate #} ptr)
    variantSet                                        <- (mkVariantSet                                    <$> {#get GDNativeInterface->variant_set #} ptr)
    variantSetNamed                                   <- (mkVariantSetNamed                               <$> {#get GDNativeInterface->variant_set_named #} ptr)
    variantSetKeyed                                   <- (mkVariantSetKeyed                               <$> {#get GDNativeInterface->variant_set_keyed #} ptr)
    variantSetIndexed                                 <- (mkVariantSetIndexed                             <$> {#get GDNativeInterface->variant_set_indexed #} ptr)
    variantGet                                        <- (mkVariantGet                                    <$> {#get GDNativeInterface->variant_get #} ptr)
    variantGetNamed                                   <- (mkVariantGetNamed                               <$> {#get GDNativeInterface->variant_get_named #} ptr)
    variantGetKeyed                                   <- (mkVariantGetKeyed                               <$> {#get GDNativeInterface->variant_get_keyed #} ptr)
    variantGetIndexed                                 <- (mkVariantGetIndexed                             <$> {#get GDNativeInterface->variant_get_indexed #} ptr)
    variantIterInit                                   <- (mkVariantIterInit                               <$> {#get GDNativeInterface->variant_iter_init #} ptr)
    variantIterNext                                   <- (mkVariantIterNext                               <$> {#get GDNativeInterface->variant_iter_next #} ptr)
    variantIterGet                                    <- (mkVariantIterGet                                <$> {#get GDNativeInterface->variant_iter_get #} ptr)
    variantHashCompare                                <- (mkVariantHashCompare                            <$> {#get GDNativeInterface->variant_hash_compare #} ptr)
    variantBooleanize                                 <- (mkVariantBooleanize                             <$> {#get GDNativeInterface->variant_booleanize #} ptr)
    variantBlend                                      <- (mkVariantBlend                                  <$> {#get GDNativeInterface->variant_blend #} ptr)
    variantInterpolate                                <- (mkVariantInterpolate                            <$> {#get GDNativeInterface->variant_interpolate #} ptr)
    variantDuplicate                                  <- (mkVariantDuplicate                              <$> {#get GDNativeInterface->variant_duplicate #} ptr)
    variantStringify                                  <- (mkVariantStringify                              <$> {#get GDNativeInterface->variant_stringify #} ptr)
  
    variantGetType                                    <- (mkVariantGetType                 . coerce       <$> {#get GDNativeInterface->variant_get_type #} ptr)
    variantHasMethod                                  <- (mkVariantHasMethod                              <$> {#get GDNativeInterface->variant_has_method #} ptr)
    variantHasMember                                  <- (mkVariantHasMember               . coerce       <$> {#get GDNativeInterface->variant_has_member #} ptr)
    variantHasKey                                     <- (mkVariantHasKey                                 <$> {#get GDNativeInterface->variant_has_key #} ptr)
    variantGetTypeName                                <- (mkVariantGetTypeName             . coerce       <$> {#get GDNativeInterface->variant_get_type_name #} ptr)
    variantCanConvert                                 <- (mkVariantCanConvert              . coerce       <$> {#get GDNativeInterface->variant_can_convert #} ptr)
    variantCanConvertStrict                           <- (mkVariantCanConvertStrict        . coerce       <$> {#get GDNativeInterface->variant_can_convert_strict #} ptr)
  
    getVariantFromTypeConstructor                     <- (mkGetVariantFromTypeConstructor  . coerce       <$> {#get GDNativeInterface->get_variant_from_type_constructor #} ptr)
    getVariantToTypeConstructor                       <- (mkGetVariantToTypeConstructor    . coerce       <$> {#get GDNativeInterface->get_variant_to_type_constructor #} ptr)
    variantGetPtrOperatorEvaluator                    <- (mkVariantGetPtrOperatorEvaluator . coerce       <$> {#get GDNativeInterface->variant_get_ptr_operator_evaluator #} ptr)
    variantGetPtrBuiltinMethod                        <- (mkVariantGetPtrBuiltinMethod     . coerce       <$> {#get GDNativeInterface->variant_get_ptr_builtin_method #} ptr)
    variantGetPtrConstructor                          <- (mkVariantGetPtrConstructor       . coerce       <$> {#get GDNativeInterface->variant_get_ptr_constructor #} ptr)
    variantGetPtrDestructor                           <- (mkVariantGetPtrDestructor        . coerce       <$> {#get GDNativeInterface->variant_get_ptr_destructor #} ptr)
    variantConstruct                                  <- (mkVariantConstruct               . coerce       <$> {#get GDNativeInterface->variant_construct #} ptr)
    variantGetPtrSetter                               <- (mkVariantGetPtrSetter            . coerce       <$> {#get GDNativeInterface->variant_get_ptr_setter #} ptr)
    variantGetPtrGetter                               <- (mkVariantGetPtrGetter            . coerce       <$> {#get GDNativeInterface->variant_get_ptr_getter #} ptr)
    variantGetPtrIndexedSetter                        <- (mkVariantGetPtrIndexedSetter     . coerce       <$> {#get GDNativeInterface->variant_get_ptr_indexed_setter #} ptr)
    variantGetPtrIndexedGetter                        <- (mkVariantGetPtrIndexedGetter     . coerce       <$> {#get GDNativeInterface->variant_get_ptr_indexed_getter #} ptr)
    variantGetPtrKeyedSetter                          <- (mkVariantGetPtrKeyedSetter       . coerce       <$> {#get GDNativeInterface->variant_get_ptr_keyed_setter #} ptr)
    variantGetPtrKeyedGetter                          <- (mkVariantGetPtrKeyedGetter       . coerce       <$> {#get GDNativeInterface->variant_get_ptr_keyed_getter #} ptr)
    variantGetPtrKeyedChecker                         <- (mkVariantGetPtrKeyedChecker      . coerce       <$> {#get GDNativeInterface->variant_get_ptr_keyed_checker #} ptr)
    variantGetConstantValue                           <- (mkVariantGetConstantValue        . coerce       <$> {#get GDNativeInterface->variant_get_constant_value #} ptr)
    variantGetPtrUtilityFunction                      <- (mkVariantGetPtrUtilityFunction                  <$> {#get GDNativeInterface->variant_get_ptr_utility_function #} ptr)

    stringNewWithLatin1Chars                          <- (mkStringNewWithLatin1Chars                      <$> {#get GDNativeInterface->string_new_with_latin1_chars #} ptr)
    stringNewWithUtf8Chars                            <- (mkStringNewWithUtf8Chars                        <$> {#get GDNativeInterface->string_new_with_utf8_chars #} ptr)
    stringNewWithUtf16Chars                           <- (mkStringNewWithUtf16Chars                       <$> {#get GDNativeInterface->string_new_with_utf16_chars #} ptr)
    stringNewWithUtf32Chars                           <- (mkStringNewWithUtf32Chars                       <$> {#get GDNativeInterface->string_new_with_utf32_chars #} ptr)
    stringNewWithWideChars                            <- (mkStringNewWithWideChars                        <$> {#get GDNativeInterface->string_new_with_wide_chars #} ptr)
    stringNewWithLatin1CharsAndLen                    <- (mkStringNewWithLatin1CharsAndLen                <$> {#get GDNativeInterface->string_new_with_latin1_chars_and_len #} ptr)
    stringNewWithUtf8CharsAndLen                      <- (mkStringNewWithUtf8CharsAndLen                  <$> {#get GDNativeInterface->string_new_with_utf8_chars_and_len #} ptr)
    stringNewWithUtf16CharsAndLen                     <- (mkStringNewWithUtf16CharsAndLen                 <$> {#get GDNativeInterface->string_new_with_utf16_chars_and_len #} ptr)
    stringNewWithUtf32CharsAndLen                     <- (mkStringNewWithUtf32CharsAndLen                 <$> {#get GDNativeInterface->string_new_with_utf32_chars_and_len #} ptr)
    stringNewWithWideCharsAndLen                      <- (mkStringNewWithWideCharsAndLen                  <$> {#get GDNativeInterface->string_new_with_wide_chars_and_len #} ptr)
    
    stringToLatin1Chars                               <- (mkStringToLatin1Chars                           <$> {#get GDNativeInterface->string_to_latin1_chars #} ptr)
    stringToUtf8Chars                                 <- (mkStringToUtf8Chars                             <$> {#get GDNativeInterface->string_to_utf8_chars #} ptr)
    stringToUtf16Chars                                <- (mkStringToUtf16Chars                            <$> {#get GDNativeInterface->string_to_utf16_chars #} ptr)
    stringToUtf32Chars                                <- (mkStringToUtf32Chars                            <$> {#get GDNativeInterface->string_to_utf32_chars #} ptr)
    stringToWideChars                                 <- (mkStringToWideChars                             <$> {#get GDNativeInterface->string_to_wide_chars #} ptr)
    stringOperatorIndex                               <- (mkStringOperatorIndex                           <$> {#get GDNativeInterface->string_operator_index #} ptr)
    stringOperatorIndexConst                          <- (mkStringOperatorIndexConst                      <$> {#get GDNativeInterface->string_operator_index_const #} ptr)

    packedByteArrayOperatorIndex                      <- (mkPackedByteArrayOperatorIndex                  <$> {#get GDNativeInterface->packed_byte_array_operator_index #} ptr)
    packedByteArrayOperatorIndexConst                 <- (mkPackedByteArrayOperatorIndexConst             <$> {#get GDNativeInterface->packed_byte_array_operator_index_const #} ptr)

    packedColorArrayOperatorIndex                     <- (mkPackedColorArrayOperatorIndex                 <$> {#get GDNativeInterface->packed_color_array_operator_index #} ptr)
    packedColorArrayOperatorIndexConst                <- (mkPackedColorArrayOperatorIndexConst            <$> {#get GDNativeInterface->packed_color_array_operator_index_const #} ptr)

    packedFloat32ArrayOperatorIndex                   <- (mkPackedFloat32ArrayOperatorIndex               <$> {#get GDNativeInterface->packed_float32_array_operator_index #} ptr)
    packedFloat32ArrayOperatorIndexConst              <- (mkPackedFloat32ArrayOperatorIndexConst          <$> {#get GDNativeInterface->packed_float32_array_operator_index_const #} ptr)
    packedFloat64ArrayOperatorIndex                   <- (mkPackedFloat64ArrayOperatorIndex               <$> {#get GDNativeInterface->packed_float64_array_operator_index #} ptr)
    packedFloat64ArrayOperatorIndexConst              <- (mkPackedFloat64ArrayOperatorIndexConst          <$> {#get GDNativeInterface->packed_float64_array_operator_index_const #} ptr)
  
    packedInt32ArrayOperatorIndex                     <- (mkPackedInt32ArrayOperatorIndex                 <$> {#get GDNativeInterface->packed_int32_array_operator_index #} ptr)
    packedInt32ArrayOperatorIndexConst                <- (mkPackedInt32ArrayOperatorIndexConst            <$> {#get GDNativeInterface->packed_int32_array_operator_index_const #} ptr)
    packedInt64ArrayOperatorIndex                     <- (mkPackedInt64ArrayOperatorIndex                 <$> {#get GDNativeInterface->packed_int64_array_operator_index #} ptr)
    packedInt64ArrayOperatorIndexConst                <- (mkPackedInt64ArrayOperatorIndexConst            <$> {#get GDNativeInterface->packed_int64_array_operator_index_const #} ptr)
  
    packedStringArrayOperatorIndex                    <- (mkPackedStringArrayOperatorIndex                <$> {#get GDNativeInterface->packed_string_array_operator_index #} ptr)
    packedStringArrayOperatorIndexConst               <- (mkPackedStringArrayOperatorIndexConst           <$> {#get GDNativeInterface->packed_string_array_operator_index_const #} ptr)
  
    packedVector2ArrayOperatorIndex                   <- (mkPackedVector2ArrayOperatorIndex               <$> {#get GDNativeInterface->packed_vector2_array_operator_index #} ptr)
    packedVector2ArrayOperatorIndexConst              <- (mkPackedVector2ArrayOperatorIndexConst          <$> {#get GDNativeInterface->packed_vector2_array_operator_index_const #} ptr)
    packedVector3ArrayOperatorIndex                   <- (mkPackedVector3ArrayOperatorIndex               <$> {#get GDNativeInterface->packed_vector3_array_operator_index #} ptr)
    packedVector3ArrayOperatorIndexConst              <- (mkPackedVector3ArrayOperatorIndexConst          <$> {#get GDNativeInterface->packed_vector3_array_operator_index_const #} ptr)
  
    arrayOperatorIndex                                <- (mkArrayOperatorIndex                            <$> {#get GDNativeInterface->array_operator_index #} ptr)
    arrayOperatorIndexConst                           <- (mkArrayOperatorIndexConst                       <$> {#get GDNativeInterface->array_operator_index_const #} ptr)
  
    dictionaryOperatorIndex                           <- (mkDictionaryOperatorIndex                       <$> {#get GDNativeInterface->dictionary_operator_index #} ptr)
    dictionaryOperatorIndexConst                      <- (mkDictionaryOperatorIndexConst                  <$> {#get GDNativeInterface->dictionary_operator_index_const #} ptr)
    
    objectMethodBindCall                              <- (mkObjectMethodBindCall                          <$> {#get GDNativeInterface->object_method_bind_call #} ptr)
    objectMethodBindPtrcall                           <- (mkObjectMethodBindPtrcall                       <$> {#get GDNativeInterface->object_method_bind_ptrcall #} ptr)
    objectDestroy                                     <- (mkObjectDestroy                                 <$> {#get GDNativeInterface->object_destroy #} ptr)
    globalGetSingleton                                <- (mkGlobalGetSingleton                            <$> {#get GDNativeInterface->global_get_singleton #} ptr)

    objectGetInstanceBinding                          <- (mkObjectGetInstanceBinding                      <$> {#get GDNativeInterface->object_get_instance_binding #} ptr)
    objectSetInstanceBinding                          <- (mkObjectSetInstanceBinding                      <$> {#get GDNativeInterface->object_set_instance_binding #} ptr)

    objectSetInstance                                 <- (mkObjectSetInstance                             <$> {#get GDNativeInterface->object_set_instance #} ptr)
  
    objectCastTo                                      <- (mkObjectCastTo                                  <$> {#get GDNativeInterface->object_cast_to #} ptr)
    objectGetInstanceFromId                           <- (mkObjectGetInstanceFromId                       <$> {#get GDNativeInterface->object_get_instance_from_id #} ptr)

    classdbConstructObject                            <- (mkClassdbConstructObject                        <$> {#get GDNativeInterface->classdb_construct_object #} ptr)
    classdbGetMethodBind                              <- (mkClassdbGetMethodBind                          <$> {#get GDNativeInterface->classdb_get_method_bind #} ptr)
    classdbGetClassTag                                <- (mkClassdbGetClassTag                            <$> {#get GDNativeInterface->classdb_get_class_tag #} ptr)
  
    classdbRegisterExtensionClass                     <- (mkClassdbRegisterExtensionClass                 <$> {#get GDNativeInterface->classdb_register_extension_class #} ptr)
    classdbRegisterExtensionClassMethod               <- (mkClassdbRegisterExtensionClassMethod           <$> {#get GDNativeInterface->classdb_register_extension_class_method #} ptr)
    classdbRegisterExtensionClassIntegerConstant      <- (mkClassdbRegisterExtensionClassIntegerConstant  <$> {#get GDNativeInterface->classdb_register_extension_class_integer_constant #} ptr)
    classdbRegisterExtensionClassProperty             <- (mkClassdbRegisterExtensionClassProperty         <$> {#get GDNativeInterface->classdb_register_extension_class_property #} ptr)
    classdbRegisterExtensionClassPropertyGroup        <- (mkClassdbRegisterExtensionClassPropertyGroup    <$> {#get GDNativeInterface->classdb_register_extension_class_property_group #} ptr)
    classdbRegisterExtensionClassPropertySubgroup     <- (mkClassdbRegisterExtensionClassPropertySubgroup <$> {#get GDNativeInterface->classdb_register_extension_class_property_subgroup #} ptr)
    classdbRegisterExtensionClassSignal               <- (mkClassdbRegisterExtensionClassSignal           <$> {#get GDNativeInterface->classdb_register_extension_class_signal #} ptr)
    classdbUnregisterExtensionClass                   <- (mkClassdbUnregisterExtensionClass               <$> {#get GDNativeInterface->classdb_unregister_extension_class #} ptr)

    pure $ GdnativeInterface
      { versionMajor                                  = versionMajor
      , versionMinor                                  = versionMinor
      , versionPatch                                  = versionPatch
      , versionString                                 = versionString

      , memAlloc                                      = memAlloc
      , memRealloc                                    = memRealloc
      , memFree                                       = memFree

      , printError                                    = printError
      , printWarning                                  = printWarning
      , printScriptError                              = printScriptError
    
      , variantNewCopy                                = variantNewCopy
      , variantNewNil                                 = variantNewNil
      , variantDestroy                                = variantDestroy
    
      , variantCall                                   = variantCall
      , variantCallStatic                             = variantCallStatic
      , variantEvaluate                               = variantEvaluate
      , variantSet                                    = variantSet
      , variantSetNamed                               = variantSetNamed
      , variantSetKeyed                               = variantSetKeyed
      , variantSetIndexed                             = variantSetIndexed
      , variantGet                                    = variantGet
      , variantGetNamed                               = variantGetNamed
      , variantGetKeyed                               = variantGetKeyed
      , variantGetIndexed                             = variantGetIndexed
      , variantIterInit                               = variantIterInit
      , variantIterNext                               = variantIterNext
      , variantIterGet                                = variantIterGet
      , variantHashCompare                            = variantHashCompare
      , variantBooleanize                             = variantBooleanize
      , variantBlend                                  = variantBlend
      , variantInterpolate                            = variantInterpolate
      , variantDuplicate                              = variantDuplicate
      , variantStringify                              = variantStringify
    
      , variantGetType                                = variantGetType
      , variantHasMethod                              = variantHasMethod
      , variantHasMember                              = variantHasMember
      , variantHasKey                                 = variantHasKey
      , variantGetTypeName                            = variantGetTypeName
      , variantCanConvert                             = variantCanConvert
      , variantCanConvertStrict                       = variantCanConvertStrict
    
      , getVariantFromTypeConstructor                 = getVariantFromTypeConstructor
      , getVariantToTypeConstructor                   = getVariantToTypeConstructor
      , variantGetPtrOperatorEvaluator                = variantGetPtrOperatorEvaluator
      , variantGetPtrBuiltinMethod                    = variantGetPtrBuiltinMethod
      , variantGetPtrConstructor                      = variantGetPtrConstructor
      , variantGetPtrDestructor                       = variantGetPtrDestructor
      , variantConstruct                              = variantConstruct
      , variantGetPtrSetter                           = variantGetPtrSetter
      , variantGetPtrGetter                           = variantGetPtrGetter
      , variantGetPtrIndexedSetter                    = variantGetPtrIndexedSetter
      , variantGetPtrIndexedGetter                    = variantGetPtrIndexedGetter
      , variantGetPtrKeyedSetter                      = variantGetPtrKeyedSetter
      , variantGetPtrKeyedGetter                      = variantGetPtrKeyedGetter
      , variantGetPtrKeyedChecker                     = variantGetPtrKeyedChecker
      , variantGetConstantValue                       = variantGetConstantValue
      , variantGetPtrUtilityFunction                  = variantGetPtrUtilityFunction

      , stringNewWithLatin1Chars                      = stringNewWithLatin1Chars
      , stringNewWithUtf8Chars                        = stringNewWithUtf8Chars
      , stringNewWithUtf16Chars                       = stringNewWithUtf16Chars
      , stringNewWithUtf32Chars                       = stringNewWithUtf32Chars
      , stringNewWithWideChars                        = stringNewWithWideChars
      , stringNewWithLatin1CharsAndLen                = stringNewWithLatin1CharsAndLen
      , stringNewWithUtf8CharsAndLen                  = stringNewWithUtf8CharsAndLen
      , stringNewWithUtf16CharsAndLen                 = stringNewWithUtf16CharsAndLen
      , stringNewWithUtf32CharsAndLen                 = stringNewWithUtf32CharsAndLen
      , stringNewWithWideCharsAndLen                  = stringNewWithWideCharsAndLen
        
      , stringToLatin1Chars                           = stringToLatin1Chars
      , stringToUtf8Chars                             = stringToUtf8Chars
      , stringToUtf16Chars                            = stringToUtf16Chars
      , stringToUtf32Chars                            = stringToUtf32Chars
      , stringToWideChars                             = stringToWideChars
      , stringOperatorIndex                           = stringOperatorIndex
      , stringOperatorIndexConst                      = stringOperatorIndexConst

      , packedByteArrayOperatorIndex                  = packedByteArrayOperatorIndex
      , packedByteArrayOperatorIndexConst             = packedByteArrayOperatorIndexConst

      , packedColorArrayOperatorIndex                 = packedColorArrayOperatorIndex
      , packedColorArrayOperatorIndexConst            = packedColorArrayOperatorIndexConst

      , packedFloat32ArrayOperatorIndex               = packedFloat32ArrayOperatorIndex
      , packedFloat32ArrayOperatorIndexConst          = packedFloat32ArrayOperatorIndexConst
      , packedFloat64ArrayOperatorIndex               = packedFloat64ArrayOperatorIndex
      , packedFloat64ArrayOperatorIndexConst          = packedFloat64ArrayOperatorIndexConst
    
      , packedInt32ArrayOperatorIndex                 = packedInt32ArrayOperatorIndex
      , packedInt32ArrayOperatorIndexConst            = packedInt32ArrayOperatorIndexConst
      , packedInt64ArrayOperatorIndex                 = packedInt64ArrayOperatorIndex
      , packedInt64ArrayOperatorIndexConst            = packedInt64ArrayOperatorIndexConst
    
      , packedStringArrayOperatorIndex                = packedStringArrayOperatorIndex
      , packedStringArrayOperatorIndexConst           = packedStringArrayOperatorIndexConst
    
      , packedVector2ArrayOperatorIndex               = packedVector2ArrayOperatorIndex
      , packedVector2ArrayOperatorIndexConst          = packedVector2ArrayOperatorIndexConst
      , packedVector3ArrayOperatorIndex               = packedVector3ArrayOperatorIndex
      , packedVector3ArrayOperatorIndexConst          = packedVector3ArrayOperatorIndexConst
    
      , arrayOperatorIndex                            = arrayOperatorIndex
      , arrayOperatorIndexConst                       = arrayOperatorIndexConst
    
      , dictionaryOperatorIndex                       = dictionaryOperatorIndex
      , dictionaryOperatorIndexConst                  = dictionaryOperatorIndexConst
       
      , objectMethodBindCall                          = objectMethodBindCall
      , objectMethodBindPtrcall                       = objectMethodBindPtrcall
      , objectDestroy                                 = objectDestroy
      , globalGetSingleton                            = globalGetSingleton

      , objectGetInstanceBinding                      = objectGetInstanceBinding
      , objectSetInstanceBinding                      = objectSetInstanceBinding

      , objectSetInstance                             = objectSetInstance
    
      , objectCastTo                                  = objectCastTo
      , objectGetInstanceFromId                       = objectGetInstanceFromId

      , classdbConstructObject                        = classdbConstructObject
      , classdbGetMethodBind                          = classdbGetMethodBind
      , classdbGetClassTag                            = classdbGetClassTag
    
      , classdbRegisterExtensionClass                 = classdbRegisterExtensionClass
      , classdbRegisterExtensionClassMethod           = classdbRegisterExtensionClassMethod
      , classdbRegisterExtensionClassIntegerConstant  = classdbRegisterExtensionClassIntegerConstant
      , classdbRegisterExtensionClassProperty         = classdbRegisterExtensionClassProperty
      , classdbRegisterExtensionClassPropertyGroup    = classdbRegisterExtensionClassPropertyGroup
      , classdbRegisterExtensionClassPropertySubgroup = classdbRegisterExtensionClassPropertySubgroup
      , classdbRegisterExtensionClassSignal           = classdbRegisterExtensionClassSignal
      , classdbUnregisterExtensionClass               = classdbUnregisterExtensionClass
      }
  poke _ _ = error "don't poke GdnativeInterface"

{#pointer *GDNativeInterface as GdnativeInterfacePtr -> GdnativeInterface #}

{#enum GDNativeInitializationLevel as GdnativeInitializationLevel {underscoreToCase}
  deriving (Show, Eq, Ord, Bounded) #}

type Initialize = Ptr () -> CEnum GdnativeInitializationLevel -> IO ()
foreign import ccall "dynamic" mkInitialize :: FunPtr Initialize -> Initialize
type Deinitialize = Ptr () -> CEnum GdnativeInitializationLevel -> IO ()
foreign import ccall "dynamic" mkDeinitialize :: FunPtr Initialize -> Initialize

data GdnativeInitialization = GdnativeInitialization
  { minimumInitializationLevel :: GdnativeInitializationLevel
  , userdata     :: Ptr ()
  , initialize   :: Initialize 
  , deinitialize :: Deinitialize 
  }

instance Storable GdnativeInitialization where
  sizeOf _ = {#sizeof GDNativeInitialization #}
  alignment _ = {#alignof GDNativeInitialization #}
  peek ptr =
    GdnativeInitialization
      <$> (toEnum . from <$> {#get GDNativeInitialization->minimum_initialization_level #} ptr)
      <*> {#get GDNativeInitialization->userdata #} ptr
      <*> (mkInitialize   . coerce <$> {#get GDNativeInitialization->initialize #} ptr)
      <*> (mkDeinitialize . coerce <$> {#get GDNativeInitialization->deinitialize #} ptr)

{#pointer *GDNativeInitialization as GdnativeInitializationPtr -> GdnativeInitialization #}

type GdnativeInitializationFunction = Ptr GdnativeInterface -> GdnativeExtensionClassLibraryPtr -> CEnum GdnativeInitialization -> IO ()
foreign import ccall "dynamic" mkGdnativeInitializationFunction
  :: FunPtr GdnativeInitializationFunction
  -> GdnativeInitializationFunction

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

