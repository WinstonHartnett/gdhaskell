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

type MemAlloc = CSize -> IO (Ptr ())
foreign import ccall "dynamic" mkMemAlloc :: FunPtr MemAlloc -> MemAlloc
type MemRealloc = Ptr () -> CSize -> IO (Ptr ())
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
type VariantIterInit = GdnativeVariantPtr -> GdnativeInt -> GdnativeVariantPtr -> Ptr GdnativeBool -> IO GdnativeBool
foreign import ccall "dynamic" mkVariantIterInit :: FunPtr VariantIterInit -> VariantIterInit
type VariantIterNext = GdnativeVariantPtr -> GdnativeInt -> GdnativeVariantPtr -> Ptr GdnativeBool -> IO GdnativeBool
foreign import ccall "dynamic" mkVariantIterNext :: FunPtr VariantIterNext -> VariantIterNext
type VariantIterGet = GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr GdnativeBool -> IO ()
foreign import ccall "dynamic" mkVariantIterGet :: FunPtr VariantIterGet -> VariantIterGet
type VariantHashCompare = GdnativeVariantPtr -> GdnativeVariantPtr -> IO GdnativeBool
foreign import ccall "dynamic" mkVariantHashCompare :: FunPtr VariantHashCompare -> VariantHashCompare
type VariantBooleanize = GdnativeVariantPtr -> IO GdnativeBool
foreign import ccall "dynamic" mkVariantBooleanize :: FunPtr VariantBooleanize -> VariantBooleanize
type VariantBlend = GdnativeVariantPtr -> GdnativeVariantPtr -> CFloat -> GdnativeVariantPtr -> IO ()
foreign import ccall "dynamic" mkVariantBlend :: FunPtr VariantBlend -> VariantBlend
type VariantInterpolate = GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeVariantPtr -> IO ()
foreign import ccall "dynamic" mkVariantInterpolate :: FunPtr VariantInterpolate -> VariantInterpolate
type VariantDuplicate = GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeBool -> IO ()
foreign import ccall "dynamic" mkVariantDuplicate :: FunPtr VariantDuplicate -> VariantDuplicate
type VariantStringify = GdnativeVariantPtr -> GdnativeStringPtr -> IO ()
foreign import ccall "dynamic" mkVariantStringify :: FunPtr VariantStringify -> VariantStringify

type VariantGetType = GdnativeVariantPtr -> IO (CEnum GdnativeVariantType)
foreign import ccall "dynamic" mkVariantGetType :: FunPtr VariantGetType -> VariantGetType
type VariantHasMethod = GdnativeVariantPtr -> GdnativeStringNamePtr -> IO GdnativeBool
foreign import ccall "dynamic" mkVariantHasMethod :: FunPtr VariantHasMethod -> VariantHasMethod
type VariantHasMember = GdnativeVariantPtr -> GdnativeStringNamePtr -> IO GdnativeBool
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
type VariantPtrSetter = CEnum GdnativeVariantType -> CString -> IO (FunPtr GdnativePtrSetter)
foreign import ccall "dynamic" mkVariantPtrSetter :: FunPtr VariantPtrSetter -> VariantPtrSetter
type VariantPtrGetter = CEnum GdnativeVariantType -> CString -> IO (FunPtr GdnativePtrGetter)
foreign import ccall "dynamic" mkVariantPtrGetter :: FunPtr VariantPtrGetter -> VariantPtrGetter
type VariantPtrIndexedSetter = CEnum GdnativeVariantType -> IO (FunPtr GdnativePtrIndexedSetter)
foreign import ccall "dynamic" mkVariantPtrIndexedSetter :: FunPtr VariantPtrIndexedSetter -> VariantPtrIndexedSetter
type VariantPtrIndexedGetter = CEnum GdnativeVariantType -> IO (FunPtr GdnativePtrIndexedGetter)
foreign import ccall "dynamic" mkVariantPtrIndexedGetter :: FunPtr VariantPtrIndexedGetter -> VariantPtrIndexedGetter
type VariantPtrKeyedSetter = CEnum GdnativeVariantType -> IO (FunPtr GdnativePtrKeyedSetter)
foreign import ccall "dynamic" mkVariantPtrKeyedSetter :: FunPtr VariantPtrKeyedSetter -> VariantPtrKeyedSetter
type VariantPtrKeyedGetter = CEnum GdnativeVariantType -> IO (FunPtr GdnativePtrKeyedGetter)
foreign import ccall "dynamic" mkVariantPtrKeyedGetter :: FunPtr VariantPtrKeyedGetter -> VariantPtrKeyedGetter
type VariantPtrKeyedChecker = CEnum GdnativeVariantType -> IO (FunPtr GdnativePtrKeyedChecker)
foreign import ccall "dynamic" mkVariantPtrKeyedChecker :: FunPtr VariantPtrKeyedChecker -> VariantPtrKeyedChecker
type VariantGetConstantValue = CEnum GdnativeVariantType -> CString -> GdnativeVariantPtr -> IO ()
foreign import ccall "dynamic" mkVariantGetConstantValue :: FunPtr VariantGetConstantValue -> VariantGetConstantValue
type VariantGetPtrUtilityFunction = CString -> GdnativeInt -> IO (FunPtr GdnativePtrUtilityFunction)
foreign import ccall "dynamic" mkVariantGetPtrUtilityFunction :: FunPtr VariantGetPtrUtilityFunction -> VariantGetPtrUtilityFunction

type StringNewWithLatin1Chars = GdnativeStringPtr -> CString -> IO ()
foreign import ccall "dynamic" mkStringNewWithLatin1Chars :: FunPtr StringNewWithLatin1Chars -> StringNewWithLatin1Chars
type StringNewWithUtf8Chars = GdnativeStringPtr -> CString -> IO ()
foreign import ccall "dynamic" mkStringNewWithUtf8Chars :: FunPtr StringNewWithUtf8Chars -> StringNewWithUtf8Chars
type StringNewWithUtf16Chars = GdnativeStringPtr -> Ptr CShort -> IO ()
foreign import ccall "dynamic" mkStringNewWithUtf16Chars :: FunPtr StringNewWithUtf16Chars -> StringNewWithUtf16Chars
type StringNewWithUtf32Chars = GdnativeStringPtr -> Ptr CInt -> IO ()
foreign import ccall "dynamic" mkStringNewWithUtf32Chars :: FunPtr StringNewWithUtf32Chars -> StringNewWithUtf32Chars
type StringNewWithWideChars = GdnativeStringPtr -> Ptr CWchar -> IO ()
foreign import ccall "dynamic" mkStringNewWithWideChars :: FunPtr StringNewWithWideChars -> StringNewWithWideChars
type StringNewWithLatin1CharsAndLen = GdnativeStringPtr -> CString -> GdnativeInt -> IO ()
foreign import ccall "dynamic" mkStringNewWithLatin1CharsAndLen :: FunPtr StringNewWithLatin1CharsAndLen -> StringNewWithLatin1CharsAndLen
type StringNewWithUtf8CharsAndLen = GdnativeStringPtr -> CString -> GdnativeInt -> IO ()
foreign import ccall "dynamic" mkStringNewWithUtf8CharsAndLen :: FunPtr StringNewWithUtf8CharsAndLen -> StringNewWithUtf8CharsAndLen
type StringNewWithUtf16CharsAndLen = GdnativeStringPtr -> Ptr CShort -> GdnativeInt -> IO ()
foreign import ccall "dynamic" mkStringNewWithUtf16CharsAndLen :: FunPtr StringNewWithUtf16CharsAndLen -> StringNewWithUtf16CharsAndLen
type StringNewWithUtf32CharsAndLen = GdnativeStringPtr -> Ptr CInt -> GdnativeInt -> IO ()
foreign import ccall "dynamic" mkStringNewWithUtf32CharsAndLen :: FunPtr StringNewWithUtf32CharsAndLen -> StringNewWithUtf32CharsAndLen
type StringNewWithWideCharsAndLen = GdnativeStringPtr -> Ptr CWchar -> GdnativeInt -> IO ()
foreign import ccall "dynamic" mkStringNewWithWideCharsAndLen :: FunPtr StringNewWithWideCharsAndLen -> StringNewWithWideCharsAndLen

type StringToLatin1Chars = GdnativeStringPtr -> CString -> GdnativeInt -> IO GdnativeInt
foreign import ccall "dynamic" mkStringToLatin1Chars :: FunPtr StringToLatin1Chars -> StringToLatin1Chars
type StringToUtf8Chars = GdnativeStringPtr -> CString -> GdnativeInt -> IO GdnativeInt
foreign import ccall "dynamic" mkStringToUtf8Chars :: FunPtr StringToUtf8Chars -> StringToUtf8Chars
type StringToUtf16Chars = GdnativeStringPtr -> Ptr CShort -> GdnativeInt -> IO GdnativeInt
foreign import ccall "dynamic" mkStringToUtf16Chars :: FunPtr StringToUtf16Chars -> StringToUtf16Chars
type StringToUtf32Chars = GdnativeStringPtr -> Ptr CInt -> GdnativeInt -> IO GdnativeInt
foreign import ccall "dynamic" mkStringToUtf32Chars :: FunPtr StringToUtf32Chars -> StringToUtf32Chars
type StringToWideChars = GdnativeStringPtr -> Ptr CWchar -> GdnativeInt -> IO GdnativeInt
foreign import ccall "dynamic" mkStringToWideChars :: FunPtr StringToWideChars -> StringToWideChars
type StringOperatorIndex = GdnativeStringPtr -> GdnativeInt -> IO (Ptr CInt)
foreign import ccall "dynamic" mkStringOperatorIndex :: FunPtr StringOperatorIndex -> StringOperatorIndex
type StringOperatorIndexConst = GdnativeStringPtr -> GdnativeInt -> IO (Ptr CInt)
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

type DictionaryOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO GdnativeVariantPtr
foreign import ccall "dynamic" mkDictionaryOperatorIndex :: FunPtr DictionaryOperatorIndex -> DictionaryOperatorIndex
type DictionaryOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO GdnativeVariantPtr
foreign import ccall "dynamic" mkDictionaryOperatorIndexConst :: FunPtr DictionaryOperatorIndexConst -> DictionaryOperatorIndexConst

type ObjectMethodBindCall = GdnativeMethodBindPtr -> GdnativeObjectPtr -> Ptr GdnativeVariantPtr -> GdnativeInt -> GdnativeVariantPtr -> Ptr GdnativeCallError -> IO ()
foreign import ccall "dynamic" mkObjectMethodBindCall :: FunPtr ObjectMethodBindCall -> ObjectMethodBindCall
type ObjectMethodBindPtrcall = GdnativeMethodBindPtr -> GdnativeObjectPtr -> GdnativeTypePtr -> GdnativeTypePtr -> IO ()
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

type ClassDbConstructObject = CString -> IO GdnativeObjectPtr
foreign import ccall "dynamic" mkClassDbConstructObject :: FunPtr ClassDbConstructObject -> ClassDbConstructObject
type ClassDbGetMethodBind = CString -> CString -> GdnativeInt -> IO GdnativeMethodBindPtr
foreign import ccall "dynamic" mkClassDbGetMethodBind :: FunPtr ClassDbGetMethodBind -> ClassDbGetMethodBind
type ClassDbGetClassTag = CString -> IO (Ptr ())
foreign import ccall "dynamic" mkClassDbGetClassTag :: FunPtr ClassDbGetClassTag -> ClassDbGetClassTag

type ClassDbRegisterExtensionClass = GdnativeExtensionClassLibraryPtr -> CString -> CString -> Ptr GdnativeExtensionClassCreationInfo -> IO ()
foreign import ccall "dynamic" mkClassDbRegisterExtensionClass :: FunPtr ClassDbRegisterExtensionClass -> ClassDbRegisterExtensionClass
type ClassDbRegisterExtensionClassMethod = GdnativeExtensionClassLibraryPtr -> CString -> Ptr GdnativeExtensionClassMethodInfo -> IO ()
foreign import ccall "dynamic" mkClassDbRegisterExtensionClassMethod :: FunPtr ClassDbRegisterExtensionClassMethod -> ClassDbRegisterExtensionClassMethod
type ClassDbRegisterExtensionClassIntegerConstant = GdnativeExtensionClassLibraryPtr -> CString -> CString -> CString -> GdnativeInt -> IO ()
foreign import ccall "dynamic" mkClassDbRegisterExtensionClassIntegerConstant :: FunPtr ClassDbRegisterExtensionClassIntegerConstant -> ClassDbRegisterExtensionClassIntegerConstant
type ClassDbRegisterExtensionClassProperty = GdnativeExtensionClassLibraryPtr -> CString -> Ptr GdnativePropertyInfo -> CString -> CString -> IO ()
foreign import ccall "dynamic" mkClassDbRegisterExtensionClassProperty :: FunPtr ClassDbRegisterExtensionClassProperty -> ClassDbRegisterExtensionClassProperty
type ClassDbRegisterExtensionClassPropertyGroup = GdnativeExtensionClassLibraryPtr -> CString -> CString -> CString -> IO ()
foreign import ccall "dynamic" mkClassDbRegisterExtensionClassPropertyGroup :: FunPtr ClassDbRegisterExtensionClassPropertyGroup -> ClassDbRegisterExtensionClassPropertyGroup
type ClassDbRegisterExtensionClassPropertySubgroup = GdnativeExtensionClassLibraryPtr -> CString -> CString -> CString -> IO ()
foreign import ccall "dynamic" mkClassDbRegisterExtensionClassPropertySubgroup :: FunPtr ClassDbRegisterExtensionClassPropertySubgroup -> ClassDbRegisterExtensionClassPropertySubgroup
type ClassDbRegisterExtensionClassSignal = GdnativeExtensionClassLibraryPtr -> CString -> CString -> Ptr GdnativePropertyInfo -> GdnativeInt -> IO ()
foreign import ccall "dynamic" mkClassDbRegisterExtensionClassSignal :: FunPtr ClassDbRegisterExtensionClassSignal -> ClassDbRegisterExtensionClassSignal
type ClassDbUnregisterExtensionClass = GdnativeExtensionClassLibraryPtr -> CString -> IO ()
foreign import ccall "dynamic" mkClassDbUnregisterExtensionClass :: FunPtr ClassDbUnregisterExtensionClass -> ClassDbUnregisterExtensionClass

data GdnativeInterface = GdnativeInterface
  { versionMajor     :: CUInt
  , versionMinor     :: CUInt
  , versionPatch     :: CUInt
  , versionString    :: CString

  , memAlloc         :: MemAlloc
  , memRealloc       :: MemRealloc
  , memFree          :: MemFree

  , printError       :: PrintError
  , printWarning     :: PrintWarning
  , printScriptError :: PrintScriptError
  
  , variantNewCopy   :: VariantNewCopy
  , variantNewNil    :: VariantNewNil
  , variantDestroy   :: VariantDestroy
  
  , variantCall       :: VariantCall
  , variantCallStatic :: VariantCallStatic
  , variantEvaluate   :: VariantEvaluate
  , variantSet        :: VariantSet
  , variantSetNamed   :: VariantSetNamed
  , variantSetKeyed   :: VariantSetKeyed
  , variantSetIndexed :: VariantSetIndexed
  , variantGet :: VariantGet
  , variantGetNamed :: VariantGetNamed
  , variantGetKeyed :: VariantGetKeyed
  , variantGetIndexed :: VariantGetIndexed
  , variantIterInit :: VariantIterInit
  , variantIterNext :: VariantIterNext
  , variantIterGet :: VariantIterGet
  , variantHashCompare :: VariantHashCompare
  , variantBooleanize :: VariantBooleanize
  , variantBlend :: VariantBlend
  , variantInterpolate :: VariantInterpolate
  , variantDuplicate :: VariantDuplicate
  , variantStringify :: VariantStringify
  
  , variantGetType :: VariantGetType
  , variantHasMethod :: VariantHasMethod
  , variantHasMember :: VariantHasMember
  , variantHasKey :: VariantHasKey
  , variantGetTypeName :: VariantGetTypeName
  , variantCanConvert :: VariantCanConvert
  , variantCanConvertStrict :: VariantCanConvertStrict

  , getVariantFromTypeConstructor  :: GetVariantFromTypeConstructor
  , getVariantToTypeConstructor    :: GetVariantToTypeConstructor
  , variantGetPtrOperatorEvaluator :: VariantGetPtrOperatorEvaluator
  , variantGetPtrBuiltinMethod     :: VariantGetPtrBuiltinMethod
  , variantGetPtrConstructor       :: VariantGetPtrConstructor
  , variantGetPtrDestructor        :: VariantGetPtrDestructor
  , variantConstruct               :: VariantConstruct
  , variantGetPtrSetter            :: VariantPtrSetter
  , variantGetPtrGetter            :: VariantPtrGetter
  , variantGetPtrIndexedSetter     :: VariantPtrIndexedSetter
  , variantGetPtrIndexedGetter     :: VariantPtrIndexedGetter
  , variantGetPtrKeyedSetter       :: VariantPtrKeyedSetter
  , variantGetPtrKeyedGetter       :: VariantPtrKeyedGetter
  , variantGetPtrKeyedChecker      :: VariantPtrKeyedChecker
  , variantGetConstantValue        :: VariantGetConstantValue
  , variantGetPtrUtilityFunction   :: VariantGetPtrUtilityFunction

  , stringNewWithLatin1Chars :: StringNewWithLatin1Chars
  , stringNewWithUtf8Chars   :: StringNewWithUtf8Chars
  , stringNewWithUtf16Chars  :: StringNewWithUtf16Chars
  , stringNewWithUtf32Chars  :: StringNewWithUtf32Chars
  , stringNewWithWideChars   :: StringNewWithWideChars
  , stringNewWithLatin1CharsAndLen :: StringNewWithLatin1CharsAndLen
  , stringNewWithUtf8CharsAndLen :: StringNewWithUtf8CharsAndLen
  , stringNewWithUtf16CharsAndLen :: StringNewWithUtf16CharsAndLen
  , stringNewWithUtf32CharsAndLen :: StringNewWithUtf32CharsAndLen
  , stringNewWithWideCharsAndLen :: StringNewWithWideCharsAndLen
    
  , stringToLatin1Chars :: StringToLatin1Chars
  , stringToUtf8Chars :: StringToUtf8Chars
  , stringToUtf16Chars :: StringToUtf16Chars
  , stringToUtf32Chars :: StringToUtf32Chars
  , stringToWideChars :: StringToWideChars
  , stringOperatorIndex :: StringOperatorIndex
  , stringOperatorIndexConst :: StringOperatorIndexConst

  , packedByteArrayOperatorIndex :: PackedByteArrayOperatorIndex
  , packedByteArrayOperatorIndexConst :: PackedByteArrayOperatorIndexConst

  , packedColorArrayOperatorIndex :: PackedColorArrayOperatorIndex
  , packedColorArrayOperatorIndexConst :: PackedColorArrayOperatorIndexConst

  , packedFloat32ArrayOperatorIndex :: PackedFloat32ArrayOperatorIndex
  , packedFloat32ArrayOperatorIndexConst :: PackedFloat32ArrayOperatorIndexConst
  , packedFloat64ArrayOperatorIndex :: PackedFloat64ArrayOperatorIndex
  , packedFloat64ArrayOperatorIndexConst :: PackedFloat64ArrayOperatorIndexConst
  
  , packedInt32ArrayOperatorIndex :: PackedInt32ArrayOperatorIndex
  , packedInt32ArrayOperatorIndexConst :: PackedInt32ArrayOperatorIndexConst
  , packedInt64ArrayOperatorIndex :: PackedInt64ArrayOperatorIndex
  , packedInt64ArrayOperatorIndexConst :: PackedInt64ArrayOperatorIndexConst
  
  , packedStringArrayOperatorIndex :: PackedStringArrayOperatorIndex
  , packedStringArrayOperatorIndexConst :: PackedStringArrayOperatorIndexConst
  
  , packedVector2ArrayOperatorIndex :: PackedVector2ArrayOperatorIndex
  , packedVector2ArrayOperatorIndexConst :: PackedVector2ArrayOperatorIndexConst
  , packedVector3ArrayOperatorIndex :: PackedVector3ArrayOperatorIndex
  , packedVector3ArrayOperatorIndexConst :: PackedVector3ArrayOperatorIndexConst
  
  , arrayOperatorIndex      :: ArrayOperatorIndex
  , arrayOperatorIndexConst :: ArrayOperatorIndexConst
  
  , dictionaryOperatorIndex      :: DictionaryOperatorIndex
  , dictionaryOperatorIndexConst :: DictionaryOperatorIndexConst
    
  , objectMethodBindCall :: ObjectMethodBindCall
  , objectMethodBindPtrcall :: ObjectMethodBindPtrcall
  , objectDestroy :: ObjectDestroy
  , globalGetSingleton :: GlobalGetSingleton

  , objectGetInstanceBinding :: ObjectGetInstanceBinding
  , objectSetInstanceBinding :: ObjectSetInstanceBinding

  , objectSetInstance :: ObjectSetInstance
  
  , objectCastTo :: ObjectCastTo
  , objectGetInstanceFromId :: ObjectGetInstanceFromId

  , classDbConstructObject :: ClassDbConstructObject
  , classDbGetMethodBind :: ClassDbGetMethodBind
  , classDbGetClassTag :: ClassDbGetClassTag
  
  , classDbRegisterExtensionClass :: ClassDbRegisterExtensionClass
  , classDbRegisterExtensionClassMethod :: ClassDbRegisterExtensionClassMethod
  , classDbRegisterExtensionClassIntegerConstant :: ClassDbRegisterExtensionClassIntegerConstant
  , classDbRegisterExtensionClassProperty :: ClassDbRegisterExtensionClassProperty
  , classDbRegisterExtensionClassPropertyGroup :: ClassDbRegisterExtensionClassPropertyGroup
  , classDbRegisterExtensionClassPropertySubgroup :: ClassDbRegisterExtensionClassPropertySubgroup
  , classDbRegisterExtensionClassSignal :: ClassDbRegisterExtensionClassSignal
  , classDbUnregisterExtensionClass :: ClassDbUnregisterExtensionClass
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
