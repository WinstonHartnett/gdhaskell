{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Godot.Extension.Ext where

import Witch
import Foreign.C
import Foreign.Storable
import Foreign.Ptr
import Data.Coerce

#include <godot/gdnative_interface.h>
type Char32T = {#type char32_t #}
type Char16T = {#type char16_t #}
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
type GDObjectInstanceID = {#type GDObjectInstanceID #}
{#enum GDNativeCallErrorType as GdnativeCallErrorType {underscoreToCase}
  deriving (Show, Eq, Ord, Bounded) #}

data GdnativeCallError = GdnativeCallError
  { error' :: GdnativeCallErrorType
  , argument :: CInt
  , expected :: CInt
  }
{#pointer *GDNativeCallError as GdnativeCallErrorPtr -> GdnativeCallError #}
instance Storable GdnativeCallError where
  sizeOf _ = {#sizeof GDNativeCallError #}
  alignment _ = {#alignof GDNativeCallError #}
  peek ptr = GdnativeCallError 
    <$> (toEnum . fromIntegral <$> {#get GDNativeCallError->error #} ptr)
    <*> {#get GDNativeCallError->argument #} ptr
    <*> {#get GDNativeCallError->expected #} ptr
type GdnativeVariantFromTypeConstructorFunc = GdnativeVariantPtr -> GdnativeTypePtr -> IO (())
foreign import ccall "dynamic" mkGdnativeVariantFromTypeConstructorFunc
  :: FunPtr GdnativeVariantFromTypeConstructorFunc
  -> GdnativeVariantFromTypeConstructorFunc
type GdnativeTypeFromVariantConstructorFunc = GdnativeTypePtr -> GdnativeVariantPtr -> IO (())
foreign import ccall "dynamic" mkGdnativeTypeFromVariantConstructorFunc
  :: FunPtr GdnativeTypeFromVariantConstructorFunc
  -> GdnativeTypeFromVariantConstructorFunc
type GdnativePtrOperatorEvaluator = GdnativeTypePtr -> GdnativeTypePtr -> GdnativeTypePtr -> IO (())
foreign import ccall "dynamic" mkGdnativePtrOperatorEvaluator
  :: FunPtr GdnativePtrOperatorEvaluator
  -> GdnativePtrOperatorEvaluator
type GdnativePtrBuiltInMethod = GdnativeTypePtr -> Ptr (GdnativeTypePtr) -> GdnativeTypePtr -> CInt -> IO (())
foreign import ccall "dynamic" mkGdnativePtrBuiltInMethod
  :: FunPtr GdnativePtrBuiltInMethod
  -> GdnativePtrBuiltInMethod
type GdnativePtrConstructor = GdnativeTypePtr -> Ptr (GdnativeTypePtr) -> IO (())
foreign import ccall "dynamic" mkGdnativePtrConstructor
  :: FunPtr GdnativePtrConstructor
  -> GdnativePtrConstructor
type GdnativePtrDestructor = GdnativeTypePtr -> IO (())
foreign import ccall "dynamic" mkGdnativePtrDestructor
  :: FunPtr GdnativePtrDestructor
  -> GdnativePtrDestructor
type GdnativePtrSetter = GdnativeTypePtr -> GdnativeTypePtr -> IO (())
foreign import ccall "dynamic" mkGdnativePtrSetter
  :: FunPtr GdnativePtrSetter
  -> GdnativePtrSetter
type GdnativePtrGetter = GdnativeTypePtr -> GdnativeTypePtr -> IO (())
foreign import ccall "dynamic" mkGdnativePtrGetter
  :: FunPtr GdnativePtrGetter
  -> GdnativePtrGetter
type GdnativePtrIndexedSetter = GdnativeTypePtr -> GdnativeInt -> GdnativeTypePtr -> IO (())
foreign import ccall "dynamic" mkGdnativePtrIndexedSetter
  :: FunPtr GdnativePtrIndexedSetter
  -> GdnativePtrIndexedSetter
type GdnativePtrIndexedGetter = GdnativeTypePtr -> GdnativeInt -> GdnativeTypePtr -> IO (())
foreign import ccall "dynamic" mkGdnativePtrIndexedGetter
  :: FunPtr GdnativePtrIndexedGetter
  -> GdnativePtrIndexedGetter
type GdnativePtrKeyedSetter = GdnativeTypePtr -> GdnativeTypePtr -> GdnativeTypePtr -> IO (())
foreign import ccall "dynamic" mkGdnativePtrKeyedSetter
  :: FunPtr GdnativePtrKeyedSetter
  -> GdnativePtrKeyedSetter
type GdnativePtrKeyedGetter = GdnativeTypePtr -> GdnativeTypePtr -> GdnativeTypePtr -> IO (())
foreign import ccall "dynamic" mkGdnativePtrKeyedGetter
  :: FunPtr GdnativePtrKeyedGetter
  -> GdnativePtrKeyedGetter
type GdnativePtrKeyedChecker = GdnativeVariantPtr -> GdnativeVariantPtr -> IO (CUInt)
foreign import ccall "dynamic" mkGdnativePtrKeyedChecker
  :: FunPtr GdnativePtrKeyedChecker
  -> GdnativePtrKeyedChecker
type GdnativePtrUtilityFunction = GdnativeTypePtr -> Ptr (GdnativeTypePtr) -> CInt -> IO (())
foreign import ccall "dynamic" mkGdnativePtrUtilityFunction
  :: FunPtr GdnativePtrUtilityFunction
  -> GdnativePtrUtilityFunction
type GdnativeClassConstructor =   IO (GdnativeObjectPtr)
foreign import ccall "dynamic" mkGdnativeClassConstructor
  :: FunPtr GdnativeClassConstructor
  -> GdnativeClassConstructor
type GdnativeInstanceBindingCreateCallback = Ptr (()) -> Ptr (()) -> IO (Ptr (()))
foreign import ccall "dynamic" mkGdnativeInstanceBindingCreateCallback
  :: FunPtr GdnativeInstanceBindingCreateCallback
  -> GdnativeInstanceBindingCreateCallback
type GdnativeInstanceBindingFreeCallback = Ptr (()) -> Ptr (()) -> Ptr (()) -> IO (())
foreign import ccall "dynamic" mkGdnativeInstanceBindingFreeCallback
  :: FunPtr GdnativeInstanceBindingFreeCallback
  -> GdnativeInstanceBindingFreeCallback
type GdnativeInstanceBindingReferenceCallback = Ptr (()) -> Ptr (()) -> GdnativeBool -> IO (GdnativeBool)
foreign import ccall "dynamic" mkGdnativeInstanceBindingReferenceCallback
  :: FunPtr GdnativeInstanceBindingReferenceCallback
  -> GdnativeInstanceBindingReferenceCallback

data GdnativeInstanceBindingCallbacks = GdnativeInstanceBindingCallbacks
  { createCallback :: GdnativeInstanceBindingCreateCallback
  , freeCallback :: GdnativeInstanceBindingFreeCallback
  , referenceCallback :: GdnativeInstanceBindingReferenceCallback
  }
{#pointer *GDNativeInstanceBindingCallbacks as GdnativeInstanceBindingCallbacksPtr -> GdnativeInstanceBindingCallbacks #}
instance Storable GdnativeInstanceBindingCallbacks where
  sizeOf _ = {#sizeof GDNativeInstanceBindingCallbacks #}
  alignment _ = {#alignof GDNativeInstanceBindingCallbacks #}
  peek ptr = GdnativeInstanceBindingCallbacks 
    <$> (mkGdnativeInstanceBindingCreateCallback <$> {#get GDNativeInstanceBindingCallbacks->create_callback #} ptr)
    <*> (mkGdnativeInstanceBindingFreeCallback <$> {#get GDNativeInstanceBindingCallbacks->free_callback #} ptr)
    <*> (mkGdnativeInstanceBindingReferenceCallback <$> {#get GDNativeInstanceBindingCallbacks->reference_callback #} ptr)
{#pointer GDExtensionClassInstancePtr as GDExtensionClassInstancePtr newtype #}
deriving newtype instance Show GDExtensionClassInstancePtr
type GdnativeExtensionClassSet = GDExtensionClassInstancePtr -> GdnativeStringNamePtr -> GdnativeVariantPtr -> IO (GdnativeBool)
foreign import ccall "dynamic" mkGdnativeExtensionClassSet
  :: FunPtr GdnativeExtensionClassSet
  -> GdnativeExtensionClassSet
type GdnativeExtensionClassGet = GDExtensionClassInstancePtr -> GdnativeStringNamePtr -> GdnativeVariantPtr -> IO (GdnativeBool)
foreign import ccall "dynamic" mkGdnativeExtensionClassGet
  :: FunPtr GdnativeExtensionClassGet
  -> GdnativeExtensionClassGet
type GdnativeExtensionClassGetRID = GDExtensionClassInstancePtr -> IO (CULong)
foreign import ccall "dynamic" mkGdnativeExtensionClassGetRID
  :: FunPtr GdnativeExtensionClassGetRID
  -> GdnativeExtensionClassGetRID

data GdnativePropertyInfo = GdnativePropertyInfo
  { type' :: CUInt
  , name :: Ptr (CChar)
  , className :: Ptr (CChar)
  , hint :: CUInt
  , hintString :: Ptr (CChar)
  , usage :: CUInt
  }
{#pointer *GDNativePropertyInfo as GdnativePropertyInfoPtr -> GdnativePropertyInfo #}
instance Storable GdnativePropertyInfo where
  sizeOf _ = {#sizeof GDNativePropertyInfo #}
  alignment _ = {#alignof GDNativePropertyInfo #}
  peek ptr = GdnativePropertyInfo 
    <$> {#get GDNativePropertyInfo->type #} ptr
    <*> {#get GDNativePropertyInfo->name #} ptr
    <*> {#get GDNativePropertyInfo->class_name #} ptr
    <*> {#get GDNativePropertyInfo->hint #} ptr
    <*> {#get GDNativePropertyInfo->hint_string #} ptr
    <*> {#get GDNativePropertyInfo->usage #} ptr

data GdnativeMethodInfo = GdnativeMethodInfo
  { name :: Ptr (CChar)
  , returnValue :: GdnativePropertyInfo
  , flags :: CUInt
  , id :: CInt
  , arguments :: Ptr (GdnativePropertyInfo)
  , argumentCount :: CUInt
  , defaultArguments :: GdnativeVariantPtr
  , defaultArgumentCount :: CUInt
  }
{#pointer *GDNativeMethodInfo as GdnativeMethodInfoPtr -> GdnativeMethodInfo #}
instance Storable GdnativeMethodInfo where
  sizeOf _ = {#sizeof GDNativeMethodInfo #}
  alignment _ = {#alignof GDNativeMethodInfo #}
  peek ptr = GdnativeMethodInfo 
    <$> {#get GDNativeMethodInfo->name #} ptr
    <*> (peek . coerce @_ @(Ptr (GdnativePropertyInfo)) =<< {#get GDNativeMethodInfo->return_value #} ptr)
    <*> {#get GDNativeMethodInfo->flags #} ptr
    <*> {#get GDNativeMethodInfo->id #} ptr
    <*> (peek . coerce @_ @(Ptr (Ptr (GdnativePropertyInfo))) =<< {#get GDNativeMethodInfo->arguments #} ptr)
    <*> {#get GDNativeMethodInfo->argument_count #} ptr
    <*> (coerce @_ @(GdnativeVariantPtr) <$> {#get GDNativeMethodInfo->default_arguments #} ptr)
    <*> {#get GDNativeMethodInfo->default_argument_count #} ptr
type GdnativeExtensionClassGetPropertyList = GDExtensionClassInstancePtr -> Ptr (CUInt) -> IO (Ptr (GdnativePropertyInfo))
foreign import ccall "dynamic" mkGdnativeExtensionClassGetPropertyList
  :: FunPtr GdnativeExtensionClassGetPropertyList
  -> GdnativeExtensionClassGetPropertyList
type GdnativeExtensionClassFreePropertyList = GDExtensionClassInstancePtr -> Ptr (GdnativePropertyInfo) -> IO (())
foreign import ccall "dynamic" mkGdnativeExtensionClassFreePropertyList
  :: FunPtr GdnativeExtensionClassFreePropertyList
  -> GdnativeExtensionClassFreePropertyList
type GdnativeExtensionClassNotification = GDExtensionClassInstancePtr -> CInt -> IO (())
foreign import ccall "dynamic" mkGdnativeExtensionClassNotification
  :: FunPtr GdnativeExtensionClassNotification
  -> GdnativeExtensionClassNotification
type GdnativeExtensionClassToString = GDExtensionClassInstancePtr -> GdnativeStringPtr -> IO (())
foreign import ccall "dynamic" mkGdnativeExtensionClassToString
  :: FunPtr GdnativeExtensionClassToString
  -> GdnativeExtensionClassToString
type GdnativeExtensionClassReference = GDExtensionClassInstancePtr -> IO (())
foreign import ccall "dynamic" mkGdnativeExtensionClassReference
  :: FunPtr GdnativeExtensionClassReference
  -> GdnativeExtensionClassReference
type GdnativeExtensionClassUnreference = GDExtensionClassInstancePtr -> IO (())
foreign import ccall "dynamic" mkGdnativeExtensionClassUnreference
  :: FunPtr GdnativeExtensionClassUnreference
  -> GdnativeExtensionClassUnreference
type GdnativeExtensionClassCallVirtual = GDExtensionClassInstancePtr -> Ptr (GdnativeTypePtr) -> GdnativeTypePtr -> IO (())
foreign import ccall "dynamic" mkGdnativeExtensionClassCallVirtual
  :: FunPtr GdnativeExtensionClassCallVirtual
  -> GdnativeExtensionClassCallVirtual
type GdnativeExtensionClassCreateInstance = Ptr (()) -> IO (GdnativeObjectPtr)
foreign import ccall "dynamic" mkGdnativeExtensionClassCreateInstance
  :: FunPtr GdnativeExtensionClassCreateInstance
  -> GdnativeExtensionClassCreateInstance
type GdnativeExtensionClassFreeInstance = Ptr (()) -> GDExtensionClassInstancePtr -> IO (())
foreign import ccall "dynamic" mkGdnativeExtensionClassFreeInstance
  :: FunPtr GdnativeExtensionClassFreeInstance
  -> GdnativeExtensionClassFreeInstance
type GdnativeExtensionClassObjectInstance = GDExtensionClassInstancePtr -> GdnativeObjectPtr -> IO (())
foreign import ccall "dynamic" mkGdnativeExtensionClassObjectInstance
  :: FunPtr GdnativeExtensionClassObjectInstance
  -> GdnativeExtensionClassObjectInstance
type GdnativeExtensionClassGetVirtual = Ptr (()) -> Ptr (CChar) -> IO (FunPtr (GdnativeExtensionClassCallVirtual))
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
  , getRidFunc :: GdnativeExtensionClassGetRID
  , classUserdata :: Ptr (())
  }
{#pointer *GDNativeExtensionClassCreationInfo as GdnativeExtensionClassCreationInfoPtr -> GdnativeExtensionClassCreationInfo #}
instance Storable GdnativeExtensionClassCreationInfo where
  sizeOf _ = {#sizeof GDNativeExtensionClassCreationInfo #}
  alignment _ = {#alignof GDNativeExtensionClassCreationInfo #}
  peek ptr = GdnativeExtensionClassCreationInfo 
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
    <*> (mkGdnativeExtensionClassGetRID <$> {#get GDNativeExtensionClassCreationInfo->get_rid_func #} ptr)
    <*> {#get GDNativeExtensionClassCreationInfo->class_userdata #} ptr
{#pointer GDNativeExtensionClassLibraryPtr as GdnativeExtensionClassLibraryPtr newtype #}
deriving newtype instance Show GdnativeExtensionClassLibraryPtr
{#enum GDNativeExtensionClassMethodFlags as GdnativeExtensionClassMethodFlags {underscoreToCase}
  deriving (Show, Eq, Ord, Bounded) #}
{#enum GDNativeExtensionClassMethodArgumentMetadata as GdnativeExtensionClassMethodArgumentMetadata {underscoreToCase}
  deriving (Show, Eq, Ord, Bounded) #}
type GdnativeExtensionClassMethodCall = Ptr (()) -> GDExtensionClassInstancePtr -> Ptr (GdnativeVariantPtr) -> GdnativeInt -> GdnativeVariantPtr -> Ptr (GdnativeCallError) -> IO (())
foreign import ccall "dynamic" mkGdnativeExtensionClassMethodCall
  :: FunPtr GdnativeExtensionClassMethodCall
  -> GdnativeExtensionClassMethodCall
type GdnativeExtensionClassMethodPtrCall = Ptr (()) -> GDExtensionClassInstancePtr -> Ptr (GdnativeTypePtr) -> GdnativeTypePtr -> IO (())
foreign import ccall "dynamic" mkGdnativeExtensionClassMethodPtrCall
  :: FunPtr GdnativeExtensionClassMethodPtrCall
  -> GdnativeExtensionClassMethodPtrCall
type GdnativeExtensionClassMethodGetArgumentType = Ptr (()) -> CInt -> IO (CInt)
foreign import ccall "dynamic" mkGdnativeExtensionClassMethodGetArgumentType
  :: FunPtr GdnativeExtensionClassMethodGetArgumentType
  -> GdnativeExtensionClassMethodGetArgumentType
type GdnativeExtensionClassMethodGetArgumentInfo = Ptr (()) -> CInt -> Ptr (GdnativePropertyInfo) -> IO (())
foreign import ccall "dynamic" mkGdnativeExtensionClassMethodGetArgumentInfo
  :: FunPtr GdnativeExtensionClassMethodGetArgumentInfo
  -> GdnativeExtensionClassMethodGetArgumentInfo
type GdnativeExtensionClassMethodGetArgumentMetadata = Ptr (()) -> CInt -> IO (CInt)
foreign import ccall "dynamic" mkGdnativeExtensionClassMethodGetArgumentMetadata
  :: FunPtr GdnativeExtensionClassMethodGetArgumentMetadata
  -> GdnativeExtensionClassMethodGetArgumentMetadata

data GdnativeExtensionClassMethodInfo = GdnativeExtensionClassMethodInfo
  { name :: Ptr (CChar)
  , methodUserdata :: Ptr (())
  , callFunc :: GdnativeExtensionClassMethodCall
  , ptrcallFunc :: GdnativeExtensionClassMethodPtrCall
  , methodFlags :: CUInt
  , argumentCount :: CUInt
  , hasReturnValue :: GdnativeBool
  , getArgumentTypeFunc :: GdnativeExtensionClassMethodGetArgumentType
  , getArgumentInfoFunc :: GdnativeExtensionClassMethodGetArgumentInfo
  , getArgumentMetadataFunc :: GdnativeExtensionClassMethodGetArgumentMetadata
  , defaultArgumentCount :: CUInt
  , defaultArguments :: Ptr (GdnativeVariantPtr)
  }
{#pointer *GDNativeExtensionClassMethodInfo as GdnativeExtensionClassMethodInfoPtr -> GdnativeExtensionClassMethodInfo #}
instance Storable GdnativeExtensionClassMethodInfo where
  sizeOf _ = {#sizeof GDNativeExtensionClassMethodInfo #}
  alignment _ = {#alignof GDNativeExtensionClassMethodInfo #}
  peek ptr = GdnativeExtensionClassMethodInfo 
    <$> {#get GDNativeExtensionClassMethodInfo->name #} ptr
    <*> {#get GDNativeExtensionClassMethodInfo->method_userdata #} ptr
    <*> (mkGdnativeExtensionClassMethodCall <$> {#get GDNativeExtensionClassMethodInfo->call_func #} ptr)
    <*> (mkGdnativeExtensionClassMethodPtrCall <$> {#get GDNativeExtensionClassMethodInfo->ptrcall_func #} ptr)
    <*> {#get GDNativeExtensionClassMethodInfo->method_flags #} ptr
    <*> {#get GDNativeExtensionClassMethodInfo->argument_count #} ptr
    <*> (coerce @_ @(GdnativeBool) <$> {#get GDNativeExtensionClassMethodInfo->has_return_value #} ptr)
    <*> (mkGdnativeExtensionClassMethodGetArgumentType <$> {#get GDNativeExtensionClassMethodInfo->get_argument_type_func #} ptr)
    <*> (mkGdnativeExtensionClassMethodGetArgumentInfo <$> {#get GDNativeExtensionClassMethodInfo->get_argument_info_func #} ptr)
    <*> (mkGdnativeExtensionClassMethodGetArgumentMetadata <$> {#get GDNativeExtensionClassMethodInfo->get_argument_metadata_func #} ptr)
    <*> {#get GDNativeExtensionClassMethodInfo->default_argument_count #} ptr
    <*> (coerce @_ @(Ptr (GdnativeVariantPtr)) <$> {#get GDNativeExtensionClassMethodInfo->default_arguments #} ptr)
{#pointer GDNativeExtensionScriptInstanceDataPtr as GdnativeExtensionScriptInstanceDataPtr newtype #}
deriving newtype instance Show GdnativeExtensionScriptInstanceDataPtr
type GdnativeExtensionScriptInstanceSet = GdnativeExtensionScriptInstanceDataPtr -> GdnativeStringNamePtr -> GdnativeVariantPtr -> IO (GdnativeBool)
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstanceSet
  :: FunPtr GdnativeExtensionScriptInstanceSet
  -> GdnativeExtensionScriptInstanceSet
type GdnativeExtensionScriptInstanceGet = GdnativeExtensionScriptInstanceDataPtr -> GdnativeStringNamePtr -> GdnativeVariantPtr -> IO (GdnativeBool)
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstanceGet
  :: FunPtr GdnativeExtensionScriptInstanceGet
  -> GdnativeExtensionScriptInstanceGet
type GdnativeExtensionScriptInstanceGetPropertyList = GdnativeExtensionScriptInstanceDataPtr -> Ptr (CUInt) -> IO (Ptr (GdnativePropertyInfo))
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstanceGetPropertyList
  :: FunPtr GdnativeExtensionScriptInstanceGetPropertyList
  -> GdnativeExtensionScriptInstanceGetPropertyList
type GdnativeExtensionScriptInstanceFreePropertyList = GdnativeExtensionScriptInstanceDataPtr -> Ptr (GdnativePropertyInfo) -> IO (())
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstanceFreePropertyList
  :: FunPtr GdnativeExtensionScriptInstanceFreePropertyList
  -> GdnativeExtensionScriptInstanceFreePropertyList
type GdnativeExtensionScriptInstanceGetPropertyType = GdnativeExtensionScriptInstanceDataPtr -> GdnativeStringNamePtr -> Ptr (GdnativeBool) -> IO (CInt)
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstanceGetPropertyType
  :: FunPtr GdnativeExtensionScriptInstanceGetPropertyType
  -> GdnativeExtensionScriptInstanceGetPropertyType
type GdnativeExtensionScriptInstanceGetOwner = GdnativeExtensionScriptInstanceDataPtr -> IO (GdnativeObjectPtr)
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstanceGetOwner
  :: FunPtr GdnativeExtensionScriptInstanceGetOwner
  -> GdnativeExtensionScriptInstanceGetOwner
type GdnativeExtensionScriptInstancePropertyStateAdd = GdnativeStringNamePtr -> GdnativeVariantPtr -> Ptr (()) -> IO (())
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstancePropertyStateAdd
  :: FunPtr GdnativeExtensionScriptInstancePropertyStateAdd
  -> GdnativeExtensionScriptInstancePropertyStateAdd
type GdnativeExtensionScriptInstanceGetPropertyState = GdnativeExtensionScriptInstanceDataPtr -> FunPtr (GdnativeExtensionScriptInstancePropertyStateAdd) -> Ptr (()) -> IO (())
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstanceGetPropertyState
  :: FunPtr GdnativeExtensionScriptInstanceGetPropertyState
  -> GdnativeExtensionScriptInstanceGetPropertyState
type GdnativeExtensionScriptInstanceGetMethodList = GdnativeExtensionScriptInstanceDataPtr -> Ptr (CUInt) -> IO (Ptr (GdnativeMethodInfo))
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstanceGetMethodList
  :: FunPtr GdnativeExtensionScriptInstanceGetMethodList
  -> GdnativeExtensionScriptInstanceGetMethodList
type GdnativeExtensionScriptInstanceFreeMethodList = GdnativeExtensionScriptInstanceDataPtr -> Ptr (GdnativeMethodInfo) -> IO (())
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstanceFreeMethodList
  :: FunPtr GdnativeExtensionScriptInstanceFreeMethodList
  -> GdnativeExtensionScriptInstanceFreeMethodList
type GdnativeExtensionScriptInstanceHasMethod = GdnativeExtensionScriptInstanceDataPtr -> GdnativeStringNamePtr -> IO (GdnativeBool)
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstanceHasMethod
  :: FunPtr GdnativeExtensionScriptInstanceHasMethod
  -> GdnativeExtensionScriptInstanceHasMethod
type GdnativeExtensionScriptInstanceCall = GdnativeExtensionScriptInstanceDataPtr -> GdnativeStringNamePtr -> Ptr (GdnativeVariantPtr) -> GdnativeInt -> GdnativeVariantPtr -> Ptr (GdnativeCallError) -> IO (())
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstanceCall
  :: FunPtr GdnativeExtensionScriptInstanceCall
  -> GdnativeExtensionScriptInstanceCall
type GdnativeExtensionScriptInstanceNotification = GdnativeExtensionScriptInstanceDataPtr -> CInt -> IO (())
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstanceNotification
  :: FunPtr GdnativeExtensionScriptInstanceNotification
  -> GdnativeExtensionScriptInstanceNotification
type GdnativeExtensionScriptInstanceToString = GdnativeExtensionScriptInstanceDataPtr -> Ptr (GdnativeBool) -> IO (Ptr (CChar))
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstanceToString
  :: FunPtr GdnativeExtensionScriptInstanceToString
  -> GdnativeExtensionScriptInstanceToString
type GdnativeExtensionScriptInstanceRefCountIncremented = GdnativeExtensionScriptInstanceDataPtr -> IO (())
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstanceRefCountIncremented
  :: FunPtr GdnativeExtensionScriptInstanceRefCountIncremented
  -> GdnativeExtensionScriptInstanceRefCountIncremented
type GdnativeExtensionScriptInstanceRefCountDecremented = GdnativeExtensionScriptInstanceDataPtr -> IO (GdnativeBool)
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstanceRefCountDecremented
  :: FunPtr GdnativeExtensionScriptInstanceRefCountDecremented
  -> GdnativeExtensionScriptInstanceRefCountDecremented
type GdnativeExtensionScriptInstanceGetScript = GdnativeExtensionScriptInstanceDataPtr -> IO (GdnativeObjectPtr)
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstanceGetScript
  :: FunPtr GdnativeExtensionScriptInstanceGetScript
  -> GdnativeExtensionScriptInstanceGetScript
type GdnativeExtensionScriptInstanceIsPlaceholder = GdnativeExtensionScriptInstanceDataPtr -> IO (GdnativeBool)
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstanceIsPlaceholder
  :: FunPtr GdnativeExtensionScriptInstanceIsPlaceholder
  -> GdnativeExtensionScriptInstanceIsPlaceholder
{#pointer GDNativeExtensionScriptLanguagePtr as GdnativeExtensionScriptLanguagePtr newtype #}
deriving newtype instance Show GdnativeExtensionScriptLanguagePtr
type GdnativeExtensionScriptInstanceGetLanguage = GdnativeExtensionScriptInstanceDataPtr -> IO (GdnativeExtensionScriptLanguagePtr)
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstanceGetLanguage
  :: FunPtr GdnativeExtensionScriptInstanceGetLanguage
  -> GdnativeExtensionScriptInstanceGetLanguage
type GdnativeExtensionScriptInstanceFree = GdnativeExtensionScriptInstanceDataPtr -> IO (())
foreign import ccall "dynamic" mkGdnativeExtensionScriptInstanceFree
  :: FunPtr GdnativeExtensionScriptInstanceFree
  -> GdnativeExtensionScriptInstanceFree
{#pointer GDNativeScriptInstancePtr as GdnativeScriptInstancePtr newtype #}
deriving newtype instance Show GdnativeScriptInstancePtr

data GdnativeExtensionScriptInstanceInfo = GdnativeExtensionScriptInstanceInfo
  { setFunc :: GdnativeExtensionScriptInstanceSet
  , getFunc :: GdnativeExtensionScriptInstanceGet
  , getPropertyListFunc :: GdnativeExtensionScriptInstanceGetPropertyList
  , freePropertyListFunc :: GdnativeExtensionScriptInstanceFreePropertyList
  , getPropertyTypeFunc :: GdnativeExtensionScriptInstanceGetPropertyType
  , getOwnerFunc :: GdnativeExtensionScriptInstanceGetOwner
  , getPropertyStateFunc :: GdnativeExtensionScriptInstanceGetPropertyState
  , getMethodListFunc :: GdnativeExtensionScriptInstanceGetMethodList
  , freeMethodListFunc :: GdnativeExtensionScriptInstanceFreeMethodList
  , hasMethodFunc :: GdnativeExtensionScriptInstanceHasMethod
  , callFunc :: GdnativeExtensionScriptInstanceCall
  , notificationFunc :: GdnativeExtensionScriptInstanceNotification
  , toStringFunc :: GdnativeExtensionScriptInstanceToString
  , refcountIncrementedFunc :: GdnativeExtensionScriptInstanceRefCountIncremented
  , refcountDecrementedFunc :: GdnativeExtensionScriptInstanceRefCountDecremented
  , getScriptFunc :: GdnativeExtensionScriptInstanceGetScript
  , isPlaceholderFunc :: GdnativeExtensionScriptInstanceIsPlaceholder
  , setFallbackFunc :: GdnativeExtensionScriptInstanceSet
  , getFallbackFunc :: GdnativeExtensionScriptInstanceGet
  , getLanguageFunc :: GdnativeExtensionScriptInstanceGetLanguage
  , freeFunc :: GdnativeExtensionScriptInstanceFree
  }
{#pointer *GDNativeExtensionScriptInstanceInfo as GdnativeExtensionScriptInstanceInfoPtr -> GdnativeExtensionScriptInstanceInfo #}
instance Storable GdnativeExtensionScriptInstanceInfo where
  sizeOf _ = {#sizeof GDNativeExtensionScriptInstanceInfo #}
  alignment _ = {#alignof GDNativeExtensionScriptInstanceInfo #}
  peek ptr = GdnativeExtensionScriptInstanceInfo 
    <$> (mkGdnativeExtensionScriptInstanceSet <$> {#get GDNativeExtensionScriptInstanceInfo->set_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceGet <$> {#get GDNativeExtensionScriptInstanceInfo->get_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceGetPropertyList <$> {#get GDNativeExtensionScriptInstanceInfo->get_property_list_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceFreePropertyList <$> {#get GDNativeExtensionScriptInstanceInfo->free_property_list_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceGetPropertyType <$> {#get GDNativeExtensionScriptInstanceInfo->get_property_type_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceGetOwner <$> {#get GDNativeExtensionScriptInstanceInfo->get_owner_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceGetPropertyState <$> {#get GDNativeExtensionScriptInstanceInfo->get_property_state_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceGetMethodList <$> {#get GDNativeExtensionScriptInstanceInfo->get_method_list_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceFreeMethodList <$> {#get GDNativeExtensionScriptInstanceInfo->free_method_list_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceHasMethod <$> {#get GDNativeExtensionScriptInstanceInfo->has_method_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceCall <$> {#get GDNativeExtensionScriptInstanceInfo->call_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceNotification <$> {#get GDNativeExtensionScriptInstanceInfo->notification_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceToString <$> {#get GDNativeExtensionScriptInstanceInfo->to_string_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceRefCountIncremented <$> {#get GDNativeExtensionScriptInstanceInfo->refcount_incremented_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceRefCountDecremented <$> {#get GDNativeExtensionScriptInstanceInfo->refcount_decremented_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceGetScript <$> {#get GDNativeExtensionScriptInstanceInfo->get_script_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceIsPlaceholder <$> {#get GDNativeExtensionScriptInstanceInfo->is_placeholder_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceSet <$> {#get GDNativeExtensionScriptInstanceInfo->set_fallback_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceGet <$> {#get GDNativeExtensionScriptInstanceInfo->get_fallback_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceGetLanguage <$> {#get GDNativeExtensionScriptInstanceInfo->get_language_func #} ptr)
    <*> (mkGdnativeExtensionScriptInstanceFree <$> {#get GDNativeExtensionScriptInstanceInfo->free_func #} ptr)
type MemAlloc = CULong -> IO (Ptr (()))
foreign import ccall "dynamic" mkMemAlloc
  :: FunPtr MemAlloc
  -> MemAlloc
type MemRealloc = Ptr (()) -> CULong -> IO (Ptr (()))
foreign import ccall "dynamic" mkMemRealloc
  :: FunPtr MemRealloc
  -> MemRealloc
type MemFree = Ptr (()) -> IO (())
foreign import ccall "dynamic" mkMemFree
  :: FunPtr MemFree
  -> MemFree
type PrintError = Ptr (CChar) -> Ptr (CChar) -> Ptr (CChar) -> CInt -> IO (())
foreign import ccall "dynamic" mkPrintError
  :: FunPtr PrintError
  -> PrintError
type PrintWarning = Ptr (CChar) -> Ptr (CChar) -> Ptr (CChar) -> CInt -> IO (())
foreign import ccall "dynamic" mkPrintWarning
  :: FunPtr PrintWarning
  -> PrintWarning
type PrintScriptError = Ptr (CChar) -> Ptr (CChar) -> Ptr (CChar) -> CInt -> IO (())
foreign import ccall "dynamic" mkPrintScriptError
  :: FunPtr PrintScriptError
  -> PrintScriptError
type GetNativeStructSize = Ptr (CChar) -> IO (CULong)
foreign import ccall "dynamic" mkGetNativeStructSize
  :: FunPtr GetNativeStructSize
  -> GetNativeStructSize
type VariantNewCopy = GdnativeVariantPtr -> GdnativeVariantPtr -> IO (())
foreign import ccall "dynamic" mkVariantNewCopy
  :: FunPtr VariantNewCopy
  -> VariantNewCopy
type VariantNewNil = GdnativeVariantPtr -> IO (())
foreign import ccall "dynamic" mkVariantNewNil
  :: FunPtr VariantNewNil
  -> VariantNewNil
type VariantDestroy = GdnativeVariantPtr -> IO (())
foreign import ccall "dynamic" mkVariantDestroy
  :: FunPtr VariantDestroy
  -> VariantDestroy
type VariantCall = GdnativeVariantPtr -> GdnativeStringNamePtr -> Ptr (GdnativeVariantPtr) -> GdnativeInt -> GdnativeVariantPtr -> Ptr (GdnativeCallError) -> IO (())
foreign import ccall "dynamic" mkVariantCall
  :: FunPtr VariantCall
  -> VariantCall
type VariantCallStatic = CInt -> GdnativeStringNamePtr -> Ptr (GdnativeVariantPtr) -> GdnativeInt -> GdnativeVariantPtr -> Ptr (GdnativeCallError) -> IO (())
foreign import ccall "dynamic" mkVariantCallStatic
  :: FunPtr VariantCallStatic
  -> VariantCallStatic
type VariantEvaluate = CInt -> GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr (GdnativeBool) -> IO (())
foreign import ccall "dynamic" mkVariantEvaluate
  :: FunPtr VariantEvaluate
  -> VariantEvaluate
type VariantSet = GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr (GdnativeBool) -> IO (())
foreign import ccall "dynamic" mkVariantSet
  :: FunPtr VariantSet
  -> VariantSet
type VariantSetNamed = GdnativeVariantPtr -> GdnativeStringNamePtr -> GdnativeVariantPtr -> Ptr (GdnativeBool) -> IO (())
foreign import ccall "dynamic" mkVariantSetNamed
  :: FunPtr VariantSetNamed
  -> VariantSetNamed
type VariantSetKeyed = GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr (GdnativeBool) -> IO (())
foreign import ccall "dynamic" mkVariantSetKeyed
  :: FunPtr VariantSetKeyed
  -> VariantSetKeyed
type VariantSetIndexed = GdnativeVariantPtr -> GdnativeInt -> GdnativeVariantPtr -> Ptr (GdnativeBool) -> Ptr (GdnativeBool) -> IO (())
foreign import ccall "dynamic" mkVariantSetIndexed
  :: FunPtr VariantSetIndexed
  -> VariantSetIndexed
type VariantGet = GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr (GdnativeBool) -> IO (())
foreign import ccall "dynamic" mkVariantGet
  :: FunPtr VariantGet
  -> VariantGet
type VariantGetNamed = GdnativeVariantPtr -> GdnativeStringNamePtr -> GdnativeVariantPtr -> Ptr (GdnativeBool) -> IO (())
foreign import ccall "dynamic" mkVariantGetNamed
  :: FunPtr VariantGetNamed
  -> VariantGetNamed
type VariantGetKeyed = GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr (GdnativeBool) -> IO (())
foreign import ccall "dynamic" mkVariantGetKeyed
  :: FunPtr VariantGetKeyed
  -> VariantGetKeyed
type VariantGetIndexed = GdnativeVariantPtr -> GdnativeInt -> GdnativeVariantPtr -> Ptr (GdnativeBool) -> Ptr (GdnativeBool) -> IO (())
foreign import ccall "dynamic" mkVariantGetIndexed
  :: FunPtr VariantGetIndexed
  -> VariantGetIndexed
type VariantIterInit = GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr (GdnativeBool) -> IO (GdnativeBool)
foreign import ccall "dynamic" mkVariantIterInit
  :: FunPtr VariantIterInit
  -> VariantIterInit
type VariantIterNext = GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr (GdnativeBool) -> IO (GdnativeBool)
foreign import ccall "dynamic" mkVariantIterNext
  :: FunPtr VariantIterNext
  -> VariantIterNext
type VariantIterGet = GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr (GdnativeBool) -> IO (())
foreign import ccall "dynamic" mkVariantIterGet
  :: FunPtr VariantIterGet
  -> VariantIterGet
type VariantHash = GdnativeVariantPtr -> IO (GdnativeInt)
foreign import ccall "dynamic" mkVariantHash
  :: FunPtr VariantHash
  -> VariantHash
type VariantRecursiveHash = GdnativeVariantPtr -> GdnativeInt -> IO (GdnativeInt)
foreign import ccall "dynamic" mkVariantRecursiveHash
  :: FunPtr VariantRecursiveHash
  -> VariantRecursiveHash
type VariantHashCompare = GdnativeVariantPtr -> GdnativeVariantPtr -> IO (GdnativeBool)
foreign import ccall "dynamic" mkVariantHashCompare
  :: FunPtr VariantHashCompare
  -> VariantHashCompare
type VariantBooleanize = GdnativeVariantPtr -> IO (GdnativeBool)
foreign import ccall "dynamic" mkVariantBooleanize
  :: FunPtr VariantBooleanize
  -> VariantBooleanize
type VariantSub = GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeVariantPtr -> IO (())
foreign import ccall "dynamic" mkVariantSub
  :: FunPtr VariantSub
  -> VariantSub
type VariantBlend = GdnativeVariantPtr -> GdnativeVariantPtr -> CFloat -> GdnativeVariantPtr -> IO (())
foreign import ccall "dynamic" mkVariantBlend
  :: FunPtr VariantBlend
  -> VariantBlend
type VariantInterpolate = GdnativeVariantPtr -> GdnativeVariantPtr -> CFloat -> GdnativeVariantPtr -> IO (())
foreign import ccall "dynamic" mkVariantInterpolate
  :: FunPtr VariantInterpolate
  -> VariantInterpolate
type VariantDuplicate = GdnativeVariantPtr -> GdnativeVariantPtr -> GdnativeBool -> IO (())
foreign import ccall "dynamic" mkVariantDuplicate
  :: FunPtr VariantDuplicate
  -> VariantDuplicate
type VariantStringify = GdnativeVariantPtr -> GdnativeStringPtr -> IO (())
foreign import ccall "dynamic" mkVariantStringify
  :: FunPtr VariantStringify
  -> VariantStringify
type VariantGetType = GdnativeVariantPtr -> IO (CInt)
foreign import ccall "dynamic" mkVariantGetType
  :: FunPtr VariantGetType
  -> VariantGetType
type VariantHasMethod = GdnativeVariantPtr -> GdnativeStringNamePtr -> IO (GdnativeBool)
foreign import ccall "dynamic" mkVariantHasMethod
  :: FunPtr VariantHasMethod
  -> VariantHasMethod
type VariantHasMember = CInt -> GdnativeStringNamePtr -> IO (GdnativeBool)
foreign import ccall "dynamic" mkVariantHasMember
  :: FunPtr VariantHasMember
  -> VariantHasMember
type VariantHasKey = GdnativeVariantPtr -> GdnativeVariantPtr -> Ptr (GdnativeBool) -> IO (GdnativeBool)
foreign import ccall "dynamic" mkVariantHasKey
  :: FunPtr VariantHasKey
  -> VariantHasKey
type VariantGetTypeName = CInt -> GdnativeStringPtr -> IO (())
foreign import ccall "dynamic" mkVariantGetTypeName
  :: FunPtr VariantGetTypeName
  -> VariantGetTypeName
type VariantCanConvert = CInt -> CInt -> IO (GdnativeBool)
foreign import ccall "dynamic" mkVariantCanConvert
  :: FunPtr VariantCanConvert
  -> VariantCanConvert
type VariantCanConvertStrict = CInt -> CInt -> IO (GdnativeBool)
foreign import ccall "dynamic" mkVariantCanConvertStrict
  :: FunPtr VariantCanConvertStrict
  -> VariantCanConvertStrict
type GetVariantFromTypeConstructor = CInt -> IO (FunPtr (GdnativeVariantFromTypeConstructorFunc))
foreign import ccall "dynamic" mkGetVariantFromTypeConstructor
  :: FunPtr GetVariantFromTypeConstructor
  -> GetVariantFromTypeConstructor
type GetVariantToTypeConstructor = CInt -> IO (FunPtr (GdnativeTypeFromVariantConstructorFunc))
foreign import ccall "dynamic" mkGetVariantToTypeConstructor
  :: FunPtr GetVariantToTypeConstructor
  -> GetVariantToTypeConstructor
type VariantGetPtrOperatorEvaluator = CInt -> CInt -> CInt -> IO (FunPtr (GdnativePtrOperatorEvaluator))
foreign import ccall "dynamic" mkVariantGetPtrOperatorEvaluator
  :: FunPtr VariantGetPtrOperatorEvaluator
  -> VariantGetPtrOperatorEvaluator
type VariantGetPtrBuiltinMethod = CInt -> Ptr (CChar) -> GdnativeInt -> IO (FunPtr (GdnativePtrBuiltInMethod))
foreign import ccall "dynamic" mkVariantGetPtrBuiltinMethod
  :: FunPtr VariantGetPtrBuiltinMethod
  -> VariantGetPtrBuiltinMethod
type VariantGetPtrConstructor = CInt -> CInt -> IO (FunPtr (GdnativePtrConstructor))
foreign import ccall "dynamic" mkVariantGetPtrConstructor
  :: FunPtr VariantGetPtrConstructor
  -> VariantGetPtrConstructor
type VariantGetPtrDestructor = CInt -> IO (FunPtr (GdnativePtrDestructor))
foreign import ccall "dynamic" mkVariantGetPtrDestructor
  :: FunPtr VariantGetPtrDestructor
  -> VariantGetPtrDestructor
type VariantConstruct = CInt -> GdnativeVariantPtr -> Ptr (GdnativeVariantPtr) -> CInt -> Ptr (GdnativeCallError) -> IO (())
foreign import ccall "dynamic" mkVariantConstruct
  :: FunPtr VariantConstruct
  -> VariantConstruct
type VariantGetPtrSetter = CInt -> Ptr (CChar) -> IO (FunPtr (GdnativePtrSetter))
foreign import ccall "dynamic" mkVariantGetPtrSetter
  :: FunPtr VariantGetPtrSetter
  -> VariantGetPtrSetter
type VariantGetPtrGetter = CInt -> Ptr (CChar) -> IO (FunPtr (GdnativePtrGetter))
foreign import ccall "dynamic" mkVariantGetPtrGetter
  :: FunPtr VariantGetPtrGetter
  -> VariantGetPtrGetter
type VariantGetPtrIndexedSetter = CInt -> IO (FunPtr (GdnativePtrIndexedSetter))
foreign import ccall "dynamic" mkVariantGetPtrIndexedSetter
  :: FunPtr VariantGetPtrIndexedSetter
  -> VariantGetPtrIndexedSetter
type VariantGetPtrIndexedGetter = CInt -> IO (FunPtr (GdnativePtrIndexedGetter))
foreign import ccall "dynamic" mkVariantGetPtrIndexedGetter
  :: FunPtr VariantGetPtrIndexedGetter
  -> VariantGetPtrIndexedGetter
type VariantGetPtrKeyedSetter = CInt -> IO (FunPtr (GdnativePtrKeyedSetter))
foreign import ccall "dynamic" mkVariantGetPtrKeyedSetter
  :: FunPtr VariantGetPtrKeyedSetter
  -> VariantGetPtrKeyedSetter
type VariantGetPtrKeyedGetter = CInt -> IO (FunPtr (GdnativePtrKeyedGetter))
foreign import ccall "dynamic" mkVariantGetPtrKeyedGetter
  :: FunPtr VariantGetPtrKeyedGetter
  -> VariantGetPtrKeyedGetter
type VariantGetPtrKeyedChecker = CInt -> IO (FunPtr (GdnativePtrKeyedChecker))
foreign import ccall "dynamic" mkVariantGetPtrKeyedChecker
  :: FunPtr VariantGetPtrKeyedChecker
  -> VariantGetPtrKeyedChecker
type VariantGetConstantValue = CInt -> Ptr (CChar) -> GdnativeVariantPtr -> IO (())
foreign import ccall "dynamic" mkVariantGetConstantValue
  :: FunPtr VariantGetConstantValue
  -> VariantGetConstantValue
type VariantGetPtrUtilityFunction = Ptr (CChar) -> GdnativeInt -> IO (FunPtr (GdnativePtrUtilityFunction))
foreign import ccall "dynamic" mkVariantGetPtrUtilityFunction
  :: FunPtr VariantGetPtrUtilityFunction
  -> VariantGetPtrUtilityFunction
type StringNewWithLatin1Chars = GdnativeStringPtr -> Ptr (CChar) -> IO (())
foreign import ccall "dynamic" mkStringNewWithLatin1Chars
  :: FunPtr StringNewWithLatin1Chars
  -> StringNewWithLatin1Chars
type StringNewWithUtf8Chars = GdnativeStringPtr -> Ptr (CChar) -> IO (())
foreign import ccall "dynamic" mkStringNewWithUtf8Chars
  :: FunPtr StringNewWithUtf8Chars
  -> StringNewWithUtf8Chars
type StringNewWithUtf16Chars = GdnativeStringPtr -> Ptr (CUShort) -> IO (())
foreign import ccall "dynamic" mkStringNewWithUtf16Chars
  :: FunPtr StringNewWithUtf16Chars
  -> StringNewWithUtf16Chars
type StringNewWithUtf32Chars = GdnativeStringPtr -> Ptr (CUInt) -> IO (())
foreign import ccall "dynamic" mkStringNewWithUtf32Chars
  :: FunPtr StringNewWithUtf32Chars
  -> StringNewWithUtf32Chars
type StringNewWithWideChars = GdnativeStringPtr -> Ptr (CInt) -> IO (())
foreign import ccall "dynamic" mkStringNewWithWideChars
  :: FunPtr StringNewWithWideChars
  -> StringNewWithWideChars
type StringNewWithLatin1CharsAndLen = GdnativeStringPtr -> Ptr (CChar) -> GdnativeInt -> IO (())
foreign import ccall "dynamic" mkStringNewWithLatin1CharsAndLen
  :: FunPtr StringNewWithLatin1CharsAndLen
  -> StringNewWithLatin1CharsAndLen
type StringNewWithUtf8CharsAndLen = GdnativeStringPtr -> Ptr (CChar) -> GdnativeInt -> IO (())
foreign import ccall "dynamic" mkStringNewWithUtf8CharsAndLen
  :: FunPtr StringNewWithUtf8CharsAndLen
  -> StringNewWithUtf8CharsAndLen
type StringNewWithUtf16CharsAndLen = GdnativeStringPtr -> Ptr (CUShort) -> GdnativeInt -> IO (())
foreign import ccall "dynamic" mkStringNewWithUtf16CharsAndLen
  :: FunPtr StringNewWithUtf16CharsAndLen
  -> StringNewWithUtf16CharsAndLen
type StringNewWithUtf32CharsAndLen = GdnativeStringPtr -> Ptr (CUInt) -> GdnativeInt -> IO (())
foreign import ccall "dynamic" mkStringNewWithUtf32CharsAndLen
  :: FunPtr StringNewWithUtf32CharsAndLen
  -> StringNewWithUtf32CharsAndLen
type StringNewWithWideCharsAndLen = GdnativeStringPtr -> Ptr (CInt) -> GdnativeInt -> IO (())
foreign import ccall "dynamic" mkStringNewWithWideCharsAndLen
  :: FunPtr StringNewWithWideCharsAndLen
  -> StringNewWithWideCharsAndLen
type StringToLatin1Chars = GdnativeStringPtr -> Ptr (CChar) -> GdnativeInt -> IO (GdnativeInt)
foreign import ccall "dynamic" mkStringToLatin1Chars
  :: FunPtr StringToLatin1Chars
  -> StringToLatin1Chars
type StringToUtf8Chars = GdnativeStringPtr -> Ptr (CChar) -> GdnativeInt -> IO (GdnativeInt)
foreign import ccall "dynamic" mkStringToUtf8Chars
  :: FunPtr StringToUtf8Chars
  -> StringToUtf8Chars
type StringToUtf16Chars = GdnativeStringPtr -> Ptr (CUShort) -> GdnativeInt -> IO (GdnativeInt)
foreign import ccall "dynamic" mkStringToUtf16Chars
  :: FunPtr StringToUtf16Chars
  -> StringToUtf16Chars
type StringToUtf32Chars = GdnativeStringPtr -> Ptr (CUInt) -> GdnativeInt -> IO (GdnativeInt)
foreign import ccall "dynamic" mkStringToUtf32Chars
  :: FunPtr StringToUtf32Chars
  -> StringToUtf32Chars
type StringToWideChars = GdnativeStringPtr -> Ptr (CInt) -> GdnativeInt -> IO (GdnativeInt)
foreign import ccall "dynamic" mkStringToWideChars
  :: FunPtr StringToWideChars
  -> StringToWideChars
type StringOperatorIndex = GdnativeStringPtr -> GdnativeInt -> IO (Ptr (CUInt))
foreign import ccall "dynamic" mkStringOperatorIndex
  :: FunPtr StringOperatorIndex
  -> StringOperatorIndex
type StringOperatorIndexConst = GdnativeStringPtr -> GdnativeInt -> IO (Ptr (CUInt))
foreign import ccall "dynamic" mkStringOperatorIndexConst
  :: FunPtr StringOperatorIndexConst
  -> StringOperatorIndexConst
type PackedByteArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO (Ptr (CUChar))
foreign import ccall "dynamic" mkPackedByteArrayOperatorIndex
  :: FunPtr PackedByteArrayOperatorIndex
  -> PackedByteArrayOperatorIndex
type PackedByteArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO (Ptr (CUChar))
foreign import ccall "dynamic" mkPackedByteArrayOperatorIndexConst
  :: FunPtr PackedByteArrayOperatorIndexConst
  -> PackedByteArrayOperatorIndexConst
type PackedColorArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO (GdnativeTypePtr)
foreign import ccall "dynamic" mkPackedColorArrayOperatorIndex
  :: FunPtr PackedColorArrayOperatorIndex
  -> PackedColorArrayOperatorIndex
type PackedColorArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO (GdnativeTypePtr)
foreign import ccall "dynamic" mkPackedColorArrayOperatorIndexConst
  :: FunPtr PackedColorArrayOperatorIndexConst
  -> PackedColorArrayOperatorIndexConst
type PackedFloat32ArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO (Ptr (CFloat))
foreign import ccall "dynamic" mkPackedFloat32ArrayOperatorIndex
  :: FunPtr PackedFloat32ArrayOperatorIndex
  -> PackedFloat32ArrayOperatorIndex
type PackedFloat32ArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO (Ptr (CFloat))
foreign import ccall "dynamic" mkPackedFloat32ArrayOperatorIndexConst
  :: FunPtr PackedFloat32ArrayOperatorIndexConst
  -> PackedFloat32ArrayOperatorIndexConst
type PackedFloat64ArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO (Ptr (CDouble))
foreign import ccall "dynamic" mkPackedFloat64ArrayOperatorIndex
  :: FunPtr PackedFloat64ArrayOperatorIndex
  -> PackedFloat64ArrayOperatorIndex
type PackedFloat64ArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO (Ptr (CDouble))
foreign import ccall "dynamic" mkPackedFloat64ArrayOperatorIndexConst
  :: FunPtr PackedFloat64ArrayOperatorIndexConst
  -> PackedFloat64ArrayOperatorIndexConst
type PackedInt32ArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO (Ptr (CInt))
foreign import ccall "dynamic" mkPackedInt32ArrayOperatorIndex
  :: FunPtr PackedInt32ArrayOperatorIndex
  -> PackedInt32ArrayOperatorIndex
type PackedInt32ArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO (Ptr (CInt))
foreign import ccall "dynamic" mkPackedInt32ArrayOperatorIndexConst
  :: FunPtr PackedInt32ArrayOperatorIndexConst
  -> PackedInt32ArrayOperatorIndexConst
type PackedInt64ArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO (Ptr (CLong))
foreign import ccall "dynamic" mkPackedInt64ArrayOperatorIndex
  :: FunPtr PackedInt64ArrayOperatorIndex
  -> PackedInt64ArrayOperatorIndex
type PackedInt64ArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO (Ptr (CLong))
foreign import ccall "dynamic" mkPackedInt64ArrayOperatorIndexConst
  :: FunPtr PackedInt64ArrayOperatorIndexConst
  -> PackedInt64ArrayOperatorIndexConst
type PackedStringArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO (GdnativeStringPtr)
foreign import ccall "dynamic" mkPackedStringArrayOperatorIndex
  :: FunPtr PackedStringArrayOperatorIndex
  -> PackedStringArrayOperatorIndex
type PackedStringArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO (GdnativeStringPtr)
foreign import ccall "dynamic" mkPackedStringArrayOperatorIndexConst
  :: FunPtr PackedStringArrayOperatorIndexConst
  -> PackedStringArrayOperatorIndexConst
type PackedVector2ArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO (GdnativeTypePtr)
foreign import ccall "dynamic" mkPackedVector2ArrayOperatorIndex
  :: FunPtr PackedVector2ArrayOperatorIndex
  -> PackedVector2ArrayOperatorIndex
type PackedVector2ArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO (GdnativeTypePtr)
foreign import ccall "dynamic" mkPackedVector2ArrayOperatorIndexConst
  :: FunPtr PackedVector2ArrayOperatorIndexConst
  -> PackedVector2ArrayOperatorIndexConst
type PackedVector3ArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO (GdnativeTypePtr)
foreign import ccall "dynamic" mkPackedVector3ArrayOperatorIndex
  :: FunPtr PackedVector3ArrayOperatorIndex
  -> PackedVector3ArrayOperatorIndex
type PackedVector3ArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO (GdnativeTypePtr)
foreign import ccall "dynamic" mkPackedVector3ArrayOperatorIndexConst
  :: FunPtr PackedVector3ArrayOperatorIndexConst
  -> PackedVector3ArrayOperatorIndexConst
type ArrayOperatorIndex = GdnativeTypePtr -> GdnativeInt -> IO (GdnativeVariantPtr)
foreign import ccall "dynamic" mkArrayOperatorIndex
  :: FunPtr ArrayOperatorIndex
  -> ArrayOperatorIndex
type ArrayOperatorIndexConst = GdnativeTypePtr -> GdnativeInt -> IO (GdnativeVariantPtr)
foreign import ccall "dynamic" mkArrayOperatorIndexConst
  :: FunPtr ArrayOperatorIndexConst
  -> ArrayOperatorIndexConst
type DictionaryOperatorIndex = GdnativeTypePtr -> GdnativeVariantPtr -> IO (GdnativeVariantPtr)
foreign import ccall "dynamic" mkDictionaryOperatorIndex
  :: FunPtr DictionaryOperatorIndex
  -> DictionaryOperatorIndex
type DictionaryOperatorIndexConst = GdnativeTypePtr -> GdnativeVariantPtr -> IO (GdnativeVariantPtr)
foreign import ccall "dynamic" mkDictionaryOperatorIndexConst
  :: FunPtr DictionaryOperatorIndexConst
  -> DictionaryOperatorIndexConst
type ObjectMethodBindCall = GdnativeMethodBindPtr -> GdnativeObjectPtr -> Ptr (GdnativeVariantPtr) -> GdnativeInt -> GdnativeVariantPtr -> Ptr (GdnativeCallError) -> IO (())
foreign import ccall "dynamic" mkObjectMethodBindCall
  :: FunPtr ObjectMethodBindCall
  -> ObjectMethodBindCall
type ObjectMethodBindPtrcall = GdnativeMethodBindPtr -> GdnativeObjectPtr -> Ptr (GdnativeTypePtr) -> GdnativeTypePtr -> IO (())
foreign import ccall "dynamic" mkObjectMethodBindPtrcall
  :: FunPtr ObjectMethodBindPtrcall
  -> ObjectMethodBindPtrcall
type ObjectDestroy = GdnativeObjectPtr -> IO (())
foreign import ccall "dynamic" mkObjectDestroy
  :: FunPtr ObjectDestroy
  -> ObjectDestroy
type GlobalGetSingleton = Ptr (CChar) -> IO (GdnativeObjectPtr)
foreign import ccall "dynamic" mkGlobalGetSingleton
  :: FunPtr GlobalGetSingleton
  -> GlobalGetSingleton
type ObjectGetInstanceBinding = GdnativeObjectPtr -> Ptr (()) -> Ptr (GdnativeInstanceBindingCallbacks) -> IO (Ptr (()))
foreign import ccall "dynamic" mkObjectGetInstanceBinding
  :: FunPtr ObjectGetInstanceBinding
  -> ObjectGetInstanceBinding
type ObjectSetInstanceBinding = GdnativeObjectPtr -> Ptr (()) -> Ptr (()) -> Ptr (GdnativeInstanceBindingCallbacks) -> IO (())
foreign import ccall "dynamic" mkObjectSetInstanceBinding
  :: FunPtr ObjectSetInstanceBinding
  -> ObjectSetInstanceBinding
type ObjectSetInstance = GdnativeObjectPtr -> Ptr (CChar) -> GDExtensionClassInstancePtr -> IO (())
foreign import ccall "dynamic" mkObjectSetInstance
  :: FunPtr ObjectSetInstance
  -> ObjectSetInstance
type ObjectCastTo = GdnativeObjectPtr -> Ptr (()) -> IO (GdnativeObjectPtr)
foreign import ccall "dynamic" mkObjectCastTo
  :: FunPtr ObjectCastTo
  -> ObjectCastTo
type ObjectGetInstanceFromId = GDObjectInstanceID -> IO (GdnativeObjectPtr)
foreign import ccall "dynamic" mkObjectGetInstanceFromId
  :: FunPtr ObjectGetInstanceFromId
  -> ObjectGetInstanceFromId
type ObjectGetInstanceId = GdnativeObjectPtr -> IO (GDObjectInstanceID)
foreign import ccall "dynamic" mkObjectGetInstanceId
  :: FunPtr ObjectGetInstanceId
  -> ObjectGetInstanceId
type ScriptInstanceCreate = Ptr (GdnativeExtensionScriptInstanceInfo) -> GdnativeExtensionScriptInstanceDataPtr -> IO (GdnativeScriptInstancePtr)
foreign import ccall "dynamic" mkScriptInstanceCreate
  :: FunPtr ScriptInstanceCreate
  -> ScriptInstanceCreate
type ClassdbConstructObject = Ptr (CChar) -> IO (GdnativeObjectPtr)
foreign import ccall "dynamic" mkClassdbConstructObject
  :: FunPtr ClassdbConstructObject
  -> ClassdbConstructObject
type ClassdbGetMethodBind = Ptr (CChar) -> Ptr (CChar) -> GdnativeInt -> IO (GdnativeMethodBindPtr)
foreign import ccall "dynamic" mkClassdbGetMethodBind
  :: FunPtr ClassdbGetMethodBind
  -> ClassdbGetMethodBind
type ClassdbGetClassTag = Ptr (CChar) -> IO (Ptr (()))
foreign import ccall "dynamic" mkClassdbGetClassTag
  :: FunPtr ClassdbGetClassTag
  -> ClassdbGetClassTag
type ClassdbRegisterExtensionClass = GdnativeExtensionClassLibraryPtr -> Ptr (CChar) -> Ptr (CChar) -> Ptr (GdnativeExtensionClassCreationInfo) -> IO (())
foreign import ccall "dynamic" mkClassdbRegisterExtensionClass
  :: FunPtr ClassdbRegisterExtensionClass
  -> ClassdbRegisterExtensionClass
type ClassdbRegisterExtensionClassMethod = GdnativeExtensionClassLibraryPtr -> Ptr (CChar) -> Ptr (GdnativeExtensionClassMethodInfo) -> IO (())
foreign import ccall "dynamic" mkClassdbRegisterExtensionClassMethod
  :: FunPtr ClassdbRegisterExtensionClassMethod
  -> ClassdbRegisterExtensionClassMethod
type ClassdbRegisterExtensionClassIntegerConstant = GdnativeExtensionClassLibraryPtr -> Ptr (CChar) -> Ptr (CChar) -> Ptr (CChar) -> GdnativeInt -> IO (())
foreign import ccall "dynamic" mkClassdbRegisterExtensionClassIntegerConstant
  :: FunPtr ClassdbRegisterExtensionClassIntegerConstant
  -> ClassdbRegisterExtensionClassIntegerConstant
type ClassdbRegisterExtensionClassProperty = GdnativeExtensionClassLibraryPtr -> Ptr (CChar) -> Ptr (GdnativePropertyInfo) -> Ptr (CChar) -> Ptr (CChar) -> IO (())
foreign import ccall "dynamic" mkClassdbRegisterExtensionClassProperty
  :: FunPtr ClassdbRegisterExtensionClassProperty
  -> ClassdbRegisterExtensionClassProperty
type ClassdbRegisterExtensionClassPropertyGroup = GdnativeExtensionClassLibraryPtr -> Ptr (CChar) -> Ptr (CChar) -> Ptr (CChar) -> IO (())
foreign import ccall "dynamic" mkClassdbRegisterExtensionClassPropertyGroup
  :: FunPtr ClassdbRegisterExtensionClassPropertyGroup
  -> ClassdbRegisterExtensionClassPropertyGroup
type ClassdbRegisterExtensionClassPropertySubgroup = GdnativeExtensionClassLibraryPtr -> Ptr (CChar) -> Ptr (CChar) -> Ptr (CChar) -> IO (())
foreign import ccall "dynamic" mkClassdbRegisterExtensionClassPropertySubgroup
  :: FunPtr ClassdbRegisterExtensionClassPropertySubgroup
  -> ClassdbRegisterExtensionClassPropertySubgroup
type ClassdbRegisterExtensionClassSignal = GdnativeExtensionClassLibraryPtr -> Ptr (CChar) -> Ptr (CChar) -> Ptr (GdnativePropertyInfo) -> GdnativeInt -> IO (())
foreign import ccall "dynamic" mkClassdbRegisterExtensionClassSignal
  :: FunPtr ClassdbRegisterExtensionClassSignal
  -> ClassdbRegisterExtensionClassSignal
type ClassdbUnregisterExtensionClass = GdnativeExtensionClassLibraryPtr -> Ptr (CChar) -> IO (())
foreign import ccall "dynamic" mkClassdbUnregisterExtensionClass
  :: FunPtr ClassdbUnregisterExtensionClass
  -> ClassdbUnregisterExtensionClass
type GetLibraryPath = GdnativeExtensionClassLibraryPtr -> GdnativeStringPtr -> IO (())
foreign import ccall "dynamic" mkGetLibraryPath
  :: FunPtr GetLibraryPath
  -> GetLibraryPath
data GdnativeInterface = GdnativeInterface
  { versionMajor :: CUInt
  , versionMinor :: CUInt
  , versionPatch :: CUInt
  , versionString :: Ptr (CChar)
  , memAlloc :: MemAlloc
  , memRealloc :: MemRealloc
  , memFree :: MemFree
  , printError :: PrintError
  , printWarning :: PrintWarning
  , printScriptError :: PrintScriptError
  , getNativeStructSize :: GetNativeStructSize
  , variantNewCopy :: VariantNewCopy
  , variantNewNil :: VariantNewNil
  , variantDestroy :: VariantDestroy
  , variantCall :: VariantCall
  , variantCallStatic :: VariantCallStatic
  , variantEvaluate :: VariantEvaluate
  , variantSet :: VariantSet
  , variantSetNamed :: VariantSetNamed
  , variantSetKeyed :: VariantSetKeyed
  , variantSetIndexed :: VariantSetIndexed
  , variantGet :: VariantGet
  , variantGetNamed :: VariantGetNamed
  , variantGetKeyed :: VariantGetKeyed
  , variantGetIndexed :: VariantGetIndexed
  , variantIterInit :: VariantIterInit
  , variantIterNext :: VariantIterNext
  , variantIterGet :: VariantIterGet
  , variantHash :: VariantHash
  , variantRecursiveHash :: VariantRecursiveHash
  , variantHashCompare :: VariantHashCompare
  , variantBooleanize :: VariantBooleanize
  , variantSub :: VariantSub
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
  , getVariantFromTypeConstructor :: GetVariantFromTypeConstructor
  , getVariantToTypeConstructor :: GetVariantToTypeConstructor
  , variantGetPtrOperatorEvaluator :: VariantGetPtrOperatorEvaluator
  , variantGetPtrBuiltinMethod :: VariantGetPtrBuiltinMethod
  , variantGetPtrConstructor :: VariantGetPtrConstructor
  , variantGetPtrDestructor :: VariantGetPtrDestructor
  , variantConstruct :: VariantConstruct
  , variantGetPtrSetter :: VariantGetPtrSetter
  , variantGetPtrGetter :: VariantGetPtrGetter
  , variantGetPtrIndexedSetter :: VariantGetPtrIndexedSetter
  , variantGetPtrIndexedGetter :: VariantGetPtrIndexedGetter
  , variantGetPtrKeyedSetter :: VariantGetPtrKeyedSetter
  , variantGetPtrKeyedGetter :: VariantGetPtrKeyedGetter
  , variantGetPtrKeyedChecker :: VariantGetPtrKeyedChecker
  , variantGetConstantValue :: VariantGetConstantValue
  , variantGetPtrUtilityFunction :: VariantGetPtrUtilityFunction
  , stringNewWithLatin1Chars :: StringNewWithLatin1Chars
  , stringNewWithUtf8Chars :: StringNewWithUtf8Chars
  , stringNewWithUtf16Chars :: StringNewWithUtf16Chars
  , stringNewWithUtf32Chars :: StringNewWithUtf32Chars
  , stringNewWithWideChars :: StringNewWithWideChars
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
  , arrayOperatorIndex :: ArrayOperatorIndex
  , arrayOperatorIndexConst :: ArrayOperatorIndexConst
  , dictionaryOperatorIndex :: DictionaryOperatorIndex
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
  , objectGetInstanceId :: ObjectGetInstanceId
  , scriptInstanceCreate :: ScriptInstanceCreate
  , classdbConstructObject :: ClassdbConstructObject
  , classdbGetMethodBind :: ClassdbGetMethodBind
  , classdbGetClassTag :: ClassdbGetClassTag
  , classdbRegisterExtensionClass :: ClassdbRegisterExtensionClass
  , classdbRegisterExtensionClassMethod :: ClassdbRegisterExtensionClassMethod
  , classdbRegisterExtensionClassIntegerConstant :: ClassdbRegisterExtensionClassIntegerConstant
  , classdbRegisterExtensionClassProperty :: ClassdbRegisterExtensionClassProperty
  , classdbRegisterExtensionClassPropertyGroup :: ClassdbRegisterExtensionClassPropertyGroup
  , classdbRegisterExtensionClassPropertySubgroup :: ClassdbRegisterExtensionClassPropertySubgroup
  , classdbRegisterExtensionClassSignal :: ClassdbRegisterExtensionClassSignal
  , classdbUnregisterExtensionClass :: ClassdbUnregisterExtensionClass
  , getLibraryPath :: GetLibraryPath
  }
{#pointer *GDNativeInterface as GdnativeInterfacePtr -> GdnativeInterface #}
instance Storable GdnativeInterface where
  sizeOf _ = {#sizeof GDNativeInterface #}
  alignment _ = {#alignof GDNativeInterface #}
  peek ptr = GdnativeInterface 
    <$> {#get GDNativeInterface->version_major #} ptr
    <*> {#get GDNativeInterface->version_minor #} ptr
    <*> {#get GDNativeInterface->version_patch #} ptr
    <*> {#get GDNativeInterface->version_string #} ptr
    <*> (mkMemAlloc <$> {#get GDNativeInterface->mem_alloc #} ptr)
    <*> (mkMemRealloc <$> {#get GDNativeInterface->mem_realloc #} ptr)
    <*> (mkMemFree <$> {#get GDNativeInterface->mem_free #} ptr)
    <*> (mkPrintError <$> {#get GDNativeInterface->print_error #} ptr)
    <*> (mkPrintWarning <$> {#get GDNativeInterface->print_warning #} ptr)
    <*> (mkPrintScriptError <$> {#get GDNativeInterface->print_script_error #} ptr)
    <*> (mkGetNativeStructSize <$> {#get GDNativeInterface->get_native_struct_size #} ptr)
    <*> (mkVariantNewCopy <$> {#get GDNativeInterface->variant_new_copy #} ptr)
    <*> (mkVariantNewNil <$> {#get GDNativeInterface->variant_new_nil #} ptr)
    <*> (mkVariantDestroy <$> {#get GDNativeInterface->variant_destroy #} ptr)
    <*> (mkVariantCall <$> {#get GDNativeInterface->variant_call #} ptr)
    <*> (mkVariantCallStatic <$> {#get GDNativeInterface->variant_call_static #} ptr)
    <*> (mkVariantEvaluate <$> {#get GDNativeInterface->variant_evaluate #} ptr)
    <*> (mkVariantSet <$> {#get GDNativeInterface->variant_set #} ptr)
    <*> (mkVariantSetNamed <$> {#get GDNativeInterface->variant_set_named #} ptr)
    <*> (mkVariantSetKeyed <$> {#get GDNativeInterface->variant_set_keyed #} ptr)
    <*> (mkVariantSetIndexed <$> {#get GDNativeInterface->variant_set_indexed #} ptr)
    <*> (mkVariantGet <$> {#get GDNativeInterface->variant_get #} ptr)
    <*> (mkVariantGetNamed <$> {#get GDNativeInterface->variant_get_named #} ptr)
    <*> (mkVariantGetKeyed <$> {#get GDNativeInterface->variant_get_keyed #} ptr)
    <*> (mkVariantGetIndexed <$> {#get GDNativeInterface->variant_get_indexed #} ptr)
    <*> (mkVariantIterInit <$> {#get GDNativeInterface->variant_iter_init #} ptr)
    <*> (mkVariantIterNext <$> {#get GDNativeInterface->variant_iter_next #} ptr)
    <*> (mkVariantIterGet <$> {#get GDNativeInterface->variant_iter_get #} ptr)
    <*> (mkVariantHash <$> {#get GDNativeInterface->variant_hash #} ptr)
    <*> (mkVariantRecursiveHash <$> {#get GDNativeInterface->variant_recursive_hash #} ptr)
    <*> (mkVariantHashCompare <$> {#get GDNativeInterface->variant_hash_compare #} ptr)
    <*> (mkVariantBooleanize <$> {#get GDNativeInterface->variant_booleanize #} ptr)
    <*> (mkVariantSub <$> {#get GDNativeInterface->variant_sub #} ptr)
    <*> (mkVariantBlend <$> {#get GDNativeInterface->variant_blend #} ptr)
    <*> (mkVariantInterpolate <$> {#get GDNativeInterface->variant_interpolate #} ptr)
    <*> (mkVariantDuplicate <$> {#get GDNativeInterface->variant_duplicate #} ptr)
    <*> (mkVariantStringify <$> {#get GDNativeInterface->variant_stringify #} ptr)
    <*> (mkVariantGetType <$> {#get GDNativeInterface->variant_get_type #} ptr)
    <*> (mkVariantHasMethod <$> {#get GDNativeInterface->variant_has_method #} ptr)
    <*> (mkVariantHasMember <$> {#get GDNativeInterface->variant_has_member #} ptr)
    <*> (mkVariantHasKey <$> {#get GDNativeInterface->variant_has_key #} ptr)
    <*> (mkVariantGetTypeName <$> {#get GDNativeInterface->variant_get_type_name #} ptr)
    <*> (mkVariantCanConvert <$> {#get GDNativeInterface->variant_can_convert #} ptr)
    <*> (mkVariantCanConvertStrict <$> {#get GDNativeInterface->variant_can_convert_strict #} ptr)
    <*> (mkGetVariantFromTypeConstructor <$> {#get GDNativeInterface->get_variant_from_type_constructor #} ptr)
    <*> (mkGetVariantToTypeConstructor <$> {#get GDNativeInterface->get_variant_to_type_constructor #} ptr)
    <*> (mkVariantGetPtrOperatorEvaluator <$> {#get GDNativeInterface->variant_get_ptr_operator_evaluator #} ptr)
    <*> (mkVariantGetPtrBuiltinMethod <$> {#get GDNativeInterface->variant_get_ptr_builtin_method #} ptr)
    <*> (mkVariantGetPtrConstructor <$> {#get GDNativeInterface->variant_get_ptr_constructor #} ptr)
    <*> (mkVariantGetPtrDestructor <$> {#get GDNativeInterface->variant_get_ptr_destructor #} ptr)
    <*> (mkVariantConstruct <$> {#get GDNativeInterface->variant_construct #} ptr)
    <*> (mkVariantGetPtrSetter <$> {#get GDNativeInterface->variant_get_ptr_setter #} ptr)
    <*> (mkVariantGetPtrGetter <$> {#get GDNativeInterface->variant_get_ptr_getter #} ptr)
    <*> (mkVariantGetPtrIndexedSetter <$> {#get GDNativeInterface->variant_get_ptr_indexed_setter #} ptr)
    <*> (mkVariantGetPtrIndexedGetter <$> {#get GDNativeInterface->variant_get_ptr_indexed_getter #} ptr)
    <*> (mkVariantGetPtrKeyedSetter <$> {#get GDNativeInterface->variant_get_ptr_keyed_setter #} ptr)
    <*> (mkVariantGetPtrKeyedGetter <$> {#get GDNativeInterface->variant_get_ptr_keyed_getter #} ptr)
    <*> (mkVariantGetPtrKeyedChecker <$> {#get GDNativeInterface->variant_get_ptr_keyed_checker #} ptr)
    <*> (mkVariantGetConstantValue <$> {#get GDNativeInterface->variant_get_constant_value #} ptr)
    <*> (mkVariantGetPtrUtilityFunction <$> {#get GDNativeInterface->variant_get_ptr_utility_function #} ptr)
    <*> (mkStringNewWithLatin1Chars <$> {#get GDNativeInterface->string_new_with_latin1_chars #} ptr)
    <*> (mkStringNewWithUtf8Chars <$> {#get GDNativeInterface->string_new_with_utf8_chars #} ptr)
    <*> (mkStringNewWithUtf16Chars <$> {#get GDNativeInterface->string_new_with_utf16_chars #} ptr)
    <*> (mkStringNewWithUtf32Chars <$> {#get GDNativeInterface->string_new_with_utf32_chars #} ptr)
    <*> (mkStringNewWithWideChars <$> {#get GDNativeInterface->string_new_with_wide_chars #} ptr)
    <*> (mkStringNewWithLatin1CharsAndLen <$> {#get GDNativeInterface->string_new_with_latin1_chars_and_len #} ptr)
    <*> (mkStringNewWithUtf8CharsAndLen <$> {#get GDNativeInterface->string_new_with_utf8_chars_and_len #} ptr)
    <*> (mkStringNewWithUtf16CharsAndLen <$> {#get GDNativeInterface->string_new_with_utf16_chars_and_len #} ptr)
    <*> (mkStringNewWithUtf32CharsAndLen <$> {#get GDNativeInterface->string_new_with_utf32_chars_and_len #} ptr)
    <*> (mkStringNewWithWideCharsAndLen <$> {#get GDNativeInterface->string_new_with_wide_chars_and_len #} ptr)
    <*> (mkStringToLatin1Chars <$> {#get GDNativeInterface->string_to_latin1_chars #} ptr)
    <*> (mkStringToUtf8Chars <$> {#get GDNativeInterface->string_to_utf8_chars #} ptr)
    <*> (mkStringToUtf16Chars <$> {#get GDNativeInterface->string_to_utf16_chars #} ptr)
    <*> (mkStringToUtf32Chars <$> {#get GDNativeInterface->string_to_utf32_chars #} ptr)
    <*> (mkStringToWideChars <$> {#get GDNativeInterface->string_to_wide_chars #} ptr)
    <*> (mkStringOperatorIndex <$> {#get GDNativeInterface->string_operator_index #} ptr)
    <*> (mkStringOperatorIndexConst <$> {#get GDNativeInterface->string_operator_index_const #} ptr)
    <*> (mkPackedByteArrayOperatorIndex <$> {#get GDNativeInterface->packed_byte_array_operator_index #} ptr)
    <*> (mkPackedByteArrayOperatorIndexConst <$> {#get GDNativeInterface->packed_byte_array_operator_index_const #} ptr)
    <*> (mkPackedColorArrayOperatorIndex <$> {#get GDNativeInterface->packed_color_array_operator_index #} ptr)
    <*> (mkPackedColorArrayOperatorIndexConst <$> {#get GDNativeInterface->packed_color_array_operator_index_const #} ptr)
    <*> (mkPackedFloat32ArrayOperatorIndex <$> {#get GDNativeInterface->packed_float32_array_operator_index #} ptr)
    <*> (mkPackedFloat32ArrayOperatorIndexConst <$> {#get GDNativeInterface->packed_float32_array_operator_index_const #} ptr)
    <*> (mkPackedFloat64ArrayOperatorIndex <$> {#get GDNativeInterface->packed_float64_array_operator_index #} ptr)
    <*> (mkPackedFloat64ArrayOperatorIndexConst <$> {#get GDNativeInterface->packed_float64_array_operator_index_const #} ptr)
    <*> (mkPackedInt32ArrayOperatorIndex <$> {#get GDNativeInterface->packed_int32_array_operator_index #} ptr)
    <*> (mkPackedInt32ArrayOperatorIndexConst <$> {#get GDNativeInterface->packed_int32_array_operator_index_const #} ptr)
    <*> (mkPackedInt64ArrayOperatorIndex <$> {#get GDNativeInterface->packed_int64_array_operator_index #} ptr)
    <*> (mkPackedInt64ArrayOperatorIndexConst <$> {#get GDNativeInterface->packed_int64_array_operator_index_const #} ptr)
    <*> (mkPackedStringArrayOperatorIndex <$> {#get GDNativeInterface->packed_string_array_operator_index #} ptr)
    <*> (mkPackedStringArrayOperatorIndexConst <$> {#get GDNativeInterface->packed_string_array_operator_index_const #} ptr)
    <*> (mkPackedVector2ArrayOperatorIndex <$> {#get GDNativeInterface->packed_vector2_array_operator_index #} ptr)
    <*> (mkPackedVector2ArrayOperatorIndexConst <$> {#get GDNativeInterface->packed_vector2_array_operator_index_const #} ptr)
    <*> (mkPackedVector3ArrayOperatorIndex <$> {#get GDNativeInterface->packed_vector3_array_operator_index #} ptr)
    <*> (mkPackedVector3ArrayOperatorIndexConst <$> {#get GDNativeInterface->packed_vector3_array_operator_index_const #} ptr)
    <*> (mkArrayOperatorIndex <$> {#get GDNativeInterface->array_operator_index #} ptr)
    <*> (mkArrayOperatorIndexConst <$> {#get GDNativeInterface->array_operator_index_const #} ptr)
    <*> (mkDictionaryOperatorIndex <$> {#get GDNativeInterface->dictionary_operator_index #} ptr)
    <*> (mkDictionaryOperatorIndexConst <$> {#get GDNativeInterface->dictionary_operator_index_const #} ptr)
    <*> (mkObjectMethodBindCall <$> {#get GDNativeInterface->object_method_bind_call #} ptr)
    <*> (mkObjectMethodBindPtrcall <$> {#get GDNativeInterface->object_method_bind_ptrcall #} ptr)
    <*> (mkObjectDestroy <$> {#get GDNativeInterface->object_destroy #} ptr)
    <*> (mkGlobalGetSingleton <$> {#get GDNativeInterface->global_get_singleton #} ptr)
    <*> (mkObjectGetInstanceBinding <$> {#get GDNativeInterface->object_get_instance_binding #} ptr)
    <*> (mkObjectSetInstanceBinding <$> {#get GDNativeInterface->object_set_instance_binding #} ptr)
    <*> (mkObjectSetInstance <$> {#get GDNativeInterface->object_set_instance #} ptr)
    <*> (mkObjectCastTo <$> {#get GDNativeInterface->object_cast_to #} ptr)
    <*> (mkObjectGetInstanceFromId <$> {#get GDNativeInterface->object_get_instance_from_id #} ptr)
    <*> (mkObjectGetInstanceId <$> {#get GDNativeInterface->object_get_instance_id #} ptr)
    <*> (mkScriptInstanceCreate <$> {#get GDNativeInterface->script_instance_create #} ptr)
    <*> (mkClassdbConstructObject <$> {#get GDNativeInterface->classdb_construct_object #} ptr)
    <*> (mkClassdbGetMethodBind <$> {#get GDNativeInterface->classdb_get_method_bind #} ptr)
    <*> (mkClassdbGetClassTag <$> {#get GDNativeInterface->classdb_get_class_tag #} ptr)
    <*> (mkClassdbRegisterExtensionClass <$> {#get GDNativeInterface->classdb_register_extension_class #} ptr)
    <*> (mkClassdbRegisterExtensionClassMethod <$> {#get GDNativeInterface->classdb_register_extension_class_method #} ptr)
    <*> (mkClassdbRegisterExtensionClassIntegerConstant <$> {#get GDNativeInterface->classdb_register_extension_class_integer_constant #} ptr)
    <*> (mkClassdbRegisterExtensionClassProperty <$> {#get GDNativeInterface->classdb_register_extension_class_property #} ptr)
    <*> (mkClassdbRegisterExtensionClassPropertyGroup <$> {#get GDNativeInterface->classdb_register_extension_class_property_group #} ptr)
    <*> (mkClassdbRegisterExtensionClassPropertySubgroup <$> {#get GDNativeInterface->classdb_register_extension_class_property_subgroup #} ptr)
    <*> (mkClassdbRegisterExtensionClassSignal <$> {#get GDNativeInterface->classdb_register_extension_class_signal #} ptr)
    <*> (mkClassdbUnregisterExtensionClass <$> {#get GDNativeInterface->classdb_unregister_extension_class #} ptr)
    <*> (mkGetLibraryPath <$> {#get GDNativeInterface->get_library_path #} ptr)
{#enum GDNativeInitializationLevel as GdnativeInitializationLevel {underscoreToCase}
  deriving (Show, Eq, Ord, Bounded) #}
type Initialize = Ptr (()) -> CInt -> IO (())
foreign import ccall "dynamic" mkInitialize
  :: FunPtr Initialize
  -> Initialize
type Deinitialize = Ptr (()) -> CInt -> IO (())
foreign import ccall "dynamic" mkDeinitialize
  :: FunPtr Deinitialize
  -> Deinitialize
data GdnativeInitialization = GdnativeInitialization
  { minimumInitializationLevel :: GdnativeInitializationLevel
  , userdata :: Ptr (())
  , initialize :: Initialize
  , deinitialize :: Deinitialize
  }
{#pointer *GDNativeInitialization as GdnativeInitializationPtr -> GdnativeInitialization #}
instance Storable GdnativeInitialization where
  sizeOf _ = {#sizeof GDNativeInitialization #}
  alignment _ = {#alignof GDNativeInitialization #}
  peek ptr = GdnativeInitialization 
    <$> (toEnum . fromIntegral <$> {#get GDNativeInitialization->minimum_initialization_level #} ptr)
    <*> {#get GDNativeInitialization->userdata #} ptr
    <*> (mkInitialize <$> {#get GDNativeInitialization->initialize #} ptr)
    <*> (mkDeinitialize <$> {#get GDNativeInitialization->deinitialize #} ptr)
type GdnativeInitializationFunction = Ptr (GdnativeInterface) -> GdnativeExtensionClassLibraryPtr -> Ptr (GdnativeInitialization) -> IO (GdnativeBool)
foreign import ccall "dynamic" mkGdnativeInitializationFunction
  :: FunPtr GdnativeInitializationFunction
  -> GdnativeInitializationFunction