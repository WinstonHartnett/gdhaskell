{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Godot.Extension.Generate.Api where

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Control.Monad.Reader (Reader)
-- import NeatInterpolation (trimming)
import Data.String.Interpolate

--------------------------------------------------------------------------------

data Header = MkHeader
  { version_major :: Int
  , version_minor :: Int
  , version_patch :: Int
  , version_status :: T.Text
  , version_build :: T.Text
  , version_full_name :: T.Text
  }
  deriving (Generic, Show)

instance A.FromJSON Header

data BuiltinSize = MkBuiltinSize
  { name :: T.Text
  , size :: Int
  }
  deriving (Generic, Show)

instance A.FromJSON BuiltinSize

data BuiltInClassSize = MkBuiltInClassSize
  { build_configuration :: T.Text
  , sizes :: V.Vector BuiltinSize
  }
  deriving (Generic, Show)

instance A.FromJSON BuiltInClassSize

data ClassOffsetMember = MkClassOffsetMember
  { member :: T.Text
  , offset :: Int
  }
  deriving (Generic, Show)

instance A.FromJSON ClassOffsetMember

data ClassOffset = MkClassOffset
  { name :: T.Text
  , members :: V.Vector ClassOffsetMember
  }
  deriving (Generic, Show)

instance A.FromJSON ClassOffset

data BuiltinClassMemberOffsets = MkBuiltinClassMemberOffsets
  { build_configuration :: T.Text
  , classes :: V.Vector ClassOffset
  }
  deriving (Generic, Show)

instance A.FromJSON BuiltinClassMemberOffsets

data EnumValue = MkEnumValue
  { name :: T.Text
  , value :: Int
  }
  deriving (Generic, Show)

instance A.FromJSON EnumValue

data EnumEntry = MkEnumEntry
  { name :: T.Text
  , values :: V.Vector EnumValue
  }
  deriving (Generic, Show)

instance A.FromJSON EnumEntry

data Argument = MkArgument
  { name :: T.Text
  , type' :: T.Text
  , default_value :: Maybe T.Text
  }
  deriving (Generic, Show)

instance A.FromJSON Argument where
  parseJSON (A.Object v) =
    MkArgument
      <$> v A..: "name"
      <*> v A..: "type"
      <*> v A..:? "default_value"
  parseJSON _ = mempty

data UtilityFunction = MkUtilityFunction
  { name :: T.Text
  , return_type :: Maybe T.Text
  , category :: T.Text
  , is_vararg :: Bool
  , hash :: Int
  , arguments :: Maybe (V.Vector Argument)
  }
  deriving (Generic, Show)

instance A.FromJSON UtilityFunction

data Operator = MkOperator
  { name :: T.Text
  , right_type :: Maybe T.Text
  , return_type :: T.Text
  }
  deriving (Generic, Show)

instance A.FromJSON Operator

data Constructor = MkConstructor
  { index :: Int
  , arguments :: (Maybe (V.Vector Argument))
  }
  deriving (Generic, Show)

instance A.FromJSON Constructor

data Method = MkMethod
  { name :: T.Text
  , return_type :: Maybe T.Text
  , is_vararg :: Bool
  , is_const :: Bool
  , is_static :: Maybe Bool
  , hash :: Maybe Int
  , arguments :: Maybe (V.Vector Argument)
  }
  deriving (Generic, Show)

instance A.FromJSON Method

data BuiltinClassMember = MkBuiltinClassMember
  { name :: T.Text
  , type' :: T.Text
  }
  deriving (Generic, Show)

instance A.FromJSON BuiltinClassMember where
  parseJSON (A.Object v) =
    MkBuiltinClassMember
      <$> v A..: "name"
      <*> v A..: "type"
  parseJSON _ = mempty

data Constant = MkConstant
  { name :: T.Text
  , type' :: T.Text
  , value :: T.Text
  }
  deriving (Generic, Show)

instance A.FromJSON Constant where
  parseJSON (A.Object v) =
    MkConstant
      <$> v A..: "name"
      <*> v A..: "type"
      <*> v A..: "value"
  parseJSON _ = mempty

data BuiltinClass = MkBuiltinClass
  { name :: T.Text
  , is_keyed :: Maybe Bool
  , operators :: V.Vector Operator
  , indexing_return_type :: Maybe T.Text
  , constructors :: V.Vector Constructor
  , methods :: Maybe (V.Vector Method)
  , constants :: Maybe (V.Vector Constant)
  , members :: Maybe (V.Vector BuiltinClassMember)
  , has_destructor :: Bool
  }
  deriving (Generic, Show)

instance A.FromJSON BuiltinClass

data Signal = MkSignal
  { name :: T.Text
  , arguments :: Maybe (V.Vector Argument)
  }
  deriving (Generic, Show)

instance A.FromJSON Signal

data Property = MkProperty
  { type' :: T.Text
  , name :: T.Text
  , setter :: T.Text
  , getter :: T.Text
  , index :: Int
  }
  deriving (Generic, Show)

instance A.FromJSON Property where
  parseJSON (A.Object v) =
    MkProperty
      <$> v A..: "type"
      <*> v A..: "name"
      <*> v A..: "setter"
      <*> v A..: "getter"
      <*> v A..: "index"
  parseJSON _ = mempty

data Class = MkClass
  { name :: T.Text
  , is_refcounted :: Bool
  , is_instantiable :: Bool
  , inherits :: Maybe T.Text
  , api_type :: T.Text
  , enums :: Maybe (V.Vector EnumEntry)
  , methods :: Maybe (V.Vector Method)
  , signals :: Maybe (V.Vector Signal)
  , properties :: Maybe (V.Vector Property)
  }
  deriving (Generic, Show)

instance A.FromJSON Class

data SingletonEntry = MkSingletonEntry
  { name :: T.Text
  , type' :: T.Text
  }
  deriving (Generic, Show)

instance A.FromJSON SingletonEntry where
  parseJSON (A.Object v) =
    MkSingletonEntry
      <$> v A..: "name"
      <*> v A..: "type"
  parseJSON _ = mempty

data NativeStructureEntry = MkNativeStructureEntry
  { name :: T.Text
  , format :: T.Text
  }
  deriving (Generic, Show)

instance A.FromJSON NativeStructureEntry

data ExtensionApi = MkExtensionApi
  { header :: Header
  , builtin_class_sizes :: V.Vector BuiltInClassSize
  , builtin_class_member_offsets :: V.Vector BuiltinClassMemberOffsets
  , global_constants :: V.Vector ()
  , global_enums :: V.Vector EnumEntry
  , utility_functions :: V.Vector UtilityFunction
  , builtin_classes :: V.Vector BuiltinClass
  , classes :: V.Vector Class
  , singletons :: V.Vector SingletonEntry
  , native_structures :: V.Vector NativeStructureEntry
  }
  deriving (Generic, Show)

instance A.FromJSON ExtensionApi

--------------------------------------------------------------------------------

genBuiltinClass :: BuiltinClass -> Reader ExtensionApi T.Text
genBuiltinClass cls = undefined
  -- let (decl :: T.Text) =
  --       [i|
  --         newtype 
  --       |]
  -- undefined