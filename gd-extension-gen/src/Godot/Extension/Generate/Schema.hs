{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Godot.Extension.Generate.Schema where

import Data.Aeson (FromJSON (parseJSON))
import Data.Aeson qualified as A
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Vector qualified as V
import Debug.Trace (trace)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

{- |
Corresponds to a list of Objects with two JSON keys that are interpreted
as HashMap keys and values, respectively.
-}
newtype Paired (k :: Symbol) (v :: Symbol) a = MkObjectMap {unObjectMap :: HM.HashMap T.Text a}
  deriving (Show, Functor, Foldable, Traversable)

instance (KnownSymbol k, KnownSymbol v, A.FromJSON a) => A.FromJSON (Paired k v a) where
  parseJSON (A.Array a) =
    MkObjectMap
      . HM.fromList
      <$> mapM
        ( \case
            A.Object c -> do
              (key :: T.Text) <- c A..: read (show $ symbolVal $ Proxy @k)
              (value' :: a) <- c A..: read (show $ symbolVal $ Proxy @v)
              pure (key, value')
            _ -> mempty
        )
        (V.toList a)
  parseJSON _ = mempty

getPaired :: T.Text -> Paired k v a -> Maybe a
getPaired k o = HM.lookup k o.unObjectMap

{- |
Corresponds to a list of Objects with a particular JSON field marked as a
key that points to the Haskell schema of the Object.
-}
newtype Keyed (k :: Symbol) t a = MkKeyedMap {unKeyedMap :: HM.HashMap t a}
  deriving (Show, Functor, Foldable, Traversable)

type Keyed' f = Keyed f T.Text

instance (Hashable t, KnownSymbol k, A.FromJSON t, A.FromJSON a) => A.FromJSON (Keyed k t a) where
  parseJSON (A.Array a) = do
    MkKeyedMap . HM.fromList <$> mapM getKeyAndRest (V.toList a)
   where
    getKeyAndRest v@(A.Object c) = do
      key <- trace (show c) $ c A..: read (show $ symbolVal $ Proxy @k)
      val <- parseJSON v
      pure (key, val)
    getKeyAndRest _ = mempty
  parseJSON _ = mempty

getKeyed :: Hashable t => t -> Keyed k t a -> Maybe a
getKeyed k o = HM.lookup k o.unKeyedMap

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
  , arguments :: Maybe (Keyed' "name" Argument)
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
  , operators :: Keyed' "name" Operator
  , indexing_return_type :: Maybe T.Text
  , constructors :: Maybe (Keyed "index" Int Constructor)
  , methods :: Maybe (Keyed' "name" Method)
  , constants :: Maybe (Keyed' "name" Constant)
  , members :: Maybe (Keyed' "name" BuiltinClassMember)
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
  , enums :: Maybe (Paired "name" "values" (Paired "name" "value" Int))
  , methods :: Maybe (Keyed' "name" Method)
  , signals :: Maybe (Keyed' "name" Signal)
  , properties :: Maybe (Keyed' "name" Property)
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
  , builtin_class_sizes ::
    Paired
      "build_configuration"
      "sizes"
      (Paired "name" "size" Int)
  , builtin_class_member_offsets ::
    Paired
      "build_configuration"
      "classes"
      (Paired "name" "members" (Paired "member" "offset" Int))
  , global_constants :: V.Vector ()
  , global_enums ::
    Keyed'
      "name"
      EnumEntry
  , utility_functions :: V.Vector UtilityFunction
  , builtin_classes :: V.Vector BuiltinClass
  , classes :: V.Vector Class
  , singletons :: V.Vector SingletonEntry
  , native_structures :: V.Vector NativeStructureEntry
  }
  deriving (Generic, Show)

instance A.FromJSON ExtensionApi
