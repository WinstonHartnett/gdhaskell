{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Godot.Extension.Generate.Utils where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Short.Internal qualified as BI
import Data.Char (toUpper)
import Data.Coerce (coerce)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromMaybe, isJust)
import Data.Primitive.ByteArray (
  ByteArray (ByteArray),
  cloneByteArray,
  copyByteArrayToPtr,
 )
import Data.Ratio (denominator, numerator)
import Data.Text qualified as T
import Data.Text.Array qualified as TI
import Data.Text.Internal qualified as TI
import Data.Vector qualified as V
import Data.Vector.Algorithms.Heap qualified as V
import Data.Word (Word8)
import Foreign (Storable (pokeByteOff), free, mallocArray0)
import Foreign.C.String (CString)
import GHC (HsLit (HsString), HsTyLit (HsStrTy), HsType (HsTyLit), NoExtField (NoExtField))
import GHC.Data.FastString (FastString, mkFastStringShortByteString)
import GHC.Exts (Int (I#), IsString (fromString), sizeofByteArray#)
import GHC.Records (HasField (getField))
import GHC.SourceGen (
  HasLit (lit),
  HsType',
  OccNameStr,
  RdrNameStr,
  occNameToStr,
  unqual,
 )
import GHC.Types.Name (mkVarOccFS)
import GHC.Types.SourceText (SourceText (NoSourceText))
import Witch (From (from), TryFrom, TryFromException (TryFromException))
import Witch.TryFrom (TryFrom (tryFrom))

-- | Intersperse an element in a Vector.
vIntersperse :: a -> V.Vector a -> V.Vector a
vIntersperse i v =
  let targetLen = (V.length v * 2) - 1
      f n
        | n `mod` 2 == 0 = v V.! (n `div` 2)
        | otherwise = i
   in V.generate targetLen f

fromEither :: Either a a -> a
fromEither (Left x) = x
fromEither (Right x) = x

--------------------------------------------------------------------------------

{- |
Precision targets of Godot's internal `real_t`.

_Note:_ 32-bit builds are not supported by GHC.
-}
data BuildConfig
  = Float64
  | Double64

buildConfigToText :: BuildConfig -> T.Text
buildConfigToText Float64 = "float_64"
buildConfigToText Double64 = "double_64"

-- | Mapping from the types in 'Foreign.C.Types' to Haskell-native types.
foreignCToHs :: HM.HashMap T.Text T.Text
foreignCToHs =
  HM.fromList
    [ ("CChar", "Int8")
    , ("CSChar", "Int8")
    , ("CUChar", "Word8")
    , ("CUShort", "Word16")
    , ("CInt", "Int32")
    , ("CUInt", "Word32")
    , ("CULong", "Word64")
    , ("CPtrdiff", "Int64")
    , ("CSize", "Word64")
    , ("CWchar", "Int32")
    , ("CSigAtomic", "Int32")
    , ("CLong", "Int64")
    , ("CLLong", "Int64")
    , ("CULLong", "Word64")
    , ("CBool", "Word8")
    , ("CIntPtr", "Int64")
    , ("CUIntPtr", "Word64")
    , ("CIntMax", "Int64")
    , ("CUIntMax", "Word64")
    , ("CClock", "Int64")
    , ("CTime", "Int64")
    , ("CUSeconds", "Word32")
    , ("CSUSeconds", "Int64")
    , ("CFloat", "Float")
    , ("CDouble", "Double")
    ]

-- | Mapping from native C types to Haskell's 'Foreign.C.Types'.
cToForeignC :: HM.HashMap T.Text T.Text
cToForeignC =
  HM.fromList
    [ ("uint64_t", "CULong")
    , ("uint32_t", "CUInt")
    , ("uint16_t", "CUShort")
    , ("uint8_t", "CUChar")
    , ("int64_t", "CLong")
    , ("int32_t", "CInt")
    , ("int16_t", "CShort")
    , ("int", "CInt")
    , ("wchar_t", "CInt")
    , ("char", "CChar")
    , ("size_t", "CULong")
    , ("float", "CFloat")
    , ("double", "CDouble")
    , ("char16_t", "CUShort")
    , ("char32_t", "CUInt")
    , ("void", "()")
    ]

{- | Conversion from Godot's `extension_api.json` types to Foreign.C
types. Some types (i.e. `float`) depend on the build configuration.
-}
godotToForeignC :: BuildConfig -> HM.HashMap T.Text T.Text
godotToForeignC bc =
  HM.fromList $ case bc of
    Float64 -> ("float", "CFloat") : nativeTys
    Double64 -> ("float", "CDouble") : nativeTys
 where
  nativeTys =
    [ ("Nil", "()")
    , ("bool", "CBool")
    , ("int", "CLong")
    ]

toNativeForeignC :: BuildConfig -> GType -> Maybe HType
toNativeForeignC cfg g = do
  fc <- HM.lookup (toText g) (godotToForeignC cfg)
  MkName <$> HM.lookup fc foreignCToHs

-- | Whether a Godot type is a primitive C type.
isGodotBaseType :: BuildConfig -> T.Text -> Bool
isGodotBaseType bc = isJust . (`HM.lookup` godotToForeignC bc)

-- | Substrings to camelize when converting Text to camelcase.
camelSubstitutions :: V.Vector (T.Text, T.Text)
camelSubstitutions = customSubstitutions <> casingSubstitutions
 where
  customSubstitutions = [("String", "GdString"), (".", "")]
  casingSubstitutions =
    V.map
      (\t -> (t, capitalizeFirst $ T.toLower t))
      $ V.modify
        ( V.sortBy
            ( \a b ->
                let aLen = T.length a
                    bLen = T.length b
                 in if aLen < bLen
                      then GT
                      else if aLen > bLen then LT else EQ
            )
        )
        [ "GDNative"
        , "AES"
        , "OGG"
        , "AABB"
        , "CPU"
        , "CSG"
        , "DTLS"
        , "GLTF"
        , "GDScript"
        , "GPU"
        , "HTTP"
        , "JSON"
        , "RPC"
        , "JNI"
        , "ORM"
        , "OS"
        , "PCK"
        , "RDA"
        , "RDF"
        , "RD"
        , "RID"
        , "TCP"
        , "UPNP"
        , "RTC"
        , "3D"
        , "2D"
        ]

autoGenHeader :: T.Text
autoGenHeader = "{- This file was generated by `gd-extension-gen`. -}"

--------------------------------------------------------------------------------

capitalizeFirst :: T.Text -> T.Text
capitalizeFirst t =
  if T.null t
    then t
    else (T.singleton . toUpper $ T.head t) <> T.tail t

{- | Convert text from snake_case to camelCase, substituting any substrings in
 'camelSubstitutions' with their camelCase variants.
-}
toCamelCase :: T.Text -> T.Text
toCamelCase t =
  let (first, rest) = T.breakOn "_" $ T.dropWhile (== '_') t
      splitRest =
        map capitalizeFirst . filter (not . T.null) $ T.splitOn "_" rest
      replace i =
        V.foldl'
          (\acc (needle, target) -> T.replace needle target acc)
          i
          camelSubstitutions
   in mconcat $ map replace (first : splitRest)

toCapitalCamelCase :: T.Text -> T.Text
toCapitalCamelCase = capitalizeFirst . toCamelCase

-- | Marshal text into a temporarily-allocated, NUL-terminated UTF-8 CString.
withUtf8 :: MonadIO m => T.Text -> (CString -> m a) -> m a
withUtf8 (TI.Text (TI.ByteArray ba#) off len) f = do
  arr <- liftIO do
    ptr <- mallocArray0 baSize
    copyByteArrayToPtr ptr ba off len
    pokeByteOff @Word8 ptr baSize 0
    pure ptr
  f arr <* liftIO (free arr)
 where
  ba = ByteArray ba#
  baSize = len - off
{-# INLINEABLE withUtf8 #-}

--------------------------------------------------------------------------------

-- | Wrapper function to make a GHC 'FastString' from Text.
fsText :: T.Text -> FastString
fsText (TI.Text (TI.ByteArray ba#) off len) =
  if off == 0 && I# (sizeofByteArray# ba#) == len
    then mkFastStringShortByteString (BI.SBS ba#)
    else
      mkFastStringShortByteString
        . (\(ByteArray ba'#) -> BI.SBS ba'#)
        $ cloneByteArray (ByteArray ba#) off len

-- | Wrapper over ghc-source-gen's 'string' function (which takes a 'String').
string :: HasLit e => T.Text -> e
string = lit . HsString NoSourceText . fsText

stringTy :: T.Text -> HsType'
stringTy = HsTyLit NoExtField . HsStrTy NoSourceText . fsText

-- | Make a new 'OccNameStr' from text.
textToOcc :: T.Text -> OccNameStr
textToOcc = occNameToStr . mkVarOccFS . fsText

instance From T.Text OccNameStr where
  from = textToOcc

instance From T.Text RdrNameStr where
  from = unqual . textToOcc

--------------------------------------------------------------------------------

-- instance TryFrom Rational Integer where
--   tryFrom r =
--     let n = numerator r
--         d = denominator r
--      in if n `mod` d == 0
--           then Right $ fromIntegral $ n `div` d
--           else Left $ TryFromException r Nothing

--------------------------------------------------------------------------------

data NameSpace
  = CName -- C name
  | HName -- Haskell name
  | GName -- GDExtension name
  deriving (Show)

data Format
  = SnakeCase
  | CamelCase
  deriving (Show)

data Capital
  = Capitalized
  | Uncapitalized
  | Unknown
  deriving (Show)

newtype Name (n :: NameSpace) (f :: Format) (c :: Capital) = MkName T.Text
  deriving (Show)

instance Monoid (Name 'HName f c) where
  mempty = MkName $ mempty

instance Semigroup (Name 'HName f c) where
  a <> b = MkName $ toText a <> toText b

instance From (Name 'HName 'CamelCase c) OccNameStr where
  from = textToOcc . coerce

instance From (Name 'HName f c) RdrNameStr where
  from = unqual . textToOcc . coerce

instance IsString (Name n f c) where
  fromString = MkName . T.pack

instance HasField "toHVal" HType HVal where
  getField = coerce

instance HasField "toHType" HVal HType where
  getField = coerce . capitalizeFirst . coerce

instance HasField "toHVal" GVal HVal where
  getField (MkName n) = MkName $ toCamelCase n

instance HasField "toGVal" GType GVal where
  getField (MkName n) = MkName $ toCamelCase $ T.toLower n

instance HasField "toHType" GType (BuildConfig -> HType) where
  getField (MkName n) b =
    case enumP of
      Just e -> MkName e
      Nothing ->
        MkName
          . fromMaybe (toCapitalCamelCase n)
          $ HM.lookup n (godotToForeignC b)
   where
    -- Special case for `enum::` types like
    enumP = case T.breakOnEnd "enum::" n of
      ("enum::", a) -> case T.breakOnEnd "." a of
        (clsDot, enumTy) -> Just $ toCamelCase $ T.init clsDot <> enumTy
      _ -> Nothing

instance HasField "toHVal" GEnum HVal where
  getField (MkName n) = MkName $ toCapitalCamelCase $ T.toLower n

instance HasField "fromForeignC" HType HType where
  getField (MkName n) = MkName $ fromMaybe n $ HM.lookup n foreignCToHs

instance HasField "capitalize" (Name n f 'Uncapitalized) (Name n f 'Capitalized) where
  getField = capitalize

instance HasField "capitalize" (Name n f 'Unknown) (Name n f 'Capitalized) where
  getField = capitalize

capitalize :: Name n f c -> Name n f 'Capitalized
capitalize (MkName n) = MkName $ capitalizeFirst n

toText :: Name n f c -> T.Text
toText = coerce

mkHType :: T.Text -> HType
mkHType = coerce

mkHVal :: T.Text -> HVal
mkHVal = coerce

mkGType :: T.Text -> GType
mkGType = coerce

mkGVal :: T.Text -> GVal
mkGVal = coerce

mkGEnum :: T.Text -> GEnum
mkGEnum = coerce

type HType = Name 'HName 'CamelCase 'Capitalized
type HVal = Name 'HName 'CamelCase 'Uncapitalized
type CType = Name 'CName 'CamelCase 'Unknown
type CVal = Name 'CName 'CamelCase 'Uncapitalized
type GType = Name 'GName 'CamelCase 'Unknown
type GVal = Name 'GName 'SnakeCase 'Uncapitalized
type GEnum = Name 'GName 'CamelCase 'Capitalized
