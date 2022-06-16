{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

module Godot.Extension.Generate.Utils where

import Data.Char (toUpper)
import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import GHC.Data.FastString (FastString, mkFastStringShortByteString)

import Control.Monad.Identity (Identity (runIdentity))
import qualified Data.ByteString.Short.Internal as BI
import Data.Maybe (fromJust, isJust)
import Data.Primitive.ByteArray (ByteArray (ByteArray), copyByteArrayToPtr, sizeofByteArray)
import qualified Data.Text.Array as TI
import qualified Data.Text.Internal as TI
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Heap as V
import Data.Word (Word8)
import Foreign (Storable (pokeByteOff), allocaArray0)
import Foreign.C.String (CString)
import GHC.IO.Unsafe (unsafePerformIO)
import Text.RE.Replace (CaptureOrdinal (getCaptureOrdinal), REContext (SUB), RELocation (locationCapture), Replace, replaceAllCaptures)
import Text.RE.TDFA (compileRegex, (*=~))

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
 types.
-}
godotToForeignC :: HM.HashMap T.Text T.Text
godotToForeignC =
  HM.fromList
    [ ("Nil", "()")
    , ("bool", "CChar")
    , ("int", "CLong")
    , ("float", "CDouble")
    ]

-- | Whether a Godot type is a primitive C type.
isGodotBaseType :: T.Text -> Bool
isGodotBaseType = isJust . (`HM.lookup` godotToForeignC)

-- | Substrings to camelize when converting Text to camelcase.
camelSubstitutions :: V.Vector (T.Text, T.Text)
camelSubstitutions =
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
      ]

vIntersperse :: a -> V.Vector a -> V.Vector a
vIntersperse i v =
  let targetLen = (V.length v * 2) - 1
      f n
        | n `mod` 2 == 0 = v V.! (n `div` 2)
        | otherwise = i
   in V.generate targetLen f

-- | Regex-based substitution function.
camelSubstitutionsRE :: T.Text -> T.Text
camelSubstitutionsRE t = replaceAllCaptures SUB f (t *=~ substRE)
 where
  substTargets = V.map snd camelSubstitutions
  substStr =
    T.unpack $
      "(?=("
        <> ( V.foldl' (<>) mempty $ vIntersperse "|" (V.map fst camelSubstitutions)
           )
        <> "))"
  substRE = fromJust (compileRegex substStr)
  f _ loc _ = Just $ substTargets V.! (getCaptureOrdinal (locationCapture loc) - 1)

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
  let (first, rest) = T.breakOn "_" t
      splitRest = map capitalizeFirst . filter (not . T.null) $ T.splitOn "_" rest
   in mconcat $ map camelSubstitutionsRE (first : splitRest)

autoGenHeader :: T.Text
autoGenHeader = "{- This file was generated by `gd-extension-gen`. -}"

-- | Wrapper function to make a GHC 'FastString' from Text.
fsText :: T.Text -> FastString
fsText (TI.Text (TI.Array ba) _ _) = mkFastStringShortByteString (BI.SBS ba)
{-# INLINE fsText #-}

-- | Marshal text into a temporarily-allocated, NUL-terminated UTF-8 CString.
withUtf8 :: T.Text -> (CString -> IO a) -> IO a
withUtf8 (TI.Text (TI.Array ba#) _ _) f =
  allocaArray0 baSize \ptr -> do
    copyByteArrayToPtr ptr ba 0 baSize
    pokeByteOff @Word8 ptr baSize 0
    f ptr
 where
  ba = ByteArray ba#
  baSize = sizeofByteArray ba