{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Item where

import Control.Applicative
import Control.Lens hiding (uncons, (|>))
import Control.Monad
import Control.Monad.State (MonadTrans (lift), State, StateT (runStateT), get, modify, runState)
import Control.Monad.Trans.Maybe
import Data.Char (toUpper)
import Data.Coerce
import Data.Foldable
import Data.Generics.Sum (AsConstructor (_Ctor))
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.List (intersperse, uncons)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Text.Builder.Linear.Buffer
import qualified Data.Text.Lazy as TL
import Debug.Trace
import Foreign (FunPtr)
import GHC.Generics
import GHC.Records (HasField (getField))
import GHC.Stack (HasCallStack)
import Language.C (CNode, NodeInfo (NodeInfo), Pos (posOf), Pretty (pretty), fileOfNode, identToString, isSourcePos, parseCFile, posFile)
import Language.C.Data.Ident (Ident (Ident))
import Language.C.Syntax.AST
import Language.C.System.GCC (newGCC)
import NeatInterpolation (trimming)
import Text.Pretty.Simple (pPrint, pShow)
import Prelude hiding (head)

fileHeader :: T.Text
fileHeader =
  [trimming|
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
  |]

head :: HasCallStack => [a] -> a
head (x : _) = x
head _ = error "H"

camelSubsitutions :: [(T.Text, T.Text)]
camelSubsitutions =
  [ ("GDNative", "Gdnative")
  ]

capitalizeFirst :: T.Text -> T.Text
capitalizeFirst t =
  if T.null t
    then t
    else (T.singleton . toUpper $ T.head t) <> T.tail t

toCamelCase :: T.Text -> T.Text
toCamelCase t =
  let (first, rest) = T.breakOn "_" t
      splitRest = map capitalizeFirst . filter (not . T.null) $ T.splitOn "_" rest
      replaceCustom i =
        foldl'
          (\acc (needle, target) -> T.replace needle target acc)
          i
          camelSubsitutions
   in foldl' (<>) "" $ map replaceCustom (first : splitRest)

--------------------------------------------------------------------------------

newtype CType = MkCType {unCType :: T.Text}
  deriving (Show, Eq, Generic)

instance Hashable CType

mkCType :: Ident -> CType
mkCType = MkCType . T.pack . identToString

newtype CField = MkCField {unCField :: T.Text}
  deriving (Show, Eq, Generic)

instance Hashable CField

mkCField :: Ident -> CField
mkCField = MkCField . T.pack . identToString

newtype HType = MkHType {unHType :: T.Text}
  deriving (Show, Eq, Generic)

instance Hashable HType

newtype HField = MkHField {unHField :: T.Text}
  deriving (Show, Eq, Generic)

instance Hashable HField

data CItem a = MkCItem {nPtrs :: Int, item :: a}
  deriving (Show, Eq, Generic)

data TypeDef = MkTypeDef {name :: CType, ty :: CItem CType}
  deriving (Show, Eq, Generic)

data FunPtrDef = MkFunPtrDef {name :: CType, returns :: CItem CType, arguments :: [CItem CType]}
  deriving (Show, Eq, Generic)

data FieldDef = MkFieldDef {name :: CField, ty :: CItem CType}
  deriving (Show, Eq, Generic)

data StructDef = MkStructDef {ty :: CType, fields :: [Either FieldDef FunPtrDef]}
  deriving (Show, Eq, Generic)

data EnumDef = MkEnumDef {name :: CType}
  deriving (Show, Eq, Generic)

data GdnativeItem
  = MkStructItem StructDef
  | MkEnumItem EnumDef
  | MkFunPtrItem FunPtrDef
  | MkTypeDefItem TypeDef
  deriving (Show)

type GD = State (HM.HashMap CType GdnativeItem)

cFieldToHField :: HM.HashMap CField HField
cFieldToHField =
  HM.fromList $
    map
      (\(c, h) -> (MkCField c, MkHField h))
      [ ("type", "type'")
      ]

cTypeToHType :: HM.HashMap CType HType
cTypeToHType =
  HM.fromList $
    map
      (\(c, h) -> (MkCType c, MkHType h))
      [ ("uint64_t", "CULong")
      , ("uint32_t", "CUInt")
      , ("uint16_t", "CUShort")
      , ("uint8_t", "CUChar")
      , ("int64_t", "CLong")
      , ("int32_t", "CInt")
      , ("int16_t", "CShort")
      , ("wchar_t", "CInt")
      , ("char", "CChar")
      , ("size_t", "CInt")
      , ("float", "CFloat")
      , ("dobule", "CDouble")
      , ("char16_t", "CShort")
      , ("char32_t", "CInt")
      , ("void", "()")
      ]

typeSpecToCType :: [(CTypeSpecifier NodeInfo -> Maybe CType)]
typeSpecToCType =
  [ fmap mkCType . preview (_Ctor @"CTypeDef" . _1)
  , fmap (const (MkCType "int")) . preview (_Ctor @"CIntType")
  , fmap (const (MkCType "char")) . preview (_Ctor @"CCharType")
  , fmap (const (MkCType "void")) . preview (_Ctor @"CVoidType")
  , fmap (const (MkCType "double")) . preview (_Ctor @"CDoubleType")
  , fmap (const (MkCType "float")) . preview (_Ctor @"CFloatType")
  , \t -> Just $ MkCType $ "UNDEFINED >>>> " <> (T.pack $ show t)
  ]

findSpecToCType :: CTypeSpecifier NodeInfo -> Maybe CType
findSpecToCType spec = listToMaybe $ mapMaybe (\test -> test spec) typeSpecToCType

cItemToCamelCase :: CItem CType -> T.Text
cItemToCamelCase (MkCItem nPtrs ty) =
  foldl' (<>) mempty $ replicate nPtrs "Ptr (" <> [toCamelCase ty.unCType] <> replicate nPtrs ")"

cItemTypeToHType :: CItem CType -> HType
cItemTypeToHType (MkCItem nPtrs ty) =
  MkHType $ foldl' (<>) mempty $ replicate nPtrs "Ptr (" <> [unHType $ fromMaybe (MkHType $ toCamelCase ty.unCType) $ HM.lookup ty cTypeToHType] <> replicate nPtrs ")"

--------------------------------------------------------------------------------

typeSpecifiers :: CDeclaration a -> [CTypeSpecifier a]
typeSpecifiers (CDecl specs _ _) = mapMaybe (^? _Ctor @"CTypeSpec") specs

storageSpecifiers :: CDeclaration a -> [CStorageSpecifier a]
storageSpecifiers (CDecl specs _ _) = mapMaybe (^? _Ctor @"CStorageSpec") specs

declarators :: CDeclaration a -> [CDeclarator a]
declarators (CDecl _ declrs _) = mapMaybe (view _1) declrs

derivedDeclrs :: CDeclarator a -> [CDerivedDeclarator a]
derivedDeclrs (CDeclr _ d _ _ _) = d

typeDef :: [CTypeSpecifier a] -> Maybe (Ident, a)
typeDef = listToMaybe . mapMaybe (preview (_Ctor @"CTypeDef"))

isTypeDef :: CDeclaration NodeInfo -> Bool
isTypeDef = isJust . typeDef . typeSpecifiers

typedef :: [CStorageSpecifier a] -> Maybe a
typedef = listToMaybe . mapMaybe (preview (_Ctor @"CTypedef"))

isTypedef :: CDeclaration a -> Bool
isTypedef = isJust . typedef . storageSpecifiers

isVoid :: [CTypeSpecifier a] -> Bool
isVoid = not . null . mapMaybe (preview (_Ctor @"CVoidType"))

enumType :: [CTypeSpecifier a] -> Maybe (CEnumeration a, a)
enumType = listToMaybe . mapMaybe (preview (_Ctor @"CEnumType"))

isEnum :: CDeclaration a -> Bool
isEnum = isJust . enumType . typeSpecifiers

isFunPtr :: CDeclaration a -> Bool
isFunPtr =
  any (isJust . preview (_Ctor @"CFunDeclr"))
    . concatMap derivedDeclrs
    . declarators

isStruct :: CDeclaration a -> Bool
isStruct = any (isJust . preview (_Ctor @"CSUType")) . typeSpecifiers

isInGdnativeFile :: CNode n => n -> Bool
isInGdnativeFile =
  maybe False (== "gd-haskell/godot-headers/godot/gdnative_interface.h")
    . fileOfNode

identToText :: Ident -> T.Text
identToText = T.pack . identToString

declName :: CDeclaration NodeInfo -> Maybe T.Text
declName decl =
  identToText
    <$> (firstOf (folded . _Ctor @"CDeclr" . _1 . _Just) . declarators $ decl)

declPtrsCt :: CDeclaration NodeInfo -> Int
declPtrsCt =
  length
    . filter (has (_Ctor @"CPtrDeclr"))
    . maybe [] derivedDeclrs
    . listToMaybe
    . declarators

declReturns :: CDeclaration NodeInfo -> Maybe (CItem CType)
declReturns decl =
  let ptrNum = declPtrsCt decl
      ty = listToMaybe . mapMaybe findSpecToCType $ typeSpecifiers decl
   in MkCItem ptrNum <$> ty

--------------------------------------------------------------------------------

registerItem :: CType -> GdnativeItem -> GD ()
registerItem c i = modify (HM.insert c i)

processEnum :: CDeclaration NodeInfo -> EnumDef
processEnum decl =
  MkEnumDef
    . mkCType
    . fromJust
    . listToMaybe
    . mapMaybe (^? _Ctor @"CDeclr" . _1 . _Just)
    $ declarators decl

processTypeDef :: CDeclaration NodeInfo -> TypeDef
processTypeDef decl =
  MkTypeDef
    (MkCType (fromJust $ declName decl))
    (fromJust $ declReturns decl)

processFunPtr :: CDeclaration NodeInfo -> FunPtrDef
processFunPtr decl =
  let childDecls =
        concat
          . mapMaybe (^? _Ctor @"CFunDeclr" . _1 . _Right . _1)
          . concatMap derivedDeclrs
          $ declarators decl
   in MkFunPtrDef
        (MkCType $ fromJust $ declName decl)
        (fromJust $ declReturns decl)
        (map (fromJust . declReturns) childDecls)

processStruct :: CDeclaration NodeInfo -> StructDef
processStruct decl =
  let fields =
        fromJust
          . firstOf
            ( folded
                . _Ctor @"CSUType"
                . _1
                . _Ctor @"CStruct"
                . _3
                . _Just
            )
          $ typeSpecifiers decl
   in MkStructDef
        (MkCType $ fromJust $ declName decl)
        (map processStructItem fields)
 where
  processField decl =
    MkFieldDef
      (MkCField . fromJust $ declName decl)
      (fromJust $ declReturns decl)
  processStructItem decl
    | isFunPtr decl = Right $ processFunPtr decl
    | otherwise = Left $ processField decl

processItem :: CDeclaration NodeInfo -> Maybe GdnativeItem
processItem decl
  | isTypedef decl && isInGdnativeFile decl =
      pure
        if
            | isEnum decl -> MkEnumItem $ processEnum decl
            | isFunPtr decl -> MkFunPtrItem $ processFunPtr decl
            | isStruct decl -> MkStructItem $ processStruct decl
            | otherwise -> MkTypeDefItem $ processTypeDef decl
  | otherwise = Nothing

--------------------------------------------------------------------------------

genEnum :: EnumDef -> T.Text
genEnum (MkEnumDef name) =
  "{#enum"
    <> name.unCType
    <> " as "
    <> toCamelCase name.unCType
    <> " {underscoreToCase}"
    <> "\n  deriving (Show, Eq, Ord, Bounded) #}"

genForeignImport :: FunPtrDef -> T.Text
genForeignImport (MkFunPtrDef name returns arguments) =
  "type "
    <> toCamelCase name.unCType
    <> " = "
    <> foldl' (<>) mempty (intersperse " -> " $ map (unHType . cItemTypeToHType) arguments)
    <> (if null arguments then "" else " -> ")
    <> "IO ("
    <> (unHType . cItemTypeToHType) returns
    <> ")"
    <> "\nforeign import ccall \"dynamic\" mk"
    <> toCamelCase name.unCType
    <> " :: FunPtr "
    <> toCamelCase name.unCType
    <> "\n  -> "
    <> toCamelCase name.unCType

genPtrDecl :: CType -> T.Text
genPtrDecl name =
  "{#pointer "
    <> name.unCType
    <> " as "
    <> toCamelCase name.unCType
    <> " newtype #}"
    <> "\nderiving newtype instance Show "
    <> toCamelCase name.unCType

genStruct :: StructDef -> T.Text
genStruct (MkStructDef ty fields) =
  let fields' = map genField fields
      hTy = toCamelCase ty.unCType
      dataDecl =
        "data "
          <> hTy
          <> " = "
          <> hTy
          <> "\n  { "
          <> foldl'
            (<>)
            mempty
            ( intersperse "\n  , " $
                map (\(f, t, _) -> f.unHField <> " :: " <> t.unHType) fields'
            )
          <> "\n  }"
      peekBody =
        case uncons fields of
          Just (x, xs) ->
            " <$> "
              <> fieldAccessor x
              <> if null xs
                then mempty
                else
                  " <*> "
                    <> ( foldl' (<>) mempty $
                          intersperse "\n      <*> " $ map fieldAccessor xs
                       )
      storableDecl =
        "instance Storable "
          <> hTy
          <> " where "
          <> "\n  sizeOf _ = {#sizeof "
          <> ty.unCType
          <> " #}"
          <> "\n  alignment _ = {#alignof "
          <> ty.unCType
          <> " #}"
          <> "\n  peek ptr = "
          <> hTy
          <> peekBody
   in foldl' (<>) mempty (mapMaybe (^. _3) fields')
        <> dataDecl
        <> "\n"
        <> genPtrDecl ty
        <> "\n"
        <> storableDecl
 where
  fieldAcquire f = "{#get " <> ty.unCType <> "->" <> f.unCField <> " #} ptr"
  fieldAccessor = \case
    Left (MkFieldDef name _) -> fieldAcquire name
    Right (MkFunPtrDef name returns args) ->
      "(mk"
        <> toCamelCase name.unCType
        <> " <$> "
        <> fieldAcquire (MkCField $ name.unCType)
        <> ")"
  genField f =
    case f of
      Left (MkFieldDef name ty) ->
        ( MkHField $ toCamelCase name.unCField
        , cItemTypeToHType ty
        , Nothing
        )
      Right fp@(MkFunPtrDef name returns args) ->
        ( MkHField $ toCamelCase name.unCType
        , MkHType $ capitalizeFirst $ toCamelCase name.unCType
        , -- TODO FunPtr technically uses CType for its name
        Just $ genForeignImport fp
        )

genTypeDef :: TypeDef -> T.Text
genTypeDef (MkTypeDef name ty) =
  if ty.nPtrs > 0
    then genPtrDecl name
    else "type " <> toCamelCase name.unCType <> " = {#type " <> name.unCType <> " #}"

genItem :: GdnativeItem -> T.Text
genItem (MkFunPtrItem f) = genForeignImport f
genItem (MkEnumItem e) = genEnum e
genItem (MkStructItem s) = genStruct s
genItem (MkTypeDefItem t) = genTypeDef t