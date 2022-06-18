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

module Godot.Extension.Generate.Interface where

import Control.Lens (
  Field1 (_1),
  Field3 (_3),
  firstOf,
  folded,
  has,
  over,
  preview,
  view,
  (^.),
  (^?),
  _Just,
  _Right,
 )
import Control.Monad.State (State, get, modify, runState)
import Data.Generics.Labels ()
import Data.Generics.Sum (AsConstructor (_Ctor))
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.List (foldl', intersperse, uncons)
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.String.Interpolate (__i)
import qualified Data.Text as T
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Godot.Extension.Generate.Utils (
  autoGenHeader,
  cToForeignC,
  capitalizeFirst,
  toCamelCase,
 )
import Language.C (CNode, NodeInfo, fileOfNode, identToString, parseCFile)
import Language.C.Data.Ident (Ident)
import Language.C.Syntax.AST (
  CDeclaration (CDecl),
  CDeclarator (..),
  CDerivedDeclarator,
  CEnumeration,
  CExternalDeclaration (CDeclExt),
  CStorageSpecifier,
  CTranslationUnit (CTranslUnit),
  CTypeSpecifier,
 )
import Language.C.System.GCC (newGCC)
import Unsafe.Coerce (unsafeCoerce)

fileHeader :: T.Text
fileHeader =
  [__i|
    #{autoGenHeader}

    {-\# LANGUAGE StandaloneDeriving \#-}
    {-\# LANGUAGE DerivingStrategies \#-}
    {-\# LANGUAGE GeneralizedNewtypeDeriving \#-}
    {-\# LANGUAGE DuplicateRecordFields \#-}
    {-\# LANGUAGE StrictData \#-}
    {-\# LANGUAGE TypeApplications \#-}
    {-\# LANGUAGE NoFieldSelectors \#-}
    {-\# LANGUAGE OverloadedRecordDot \#-}

    module Godot.Extension.Extension where

    import Witch
    import Foreign.C
    import Foreign.Storable
    import Foreign.Ptr
    import Data.Coerce
    import Data.IORef
    import System.IO.Unsafe (unsafePerformIO)
    import Data.Functor ((<&>))

    \#include <godot/gdnative_interface.h>
  |]

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

-- | Wrapper around either a CType or CField that tracks the number of pointers.
data CItem a = MkCItem {nPtrs :: Int, item :: a}
  deriving (Show, Eq, Generic)

data TypeDef = MkTypeDef {name :: CType, ty :: CItem CType}
  deriving (Show, Eq, Generic)

data FunPtrDef = MkFunPtrDef
  { name :: CType
  , returns :: CItem CType
  , arguments :: [CItem CType]
  }
  deriving (Show, Eq, Generic)

data FieldDef = MkFieldDef {name :: CField, ty :: CItem CType}
  deriving (Show, Eq, Generic)

data StructDef = MkStructDef
  { ty :: CType
  , fields :: [Either FieldDef FunPtrDef]
  }
  deriving (Show, Eq, Generic)

data EnumDef = MkEnumDef {name :: CType}
  deriving (Show, Eq, Generic)

data GdnativeItem
  = MkStructItem StructDef
  | MkEnumItem EnumDef
  | MkFunPtrItem FunPtrDef
  | MkTypeDefItem TypeDef
  deriving (Show)

type InterfaceGen = State (HM.HashMap CType GdnativeItem)

{- | Special replacements when converting from C struct fields to Haskell
 fields.
-}
cFieldToHFieldReplace :: HM.HashMap CField HField
cFieldToHFieldReplace =
  HM.fromList $
    map
      (\(c, h) -> (MkCField c, MkHField h))
      [ ("type", "type'")
      , ("error", "error'")
      , ("id", "id'")
      ]

cTypeToHTypeReplace :: HM.HashMap CType HType
cTypeToHTypeReplace = unsafeCoerce cToForeignC -- TODO map over these elems like a sane person.

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
findSpecToCType spec =
  listToMaybe $
    mapMaybe (\test -> test spec) typeSpecToCType

cItemToCamelCase :: CItem CType -> T.Text
cItemToCamelCase (MkCItem nPtrs' ty') =
  mconcat $
    replicate nPtrs' "Ptr ("
      <> [toCamelCase ty'.unCType]
      <> replicate nPtrs' ")"

cItemTypeToHType :: CItem CType -> HType
cItemTypeToHType (MkCItem nPtrs' ty') =
  MkHType
    . mconcat
    $ replicate nPtrs' "Ptr ("
      <> [ unHType
            . fromMaybe (MkHType $ toCamelCase ty'.unCType)
            $ HM.lookup ty' cTypeToHTypeReplace
         ]
      <> replicate nPtrs' ")"

cFieldToHField :: CField -> HField
cFieldToHField cf =
  MkHField . fromMaybe (toCamelCase cf.unCField)
    . fmap unHField
    . (`HM.lookup` cFieldToHFieldReplace)
    $ cf

--------------------------------------------------------------------------------

typeSpecifiers :: CDeclaration a -> [CTypeSpecifier a]
typeSpecifiers (CDecl specs _ _) = mapMaybe (^? _Ctor @"CTypeSpec") specs
typeSpecifiers _ = undefined

storageSpecifiers :: CDeclaration a -> [CStorageSpecifier a]
storageSpecifiers (CDecl specs _ _) = mapMaybe (^? _Ctor @"CStorageSpec") specs
storageSpecifiers _ = undefined

declarators :: CDeclaration a -> [CDeclarator a]
declarators (CDecl _ declrs _) = mapMaybe (view _1) declrs
declarators _ = undefined

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
      ty' = listToMaybe . mapMaybe findSpecToCType $ typeSpecifiers decl
   in MkCItem ptrNum <$> ty'

--------------------------------------------------------------------------------

-- | Register a 'GdnativeItem' definition for use in future generation.
registerItem :: CType -> GdnativeItem -> InterfaceGen ()
registerItem c i = modify (HM.insert c i)

-- | Convert an enum 'CDeclaration' into an 'EnumDef'.
processEnum :: CDeclaration NodeInfo -> EnumDef
processEnum decl =
  MkEnumDef
    . mkCType
    . fromJust
    . listToMaybe
    . mapMaybe (^? _Ctor @"CDeclr" . _1 . _Just)
    $ declarators decl

-- | Convert a typedef 'CDeclaration' into an 'TypeDef'.
processTypeDef :: CDeclaration NodeInfo -> TypeDef
processTypeDef decl =
  MkTypeDef
    (MkCType (fromJust $ declName decl))
    (fromJust $ declReturns decl)

-- | Convert a function pointer typedef 'CDeclaration' into an 'FunPtrDef'.
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

-- | Convert a struct 'CDeclaration' into an 'StructDef'.
processStruct :: CDeclaration NodeInfo -> StructDef
processStruct decl =
  let fields' =
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
        (map processStructItem fields')
 where
  processField decl' =
    MkFieldDef
      (MkCField . fromJust $ declName decl')
      (fromJust $ declReturns decl')
  processStructItem decl'
    | isFunPtr decl' = Right $ processFunPtr decl'
    | otherwise = Left $ processField decl'

-- | Turn a 'CDeclaration' into a 'GdnativeItem'.
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

-- | Generate a c2hs enum shim.
genEnum :: EnumDef -> InterfaceGen T.Text
genEnum e@(MkEnumDef name') = do
  let hName = toCamelCase name'.unCType
  registerItem name' (MkEnumItem e)
  pure
    [__i|
      {\#enum #{unCType name'} as #{hName} {underscoreToCase}
        deriving (Show, Eq, Ord, Bounded) \#}
      instance From #{hName} CInt where
        from = fromIntegral . fromEnum
      instance From CInt #{hName} where
        from = toEnum . fromIntegral
    |]

-- | Generate a foreign import shim from a function pointer.
genForeignImport :: FunPtrDef -> InterfaceGen T.Text
genForeignImport fp@(MkFunPtrDef name' returns' arguments') = do
  let hName = capitalizeFirst $ toCamelCase name'.unCType

  args <- mconcat . intersperse " -> " <$> mapM genArg arguments'
  let preRet :: T.Text = if null arguments' then "" else "->"
  rets <- genArg (over #nPtrs (subtract 1) returns')

  registerItem name' (MkFunPtrItem fp)
  pure
    [__i|
      type #{hName} = #{args} #{preRet} IO (#{rets})
      foreign import ccall "dynamic" mk#{hName}
        :: FunPtr #{hName}
        -> #{hName}
    |]
 where
  genArg cTy = do
    entry <- HM.lookup cTy.item <$> get
    pure case entry of
      Just (MkFunPtrItem _) -> [__i|FunPtr (#{unHType $ cItemTypeToHType cTy})|]
      Just (MkEnumItem _) -> "CInt"
      _ -> unHType (cItemTypeToHType cTy)

-- | Generate a c2hs newtype pointer shim.
genNewtypePtrDecl :: CType -> InterfaceGen T.Text
genNewtypePtrDecl name' = do
  let hName = toCamelCase name'.unCType
  registerItem name' (MkTypeDefItem $ MkTypeDef name' (MkCItem 0 name'))
  pure
    [__i|
         {\#pointer #{unCType name'} as #{hName} newtype \#}
         deriving newtype instance Show #{hName}
       |]

-- | Generate a c2hs pointer shim (for a struct).
genPtrDecl :: CType -> T.Text
genPtrDecl name' =
  let hName = toCamelCase name'.unCType
   in [__i|{\#pointer *#{unCType name'} as #{hName}Ptr->#{hName} \#}|]

{- | Generate a Haskell structure definition, along with a 'Storable' instance
 and a c2hs pointer shim.
-}
genStruct :: StructDef -> InterfaceGen T.Text
genStruct s@(MkStructDef name' fields') = do
  let hName = toCamelCase name'.unCType
      cName = name'.unCType

  generatedFields <- mapM genField fields'
  let fieldEntries =
        mconcat
          ( intersperse "\n  , " $
              map (\(f, t, _) -> f.unHField <> " :: " <> t.unHType) generatedFields
          )
      dataDecl =
        [__i|
          data #{hName} = #{hName}
            { #{fieldEntries}
            }
        |]
  peekBody <-
    case uncons fields' of
      Just (x, xs) -> do
        access <- genFieldAccessor x
        accessors <- mapM genFieldAccessor xs
        pure $
          "<$> " <> access
            <> if null xs
              then mempty
              else
                "\n    <*> "
                  <> mconcat (intersperse "\n    <*> " accessors)
      Nothing -> pure ""
  let storableDecl =
        [__i|
          instance Storable #{hName} where
            sizeOf _ = {\#sizeof #{cName} \#}
            alignment _ = {\#alignof #{cName} \#}
            peek ptr = #{hName}
              #{peekBody}
            poke _ _ = error "Don't poke #{hName}"
        |]
  modify (HM.insert name' (MkStructItem s))
  pure
    . mconcat
    . intersperse "\n"
    $ mapMaybe (^. _3) generatedFields <> [dataDecl, genPtrDecl name', storableDecl]
 where
  fieldAcquire f = [__i|{\#get #{unCType name'}->#{unCField f}\#} ptr|] :: T.Text
  funPtrConversionFunc fname ftype =
    [__i|
      (mk#{capitalizeFirst $ toCamelCase $ unCType ftype} <$> #{fieldAcquire fname})
    |]
  enumConversionFunc fname =
    [__i|
      (toEnum . fromIntegral <$> #{fieldAcquire fname})
    |]
  structConversionFunc fname ftype =
    [__i|
      (peek . coerce @_ @(#{unHType $ cItemTypeToHType ftype}) =<< #{fieldAcquire fname})
    |]
  typeDefConversion fname ftype =
    [__i|
      (coerce @_ @(#{unHType $ cItemTypeToHType ftype}) <$> #{fieldAcquire fname})
    |]
  -- Generate the accessor and conversion code used to peek a C struct into
  -- a Haskell type.
  genFieldAccessor = \case
    Left (MkFieldDef fname fTy) -> do
      entry <- HM.lookup fTy.item <$> get
      pure case entry of
        Just (MkFunPtrItem _) -> funPtrConversionFunc fname (fTy.item)
        Just (MkEnumItem _) -> enumConversionFunc fname
        Just (MkStructItem _) -> structConversionFunc fname (over #nPtrs (+ 1) fTy)
        Just (MkTypeDefItem _) -> typeDefConversion fname fTy
        _ -> fieldAcquire fname
    Right (MkFunPtrDef fname _ _) ->
      pure
        [__i|
          (mk#{capitalizeFirst $ toCamelCase $ unCType fname} <$> #{fieldAcquire $ MkCField $ unCType $ fname})
        |]
  -- Generate Haskell data fields from C struct fields.
  genField f =
    case f of
      Left (MkFieldDef fname ty') ->
        pure
          ( cFieldToHField $ MkCField $ toCamelCase fname.unCField
          , cItemTypeToHType ty'
          , Nothing
          )
      Right fp@(MkFunPtrDef fname _ _) -> do
        fi <- genForeignImport fp
        pure
          ( MkHField $ toCamelCase fname.unCType
          , MkHType $ capitalizeFirst $ toCamelCase fname.unCType
          , -- TODO FunPtr technically uses CType for its name
          Just fi
          )

-- | Generate a c2hs typedef shim.
genTypeDef :: TypeDef -> InterfaceGen T.Text
genTypeDef t@(MkTypeDef name' ty') = do
  if ty'.nPtrs > 0
    then genNewtypePtrDecl name'
    else do
      modify (HM.insert name' (MkTypeDefItem t))
      pure $
        [__i|
          type #{capitalizeFirst $ toCamelCase $ unCType name'} =
            {\#type #{unCType name'} \#}
        |]

-- | Generate any 'GdnativeItem'.
genItem :: GdnativeItem -> InterfaceGen T.Text
genItem = \case
  MkFunPtrItem f -> genForeignImport f
  MkEnumItem e -> genEnum e
  -- MkStructItem s -> mconcat <$> sequence [genStruct s, pure "\n", genStructHelpers s]
  MkStructItem s@(MkStructDef n _) ->
    if n.unCType == "GDNativeInterface"
      then mconcat <$> sequence [genStruct s, pure "\n", genStructHelpers s]
      else genStruct s
  MkTypeDefItem t -> genTypeDef t

genStructHelpers :: StructDef -> InterfaceGen T.Text
genStructHelpers (MkStructDef _ fields') =
  mconcat
    . intersperse "\n"
    . mapMaybe id
    <$> mapM fieldToHelperShim fields'
 where
  fieldToHelperShim = \case
    Left (MkFieldDef fn _) -> do
      entry <- HM.lookup (MkCType $ fn.unCField) <$> get
      pure case entry of
        Just (MkFunPtrItem (MkFunPtrDef n _ _)) ->
          Just $ genHelperShim (MkCField $ n.unCType)
        _ -> Nothing
    Right (MkFunPtrDef n _ _) -> pure $ Just $ genHelperShim (MkCField $ n.unCType)
  genHelperShim f =
    [__i|
      #{unHField $ cFieldToHField f} =
        unsafePerformIO
          (readIORef interface <&> (.#{unHField $ cFieldToHField f}))
      {-\# NOINLINE #{unHField $ cFieldToHField f} \#-}
    |]

footer :: T.Text
footer =
  [__i|
    interface :: IORef GdnativeInterface
    interface =
      unsafePerformIO
        . newIORef
        $ error "Attempted to access GdnativeInterface before initialization!"
    {-\# NOINLINE interface \#-}
    
    initInterface :: GdnativeInterfacePtr -> IO ()
    initInterface i = writeIORef interface =<< peek i

    withInterface :: (GdnativeInterface -> IO a) -> IO a
    withInterface f = readIORef interface >>= f
  |]

parseAndGenerate :: HasCallStack => IO T.Text
parseAndGenerate = do
  !parsedGdnativeInterface <-
    parseCFile
      (newGCC "gcc")
      Nothing
      ["-Igd-haskell/godot-headers/godot/"]
      "gd-haskell/godot-headers/godot/gdnative_interface.h"
  let decls = map
        ( \case
            (CDeclExt d) -> d
            _ -> undefined
        )
        $ case parsedGdnativeInterface of
          Left _ -> undefined
          Right (CTranslUnit t _) -> t
  let genItems = mapM genItem $ mapMaybe processItem decls
  pure $
    fileHeader
      <> "\n"
      <> ( foldl' (<>) mempty
            . intersperse "\n"
            $ fst $ runState genItems mempty
         )
      <> "\n"
      <> footer