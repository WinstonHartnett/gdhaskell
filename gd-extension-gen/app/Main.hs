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

module Main where

import Control.Applicative
import Control.Lens hiding (uncons, (|>))
import Control.Monad
import Control.Monad.State (State, StateT (runStateT), get, modify, runState)
import Data.Char (toUpper)
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
import Text.Pretty.Simple (pPrint)
import Prelude hiding (head)
import Item (processItem, genItem)

-- head :: HasCallStack => [a] -> a
-- head (x : _) = x
-- head _ = error "H"

-- fromLeft :: Either a b -> (b -> a) -> a
-- fromLeft (Left x) _ = x
-- fromLeft (Right x) f = f x

-- data TypeText = MkTypeName {c :: T.Text, h :: T.Text}
--   deriving (Show, Eq, Generic)

-- instance Hashable TypeText

-- instance Monoid TypeText where
--   mempty = MkTypeName "" ""

-- instance Semigroup TypeText where
--   a <> b = MkTypeName (a.c <> b.c) (a.h <> b.h)

-- data FieldText = MkFieldName {c :: T.Text, h :: T.Text}
--   deriving (Show, Eq, Generic)

-- instance Hashable FieldText

-- instance Monoid FieldText where
--   mempty = MkFieldName "" ""

-- instance Semigroup FieldText where
--   a <> b = MkFieldName (a.c <> b.c) (a.h <> b.h)

-- -- | Text subtitutions to be performed when converting C field names to Haskell fields.
-- fieldReplacements :: HM.HashMap T.Text T.Text
-- fieldReplacements =
--   HM.fromList
--     [ ("type", "type'")
--     , ("Type", "Type'")
--     ]

-- replaceText :: T.Text -> T.Text
-- replaceText t = fromMaybe t $ HM.lookup t fieldReplacements

-- typeName :: T.Text -> TypeText
-- typeName c =
--   MkTypeName
--     { c = c
--     , h = replaceText $ capitalizeFirst $ toCamelCase c
--     }

-- fieldName :: T.Text -> FieldText
-- fieldName c =
--   MkFieldName
--     { c = c
--     , h = replaceText $ toCamelCase c
--     }

-- -- data StorableImplInfo
-- --   = -- | Adds function pointer import.
-- --     MkFunPtr TypeText FieldText
-- --   | -- | Adds coerce after function pointer.
-- --     MkRequiresCoerce TypeText FieldText
-- --   | -- | Gets field and applies custom marshalling.
-- --     MkRequiresCustomMarshalling T.Text TypeText FieldText
-- --   | -- | Just gets field from C.
-- --     MkBare TypeText FieldText

-- -- | Global substitutions to perform when converting to camel case.
-- globalReplacements :: [(T.Text, T.Text)]
-- globalReplacements =
--   [ ("GDNative", "Gdnative")
--   ]

-- capitalizeFirst :: T.Text -> T.Text
-- capitalizeFirst t =
--   if T.null t
--     then t
--     else (T.singleton . toUpper $ T.head t) <> T.tail t

-- toCamelCase :: T.Text -> T.Text
-- toCamelCase t =
--   let (first, rest) = T.breakOn "_" t
--       splitRest = map capitalizeFirst . filter (not . T.null) $ T.splitOn "_" rest
--       replaceCustom i =
--         foldl'
--           (\acc (needle, target) -> T.replace needle target acc)
--           i
--           globalReplacements
--    in foldl' (<>) "" $ map replaceCustom (first : splitRest)

-- buildText :: [T.Text] -> T.Text
-- buildText t = runBuffer (\b -> foldlIntoBuffer (|>) b t)

-- --------------------------------------------------------------------------------


-- -- | Generate enum shim text.
-- genEnumDecl :: TypeText -> T.Text
-- genEnumDecl g =
--   buildText
--     [ "{#enum "
--     , g.c
--     , " as "
--     , g.h
--     , " {underscoreToCase}"
--     , "\n  deriving (Show, Eq, Ord, Bounded) #}"
--     ]

-- -- | Generate pointer shim text.
-- genPtrDecl :: TypeText -> T.Text
-- genPtrDecl g =
--   buildText
--     [ "{#pointer"
--     , g.c
--     , " as "
--     , g.h
--     , " newtype #}"
--     , "\nderiving newtype instance Show"
--     , g.h
--     ]

-- genStorableImpl :: TypeText -> [Either BareFieldType FunPtrType] -> T.Text
-- genStorableImpl g i =
--   buildText
--     [ "instance Storable "
--     , g.h
--     , " where "
--     , "\n  sizeOf _ = {#sizeof "
--     , g.c
--     , " #}"
--     , "\n  alignment _ = {#alignof "
--     , g.c
--     , " #}"
--     , "\n  peek ptr = "
--     , g.h
--     , peekBody
--     ]
--  where
--   fieldAcquire t f = buildText ["{#get ", t.c, "->", f.c, " #} ptr"]
--   convertItemToStoreBody i = case i of
--     Left (name, _) -> fieldAcquire g (fieldName name)
--     Right (name, _, _) ->
--       let fname = fieldName name
--        in "(mk" <> fname.h <> " <$> " <> fieldAcquire g fname <> ")"
--   peekBody = case uncons i of
--     Just (x, xs) ->
--       buildText $
--         " <$> " :
--         convertItemToStoreBody x :
--         if null xs then mempty else " <*> " : (intersperse "\n      <*> " $ map convertItemToStoreBody xs)
--     Nothing -> ""

-- genForeignImportDecl :: TypeText -> T.Text -> [T.Text] -> T.Text
-- genForeignImportDecl t returns a =
--   buildText
--     [ "type "
--     , t.h
--     , " = "
--     , buildText $ intersperse " -> " $ a ++ ["IO (" <> returns <> ")"]
--     , "\nforeign import ccall \"dynamic\" mk"
--     , t.h
--     , " :: FunPtr "
--     , t.h
--     , "\n  -> "
--     , t.h
--     ]

-- typeVariantToTypeText :: TypeVariant T.Text -> T.Text
-- typeVariantToTypeText = \case
--   MkArg t -> toCamelCase $ fromMaybe t $ HM.lookup t cTypeReplacements
--   MkPtr v -> "Ptr (" <> typeVariantToTypeText v <> ")"

-- convertVariant :: TypeVariant T.Text -> State GdnativeState T.Text
-- convertVariant tv = do
--   entry <- HM.lookup (typeName $ pullArg tv) . fst <$> get
--   let typeText = case entry of
--         Just e -> case e of
--           MkStructDef (name, _) -> toCamelCase name
--           -- MkEnumDef name -> "CEnum " <> toCamelCase name
--           MkEnumDef name -> "CInt"
--           MkTypeSyn (name, _) -> toCamelCase name
--           MkFunPtrDef (name, _, _) -> toCamelCase name
--         Nothing -> toCamelCase $ fromMaybe (pullArg tv) $ HM.lookup (pullArg tv) cTypeReplacements
--   pure $ wrapTypeVariantText typeText tv
--  where
--   wrapTypeVariantText innerText = \case
--     MkArg _ -> innerText
--     MkPtr v -> "Ptr (" <> wrapTypeVariantText innerText v <> ")"

-- genDataDecl :: TypeText -> [(FieldText, [TypeVariant T.Text])] -> State GdnativeState T.Text
-- genDataDecl t fields = do
--   convertedFields <- foldl' (<>) mempty . intersperse "\n  , " <$> mapM convertField fields
--   pure $
--     buildText
--       [ "data "
--       , t.h
--       , " = "
--       , t.h
--       , "\n  { "
--       , convertedFields
--       , "\n  }"
--       ]
--  where
--   convertField (fname, tv) = do
--     convertedArgs <- mapM convertVariant tv
--     pure
--       . buildText
--       $ fname.h :
--       " :: " :
--       intersperse
--         " -> "
--         ( case unsnoc convertedArgs of
--             Just (xs@(_ : _), x) -> xs `snoc` ("IO (" <> x <> ")")
--             _ -> convertedArgs
--         )

-- --------------------------------------------------------------------------------

-- typeSpecifiers :: CDeclaration a -> Maybe [CTypeSpecifier a]
-- typeSpecifiers (CDecl specs _ _) = Just $ mapMaybe (^? _Ctor @"CTypeSpec") specs
-- typeSpecifiers _ = Nothing

-- storageSpecifiers :: CDeclaration a -> Maybe [CStorageSpecifier a]
-- storageSpecifiers (CDecl specs _ _) = Just $ mapMaybe (^? _Ctor @"CStorageSpec") specs
-- storageSpecifiers _ = Nothing

-- declarators :: CDeclaration a -> Maybe [CDeclarator a]
-- declarators (CDecl _ declrs _) = Just $ mapMaybe (view _1) declrs

-- derivedDeclrs :: CDeclarator a -> [CDerivedDeclarator a]
-- derivedDeclrs (CDeclr _ d _ _ _) = d

-- typeDef :: [CTypeSpecifier a] -> Maybe (Ident, a)
-- typeDef = listToMaybe . mapMaybe (preview (_Ctor @"CTypeDef"))

-- isTypeDef :: CDeclaration NodeInfo -> Bool
-- isTypeDef = isJust . join . fmap typeDef . typeSpecifiers

-- typedef :: [CStorageSpecifier a] -> Maybe a
-- typedef = listToMaybe . mapMaybe (preview (_Ctor @"CTypedef"))

-- isTypedef :: CDeclaration a -> Bool
-- isTypedef = isJust . join . fmap typedef . storageSpecifiers

-- isVoid :: [CTypeSpecifier a] -> Bool
-- isVoid = not . null . mapMaybe (preview (_Ctor @"CVoidType"))

-- enumType :: [CTypeSpecifier a] -> Maybe (CEnumeration a, a)
-- enumType = listToMaybe . mapMaybe (preview (_Ctor @"CEnumType"))

-- isEnum :: CDeclaration a -> Bool
-- isEnum = isJust . join . fmap enumType . typeSpecifiers

-- isFunPtr :: CDeclaration a -> Bool
-- isFunPtr =
--   any (isJust . preview (_Ctor @"CFunDeclr"))
--     . concatMap derivedDeclrs
--     . fromMaybe []
--     . declarators

-- isStruct :: CDeclaration a -> Bool
-- isStruct = any (isJust . preview (_Ctor @"CSUType")) . fromMaybe [] . typeSpecifiers

-- isInGdnativeFile :: CNode n => n -> Bool
-- isInGdnativeFile =
--   maybe False (== "gd-haskell/godot-headers/godot/gdnative_interface.h")
--     . fileOfNode

-- isRelevantToplevelDeclaration :: CDeclaration NodeInfo -> Bool
-- isRelevantToplevelDeclaration decl =
--   isInGdnativeFile decl
--     && isTypedef decl
--     && (isStruct decl || isFunPtr decl || isEnum decl)

-- processEnum :: CDeclaration a -> State GdnativeState T.Text
-- processEnum (CDecl specifiers declrs _) = do
--   let getDeclrIdent (Just (CDeclr (Just ident) _ _ _ _), _, _) = Just ident
--       getDeclrIdent _ = Nothing
--       ident = identToText $ fromJust $ foldr (<|>) Nothing $ map getDeclrIdent declrs
--   registerItem (MkEnumDef ident) >> pure ident

-- isPtr :: [CDerivedDeclarator a] -> Bool
-- isPtr = any (isJust . preview (_Ctor @"CPtrDeclr"))

-- ctPtr :: [CDerivedDeclarator a] -> Int
-- ctPtr = length . filter (isJust . preview (_Ctor @"CPtrDeclr"))

-- -- | Override some C type names when converting to Haskell names.
-- cTypeReplacements :: HM.HashMap T.Text T.Text
-- cTypeReplacements =
--   HM.fromList
--     [ ("uint64_t", "CULong")
--     , ("uint32_t", "CUInt")
--     , ("uint16_t", "CUShort")
--     , ("uint8_t", "CUChar")
--     , ("int64_t", "CLong")
--     , ("int32_t", "CInt")
--     , ("int16_t", "CShort")
--     , ("wchar_t", "CInt")
--     , ("char", "CChar")
--     , ("size_t", "CInt")
--     , ("float", "CFloat")
--     , ("dobule", "CDouble")
--     , ("char16_t", "CShort")
--     , ("char32_t", "CInt")
--     , ("void", "()")
--     ]

-- typeSpecMap :: Show a => [(CTypeSpecifier a -> Maybe T.Text)]
-- typeSpecMap =
--   [ fmap (replace . identToText) . preview (_Ctor @"CTypeDef" . _1)
--   , fmap (const "int") . preview (_Ctor @"CIntType")
--   , fmap (const "char") . preview (_Ctor @"CCharType")
--   , fmap (const "void") . preview (_Ctor @"CVoidType")
--   , fmap (const "double") . preview (_Ctor @"CDoubleType")
--   , fmap (const "float") . preview (_Ctor @"CFloatType")
--   , \t -> Just $ "UNDEFINED >>>> " <> (T.pack $ show t)
--   ]
--  where
--   replace t = fromMaybe t $ HM.lookup t cTypeReplacements

-- typeSpecToText :: Show a => CTypeSpecifier a -> Maybe T.Text
-- typeSpecToText spec = listToMaybe $ mapMaybe (\test -> test spec) typeSpecMap

-- findSpecToText :: Show a => [CTypeSpecifier a] -> T.Text
-- findSpecToText = fromJust . listToMaybe . mapMaybe typeSpecToText

-- identToText :: Ident -> T.Text
-- identToText = T.pack . identToString

-- data TypeVariant a
--   = MkPtr (TypeVariant a)
--   | MkArg a
--   deriving (Show)

-- mkNPtrs :: Int -> a -> TypeVariant a
-- mkNPtrs x t
--   | x <= 0 = MkArg t
--   | otherwise = MkPtr (mkNPtrs (x - 1) t)

-- nPtrs :: TypeVariant a -> Int
-- nPtrs (MkPtr x) = 1 + nPtrs x
-- nPtrs (MkArg a) = 0

-- pullArg :: TypeVariant a -> a
-- pullArg (MkPtr x) = pullArg x
-- pullArg (MkArg a) = a

-- type BareFieldType = (T.Text, TypeVariant T.Text)
-- type FunPtrType = (T.Text, TypeVariant T.Text, [TypeVariant T.Text])

-- processFunPtr :: forall a. (HasCallStack, Show a) => CDeclaration a -> State GdnativeState FunPtrType
-- processFunPtr decl = do
--   let specs = fromJust $ typeSpecifiers decl
--       declrs = fromJust $ declarators decl
--       returns =
--         let primaryName = findSpecToText specs
--             ptrNum = ctPtr . derivedDeclrs . head $ declrs
--          in mkNPtrs (ptrNum - 1) primaryName
--       dDeclrs = concatMap derivedDeclrs declrs
--       funName =
--         -- Function's name.
--         identToText . fromJust
--           . join
--           . preview (_Ctor @"CDeclr" . _1)
--           $ head declrs
--       funDecls =
--         -- Parameters for the function.
--         concat $
--           mapMaybe (preview (_Ctor @"CFunDeclr" . _1 . _Right . _1)) dDeclrs
--   let funPtrDef = (funName, returns, map convertDeclToArg funDecls)
--   registerItem (MkFunPtrDef funPtrDef) >> pure funPtrDef
--  where
--   convertDeclToArg decl =
--     let specs = fromJust $ typeSpecifiers decl
--         declrs = fromJust $ declarators decl
--         pType = findSpecToText specs
--      in if fromMaybe False (isPtr . derivedDeclrs <$> listToMaybe declrs)
--           then MkPtr (MkArg pType)
--           else MkArg pType

-- processBareField :: Show a => CDeclaration a -> State GdnativeState BareFieldType
-- processBareField decl = do
--   let specs = fromJust $ typeSpecifiers decl
--       declrs = fromJust $ declarators decl
--       fieldName = identToText . fromJust . join . firstOf (folded . _Ctor @"CDeclr" . _1) $ declrs
--       returns =
--         let primaryName = findSpecToText specs
--             ptrNum = ctPtr . derivedDeclrs . head $ declrs
--          in mkNPtrs (ptrNum - 1) primaryName
--   let bareFieldDef = (fieldName, returns)
--   registerItem (MkTypeSyn bareFieldDef) >> pure bareFieldDef

-- processStruct ::
--   forall a.
--   (HasCallStack, Show a) =>
--   CDeclaration a ->
--   State GdnativeState (T.Text, [Either BareFieldType FunPtrType])
-- processStruct decl = do
--   let specs = fromJust $ typeSpecifiers decl
--       structName =
--         identToText
--           . fromJust
--           . join
--           . firstOf (folded . _Ctor @"CDeclr" . _1)
--           $ fromJust $ declarators decl
--       (fields :: [CDeclaration a]) =
--         fromJust
--           . join
--           $ firstOf
--             ( folded
--                 . _Ctor @"CSUType"
--                 . _1
--                 . _Ctor @"CStruct"
--                 . _3
--             )
--             specs
--   structDef <- (structName <> "Ptr",) <$> mapM processField fields
--   registerItem (MkStructDef structDef) >> pure structDef
--  where
--   processField decl
--     | isFunPtr decl = do
--         fPtr <- processFunPtr decl
--         registerItem (MkFunPtrDef fPtr)
--         pure $ Right fPtr
--     | otherwise = do
--         bField <- processBareField decl
--         -- alreadySeen <- if nPtrs (snd bField) > 0 
--         --   seenItem $ typeName $ snd bField
--         undefined
--     -- | otherwise = Left <$> processBareField decl

-- data GdnativeItem
--   = MkStructDef (T.Text, [Either BareFieldType FunPtrType])
--   | MkEnumDef T.Text
--   | MkFunPtrDef FunPtrType
--   | MkTypeSyn BareFieldType
--   deriving (Show)

-- processGdnativeItem :: (CNode (CDeclaration a), Show a) => CDeclaration a -> State GdnativeState (Maybe GdnativeItem)
-- processGdnativeItem decl =
--   if isTypedef decl && isInGdnativeFile decl
--     then
--       Just
--         <$> if
--             | isEnum decl -> MkEnumDef <$> processEnum decl
--             | isFunPtr decl -> MkFunPtrDef <$> processFunPtr decl
--             | isStruct decl -> MkStructDef <$> processStruct decl
--             | otherwise -> MkTypeSyn <$> processBareField decl
--     else pure Nothing

-- --------------------------------------------------------------------------------

-- type GdnativeState = (HM.HashMap TypeText GdnativeItem, S.Seq T.Text)

-- seenItem :: TypeText -> State GdnativeState Bool
-- seenItem t = isJust . HM.lookup t . fst <$> get

-- registerItem :: GdnativeItem -> State GdnativeState ()
-- registerItem i = modify (over _1 (HM.insert (typeName $ getItemName i) i))
--  where
--   getItemName = \case
--     MkStructDef (name, _) -> name
--     MkEnumDef name -> name
--     MkFunPtrDef (name, _, _) -> name
--     MkTypeSyn (name, _) -> name

-- genSynDecl :: (T.Text, TypeVariant T.Text) -> State GdnativeState T.Text
-- genSynDecl (name, tv) = do
--   case tv of
--     MkArg a -> pure $ "type " <> (typeName name).h <> " = {#type " <> (pullArg tv) <> " #}"
--     MkPtr v -> pure ""
--   -- pure "TYSYN >>>>>>"

-- itemToRelevantShim :: GdnativeItem -> State GdnativeState T.Text
-- itemToRelevantShim (MkStructDef (name, fields)) = do
--   let convertFieldEntry = \case
--         Left (bName, bType) -> (fieldName bName, [bType])
--         Right (fName, returns, args) ->
--           ( fieldName fName
--           , args ++ [returns]
--           )
--   dataD <- genDataDecl (typeName name) (map convertFieldEntry fields)
--   let pointerD = genPtrDecl (typeName name)
--   let storableD = genStorableImpl (typeName name) fields
--   pure $ buildText $ intersperse "\n" $ [dataD, pointerD, storableD]
-- itemToRelevantShim (MkFunPtrDef (name, returns, args)) = genForeignImportDecl (typeName name) <$> convertVariant returns <*> mapM convertVariant args
-- itemToRelevantShim (MkEnumDef name) = pure $ genEnumDecl (typeName name)
-- itemToRelevantShim (MkTypeSyn syn) = genSynDecl syn

-- --------------------------------------------------------------------------------

-- main :: IO ()
-- main = do
--   !parsedGdnativeInterface <-
--     parseCFile
--       (newGCC "gcc")
--       Nothing
--       ["-Igd-haskell/godot-headers/godot/"]
--       "gd-haskell/godot-headers/godot/gdnative_interface.h"
--   let decls = map (\(CDeclExt d) -> d) $ case parsedGdnativeInterface of
--         Left e -> undefined
--         Right (CTranslUnit t _) -> t

--   let (res, _) = flip runState (mempty, mempty) do
--         processedItems <- mapMaybe id <$> mapM processGdnativeItem decls
--         mapM itemToRelevantShim . map snd . HM.toList . fst =<< get
--   pPrint res

--   -- let processedItems = mapMaybe processGdnativeItem $ map (\(CDeclExt d) -> d) cTranslUnit

--   -- pPrint $ runState (mapM itemToRelevantShim processedItems) (mempty, mempty)
--   -- mapMaybe (\(CDeclExt d) -> processGdnativeItem d) cTranslUnit

--   pure ()


main :: IO ()
main = do
  !parsedGdnativeInterface <-
    parseCFile
      (newGCC "gcc")
      Nothing
      ["-Igd-haskell/godot-headers/godot/"]
      "gd-haskell/godot-headers/godot/gdnative_interface.h"
  let decls = map (\(CDeclExt d) -> d) $ case parsedGdnativeInterface of
        Left e -> undefined
        Right (CTranslUnit t _) -> t

  pPrint $ map genItem $ mapMaybe processItem decls

  undefined