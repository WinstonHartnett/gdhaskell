{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Godot.Extension.Generate.Api where

import Control.Monad.Reader (MonadReader (ask), Reader, guard)
import Data.Coerce (coerce)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Ratio (denominator, numerator)
import Data.String (IsString (fromString))
import Data.Text qualified as T
import GHC.Hs.Lit (HsLit (HsString))
import GHC.Records (HasField (getField), getField)
import GHC.SourceGen hiding (from, guard, string)
import GHC.Types.Name (mkVarOccFS)
import GHC.Types.SourceText (SourceText (NoSourceText))
import Godot.Extension.Generate.Schema
import Godot.Extension.Generate.Utils (capitalizeFirst, foreignCToHs, fsText, toCamelCase, toCapitalCamelCase)
import Witch hiding (over)

import qualified Text.Megaparsec as P
import Godot.Parser.Resource (valP, GdValue(..))

instance TryFrom Rational Int where
  tryFrom r =
    let n = numerator r
        d = denominator r
     in if n `mod` d == 0 && wInBounds n && wInBounds d
          then Right $ fromIntegral $ n `div` d
          else Left $ TryFromException r Nothing
   where
    wInBounds l =
      l <= fromIntegral (maxBound @Int)
        && l >= fromIntegral (minBound @Int)

parseGdValue :: T.Text -> HsExpr'
parseGdValue inp = undefined
 where

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

instance Monoid (Name 'HName f c) where
  mempty = MkName $ mempty

instance Semigroup (Name 'HName f c) where
  a <> b = MkName $ toText a <> toText b

type HType = Name 'HName 'CamelCase 'Capitalized
type HVal = Name 'HName 'CamelCase 'Uncapitalized
type CType = Name 'CName 'CamelCase 'Unknown
type CVal = Name 'CName 'CamelCase 'Uncapitalized
type GType = Name 'GName 'CamelCase 'Unknown
type GVal = Name 'GName 'SnakeCase 'Uncapitalized

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

data Api = MkApi
  { extensionApi :: ExtensionApi
  , buildConfig :: BuildConfig
  }

type ApiGen = Reader Api

api :: ApiGen ExtensionApi
api = (.extensionApi) <$> ask

bld :: ApiGen BuildConfig
bld = (.buildConfig) <$> ask

-- | Wrapper over ghc-source-gen's 'string' function (which takes a 'String').
string :: HasLit e => T.Text -> e
string = lit . HsString NoSourceText . fsText
{-# INLINE string #-}

-- | Make a new 'OccNameStr' from text.
textToOcc :: T.Text -> OccNameStr
textToOcc = occNameToStr . mkVarOccFS . fsText
{-# INLINE textToOcc #-}

instance From T.Text OccNameStr where
  from = textToOcc

instance From T.Text RdrNameStr where
  from = unqual . textToOcc

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

--------------------------------------------------------------------------------

{- | Generate an opaque 'newtype' shim.

 @
   newtype Vector2 = MkVector2 { unVector2 :: Ptr Vector2 }
     deriving Show
 @
-}
genNewtype :: BuiltinClass -> ApiGen HsDecl'
genNewtype = undefined

{- | Generate overloaded constructor instances for builtins.

  @
    instance MonadIO m => Construct (m Vector2) where
      construct = liftIO do
        r <- memAlloc 8
        ptrCstr <- ptrConstructor (from GdnativeVariantTypeVector2) 0
        ptrCstr (coerce r) nullPtr
        pure $ MkVector2 (coerce r)
     {\-# INLINABLE construct #-\}
  @
-}
genCstr :: BuiltinClass -> ApiGen HsDecl'
genCstr cls = undefined

-- | Generate field getters and setters for a builtin class.
genBuiltinMemberShims :: BuiltinClass -> ApiGen [HsDecl']
genBuiltinMemberShims b =
  case b.members of
    Just members' ->
      concat
        <$> mapM (\m -> (++) <$> genGetter m <*> genSetter m) members'
    Nothing -> pure []
 where
  getOffset bm = do
    cfg <- buildConfigToText <$> bld
    ( \t ->
        getPaired cfg (builtin_class_member_offsets t)
          >>= getPaired b.name
          >>= getPaired bm.name
      )
      <$> api
  genShim f bm = do
    cfg <- bld
    offset' <- getOffset bm
    case offset' of
      Just offset ->
        f
          ((mkGType b.name).toHType cfg)
          (mkGVal $ bm.name)
          (mkGType $ bm.type')
          offset
      Nothing -> pure []
  genSetter = genShim genBuiltinSetterShim
  genGetter = genShim genBuiltinGetterShim

-- @ withUtf8 "str" \ptrName -> inner... @
withUtf8Expr :: GVal -> HVal -> HsExpr' -> HsExpr'
withUtf8Expr str ptrName inner =
  var "withUtf8" @@ string (toText str) @@ lambda [bvar (from ptrName)] inner

unwrap :: HType -> RdrNameStr -> HsExpr'
unwrap ty val = par (var (from $ "un" <> ty) @@ var val)

coerceUnwrap :: HType -> RdrNameStr -> HsExpr'
coerceUnwrap ty val = var "coerce" @@ unwrap ty val

{- |
Basic Godot IO marshaller.

>>> bindPtr "_get" "normal" a
_getNormalBind = unsafePerformIO $
  withUtf8 "normal" \str -> a
-}
bindPtr :: HasValBind t => T.Text -> GVal -> HsExpr' -> [t]
bindPtr pfx memberNm inner =
  let bindName = from $ pfx <> toText memberNm.toHVal.toHType <> "Bind"
   in [ funBind bindName . match [] $
          op
            (var "unsafePerformIO")
            "$"
            (withUtf8Expr memberNm "str" inner)
      , noInline' bindName
      ]

{- |
Shim to make a Godot variant setter or getter.

>>> gdVariant "Setter" "AABB" str
mkGdnativePtrSetter <$> variantGetPtr (from $ GdnativeVariantTypeAabb) str
-}
gdVariantGetPtr :: BuildConfig -> T.Text -> GType -> HsExpr'
gdVariantGetPtr cfg func ty =
  op
    (var (from $ "mkGdnativePtr" <> func))
    "<$>"
    ( var (from $ "variantGetPtr" <> func)
        @@ par (var "from" @@ var (from $ "GdnativeVaraintType" <> ty.toHType cfg))
        @@ var "str"
    )

{- |
Generate a member accessor function.

There are two cases:

  1. If the member type is a C marshallable:

    @
    getX :: MonadIO m => Vector2 -> m Double
    getX v = from <$> peekByteOff \@CDouble (coerce $ unVector2 v) 0
    {\-# INLINABLE getX \-#}
    @

  2. Or if the member is a complex Godot type:

    @
    getNormal :: MonadIO m => Plane -> m Vector3
    getNormal v = liftIO do
      r <- construct
      _getNormalBind (coerce $ unPlane v) (coerce $ unVector3 r)
      pure r
     where
      {\-# NOINLINE _getNormalBind \-#}
      _getNormalBind = unsafePerformIO $
        withUtf8 "normal" \str ->
          mkGdnativePtrGetter <$> variantGetPtrGetter (from GdnativeVariantTypePlane) str
    {\-# INLINABLE getNormal \-#}
    @
-}
genBuiltinGetterShim :: HType -> GVal -> GType -> Int -> ApiGen [HsDecl']
genBuiltinGetterShim instanceTy memberNm memberTy offset = do
  cfg <- bld
  let outTy = case toNativeForeignC cfg memberTy of
        Just ty -> ty
        Nothing -> from $ memberTy.toHType cfg
  let getFuncSig =
        typeSig getFuncName $
          [var "MonadIO" @@ var "m"]
            ==> var (from instanceTy)
            --> var "m" @@ var (from outTy)
      -- Complex marshalling.
      getComplex =
        funBind getFuncName
          . matchGRHSs [bvar "v"]
          $ rhs
            ( var "liftIO"
                @@ do'
                  [ bvar "r" <-- var "construct"
                  , stmt $
                      var (from $ "get" <> memberNm.toHVal.toHType <> "Bind")
                        @@ coerceUnwrap instanceTy "v"
                        @@ coerceUnwrap (memberTy.toHType cfg) "r"
                  , stmt $ var "pure" @@ var "r"
                  ]
            )
            `where'` bindPtr "get" memberNm (gdVariantGetPtr cfg "Getter" memberTy)
      -- Native C marshalling.
      getNative =
        funBind getFuncName
          . matchGRHSs [bvar "v"]
          $ rhs
            ( op
                (var "from" `tyApp` var (from $ memberTy.toHType cfg))
                "<$>"
                ( var "peekByteOff"
                    @@ par (var (from $ "un" <> instanceTy) @@ var "v")
                    @@ int (from offset)
                )
            )
  let getBody = case toNativeForeignC cfg memberTy of
        Just _ -> getNative
        Nothing -> getComplex
  pure $ [inlinable getFuncName, getFuncSig, getBody]
 where
  getFuncName = from $ "get" <> memberNm.toHVal.toHType

{- |
Generate a member setter function.

There are two cases:

  1. If the member type is a C marshallable:

    @
    setX :: MonadIO m => Double -> Vector2 -> m ()
    setX a v = pokeByteOff (unVector2 v) 0 a
    {\-# INLINABLE setX \-#}
    @

  2. Or if the member is a complex Godot type:

    @
    setNormal :: MonadIO m => Vector3 -> Plane -> m ()
    setNormal a v = liftIO $ _setNormalBind (coerce $ unPlane v) (coerce $ unVector3 a)
     where
      {\-# NOINLINE _setNormalBind \-#}
      _setNormalBind = unsafePerformIO $
        withUtf8 "normal" \str ->
          mkGdnativePtrSetter <$> variantGetPtrSetter (from GdnativeVariantTypePlane) str
    {\-# INLINABLE setNormal \-#}
    @
-}
genBuiltinSetterShim :: HType -> GVal -> GType -> Int -> ApiGen [HsDecl']
genBuiltinSetterShim instanceTy memberNm memberTy offset = do
  cfg <- bld
  let inTy = case toNativeForeignC cfg memberTy of
        Just ty -> ty
        Nothing -> from $ memberTy.toHType cfg
  let setFuncSig =
        typeSig setFuncName $
          [var "MonadIO" @@ var "m"]
            ==> var (from inTy)
            --> var (from instanceTy)
            --> var "m" @@ var "()"
      setComplex =
        funBind setFuncName
          . matchGRHSs [bvar "a", bvar "v"]
          $ rhs
            ( op
                (var "liftIO")
                "$"
                ( var (from $ "set" <> memberNm.toHVal.toHType <> "Bind")
                    @@ coerceUnwrap instanceTy "v"
                    @@ coerceUnwrap (memberTy.toHType cfg) "a"
                )
            )
            `where'` bindPtr "set" memberNm (gdVariantGetPtr cfg "Setter" memberTy)
      setNative =
        funBind setFuncName
          . matchGRHSs [bvar "a", bvar "v"]
          $ rhs
            ( var "pokeByteOff"
                @@ unwrap instanceTy "v"
                @@ int (fromIntegral offset)
                @@ par
                  ( var "from"
                      `tyApp` (var "_")
                      `tyApp` var (from $ memberTy.toHType cfg) @@ var "a"
                  )
            )
  let setBody = case toNativeForeignC cfg memberTy of
        Just _ -> setNative
        Nothing -> setComplex
  pure $ [inlinable setFuncName, setFuncSig, setBody]
 where
  setFuncName = from $ "set" <> memberNm.toHVal.toHType

-- -- | Convert a parsed 'GdValue' to an equivalent Haskell expression.
-- -- If an expression is a 'GdNum' then 'isFloat' determines how it's
-- -- interpreted.
-- gdValToExpr :: Bool -> GdValue -> HsExpr'
-- gdValToExpr isFloat = \case
--   -- GdInt i -> int $ fromIntegral i
--   GdBool b -> if b then var "True" else var "False"
--   -- GdFloat f -> frac $ realToFrac f
--   GdNum r ->
--     if isFloat
--       then frac r
--       else int (fromRight undefined $ tryFrom r)
--   GdString s -> string s
--   GdCstr (nm, args) -> var "construct"
--   GdArr a -> undefined
--   GdDict d -> undefined
--   GdNull -> undefined

-- gdValToExpr :: (Rational -> HsExpr') -> GdValue -> HsExpr'
-- gdValToExpr numConv = \case
--   GdBool v -> if v then var "True" else var "False"
--   GdString v -> string v
--   GdCstr v -> op (var "unsafePerformIO") "$" (
--     withUtf8 "Text" (CString -> m a)
--     )
--   GdNum v -> numConv v
--   GdDict _ -> undefined
--   GdArr _ -> undefined
--   GdNull -> undefined

genBuiltinConstants :: BuiltinClass -> ApiGen [HsDecl']
genBuiltinConstants b = do
  cfg <- bld
  
  undefined
 where
  -- | Convert a constant to an 'HsDecl'.
  genConstantVal c =
    case fromJust $ P.parseMaybe valP c.value of
      GdBool v -> if v then var "True" else var "False"
      GdNum v -> undefined
      GdString v -> string v
      GdCstr v -> undefined
      GdArr _ -> undefined
      GdDict _ -> undefined
      GdNull -> undefined

-- | Generate methods.
genBuiltinMethods :: BuiltinClass -> ApiGen [HsDecl']
genBuiltinMethods b =
  case b.constants of
    Just c -> undefined
    Nothing -> pure []