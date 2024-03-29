{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Godot.Extension.Generate.Api where

import Control.Applicative (asum)
import Control.Lens (each, over)
import Control.Monad (guard)
import Control.Monad.Reader (MonadReader (ask), Reader)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Either (fromRight)
import Data.Foldable (foldl')
import Data.HashMap.Strict qualified as HM
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.Display
import Data.Text.Read qualified as T
import Data.Vector qualified as V
import GHC.SourceGen hiding (from, guard, string, stringTy)
import Godot.Extension.Generate.Schema
import Godot.Extension.Generate.Utils (
  BuildConfig,
  GType,
  GVal,
  HType,
  HVal,
  buildConfigToText,
  capitalizeFirst,
  fromEither,
  mkGEnum,
  mkGType,
  mkGVal,
  string,
  stringTy,
  toNativeForeignC,
  toText,
 )
import Godot.Parser.Resource (GdValue (..), valP)
import Text.Megaparsec qualified as P
import Witch hiding (over)

data Api = MkApi
  { extensionApi :: ExtensionApi
  , buildConfig :: BuildConfig
  }

type ApiGen = Reader Api

api :: ApiGen ExtensionApi
api = (.extensionApi) <$> ask

bld :: ApiGen BuildConfig
bld = (.buildConfig) <$> ask

--------------------------------------------------------------------------------

{- |
Generate an opaque 'newtype' shim.

@
  newtype Vector2 = MkVector2 { unVector2 :: ForeignPtr Vector2 }
    deriving Show
@
-}
genNewtype :: BuiltinClass -> ApiGen HsDecl'
genNewtype = undefined

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
        Just ty -> Left $ ty
        Nothing -> Right $ from $ memberTy.toHType cfg
  let getFuncSig =
        typeSig getFuncName $
          [var "MonadIO" @@ var "m"]
            ==> var (from instanceTy)
            --> var "m" @@ var (from $ fromEither outTy)
  -- Complex marshalling.
  let getComplex =
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
  let getNative =
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
  let getBody = either (const getNative) (const getComplex) outTy
  pure $ [inlinable getFuncName, getFuncSig, getBody]
 where
  getFuncName = from $ "get" <> memberNm.toHVal.toHType

{- |
Generate a member setter function.

There are two cases:

  1. If the member type is a C marshallable:

    @
    setX :: MonadIO m => Double -> Vector2 -> m ()
    setX a v = pokeByteOff (unVector2 v) 0 a -- TODO Make this change to from @_ @CDouble a
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
        Just ty -> Left $ ty
        Nothing -> Right $ from $ memberTy.toHType cfg
  let setFuncSig =
        typeSig setFuncName $
          [var "MonadIO" @@ var "m"]
            ==> var (from $ fromEither inTy)
            --> var (from instanceTy)
            --> var "m" @@ var "()"
  let setComplex =
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
  let setNative =
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
  let setBody = either (const setNative) (const setComplex) inTy
  pure $ [inlinable setFuncName, setFuncSig, setBody]
 where
  setFuncName = from $ "set" <> memberNm.toHVal.toHType

{- |
Generate an overloaded builtin constant.

@
-- INLINE _AXIS_X
_AXIS_X :: Constant Vector2 "_AXIS_X" f => f
_AXIS_X = constant \@Vector2 \@"_AXIS_X"
@

There are two cases:

  1. If the constant type is a C marshallable:

    @
    instance Constant Vector2 "_AXIS_X" Int where
      -- INLINE constant
      constant = from \@CInt 0
    @

  2. Or if the constant type is a complex Godot type:

    @
    instance MonadIO m => Constant Vector2 "_ZERO" (m Vector2) where
      -- INLINE constant
      constant = construct 0.0 0.0

    instance Constant Vector2 "_ZERO" V2 where
      -- INLINE constant
      constant = constant'
       where
        -- NOINLINE constantBind
        constant' = unsafePerformIO $ from \@_ \@(IO V2) =<< construct 0.0 0.0
    @
-}
genBuiltinConstant :: BuiltinClass -> Constant -> ApiGen [HsDecl']
genBuiltinConstant b c = do
  cfg <- bld
  let outTy = case toNativeForeignC cfg constTy of
        Just ty -> Left $ ty
        Nothing -> Right $ constTy.toHType cfg
      builtinTy = mkGType b.name
      builtinHTy = builtinTy.toHType cfg
      instSig = case outTy of
        Left t ->
          var "Constant"
            @@ var (from builtinHTy)
            @@ stringTy ("_" <> c.name)
            @@ var (from t.fromForeignC)
        Right _ ->
          [ var "MonadIO"
              @@ var "m"
          , var "From" @@ var "Vector2" @@ par (var "m" @@ var "t")
          ]
            ==> var "Constant"
            @@ var (from builtinHTy)
            @@ stringTy ("_" <> c.name)
            @@ par (var "m" @@ var "t")
  instBody <- case outTy of
    Left t -> pure $ bodyPfx $ var "from" `tyApp` var (from t) @@ nativeRet
    Right t -> do
      (_, _, args) <-
        fromJust <$> findConstructor (fromJust $ P.parseMaybe valP c.value)
      let e = cstrExpr args
      pure . bodyPfx $ op (var "from" `tyApp` var (from t)) "=<<" e
  let constDecl =
        funBind (from $ "_" <> c.name)
          . matchGRHSs []
          . rhs
          $ var "constant"
            `tyApp` var (from builtinHTy)
            `tyApp` stringTy ("_" <> c.name)
  pure [instance' instSig [instBody], constDecl]
 where
  constTy = mkGType c.type'
  bodyPfx = funBind "constant" . matchGRHSs [] . rhs
  nativeRet = case c.type' of
    "int" -> int $ fst $ fromRight undefined $ T.decimal c.value
    "float" -> frac $ realToFrac $ fst $ fromRight undefined $ T.double c.value
    "bool" -> var $ from $ capitalizeFirst c.value
    _ -> undefined

{- |
Convert parsed arguments and constructor argument definitions to an expression.

>>> cstrExpr [(GdNum 0.0, ...), (GdNum 0.0, ...)]
construct 0.0 0.0
-}
cstrExpr :: V.Vector (GdValue, Argument) -> HsExpr'
cstrExpr args = V.foldl' (\acc t -> acc @@ valConv t) (var "construct") args
 where
  valConv (v, a) =
    case v of
      GdNum n ->
        if a.type' == "int"
          then int $ fromRight undefined $ tryFrom n
          else frac $ realToFrac n
      GdBool n -> if n then var "True" else var "False"
      GdString n -> string n
      GdArr _ -> undefined
      GdDict _ -> undefined
      GdCstr _ -> undefined
      GdNull -> undefined

{- |
Given a parsed GDExtension constructor, find the corresponding Constructor
in the Extension API.
-}
findConstructor ::
  GdValue ->
  ApiGen (Maybe (BuiltinClass, Constructor, V.Vector (GdValue, Argument)))
findConstructor (GdCstr (nm, args)) = do
  api' <- api
  pure do
    bTarget <- V.find (\b -> b.name == nm) api'.builtin_classes
    asum
      . V.map (\(_, c) -> (bTarget,c,) <$> matchCstr (V.fromList args) c)
      . V.fromList
      . HM.toList
      . unKeyedMap
      =<< bTarget.constructors
 where
  isSameCstr p arg = case p of
    GdNum _ ->
      arg.type' == "float"
        || arg.type' == "int"
        || "enum::" `T.isPrefixOf` arg.type'
    GdBool _ -> arg.type' == "bool"
    GdString _ -> arg.type' == "String"
    GdArr _ -> arg.type' == "Array"
    GdDict _ -> arg.type' == "Dictionary"
    GdCstr _ -> True
    GdNull -> True
  matchCstr pVals cstr =
    if length pVals == length (fromJust cstr.arguments)
      then
        traverse
          (\(p, arg) -> guard (isSameCstr p arg) *> Just (p, arg))
          $ V.zip pVals (fromJust cstr.arguments)
      else undefined
findConstructor _ = pure Nothing

-- | Generate an enum.
genEnum :: EnumEntry -> ApiGen [HsDecl']
genEnum e = do
  cfg <- bld
  pure
    [ data'
        (from $ eName.toHType cfg)
        []
        ( map (\v -> prefixCon (from $ (mkGEnum v.name).toHVal) []) $
            V.toList e.values
        )
        [deriving' [var "Show", var "Enum"]]
    ]
 where
  eName = mkGType e.name

{- |
>>> marshalStub "a1" (MkHType "AesContextMode")  ...
marshal @_ @AesContextMode \b1 -> ...
-}
marshalStub :: T.Text -> HType -> HsExpr' -> HsExpr'
marshalStub vName targetTy inner =
  var "marshall" `tyApp` var "_" `tyApp` var (from $ targetTy)
    @@ lambda [bvar $ from $ T.replace "a" "b" vName] inner

{- |
Generate builtin methods.

@
-- In Vector2.hs
-- INLINE angleTo
angleTo ::
  forall a1 a2 m.
  (  MonadIO m,
  ,  Marshal a1 Vector2
  ,  Marshal a2 Vector2 )
  => a1
  -> a2
  -> m Double
angleTo a1 a2 = callPtrBuiltin @m @a1 @a2 angleToBind a1 a2
 where
  -- NOINLINE angleToBind
  angleToBind = unsafePerformIO $
    withUtf8 "angle_to" \p1 ->
      mkGdnativePtrBuiltinMethod
        <$> variantGetPtrBuiltinMethod (from GdnativeVariantTypeVector2) p1 0000
@
-}
genBuiltinMethods :: BuiltinClass -> ApiGen [HsDecl']
genBuiltinMethods b =
  case b.constants of
    Just c -> undefined
    Nothing -> pure []

{- |
Generate object methods.

@
-- INLINE start
start ::
  forall a1 a2 a3 a4 m.
  (  MonadIO m,
     Marshal a1 GdnativeObjectPtr,
     Marshal a2 CInt,
     Marshal a3 PackedByteArray,
     Marshal a4 PackedByteArray )
  => a1
  -> a2
  -> a3
  -> a4
  -> m Error
start a1 a2 a3 a4 =
  callPtr
    @m @a1 @a2 @CInt
    @a3 @PackedByteArray
    @a4 @PackedByteArray
    @Error @Error
    startBind a1 a2 a3 a4
 where
  -- NOINLINE startBind
  startBind = unsafePerformIO $
    withUtf8 "AESContext" \p1 ->
      withUtf8 "start" \p2 ->
        mkGdnativeMethodBindPtr <$> classDbGetMethodBind p1 p2 0000
-- TODO Proper varargs
@
-}
genObjectMethods :: Class -> ApiGen [HsDecl']
genObjectMethods c = do
  cfg <- bld
  undefined

genObjectMethod :: Class -> Method -> ApiGen [HsDecl']
genObjectMethod c m = do
  cfg <- bld
  if m.is_vararg
    then pure [] -- TODO
    else do
      undefined
 where

{- | Generate marshalling stubs for different numbers of arguments.
@
-- In Calls.hs
-- INLINABLE callPtrBuiltin_1
callPtrBuiltin_1 ::
  forall m a1 b1 a2 b2 r s.
  (  MonadIO m
  ,  PrimMonad m
  ,  MarshalTo a1 b1
  ,  MarshalTo a2 b2
  ,  MarshalFrom m r s
  )
  => GdnativePtrBuiltinMethod
  -> a1
  -> a2
  -> m s
callPtrBuiltin_1 bPtr a1 a2 =
  marshal @_ @_ @b1 a1 \b1 ->
    marshal @_ @_ @b2 a2 \b2 ->
      ba <- newPinnedByteArray 16
      writeByteArray ba 0 b1
      marshalFrom @_ @r @s \p1 ->
        bPtr
          (coerce b2)
          (coerce $ mutableByteArrayContents ba)
          (coerce p1)
          1

-- INLINABLE callPtr_3
callPtr_3 ::
  forall m a1 a2 b2 a3 b3 a4 b4 r s.
  (  MonadIO m
  ,  PrimMonad m
  ,  MarshalTo m a1 GdnativeObjectPtr
  ,  MarshalTo m a2 b2
  ,  MarshalTo m a3 b3
  ,  MarshalTo m a4 b4
  ,  MarshalFrom m r s )
  => GdnativeMethodBindPtr
  -> a1
  -> a2
  -> a3
  -> m s
callPtr_3 bPtr a1 a2 a3 =
  marshal @_ @_ @b1 a1 \b1 ->
    marshal @_ @_ @b2 a2 \b2 ->
      marshal @_ @_ @b3 a3 \b3 ->
        ba <- newPinnedByteArray 16
        writeByteArray ba 0 b1
        writeByteArray ba 8 b2
        marshalFrom @_ @r @s \p1 -> liftIO $
          objectMethodBindPtrcall
            (coerce b3)
            (coerce $ mutableByteArrayContents ba)
            (coerce p1)
@
-}
genBuiltinPtrcall ::
  -- | Number of arguments.,
  Int ->
  ApiGen [HsDecl']
genBuiltinPtrcall nArgs = do
  pure [typeSig']
 where
  funName = from $ "callPtrBuiltin_" <> display nArgs

  types =
    map
      (\n -> ("a" <> display n, "b" <> display n))
      [0 :: Int .. nArgs]
  typeArgs =
    forall' $
      [bvar "m"]
        ++ concatMap (\(a, b) -> [bvar $ from $ a, bvar $ from $ b]) types
        ++ [bvar "r", bvar "s"]
  constraints =
    let mTo (a, b) = var "MarshalTo" @@ bvar (from a) @@ bvar (from b)
        mFrom = var "MarshalFrom" @@ var "m" @@ var "r" @@ var "s"
     in [var "MonadIO" @@ var "m", var "PrimMonad" @@ var "m"]
          ++ map mTo types
          ++ [mFrom]
  arrows = (map (bvar . from . fst) types) ++ [var "m" @@ var "s"]
  typeSig' =
    typeSig funName
      . typeArgs
      $ constraints ==> foldl' (@@) (var "GdnativePtrBuiltinMethod") arrows

  marshalTo (t1, t2) =
    var "marshalTo"
      `tyApp` var "_"
      `tyApp` var "_"
      `tyApp` var (from t1)
      `tyApp` var (from t2)

  marshalTos 0 i = i
  marshalTos n i =
    marshalTo ("b" <> display n, "a" <> display n)
      @@ marshalTos (n - 1) i

  marshalFrom t inner =
    var "marshalFrom"
      `tyApp` var "_"
      `tyApp` var "r"
      `tyApp` var "s"
      @@ inner

-- funBinds' = funBind funName $ match ([Pat']) HsExpr'

-- typeArgs = forall' $ bvar "m" : (concatMap (\n -> ["a" <> display n, "b" <> display n]) [0..])

{- |
Generate overloaded constructors for a builtin.

@
-- NOINLINE constructBind
constructBind1 = unsafePerformIO $
  mkGdnativePtrConstructor <$> variantGetPtrConstructor (from GdnativeVariantTypeVector2) 0

instance MonadIO m => Construct (m Vector2) where
  -- INLINABLE construct
  construct = liftIO do
    r <- memAlloc 16
    constructBind1 r nullPtr
    pure $ MkVector2 r

instance MonadIO m => Construct (m (InternalAlloc Vector2)) where
  -- INLINABLE construct
  construct = liftIO do
    r <- newPinnedByteArray 16
    constructBind1 (coerce $ mutableByteArrayContents r) nullPtr
    pure $ MkInternalAlloc r
@
-}
genBuiltinCstrs :: BuiltinClass -> ApiGen [HsDecl']
genBuiltinCstrs b = do
  undefined