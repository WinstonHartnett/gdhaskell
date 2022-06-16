{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Godot.Extension.Generate.Api where

import Control.Monad.Reader (MonadReader (ask), Reader)
import qualified Data.Text as T
import GHC.Hs.Lit (HsLit (HsString))
import GHC.SourceGen hiding (string)
import GHC.Types.SourceText (SourceText (NoSourceText))
import Godot.Extension.Generate.Schema
import Godot.Extension.Generate.Utils (fsText, godotToForeignC)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)

-- | Precision targets of Godot's internal `real_t`.
--
-- _Note:_ 32-bit builds are not supported by GHC.
data BuildConfig
  = Float64
  | Double64

buildConfigToText :: BuildConfig -> T.Text
buildConfigToText Float64 = "float_64"
buildConfigToText Double64 = "double_64"

data Api = MkApi
  { extensionApi :: ExtensionApi
  , buildConfig  :: BuildConfig
  }

type ApiGen = Reader Api

api :: ApiGen ExtensionApi
api = (.extensionApi) <$> ask

bld :: ApiGen BuildConfig
bld = (.buildConfig) <$> ask

-- | Wrapper over ghc-source-gen's 'string' function (which takes a 'String').
string :: HasLit e => T.Text -> e
string = lit . HsString NoSourceText . fsText

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
        vPtr <- memAlloc 8
        ptrCstr <- ptrConstructor (from GdnativeVariantTypeVector2) 0
        ptrCstr (coerce vPtr) nullPtr
        pure $ MkVector2 (coerce vPtr)
     {\-# INLINABLE construct #-\}
  @
-}
genCstr :: BuiltinClass -> ApiGen HsDecl'
genCstr cls = undefined

-- | Generate field getters and setters for a builtin class. See 'genMemberShim'.
genMemberShims :: BuiltinClass -> ApiGen (Maybe [HsDecl'])
genMemberShims b =
  case b.members of
    Just members' -> do
      -- typeSig OccNameStr HsType'
      undefined
    Nothing -> pure Nothing

{- | Generate a field accessor and setter for a builtin.

  @
    getX :: Vector2 -> IO Double
    getX v = from <$> peekByteOff @CDouble (coerce $ unVector v) 0

    setX :: Float -> Vector2 -> IO ()
    setX f v = pokeByteOff (unVector2 v) 0 f
  @
-}
-- TODO Make this poke core structs directly?
genMemberShim :: BuiltinClass -> BuiltinClassMember -> ApiGen [HsDecl']
genMemberShim b m = do
  ext <- api
  cfg <- buildConfigToText <$> bld
  let size' =
        fromJust $
          getObject cfg ext.builtin_class_sizes
            >>= getObject (b.name)
      offset' =
        fromJust $
          getObject cfg ext.builtin_class_member_offsets
            >>= getObject (b.name)
            >>= getObject (m.name)
  let ty' = m.type'
      hTy' = case HM.lookup ty' godotToForeignC of
        Just t -> undefined
        Nothing -> undefined
  undefined

{- | Generate methods.

 @
   angle :: Vector2 -> IO Float
   angle =
-}
genMethods :: BuiltinClass -> ApiGen HsDecl'
genMethods = undefined