{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Control.Monad.Reader (runReader, msum)
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as BS
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC (runGhc)
import "ghc" GHC.Driver.Session (HasDynFlags (getDynFlags))
import GHC.Paths (libdir)
import GHC.SourceGen (module')
import GHC.SourceGen.Pretty (putPpr)
import Godot.Extension.Generate.Api
import Godot.Extension.Generate.Schema (BuiltinClass (..), ExtensionApi (builtin_classes))

main :: IO ()
main = do
  -- T.writeFile "gd-haskell/src/Godot/Extension/Extension.chs" =<< parseAndGenerate
  f <- BS.readFile "gd-haskell/godot-headers/extension_api.json"
  -- -- let res = builtin_class_member_offsets $ fromRight undefined $ A.eitherDecode @ExtensionApi f
  case A.eitherDecode @ExtensionApi f of
    Left e -> error $ show e
    Right res -> do
      let a = res.builtin_classes
      -- let (Just a) = V.find (\(b :: BuiltinClass) -> b.name == ("Vector2" :: T.Text)) res.builtin_classes
      -- let m = V.last $ fromJust $ a.members
      print libdir
      let mod = module' (Just "Test") (Nothing) [] (runReader (msum <$> V.mapM genBuiltinMemberShims a) (MkApi res Float64))
      runGhc (Just libdir) do
        df <- getDynFlags
        putPpr mod

-- putPpr (inlinable' "hello_there")
-- let t = showPpr df mod
-- liftIO $ T.putStrLn $ T.pack t