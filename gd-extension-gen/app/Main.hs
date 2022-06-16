{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import Data.Either (fromLeft, fromRight)
import qualified Data.Text.IO as T
import Godot.Extension.Generate.Interface (parseAndGenerate)
import Godot.Extension.Generate.Schema (ExtensionApi (builtin_class_member_offsets))
import Godot.Extension.Generate.Utils (camelSubstitutionsRE, camelSubstitutions)
import qualified Data.Text as T
import Data.Foldable (foldl')

myReplace i = foldl' (\acc (needle, target) -> T.replace needle target acc) i camelSubstitutions

main :: IO ()
main = do
  -- T.putStrLn =<< (myReplace <$> T.readFile "/stg/Code/Haskell/gdhaskell/gd-haskell/godot-headers/godot/gdnative_interface.h")
  T.putStrLn =<< (camelSubstitutionsRE <$> T.readFile "/stg/Code/Haskell/gdhaskell/gd-haskell/godot-headers/godot/gdnative_interface.h")
  -- T.writeFile "gd-haskell/src/Godot/Extension/Extension.chs" =<< parseAndGenerate
  -- f <- BS.readFile "gd-haskell/godot-headers/extension_api.json"
  -- -- let res = builtin_class_member_offsets $ fromRight undefined $ A.eitherDecode @ExtensionApi f
  -- case A.eitherDecode @ExtensionApi f of
  --   Left e -> error $ show e
  --   Right res -> print $ builtin_class_member_offsets res