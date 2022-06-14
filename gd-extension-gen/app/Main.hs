module Main where

import qualified Data.Text.IO as T
import Godot.Extension.Generate.Interface ( parseAndGenerate )

main :: IO ()
main = T.writeFile "gd-haskell/src/Godot/Extension/Extension.chs" =<< parseAndGenerate