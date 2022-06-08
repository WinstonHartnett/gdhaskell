{-# LANGUAGE ScopedTypeVariables #-}

module Godot.MyLib (someFunc) where

import Godot.Extension.Extension

someFunc :: IO ()
someFunc = do
  -- let a = GdNativeCallError GdnativeCallOk 0 0
  -- let a = GdNativeTypePtr undefined
  putStrLn "someFunc"
