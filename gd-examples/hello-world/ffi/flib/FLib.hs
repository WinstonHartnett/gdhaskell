{-# LANGUAGE ForeignFunctionInterface #-}

module FLib where

import Godot.Extension.Extension

godot_library_init :: GdnativeInterfacePtr -> GdnativeExtensionClassLibraryPtr -> GdnativeInitializationPtr -> IO ()
godot_library_init p_interface p_library r_initialization = do
  putStrLn "Initialized."
  undefined

foreign export ccall godot_library_init :: GdnativeInterfacePtr -> GdnativeExtensionClassLibraryPtr -> GdnativeInitializationPtr -> IO ()