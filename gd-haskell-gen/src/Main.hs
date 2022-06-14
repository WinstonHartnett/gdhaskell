{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImplicitParams #-}

module Main (main) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import Effectful
import Effectful.State.Static.Local
import ExtensionApi
import GHC.Records
import Language.Haskell.TH
import qualified Data.Text as T
import GHC.OverloadedLabels (IsLabel)
import Foreign


data Object

data Instance a = MkInstance
  { base   :: Object
  , script :: a
  }



















-- data Player = MkPlayer
--   { speed      :: Float
--   , screenSize :: V2 Float
--   }
--   deriving (Generic)
-- 
-- instance Default Player where
--   def = MkPlayer 0.0 (V2 0.0 0.0)
-- 
-- ready :: Godot Player ()
-- ready = do
--   base' <- base
--   change (set #screenSize . view #y) =<< getViewportRect base
--   hide base
-- 
-- start :: Godot Player ()
-- start = do
--   base' <- base
--   setPosition pos base'
--   show base'
--   getNode @"CollisionShape2D" base' >>= (`setDisabled` False)
--
-- process :: Float -> Godot Player ()
-- process delta = do
--   base' <- base
--   animSprite <- getNode @"AnimatedSprite3D" base
--   screenSize <- getViewportRect base
--   [left, right, up, down] <- do
--     Just inp <- getSingleton @Input
--     mapM (isActionPressed inp) ["ui_left", "ui_right", "ui_up", "ui_down"]
--   let velocity =
--         let bVal b v = if b then v else 0
--         in V2 ...
--   if velocity /= zero
--     then do
--       pos <- getPosition base
--       let velocity' = normalize velocity 
--  
-- onPlayerBodyEntered :: Node -> Godot Player ()
-- onPlayerBodyEntered body = do
--   base' <- base
--   hide base
--   emitSignal @"hit" [] base
--   getNode @"CollisionShape2D" base
--     >>= callDefferred "set_disabled" [from True]
--
-- {-# ANN type Player (exports ['ready, 'start, 'process, 'onPlayerBodyEnetered]) #-}
--
-- godot ''Player

generateEnum :: Eff '[] ()
generateEnum = undefined

--------------------------------------------------------------------------------

main :: IO ()
main = do
  pure ()
  -- f <- BS.readFile "godot-headers/extension_api.json"
  -- let res = A.eitherDecode @ExtensionApi f
  -- print $ res
