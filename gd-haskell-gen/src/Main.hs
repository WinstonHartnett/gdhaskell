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
--   modify $ set #screenSize . view #y =<< getViewportRect =<< self
--   hide =<< self
-- 
-- start :: Godot Player ()
-- start = do
--   setPosition pos =<< self
--   show =<< self
--   self >>= getNode @"CollisionShape2D" >>= (`setDisabled` False)
--
-- process :: Float -> Godot Player ()
-- process delta = do
--   animSprite <- getNode @"AnimatedSprite3D" =<< self
--   screenSize <- self >>= getViewportRect
--   [left, right, up, down] <- do
--     Just inp <- getSingleton @Input
--     mapM (isActionPressed inp) ["ui_left", "ui_right", "ui_up", "ui_down"]
--   let velocity =
--         let bVal b v = if b then v else 0
--         in V2 ...
--   if velocity /= zero
--     then do
--       pos <- self >>= getPosition
--       let velocity' = normalize velocity 
--  
-- onPlayerBodyEntered :: Node -> Godot Player ()
-- onPlayerBodyEntered body = do
--   hide =<< self
--   emitSignal @"hit" [] =<< self
--   self 
--     >>= getNode @"CollisionShape2D" 
--     >>= callDefferred "set_disabled" [from True]
--
-- {-# ANN Player (exports ['ready, 'start, 'process] <> exportsAs [('onPlayerBodyEntered, "on_Player_body_entered")]) #-}
--
-- godot ''Player

generateEnum :: Eff '[] ()
generateEnum = undefined

--------------------------------------------------------------------------------

main :: IO ()
main = do
  f <- BS.readFile "godot-headers/extension_api.json"
  let res = A.eitherDecode @ExtensionApi f
  print $ res
