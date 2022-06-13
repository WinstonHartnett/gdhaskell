{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Godot.Extension.Core where

import GHC.Records
import GHC.OverloadedLabels
import GHC.Base
import GHC.TypeLits
import Control.Monad
import Godot.Extension.Extension

class a <: b where
  upcast :: a -> b

instance {-# OVERLAPPABLE #-} x <: x where
  upcast = id

-- 1. Generate wrapper type (ptr void)
data ExampleBuiltin = MkExampleBuiltin GdnativeVariantPtr

-- 2. 

-- Builtin Steps
--





















-- playerProcess :: Float -> Godot Player ()
-- playerProcess delta = do
--   animSprite <- getNode @"AnimatedSprite" self
--   screenSize <- self >>= getViewportRect <&> view #y
--   [left, right, up, down] <- do
--     Just inp <- getSingleton @Input
--     mapM (isActionPressed inp) ["ui_left", "ui_right", "ui_up", "ui_down"]
--   let velocity =
--         let bVal b v = if b then v else 0
--          in V2 (bVal left -1 + bVal right 1) (bVal up -1 + bVal down 1)
--   if velocity /= zero
--     then do
--       speed <- self.speed
--       pos   <- from =<< self.position
--       let velocity' = normalize velocity * speed
--           pos'      = pos + velocity' * delta
--           clamp v a b = max a (min b v)
--       setPosition <$> self <*> pure newPos
--       play (Just "") Nothing animSprite
--       if velocity'.x /= 0
--         then do
--           setAnimation "right" animSprite
--           setFlipV (velocity'.y > 0) animSprite
--           setFlipH (velocity'.x < 0) animSprite
--           play "animName" True animSprite
--         else when (velocity'.y /= 0) do
--           setAnimation "up" animSprite
--           setFlipV (velocity'.y > 0) animSprite
--    else stop animSprite