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

-- class NodeMethod target methodName bindings returnType where
--   nodeMethod :: target -> returnType

-- data TestS = MkTestS { x :: Int }

-- data Object

-- data Instance n = MkInstance Object n

-- type Godot n a = Eff '[State (Instance n), IOE] a

-- data TestP = MkTestP Int

-- self :: forall n. Godot n (Instance n)
-- self = get @(Instance n)

-- instance HasField f n a => HasField f (Instance n) a where
--   getField (MkInstance _ i) = getField @f i

-- instance HasField f n x => HasField f (Godot n a) (Godot n x) where
--   getField _ = do
--     (MkInstance obj n) <- get @(Instance n)
--     pure $ getField @f n

-- testF :: Godot TestS ()
-- testF = do
--   s <- self
--   undefined

-- -- instance HasField f n a => HasField f (Instance n) (Godot n a) where
-- --   getField (MkInstance obj x) = getField @f x

-- -- newGame :: Godot Main ()
-- -- newGame = do
-- --   modify $ set #score 0
-- --   position <- self.getNode @"StartPosition" >>= getPosition
-- --   void $ call <$> self.getNode @"Player" <*> pure "_start" <*> pure [from position]
-- --   self.getNode @"StartTimer" >>= start `flip` Nothing
-- --   hud <- self.getNode @"HUD"
-- --   updateScore hud 0 >> showMessage hud "Get ready!"

-- -- onMobTimerTimeout :: Godot Main ()
-- -- onMobTimerTimeout = do
-- --   mobSpawnLoc <- getNode @"MobPath/MobSpawnLocation" self
-- --   setOffset mobSpawnLoc . from =<< randomIO
-- --   mob <- 
-- --     self.mobScene 
-- --       >>= (`PackedScene.instance` SceneFlagsNone) 
-- --       >>= asNativeScript 
-- --       <&> fromJust
-- --  addChild self mob (Just False) (Just InternalModeDisabled)
-- --  direction <- getRotation mobSpawnLoc <&> (+ pi / 2)
-- --  setPosition mob =<< getPosition mobSpawnLoc
-- --  direction' <- (direction +) <$> randomRIO ((-pi) / 4, pi / 4)
-- --  setRotation mob direction'
-- --  (,) <$> mob.minSpeed <*> mob.maxSpeed >>= randomRIO >>= (\x -> from $ V2 x 0) >>= (`rotated` direction') >>= setLienarVelocity mob

-- generateEnum :: Eff '[] ()
-- generateEnum = undefined

generateEnum :: Eff '[] ()
generateEnum = undefined

--------------------------------------------------------------------------------

main :: IO ()
main = do
  f <- BS.readFile "godot-headers/extension_api.json"
  let res = A.eitherDecode @ExtensionApi f
  print $ res
