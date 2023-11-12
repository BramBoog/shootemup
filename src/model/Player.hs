{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Model.Player where

import Model.Movement (Position, Vector, HasPosition (pos, hitboxSize, hit), move)
import Model.Parameters (screenMinY, screenMaxY, playerShootingCooldown, playerSize)
import Model.Shooting (
    Bullet,
    Weapon,
    CanShoot (cooldown, lowerCooldown, resetCooldown, weapon, shootsRightward)
  )

import Data.Maybe (mapMaybe)
import Data.Aeson
import GHC.Generics


data Player = Player {
  playerPos :: Position,
  speed :: Float,
  playerWeapon :: Weapon,
  lives :: Int,
  playerCooldown :: Float
} deriving (Generic, Show)

instance HasPosition Player where
  pos = playerPos
  hitboxSize _ = playerSize

instance CanShoot Player where
  shootsRightward _ = True
  cooldown = playerCooldown
  lowerCooldown t p@Player{playerCooldown} = p{playerCooldown = playerCooldown - t}
  resetCooldown p@Player{playerCooldown} = p{playerCooldown = playerShootingCooldown}
  weapon = playerWeapon

instance ToJSON Player where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Player where

-- player only moves in y, cannot move beyond max and min y
movePlayer :: Player -> Float -> Player
movePlayer pl dy = let p = playerPos pl
                       s = speed pl
                       (x, y) = move p (0, dy * s)
                    in pl {playerPos = (x, min (max y screenMinY) screenMaxY)}

-- For a list of objects of a certain type, obtain all objects which have hit the player
hitsOnPlayer :: HasPosition a => Player -> [a] -> [a]
hitsOnPlayer p = map snd . mapMaybe (hit p) -- mapMaybe throws out all Nothings and returns a [(Player, a)], so we need map snd to get [a]
