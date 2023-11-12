{-# LANGUAGE NamedFieldPuns #-}
module Model.Player where

import Model.Movement (Position, Vector, HasPosition (pos, hit), move, Direction (ToTop, ToBottom))
import Model.Parameters (screenMinY, screenMaxY, playerShootingCooldown, playerNormalVerticalSpeed, playerBoostedVerticalSpeed)
import Model.Shooting (
    Bullet,
    Weapon,
    CanShoot (cooldown, lowerCooldown, resetCooldown, weapon, shootsRightward)
  )
import Data.Maybe (mapMaybe)

data Player = Player {
  playerPos :: Position,
  speed :: PlayerSpeed,
  playerWeapon :: Weapon,
  lives :: Int,
  playerCooldown :: Float
}

data PlayerSpeed = Normal | Boosted

instance Show Player where
  show player = "Player"

instance HasPosition Player where
  pos = playerPos

instance CanShoot Player where
  shootsRightward _ = True
  cooldown = playerCooldown
  lowerCooldown t p@Player{playerCooldown} = p{playerCooldown = playerCooldown - t}
  resetCooldown p@Player{playerCooldown} = p{playerCooldown = playerShootingCooldown}
  weapon = playerWeapon

-- player only moves in y, cannot move beyond max and min y
movePlayer :: Direction -> Player -> Player
movePlayer d pl@Player{playerPos, speed} = let dy = case speed of
                                                      Normal -> playerNormalVerticalSpeed
                                                      Boosted -> playerBoostedVerticalSpeed
                                               sign = case d of
                                                        ToTop -> 1
                                                        ToBottom -> -1
                                                        _ -> error "Player can only move up or down."
                                               (x, y) = move playerPos (0, sign * dy)
                                            in pl{playerPos = (x, min (max y screenMinY) screenMaxY)}

-- For a list of objects of a certain type, obtain all objects which have hit the player
hitsOnPlayer :: HasPosition a => Player -> [a] -> [a]
hitsOnPlayer p = map snd . mapMaybe (hit p) -- mapMaybe throws out all Nothings and returns a [(Player, a)], so we need map snd to get [a]
