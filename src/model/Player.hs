{-# LANGUAGE NamedFieldPuns #-}
module Model.Player where

import Model.Movement (Position, Vector, HasPosition (pos, hit), move, Direction (ToTop, ToBottom))
import Model.Parameters (screenMinY, screenMaxY, playerShootingCooldown, powerUpDuration, playerNormalVerticalSpeed, playerBoostedVerticalSpeed)
import Model.Shooting (
    Bullet,
    Weapon (Single, Burst, Cone),
    CanShoot (cooldown, lowerCooldown, resetCooldown, weapon, shootsRightward)
  )
import Model.PowerUp

import Data.Maybe (mapMaybe)

data Player = Player {
  playerPos :: Position,
  speed :: PlayerSpeed,
  playerWeapon :: Weapon,
  lives :: Int,
  playerCooldown :: Float,
  weaponPowerUpTimer :: Maybe Float,
  speedBoostPowerUpTimer :: Maybe Float
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

pickUpPowerUp :: PowerUp -> Player -> Player
pickUpPowerUp (BurstFire _) pl = pl{playerWeapon = Burst, weaponPowerUpTimer = Just powerUpDuration}
pickUpPowerUp (ConeFire _) pl = pl{playerWeapon = Cone, weaponPowerUpTimer = Just powerUpDuration}
pickUpPowerUp (SpeedBoost _) pl = pl{speed = Boosted, speedBoostPowerUpTimer = Just powerUpDuration}

-- Lower the player's weaponPowerUpTimer and speedBoostPowerUpTimer if it has them. If they goes lower than 0, remove the powerup
lowerPowerUpTimers :: Float -> Player -> Player
lowerPowerUpTimers t pl@Player{weaponPowerUpTimer, speedBoostPowerUpTimer} =
  let loweredWeaponTimer = lowerWeaponTimer pl
   in lowerSpeedBoostTimer loweredWeaponTimer
  where
    lowerWeaponTimer pl'@Player{weaponPowerUpTimer} = case lowerTimer weaponPowerUpTimer of
                                                        Nothing -> pl'{playerWeapon = Single, weaponPowerUpTimer = Nothing}
                                                        Just a -> pl'{weaponPowerUpTimer = Just a}
    lowerSpeedBoostTimer pl'@Player{speedBoostPowerUpTimer} = case lowerTimer speedBoostPowerUpTimer of
                                                                Nothing -> pl'{speed = Normal, speedBoostPowerUpTimer = Nothing}
                                                                Just a -> pl'{speedBoostPowerUpTimer = Just a}
    lowerTimer :: Maybe Float -> Maybe Float
    lowerTimer m = m >>= (\t' -> let lowered = t' - t in if lowered <= 0 then Nothing else Just lowered)
