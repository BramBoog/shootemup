{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Model.Shooting (
  CanShoot (cooldown, lowerCooldown, weapon, shootsRightward, shoot, resetCooldown, allShoot),
  Bullet (Bullet, bulletPos, bulletVector),
  moveBullet,
  Weapon (Single, Burst, Cone),
) where

import Model.Movement (
    Position,
    Vector,
    HasPosition,
    move,
    pos
  )
import Model.Parameters

import Data.Aeson
import GHC.Generics


class HasPosition a => CanShoot a where
  shootsRightward :: a -> Bool
  cooldown :: a -> Float
  lowerCooldown :: Float -> a -> a
  resetCooldown :: a -> a
  weapon :: a -> Weapon

  shoot :: a -> (a, [Bullet])
  shoot a = if cooldown a <= 0 then (resetCooldown a, shootWeapon (weapon a))
                               else (a, [])
    where
      -- All bullet direction vectors
      straight = (bulletHorizontalSpeed, 0)
      upward   = (bulletHorizontalSpeed, bulletVerticalSpeed)
      downward = (bulletHorizontalSpeed, -bulletVerticalSpeed)

      -- Generates bullets based on the weapon.
      shootWeapon :: Weapon -> [Bullet]
      shootWeapon Single = [generateBullet (standardBulletDisplacement, 0) straight]
      shootWeapon Burst  = [
          generateBullet (standardBulletDisplacement, 0) straight,
          generateBullet (standardBulletDisplacement + burstBulletDisplacement, 0) straight,
          generateBullet (standardBulletDisplacement + burstBulletDisplacement * 2, 0) straight
        ]
      shootWeapon Cone   = [
          generateBullet (standardBulletDisplacement, standardBulletDisplacement) upward,
          generateBullet (standardBulletDisplacement, 0) straight,
          generateBullet (standardBulletDisplacement, -standardBulletDisplacement) downward
        ]

      -- Generates a bullet based on the current (pos a), displaces it by (dx, dy) and gives it a direction vector.
      -- (shootsRightward a) determines whether the bullets need to be displaced to the right (+) or to the left (-),
      -- and whether the x-component of their vector needs to be positive (towards the right) or negative (towards the left).
      generateBullet :: Vector -> Vector -> Bullet
      generateBullet (dx, dy) (vx, vy) = let (x, y) = pos a
                                          in if shootsRightward a then Bullet (x + dx, y + dy) (vx, vy)
                                                                  else Bullet (x - dx, y + dy) (-vx, vy)

  -- For a list of instances, let each of them shoot and combine the new instances (with reset cooldowns) and the generated bullets into separate lists.
  allShoot :: [a] -> ([a], [Bullet])
  allShoot as = let tups = map shoot as
                in (map fst tups, concatMap snd tups)

data Bullet = Bullet {bulletPos :: Position, bulletVector :: Vector} deriving (Eq, Show, Generic)

instance HasPosition Bullet where
  pos = bulletPos

instance ToJSON Bullet where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Bullet where

moveBullet :: Bullet -> Bullet
moveBullet b@Bullet {bulletPos, bulletVector} = b {bulletPos = move bulletPos bulletVector}

data Weapon = Single | Burst | Cone deriving (Show, Generic)

instance ToJSON Weapon where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Weapon where

