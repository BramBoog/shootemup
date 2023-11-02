{-# LANGUAGE NamedFieldPuns #-}
module Model.Shooting (
  CanShoot,
  shoot,
  Bullet (Bullet, bulletPos, bulletVector),
  moveBullet,
  Weapon (Single, Burst, Cone),
  shootWeapon
) where

import Model.Movement (
    Position,
    Vector,
    HasPosition,
    move,
    pos
  )
import Model.Parameters

class HasPosition a => CanShoot a where
  shoot :: a -> [Bullet]

  -- Generates bullets based on the weapon and (pos a).
  -- goesRightWard determines whether the bullets need to be displaced to the right (+) or to the left (-) of (pos a),
  -- and whether the x-component of their vector needs to be positive (towards the right) or negative (towards the left).
  shootWeapon :: Weapon -> a -> Bool -> [Bullet]
  shootWeapon Single a goesRightWard = [generateBullet (pos a) (standardBulletDisplacement, 0) straight goesRightWard]
  shootWeapon Burst  a goesRightWard = [
      generateBullet (pos a) (standardBulletDisplacement, 0) straight goesRightWard,
      generateBullet (pos a) (standardBulletDisplacement * 2, 0) straight goesRightWard,
      generateBullet (pos a) (standardBulletDisplacement * 3, 0) straight goesRightWard
    ]
  shootWeapon Cone   a goesRightWard = [
      generateBullet (pos a) (standardBulletDisplacement, standardBulletDisplacement) upward goesRightWard,
      generateBullet (pos a) (standardBulletDisplacement, 0) straight goesRightWard,
      generateBullet (pos a) (standardBulletDisplacement, -standardBulletDisplacement) downward goesRightWard
    ]

data Bullet = Bullet {bulletPos :: Position, bulletVector :: Vector}

instance HasPosition Bullet where
  pos = bulletPos

moveBullet :: Bullet -> Bullet
moveBullet b@Bullet {bulletPos, bulletVector} = b {bulletPos = move bulletPos bulletVector}

data Weapon = Single | Burst | Cone deriving Show

instance Show Bullet where
    show bullet = "Bullet"

-- All bullet direction vectors
straight = (bulletHorizontalSpeed, 0)
upward   = (bulletHorizontalSpeed, bulletVerticalSpeed)
downward = (bulletHorizontalSpeed, -bulletVerticalSpeed)

-- Generates a bullet based on the current position, displaces it by (dx, dy) and gives it a direction vector.
-- goesRightWard determines whether the bullets need to be displaced to the right (+) or to the left (-),
-- and whether the x-component of their vector needs to be positive (towards the right) or negative (towards the left).
generateBullet :: Position -> Vector -> Vector -> Bool -> Bullet
generateBullet (x, y) (dx, dy) (vx, vy) goesRightWard = if goesRightWard then Bullet (x + dx, y + dy) (vx, vy)
                                                                         else Bullet (x - dx, y + dy) (-vx, vy)
