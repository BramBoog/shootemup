{-# LANGUAGE NamedFieldPuns #-}
module Model.Shooting (
  CanShoot,
  shoot,
  Bullet (Bullet, bulletPos, bulletVector),
  moveBullet,
  Weapon (Single, Burst, Cone),
  shootWeapon
) where

import Model.General (
    Position,
    Vector,
    HasPosition,
    move,
    pos
  )
import Model.Parameters

class HasPosition a => CanShoot a where
  shoot :: a -> [Bullet]

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

data Weapon = Single | Burst | Cone

-- All bullet direction vectors
straight = (bulletHorizontalSpeed, 0)
upward   = (bulletHorizontalSpeed, bulletVerticalSpeed)
downward = (bulletHorizontalSpeed, -bulletVerticalSpeed)

generateBullet :: Position -> Vector -> Vector -> Bool -> Bullet
generateBullet (x, y) (dx, dy) (vx, vy) goesRightWard = if goesRightWard then Bullet (x + dx, y + dy) (vx, vy)
                                                                         else Bullet (x - dx, y + dy) (-vx, vy)
