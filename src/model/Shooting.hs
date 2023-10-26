{-# LANGUAGE NamedFieldPuns #-}
module Model.Shooting (
  CanShoot,
  shoot,
  Bullet (Bullet, bulletPos, bulletVector),
  moveBullet,
  Weapon (Single, Burst, Cone)
) where

import Model.General

-- CanShoot type class, Bullet data type, Weapon data type

class CanShoot a where 
  shoot :: a -> [Bullet]

data Bullet = Bullet {bulletPos :: Position, bulletVector :: Vector}

moveBullet :: Bullet -> Bullet
moveBullet b@Bullet {bulletPos, bulletVector} = b {bulletPos = move bulletPos bulletVector}

data Weapon = Single | Burst | Cone
