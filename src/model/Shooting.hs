module Model.Shooting where

import Model.General

-- CanShoot type class, Bullet data type, Weapon data type

data Bullet = Bullet {bulletPos :: Position, bulletVector :: Vector}

data Weapon = Single | Burst | Cone deriving Show

instance Show Bullet where
    show bullet = "Bullet"
