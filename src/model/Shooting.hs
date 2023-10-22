module Model.Shooting where

import Model.General

-- CanShoot type class, Bullet data type, Weapon data type

data Bullet = Bullet {pos :: Position, vector :: Vector}

data Weapon = Single | Burst | Cone
