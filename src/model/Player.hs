module Model.Player where

import Model.General (Position, Vector, HasPosition, pos, move)
import Model.Shooting (Weapon, shootWeapon, CanShoot, shoot)

data Player = Player {
  playerPos :: Position,
  speed :: Float, 
  weapon :: Weapon, 
  lives :: Int 
}

instance HasPosition Player where
  pos = playerPos

instance CanShoot Player where
  shoot p = shootWeapon (weapon p) p True

-- player only moves in y
movePlayer :: Player -> Float -> Player
movePlayer pl dy = let p = playerPos pl
                       s = speed pl
                    in pl {playerPos = move p (0, dy * s)}
