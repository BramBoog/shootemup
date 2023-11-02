module Model.Player where

import Model.General (Position, Vector, HasPosition, pos, move)
import Model.Parameters
import Model.Shooting (Bullet, Weapon, shootWeapon, CanShoot, shoot)

data Player = Player {
  playerPos :: Position,
  speed :: Float, 
  weapon :: Weapon, 
  lives :: Int 
}

instance Show Player where
  show player = "Player"

instance HasPosition Player where
  pos = playerPos

instance CanShoot Player where
  shoot p = shootWeapon (weapon p) p True

-- player only moves in y, cannot move beyond max and min y
movePlayer :: Player -> Float -> Player
movePlayer pl dy = let p = playerPos pl
                       s = speed pl
                       (x, y) = move p (0, dy * s)
                    in pl {playerPos = (x, min (max y screenMin) screenMax)}

-- Determine if a bullet hits a player. Can use later to despawn the bullet and subtract a life from the player.
playerIsHit :: Player -> Bullet -> Maybe Bullet
playerIsHit p b = if pos p == pos b then Just b
                                    else Nothing
