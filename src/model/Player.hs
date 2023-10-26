module Model.Player where

import Model.General (Position)
import Model.Shooting (Weapon)

data Player = Player {
  playerPos :: Position,
  speed :: Float, 
  weapon :: Weapon, 
  lives :: Int 
}
