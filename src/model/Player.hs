module Model.Player where

import Model.General (Position)
import Model.Shooting (Weapon)

data Player = Player {
  pos :: Position, 
  speed :: Int, 
  weapon :: Weapon, 
  lives :: Int 
}
