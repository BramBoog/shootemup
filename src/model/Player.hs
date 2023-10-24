module Model.Player where

import Model.General (Position)
import Model.Shooting (Weapon)

data Player = Player {
  playerPos :: Int, -- Only vertical position 
  speed :: Int, 
  weapon :: Weapon, 
  lives :: Int 
}
