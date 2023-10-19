module Model.Player where

data Player = Player {
    pos :: Position, 
    speed :: Int, 
    weapon :: Weapon, 
    lives :: Int 
} 

 