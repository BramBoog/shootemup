{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Model.PowerUp where

import Model.Movement (Position, HasPosition (pos, hitboxSize))
import Model.Randomness
import Model.Parameters

import Data.Aeson
import GHC.Generics

data PowerUp = BurstFire {burstFirePos :: Position}
              | ConeFire {coneFirePos :: Position}
              | SpeedBoost {speedBoostPos :: Position} deriving (Generic, Show)
            
instance HasPosition PowerUp where
  pos BurstFire {burstFirePos} = burstFirePos
  pos ConeFire {coneFirePos} = coneFirePos
  pos SpeedBoost {speedBoostPos} = speedBoostPos

  hitboxSize _ = powerupSize

instance ToJSON PowerUp where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON PowerUp where

-- Randomly chooses a powerup type and creates a new powerup of that type at a random y and the player's x coordinates
spawnPowerUp :: IO PowerUp
spawnPowerUp = do ry <- randomY powerupSize
                  powerUpConstr <- chooseWithProb powerUpProbDist
                  return (powerUpConstr (playerX, ry))
                  
powerUpProbDist :: ProbDist (Position -> PowerUp)
powerUpProbDist = ProbDist [(1, BurstFire), (1, ConeFire), (1, SpeedBoost)]
