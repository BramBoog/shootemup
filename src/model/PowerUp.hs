{-# LANGUAGE NamedFieldPuns #-}
module Model.PowerUp where

import Model.Movement (Position, HasPosition (pos))
import Model.Randomness
import Model.Parameters
  
data PowerUp = BurstFire {burstFirePos :: Position}
              | ConeFire {coneFirePos :: Position}
              | SpeedBoost {speedBoostPos :: Position}
            
instance Show PowerUp where
  show BurstFire {burstFirePos} = "BurstFire"
  show ConeFire {coneFirePos} = "BurstFire"
  show SpeedBoost {speedBoostPos} = "SpeedBoost"

instance HasPosition PowerUp where
  pos BurstFire {burstFirePos} = burstFirePos
  pos ConeFire {coneFirePos} = coneFirePos
  pos SpeedBoost {speedBoostPos} = speedBoostPos

-- Randomly chooses a powerup type and creates a new powerup of that type at a random y and the player's x coordinates
spawnPowerUp :: IO PowerUp
spawnPowerUp = do ry <- randomY powerupSize
                  powerUpConstr <- chooseWithProb powerUpProbDist
                  return (powerUpConstr (playerX, ry))
                  
powerUpProbDist :: ProbDist (Position -> PowerUp)
powerUpProbDist = ProbDist [(1, BurstFire), (1, ConeFire), (1, SpeedBoost)]
