module Model.PowerUp where

import Model.Movement (Position)
  
data PowerUp = BurstFire {burstFirePos :: Position}
              | ConeFire {coneFirePos :: Position}
              | SpeedBoost {speedBoostPos :: Position} deriving (Eq, Show)
              