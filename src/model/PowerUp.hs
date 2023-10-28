{-# LANGUAGE NamedFieldPuns #-}
module Model.PowerUp where

import Model.General (Position)
  
data PowerUp = BurstFire {burstFirePos :: Position}
              | ConeFire {coneFirePos :: Position}
              | SpeedBoost {speedBoostPos :: Position}
            
instance Show PowerUp where
    show BurstFire {burstFirePos} = "BurstFire"
    show ConeFire {coneFirePos} = "BurstFire"
    show SpeedBoost {speedBoostPos} = "SpeedBoost"