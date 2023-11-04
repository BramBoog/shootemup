{-# LANGUAGE NamedFieldPuns #-}
module Model.PowerUp where

import Model.Movement (Position, HasPosition (pos))
  
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

    
  
