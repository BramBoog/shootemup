module Model.PowerUp where
    
data PowerUp = BurstFire {pos :: Position} 
             | ConeFire {pos :: Position} 
             | SpeedBoost {pos :: Position}      