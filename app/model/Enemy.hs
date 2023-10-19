module Model.Enemy where
    -- All five enemy data types, enemy type class

data BasicEnemy = BasicEnemy {pos :: Position, cooldown :: Int} 
data BurstEnemy = BurstEnemy {pos :: Position, cooldown :: Int} 
data ConeEnemy = ConeEnemy {pos :: Position, cooldown :: Int} 
data BasicPlayerSeekingEnemy = BasicPlayerSeekingEnemy { 
        pos :: Position, 
        cooldown :: Int 
} 
data FastPlayerSeekingEnemy = FastPlayerSeekingEnemy {pos :: Position} 
  