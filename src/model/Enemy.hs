module Model.Enemy where

import Model.General (Position)

-- All five enemy data types, enemy type class
data BasicEnemy = BasicEnemy {basicEnemyPos :: Position, basicEnemyCooldown :: Int}
data BurstEnemy = BurstEnemy {burstEnemyPos :: Position, burstEnemyCooldown :: Int}
data ConeEnemy = ConeEnemy {coneEnemyPos :: Position, coneEnemyCooldown :: Int}
data BasicPlayerSeekingEnemy = BasicPlayerSeekingEnemy {basicSeekingPos :: Position, basicSeekingCooldown :: Int}
data FastPlayerSeekingEnemy = FastPlayerSeekingEnemy {fastSeekingPos :: Position}
