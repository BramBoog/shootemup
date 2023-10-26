{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module Model.Enemy where

import Model.General (Position, Vector, move)
import Model.Parameters
import Model.Shooting (CanShoot, shoot, Bullet (Bullet, bulletPos, bulletVector))
import Model.Player

-- All five enemy data types, enemy type class.
data BasicEnemy = BasicEnemy {basicEnemyPos :: Position, basicEnemyCooldown :: Float}
data BurstEnemy = BurstEnemy {burstEnemyPos :: Position, burstEnemyCooldown :: Float}
data ConeEnemy = ConeEnemy {coneEnemyPos :: Position, coneEnemyCooldown :: Float}
data BasicPlayerSeekingEnemy = BasicPlayerSeekingEnemy {basicSeekingPos :: Position, basicSeekingCooldown :: Float}
data FastPlayerSeekingEnemy = FastPlayerSeekingEnemy {fastSeekingPos :: Position}


-- All enemies are instances of this class.
class Enemy a where 
  -- Given an enemy and a bullet, return these arguments if the enemy is hit for despawning both. 
  hit :: a -> Bullet -> Maybe (a, Bullet)
  hit e b | pos e == bulletPos b = Just (e, b) -- Enemy is hit. Change to this later: if the position is approximately equal.
          | otherwise = Nothing -- Enemy is not hit.
  -- Render enemy at a random vertical position.
  spawn :: a
  -- Return the position of an Enemy
  pos :: a -> Position

-- Enemy types as instances of type class Enemy (Too much repitition?)
instance Enemy BasicEnemy where
  spawn = BasicEnemy {basicEnemyPos = (screenMax, randomY) , basicEnemyCooldown = enemyShootingCooldown} -- Spawn an enemy at right with a random y pos.
  pos = basicEnemyPos

instance Enemy BurstEnemy where
  spawn = BurstEnemy {burstEnemyPos = (screenMax, randomY) , burstEnemyCooldown = enemyShootingCooldown}  
  pos = burstEnemyPos

instance Enemy ConeEnemy where
  spawn = ConeEnemy {coneEnemyPos = (screenMax, randomY) , coneEnemyCooldown = enemyShootingCooldown}    
  pos = coneEnemyPos

instance Enemy BasicPlayerSeekingEnemy where
  spawn = BasicPlayerSeekingEnemy {basicSeekingPos = (screenMax, randomY) , basicSeekingCooldown = enemyShootingCooldown}      
  pos = basicSeekingPos

instance Enemy FastPlayerSeekingEnemy where
  spawn = FastPlayerSeekingEnemy {fastSeekingPos = (screenMax, randomY)}     
  pos = fastSeekingPos
       

-- All enemies, except the player seeking types, are instances of this class.
class Enemy a => MoveBasic a where 
  moveBasic :: a -> a 
    
instance MoveBasic BasicEnemy where
  moveBasic b = let vec = (-basicEnemyHorizontalSpeed, 0)
                    p   = pos b
                 in b {basicEnemyPos = move p vec}

instance MoveBasic BurstEnemy where
  moveBasic b = let vec = (-basicEnemyHorizontalSpeed, 0)
                    p   = pos b
                 in b {burstEnemyPos = move p vec}

instance MoveBasic ConeEnemy where
  moveBasic b = let vec = (-basicEnemyHorizontalSpeed, 0)
                    p   = pos b
                 in b {coneEnemyPos = move p vec}

-- Only the two player seeking enemies are instances of this class.
class Enemy a => MovePlayerSeeking a where 
  moveSeeking :: a -> Player -> a

  -- Determines whether the enemy needs to move up, down, or neither to get closer to the player vertically
  yMovementSign :: a -> Player -> Float
  yMovementSign e player | y < playerY = 1
                         | y == playerY = 0
                         | otherwise = -1
    where (_, y) = pos e
          playerY = snd $ playerPos player

instance MovePlayerSeeking BasicPlayerSeekingEnemy where
  moveSeeking b player = let p    = pos b
                             sign = yMovementSign b player
                             vec  = (-basicEnemyHorizontalSpeed, basicEnemyVerticalSpeed * sign)
                         in b {basicSeekingPos = move p vec}

instance MovePlayerSeeking FastPlayerSeekingEnemy where
  moveSeeking b player = let p    = pos b
                             sign = yMovementSign b player
                             vec  = (-fastEnemyHorizontalSpeed, fastEnemyVerticalSpeed * sign)
                          in b {fastSeekingPos = move p vec}

-- All bullet direction vectors
left = (-bulletHorizontalSpeed, 0)
upward   = (-bulletHorizontalSpeed, bulletVerticalSpeed)
downward = (-bulletHorizontalSpeed, -bulletVerticalSpeed)

instance CanShoot BasicEnemy where
  shoot b = let (x, y) = pos b in [Bullet (x - 0.02, y) left] -- One bullet
                
instance CanShoot BurstEnemy where
  shoot b = let (x, y) = pos b in
    -- Three bullets in one horizontal line, each a bit more to the left of the enem than the last.
    [Bullet (x - 0.02, y) left, Bullet(x - 0.04, y) left, Bullet (x - 0.06, y) left]

instance CanShoot ConeEnemy where
  shoot c = let (x, y) = pos c in
    [Bullet (x - 0.02, y) left, Bullet (x - 0.02, y + 0.02) upward, Bullet (x - 0.02, y - 0.02) downward] -- Three bullets in three different directions

instance CanShoot BasicPlayerSeekingEnemy where
  shoot b = let (x, y) = pos b in [Bullet (x - 0.02, y) left] -- One bullet
  