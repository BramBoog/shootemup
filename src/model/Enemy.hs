{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module Model.Enemy where

import Model.Movement (Position, Vector, move, HasPosition, pos)
import Model.Parameters
import Model.Shooting (
    CanShoot,
    shoot,
    shootWeapon,
    Bullet (Bullet, bulletPos, bulletVector),
    Weapon (Single, Burst, Cone)
  )
import Model.Player

-- All five enemy data types, enemy type class.
data BasicEnemy = BasicEnemy {basicEnemyPos :: Position, basicEnemyCooldown :: Float} deriving (Eq, Show)
data BurstEnemy = BurstEnemy {burstEnemyPos :: Position, burstEnemyCooldown :: Float} deriving (Eq, Show)
data ConeEnemy = ConeEnemy {coneEnemyPos :: Position, coneEnemyCooldown :: Float} deriving (Eq, Show)
data BasicPlayerSeekingEnemy = BasicPlayerSeekingEnemy {basicSeekingPos :: Position, basicSeekingCooldown :: Float} deriving (Eq, Show)
data FastPlayerSeekingEnemy = FastPlayerSeekingEnemy {fastSeekingPos :: Position} deriving (Eq, Show)

instance HasPosition BasicEnemy where
  pos = basicEnemyPos

instance HasPosition BurstEnemy where
  pos = burstEnemyPos

instance HasPosition ConeEnemy where
  pos = coneEnemyPos

instance HasPosition BasicPlayerSeekingEnemy where
  pos = basicSeekingPos

instance HasPosition FastPlayerSeekingEnemy where
  pos = fastSeekingPos

-- All enemies are instances of this class.
class HasPosition a => Enemy a where 
  -- Given an enemy and a bullet, return these arguments if the enemy is hit for despawning both. 
  hit :: a -> Bullet -> Maybe (a, Bullet)
  hit e b | pos e == bulletPos b = Just (e, b) -- Enemy is hit. Change to this later: if the position is approximately equal.
          | otherwise = Nothing -- Enemy is not hit.
  -- Render enemy at a random vertical position.
  spawn :: a

-- Enemy types as instances of type class Enemy
instance Enemy BasicEnemy where
  spawn = BasicEnemy {basicEnemyPos = (screenMax, randomY) , basicEnemyCooldown = enemyShootingCooldown} -- Spawn an enemy at right with a random y pos.

instance Enemy BurstEnemy where
  spawn = BurstEnemy {burstEnemyPos = (screenMax, randomY) , burstEnemyCooldown = enemyShootingCooldown}  

instance Enemy ConeEnemy where
  spawn = ConeEnemy {coneEnemyPos = (screenMax, randomY) , coneEnemyCooldown = enemyShootingCooldown}    

instance Enemy BasicPlayerSeekingEnemy where
  spawn = BasicPlayerSeekingEnemy {basicSeekingPos = (screenMax, randomY) , basicSeekingCooldown = enemyShootingCooldown}      

instance Enemy FastPlayerSeekingEnemy where
  spawn = FastPlayerSeekingEnemy {fastSeekingPos = (screenMax, randomY)}     
       

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

instance CanShoot BasicEnemy where
  shoot b = shootWeapon Single b False
                
instance CanShoot BurstEnemy where
  shoot b = shootWeapon Burst b False

instance CanShoot ConeEnemy where
  shoot c = shootWeapon Cone c False

instance CanShoot BasicPlayerSeekingEnemy where
  shoot b = shootWeapon Single b False
  