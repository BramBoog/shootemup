{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module Model.Enemy where

import Model.Movement (Position, Vector, move, HasPosition (pos, hit))
import Model.Parameters
import Model.Shooting (
    CanShoot (cooldown, lowerCooldown, resetCooldown, weapon, shootsRightward),
    Bullet (Bullet, bulletPos, bulletVector),
    Weapon (Single, Burst, Cone),
  )
import Model.Player
import Data.Maybe

-- All enemy data types. Despite seeming similar, they are separated so they can be rendered differently by the View.
data BasicEnemy = BasicEnemy {basicEnemyPos :: Position, basicEnemyCooldown :: Float} deriving Eq
data BurstEnemy = BurstEnemy {burstEnemyPos :: Position, burstEnemyCooldown :: Float} deriving Eq
data ConeEnemy = ConeEnemy {coneEnemyPos :: Position, coneEnemyCooldown :: Float} deriving Eq
data BasicPlayerSeekingEnemy = BasicPlayerSeekingEnemy {basicSeekingPos :: Position, basicSeekingCooldown :: Float} deriving Eq
data FastPlayerSeekingEnemy = FastPlayerSeekingEnemy {fastSeekingPos :: Position} deriving Eq

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
  -- Render enemy at a random vertical position.
  spawn :: a

  -- For a list of enemies of some type, calculate all hits with a list of bullets and separate the hit enemies and bullets out into two lists
  hitByBulletsList :: [a] -> [Bullet] -> ([a], [Bullet])
  hitByBulletsList enemies bs = let hits = catMaybes [hit e b | e <- enemies, b <- bs]
                                 in (map fst hits, map snd hits)

-- Enemy types as instances of type class Enemy
instance Enemy BasicEnemy where
  spawn = BasicEnemy {basicEnemyPos = (screenMaxX, randomY) , basicEnemyCooldown = enemyShootingCooldown} -- Spawn an enemy at right with a random y pos.

instance Enemy BurstEnemy where
  spawn = BurstEnemy {burstEnemyPos = (screenMaxX, randomY) , burstEnemyCooldown = enemyShootingCooldown}  

instance Enemy ConeEnemy where
  spawn = ConeEnemy {coneEnemyPos = (screenMaxX, randomY) , coneEnemyCooldown = enemyShootingCooldown}    

instance Enemy BasicPlayerSeekingEnemy where
  spawn = BasicPlayerSeekingEnemy {basicSeekingPos = (screenMaxX, randomY) , basicSeekingCooldown = enemyShootingCooldown}      

instance Enemy FastPlayerSeekingEnemy where
  spawn = FastPlayerSeekingEnemy {fastSeekingPos = (screenMaxX, randomY)}     
       

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
  moveSeeking :: Player -> a -> a

  -- Determines whether the enemy needs to move up, down, or neither to get closer to the player vertically
  yMovementSign :: Player -> a -> Float
  yMovementSign player e | y < playerY = 1
                         | y == playerY = 0
                         | otherwise = -1
    where (_, y) = pos e
          playerY = snd $ playerPos player

instance MovePlayerSeeking BasicPlayerSeekingEnemy where
  moveSeeking player b = let p    = pos b
                             sign = yMovementSign player b
                             vec  = (-basicEnemyHorizontalSpeed, basicEnemyVerticalSpeed * sign)
                         in b {basicSeekingPos = move p vec}

instance MovePlayerSeeking FastPlayerSeekingEnemy where
  moveSeeking player b = let p    = pos b
                             sign = yMovementSign player b
                             vec  = (-fastEnemyHorizontalSpeed, fastEnemyVerticalSpeed * sign)
                          in b {fastSeekingPos = move p vec}
  
instance CanShoot BasicEnemy where
  shootsRightward _ = False
  cooldown = basicEnemyCooldown
  lowerCooldown t p@BasicEnemy{basicEnemyCooldown} = p{basicEnemyCooldown = basicEnemyCooldown - t}
  resetCooldown p@BasicEnemy{basicEnemyCooldown} = p{basicEnemyCooldown = enemyShootingCooldown}
  weapon _ = Single
                
instance CanShoot BurstEnemy where
  shootsRightward _ = False
  cooldown = burstEnemyCooldown
  lowerCooldown t p@BurstEnemy{burstEnemyCooldown} = p{burstEnemyCooldown = burstEnemyCooldown - t}
  resetCooldown p@BurstEnemy{burstEnemyCooldown} = p{burstEnemyCooldown = enemyShootingCooldown}
  weapon _ = Burst

instance CanShoot ConeEnemy where
  shootsRightward _ = False
  cooldown = coneEnemyCooldown
  lowerCooldown t p@ConeEnemy{coneEnemyCooldown} = p{coneEnemyCooldown = coneEnemyCooldown - t}
  resetCooldown p@ConeEnemy{coneEnemyCooldown} = p{coneEnemyCooldown = enemyShootingCooldown}
  weapon _ = Cone

instance CanShoot BasicPlayerSeekingEnemy where
  shootsRightward _ = False
  cooldown = basicSeekingCooldown
  lowerCooldown t p@BasicPlayerSeekingEnemy{basicSeekingCooldown} = p{basicSeekingCooldown = basicSeekingCooldown - t}
  resetCooldown p@BasicPlayerSeekingEnemy{basicSeekingCooldown} = p{basicSeekingCooldown = enemyShootingCooldown}
  weapon _ = Single
  
-- Show is used to in a dictionary in View to return a picture for each renderable data type.
instance Show BasicEnemy where
        show basicEnemy = "BasicEnemy"

instance Show BurstEnemy where 
        show burstEnemy = "BurstEnemy"  

instance Show ConeEnemy where 
        show coneEnemy = "ConeEnemy"   

instance Show BasicPlayerSeekingEnemy where 
        show basicPlayerSeekingEnemy = "BasicPlayerSeekingEnemy"   

instance Show FastPlayerSeekingEnemy where 
        show fastPlayerSeekingEnemy = "FastPlayerSeekingEnemy"  
