{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module Model.Enemy where

import Model.General (Position, Vector)
import Model.Shooting (Bullet (Bullet, bulletPos, bulletVector))
import Model.Player

-- All five enemy data types, enemy type class.
data BasicEnemy = BasicEnemy {basicEnemyPos :: Position, basicEnemyCooldown :: Int} 
data BurstEnemy = BurstEnemy {burstEnemyPos :: Position, burstEnemyCooldown :: Int} 
data ConeEnemy = ConeEnemy {coneEnemyPos :: Position, coneEnemyCooldown :: Int} 
data BasicPlayerSeekingEnemy = BasicPlayerSeekingEnemy {basicSeekingPos :: Position, basicSeekingCooldown :: Int} 
data FastPlayerSeekingEnemy = FastPlayerSeekingEnemy {fastSeekingPos :: Position} 


-- All enemies are instances of this class.
class Enemy a where 
    -- Given an enemy and a bullet, return these arguments if the enemy is hit for despawning both. 
    hit:: a -> Bullet -> Maybe (a, Bullet) 
    -- Render enemy at a random vertical position.
    spawn :: a
    -- Return the position of an Enemy
    pos :: a -> Position

-- Default values, change these later.
screenWidth :: Int
screenWidth = 500
randomY :: Int
randomY = 300   

-- Implementation of method 'hit' for each given enemy. 
enemyIsHit :: Enemy e => e -> Bullet -> Maybe (e, Bullet)
enemyIsHit e b | pos e == bulletPos b = Just (e, b) -- Enemy is hit. Change to this later: if the position is approximately equal.
               | otherwise = Nothing -- Enemy is not hit.

-- Enemy types as instances of type class Enemy (Too much repitition?)
instance Enemy BasicEnemy where
        hit = enemyIsHit
        spawn = BasicEnemy {basicEnemyPos = (screenWidth, randomY) , basicEnemyCooldown = cooldown} -- Spawn an enemy at right with a random y pos.
                where -- Default value, change this later.
                        cooldown = 2
        pos = basicEnemyPos

instance Enemy BurstEnemy where
        hit = enemyIsHit
        spawn = BurstEnemy {burstEnemyPos = (screenWidth, randomY) , burstEnemyCooldown = cooldown}  
                where
                        cooldown = 3
        pos = burstEnemyPos

instance Enemy ConeEnemy where
        hit = enemyIsHit
        spawn = ConeEnemy {coneEnemyPos = (screenWidth, randomY) , coneEnemyCooldown = cooldown}    
                where
                        cooldown = 2
        pos = coneEnemyPos

instance Enemy BasicPlayerSeekingEnemy where
        hit = enemyIsHit
        spawn = BasicPlayerSeekingEnemy {basicSeekingPos = (screenWidth, randomY) , basicSeekingCooldown = cooldown}      
                where
                        cooldown = 1
        pos = basicSeekingPos

instance Enemy FastPlayerSeekingEnemy where
        hit = enemyIsHit
        spawn = FastPlayerSeekingEnemy {fastSeekingPos = (screenWidth, randomY)}     
        pos = fastSeekingPos
       

-- All enemies, except the player seeking types, are instances of this class.
class MoveBasic a where 
        moveBasic :: a -> a 
    
instance MoveBasic BasicEnemy where
        moveBasic b = let (x, y) = pos b in
                b {basicEnemyPos = (x - 1, y)}

instance MoveBasic BurstEnemy where
        moveBasic b = let (x, y) = pos b in
                b {burstEnemyPos = (x - 1, y)}

instance MoveBasic ConeEnemy where
        moveBasic c = let (x, y) = pos c in
                c {coneEnemyPos = (x - 1, y)} 


    -- Only the two player seeking enemies are instances of this class.
class MovePlayerSeeking a where 
        moveSeeking :: a -> Player -> a        

instance MovePlayerSeeking BasicPlayerSeekingEnemy where
        moveSeeking b player = let (x, y) = pos b in
                b {basicSeekingPos = (x - 1, y + seeking_speed * (y - playerPos player))} -- The vertical movement is based on the vertical distance between player and enemy.
                where seeking_speed = 1 -- How fast the enemy approaches the player vertically
                

instance MovePlayerSeeking FastPlayerSeekingEnemy where
        moveSeeking :: FastPlayerSeekingEnemy -> Player -> FastPlayerSeekingEnemy
        moveSeeking f player = let (x, y) = pos f in
                f {fastSeekingPos = (x - 1, y + seeking_speed * (y - playerPos player))}
                where seeking_speed = 2



-- All enemies, except the fast player seeking type, are instances of this class.
class CanShoot a where 
        shoot :: a -> [Bullet] 

-- All bullet direction vectors
left = (-1, 0)
upward   = (-1, 1)
downward = (-1, -1)

instance CanShoot BasicEnemy where
        shoot b = [Bullet {bulletPos = pos b, bulletVector = left}] -- One bullet
                
instance CanShoot BurstEnemy where
        shoot b = let (x, y) = pos b in
                [Bullet {bulletPos = (x, y), bulletVector = left}, Bullet {bulletPos = (x + 1, y), bulletVector = left}, Bullet {bulletPos = (x + 2, y), bulletVector = left}] -- Three bullets in one horizontal line, two start a bit to the left of the enemy.

instance CanShoot ConeEnemy where
        shoot c = [Bullet  {bulletPos = pos c, bulletVector = left}, Bullet  {bulletPos = pos c, bulletVector = upward}, Bullet  {bulletPos = pos c, bulletVector = downward}] -- Three bullets in three different directions

instance CanShoot BasicPlayerSeekingEnemy where
        shoot b = [Bullet {bulletPos = pos b, bulletVector = left}] -- One bullet
        
        

  