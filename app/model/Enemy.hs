module Model.Enemy where

import Model.General (Position)

-- All five enemy data types, enemy type class.
data BasicEnemy = BasicEnemy {basicEnemyPos :: Position, basicEnemyCooldown :: Int} 
data BurstEnemy = BurstEnemy {burstEnemyPos :: Position, burstEnemyCooldown :: Int} 
data ConeEnemy = ConeEnemy {coneEnemyPos :: Position, coneEnemyCooldown :: Int} 
data BasicPlayerSeekingEnemy = BasicPlayerSeekingEnemy {basicSeekingPos :: Position, basicSeekingCooldown :: Int} 
data FastPlayerSeekingEnemy = FastPlayerSeekingEnemy {fastSeekingPos :: Position} 



-- All enemies are instances of this class.
class Enemy a where 
    -- Given an enemy and a bullet, return nothing if the enemy is hit, and return both if the enemy is not hit. 
    hit:: a -> Bullet -> Maybe (a, Bullet) 
    -- Render enemy at a random vertical position.
    spawn :: a 

-- Enemy types as instances of type class Enemy (Too much repitition?)
instance Enemy BasicEnemy b where
        hit b bullet | basicEnemyPos == bulletPos = Nothing -- Enemy is hit. Change to: if the position is approximately equal.
                    | otherwise                  = b bullet -- Enemy is not hit.
        spawn = BasicEnemy {(screenWidth, randomY) , basicEnemyCooldown} -- Spawn an enemy at right with a random y pos.

instance Enemy BurstEnemy b where
        hit b bullet | burstEnemyPos == bulletPos = Nothing 
                    | otherwise                  = b bullet 
        spawn = BurstEnemy {(screenWidth, randomY) , burstEnemyCooldown}    

instance Enemy ConeEnemy c where
        hit c bullet | coneEnemyPos == bulletPos = Nothing
                    | otherwise                 = c bullet 
        spawn = ConeEnemy {(screenWidth, randomY) , coneEnemyCooldown}    

instance Enemy BasicPlayerSeekingEnemy b where
        hit b bullet | basicSeekingPos == bulletPos = Nothing 
                    | otherwise                  = b bullet 
        spawn = BasicPlayerSeekingEnemy {(screenWidth, randomY) , basicSeekingCooldown}      

instance Enemy FastPlayerSeekingEnemy f where
        hit f bullet | fastSeekingPos == bulletPos = Nothing 
                    | otherwise                   = f bullet 
        spawn = FastPlayerSeekingEnemy {(screenWidth, randomY) , fastSeekingCooldown}      

       

-- All enemies, except the player seeking types, are instances of this class.
class MoveBasic a where 
        move :: a -> a 
    
instance MoveBasic BasicEnemy b where
        move b =  BasicEnemy {basicEnemyPos = (fst (basicEnemyPos b) - 1, snd (basicEnemyPos b))} -- Not completely sure how to change the tuple values of position.

instance MoveBasic BurstEnemy b where
        move b =  BurstEnemy {burstEnemyPos = (fst (burstEnemyPos b) - 1, snd (burstEnemyPos b))}

instance MoveBasic ConeEnemy c where
        move b =  ConeEnemy {coneEnemyPos = (fst (coneEnemyPos c) - 1, snd (coneEnemyPos c))} 


    -- Only the two player seeking enemies are instances of this class.
class MovePlayerSeeking where 
        move :: a -> Player -> a        

instance MovePlayerSeeking BasicPlayerSeekingEnemy b where
        move b player =  BasicPlayerSeekingEnemy {basicSeekingPos = (fst (basicEnemyPos b) - 1, seeking_speed * abs (snd (basicSeekingPos b) - playerPos player))} -- The vertical movement is based on the vertical distance between player and enemy.
                where seeking_speed = 0.25 -- How fast the enemy approaches the player vertically

instance MovePlayerSeeking FastPlayerSeekingEnemy f where
        move f player =  FastPlayerSeekingEnemy {fastSeekingPos = (fst (fastEnemyPos f) - 1, seeking_speed * abs (snd (fastSeekingPos f) - playerPos player))}
                where seeking_speed = 0.45



-- All enemies, except the fast player seeking type, are instances of this class.
class CanShoot where 
        shoot :: a -> [Bullets] 

-- All bullet direction vectors
left = (-1, 0)
upward   = (-1, 1)
downward = (-1, -1)

instance CanShoot BasicEnemy b where
        shoot b {basicEnemyPos} = [Bullet {basicEnemyPos, left}] -- One bullet
                
instance CanShoot BurstEnemy b where
        shoot b {burstEnemyPos} = [Bullet {burstEnemyPos, left}, Bullet {burstEnemyPos - 1, left}, Bullet {burstEnemyPos - 2, left}] -- Three bullets in one horizontal line, two start a bit to the left of the enemy.

instance CanShoot ConeEnemy c where
        shoot c {coneEnemyPos} = [Bullet {coneEnemyPos, left}, Bullet {coneEnemyPos, upward}, Bullet {coneEnemyPos, downward}] -- Three bullets in three different directions

instance CanShoot BasicPlayerSeekingEnemy b where
        shoot b {basicSeekingPos} = [Bullet {basicSeekingPos, left}] -- One bullet
        
        
