module Model.GameState where

data GameState = { 
    isPaused :: Bool, 
    player :: Player,  
    enemies :: (
        [BasicEnemy], 
        [BurstEnemy], 
        [ConeEnemy], 
        [BasicPlayerSeekingEnemy], 
        [FastPlayerSeekingEnemy] 
    ), 
    bullets :: [Bullets], 
    score :: Score, 
    powerUps :: [PowerUp] 
} 