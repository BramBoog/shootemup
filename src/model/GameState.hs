{-# LANGUAGE NamedFieldPuns #-}
module Model.GameState where

import Model.Movement (outOfBounds)
import Model.Player
import Model.Enemy
import Model.Shooting
import Model.PowerUp
import View.Animations
import Data.List ((\\))

data GameState = GameState {
  phase :: GamePhase,
  player :: Player,
  enemies :: (
    [BasicEnemy],
    [BurstEnemy],
    [ConeEnemy],
    [BasicPlayerSeekingEnemy],
    [FastPlayerSeekingEnemy]
  ),
  bullets :: [Bullet],
  score :: Score,
  powerUps :: [PowerUp],
  animations :: [Animation]
} deriving Show

data GamePhase = Playing | Paused | GameOver deriving (Eq, Show)
type Score = Int
type GameStateTransform = GameState -> GameState

initialState :: GameState
initialState = GameState {
  phase = Playing,
  player = initialPlayer,
  enemies = initialEnemies,
  bullets = [],
  score = 0,
  powerUps = initialPowerUps
}
  where 
    initialPlayer = Player {playerPos = (-360, 100), speed = 5, playerWeapon = Single, lives = 10, playerCooldown = 5}
    initialEnemies = ([], [BurstEnemy (300, 80) 1], [ConeEnemy (300, -20) 1], [], [FastPlayerSeekingEnemy (300, -80)])
    initialPowerUps = [BurstFire {burstFirePos = (-360, -200)}]


moveBullets :: GameStateTransform
moveBullets gs = gs{bullets = map moveBullet (bullets gs)}

enemiesMove :: GameStateTransform
enemiesMove gs@GameState{
  player,
  enemies = (basicEnemies, burstEnemies, coneEnemies, basicPlayerSeekingEnemies, fastPlayerSeekingEnemies)
} = gs{enemies = (
      map moveBasic basicEnemies,
      map moveBasic burstEnemies,
      map moveBasic coneEnemies,
      map (moveSeeking player) basicPlayerSeekingEnemies,
      map (moveSeeking player) fastPlayerSeekingEnemies
    )}

-- Despawn all enemies an bullets which have gone out of bounds.
despawnOutOfBounds :: GameStateTransform
despawnOutOfBounds gs@GameState{
  enemies = (basicEnemies, burstEnemies, coneEnemies, basicPlayerSeekingEnemies, fastPlayerSeekingEnemies),
  bullets
} = let basicOutOfBounds        = filter outOfBounds basicEnemies
        burstOutOfBounds        = filter outOfBounds burstEnemies
        coneOutOfBounds         = filter outOfBounds coneEnemies
        basicSeekingOutOfBounds = filter outOfBounds basicPlayerSeekingEnemies
        fastSeekingOutOfBounds  = filter outOfBounds fastPlayerSeekingEnemies
        bulletsOutOfBounds      = filter outOfBounds bullets
     in despawn basicOutOfBounds
      $ despawn burstOutOfBounds
      $ despawn coneOutOfBounds
      $ despawn basicSeekingOutOfBounds
      $ despawn fastSeekingOutOfBounds
      $ despawn bulletsOutOfBounds gs 

-- Determines which enemies have been hit by bullets and despawns both them and the corresponding bullets.
killEnemies :: GameStateTransform
killEnemies gs@GameState{
  enemies = (basicEnemies, burstEnemies, coneEnemies, basicPlayerSeekingEnemies, fastPlayerSeekingEnemies),
  bullets
} = let (basicHit, bulletsHitBasic)               = hitByBulletsList basicEnemies bullets
        (burstHit, bulletsHitBurst)               = hitByBulletsList burstEnemies bullets
        (coneHit, bulletsHitCone)                 = hitByBulletsList coneEnemies bullets
        (basicSeekingHit, bulletsHitBasicSeeking) = hitByBulletsList basicPlayerSeekingEnemies bullets
        (fastSeekingHit, bulletsHitFastSeeking)   = hitByBulletsList fastPlayerSeekingEnemies bullets
        allBulletsHit                             = bulletsHitBasic
                                                 ++ bulletsHitBurst
                                                 ++ bulletsHitCone
                                                 ++ bulletsHitBasicSeeking
                                                 ++ bulletsHitFastSeeking
     in despawn basicHit
      $ despawn burstHit
      $ despawn coneHit
      $ despawn basicSeekingHit
      $ despawn fastSeekingHit
      $ despawn allBulletsHit gs

-- Lowers the shooting cooldowns by the elapsed time for the player an all enemies types that can shoot.
lowerCooldowns :: Float -> GameStateTransform
lowerCooldowns t gs@GameState{
  player,
  enemies = (basicEnemies, burstEnemies, coneEnemies, basicPlayerSeekingEnemies, fastPlayerSeekingEnemies)
} = let basicLowered   = map (lowerCooldown t) basicEnemies
        burstLowered   = map (lowerCooldown t) burstEnemies
        coneLowered    = map (lowerCooldown t) coneEnemies
        seekingLowered = map (lowerCooldown t) basicPlayerSeekingEnemies
     in gs{
       player = lowerCooldown t player,
       enemies = (basicLowered, burstLowered, coneLowered, seekingLowered, fastPlayerSeekingEnemies)
     }

-- Let all enemies shoot, if their cooldown allows. Add the new enemies (with reset cooldowns) and newly generated bullets to the new GameState
enemiesShoot :: GameStateTransform
enemiesShoot gs@GameState{
  enemies = (basicEnemies, burstEnemies, coneEnemies, basicPlayerSeekingEnemies, fastPlayerSeekingEnemies),
  bullets
} = let (newBasics, newBulletsBasic)     = allShoot basicEnemies
        (newBursts, newBulletsBurst)     = allShoot burstEnemies
        (newCones, newBulletsCone)       = allShoot coneEnemies
        (newSeekings, newBulletsSeeking) = allShoot basicPlayerSeekingEnemies
        allNewBullets                    = newBulletsBasic
                                        ++ newBulletsBurst
                                        ++ newBulletsCone
                                        ++ newBulletsSeeking
     in gs{
       enemies = (newBasics, newBursts, newCones, newSeekings, fastPlayerSeekingEnemies),
       bullets = bullets ++ allNewBullets
     }

-- Calculate all enemies and bullets which hit the player; despawn all of them and subtract a life from the player for each.
takeHitsPlayer :: GameStateTransform
takeHitsPlayer gs@GameState{
  player,
  enemies = (basicEnemies, burstEnemies, coneEnemies, basicPlayerSeekingEnemies, fastPlayerSeekingEnemies),
  bullets
} = let basicHit        = hitsOnPlayer player basicEnemies
        burstHit        = hitsOnPlayer player burstEnemies
        coneHit         = hitsOnPlayer player coneEnemies
        basicSeekingHit = hitsOnPlayer player basicPlayerSeekingEnemies
        fastSeekingHit  = hitsOnPlayer player fastPlayerSeekingEnemies
        bulletsHit      = hitsOnPlayer player bullets
        lostLives       = length basicHit
                        + length burstHit 
                        + length coneHit
                        + length basicSeekingHit
                        + length fastSeekingHit
                        + length bulletsHit
     in despawn basicHit
      $ despawn burstHit
      $ despawn coneHit
      $ despawn basicSeekingHit
      $ despawn fastSeekingHit
      $ despawn bulletsHit
      $ subtractLives lostLives gs
     where
       subtractLives :: Int -> GameStateTransform
       subtractLives x gs@GameState{player} = gs{player = player {lives = lives player - x}}

gameOverIfNoLives :: GameStateTransform
gameOverIfNoLives gs@GameState{player} = if lives player <= 0 then gs{phase = GameOver}
                                         else gs

-- Combine all GameStateTransforms that occur on a time step into one, the game is in Playing phase.
updateOnStep :: Float -> GameStateTransform
updateOnStep t gs@GameState{phase} = case phase of                    -- read these in reverse order, since that's the order they're applied
                                       Playing -> gameOverIfNoLives   -- finally, transition to GameOver phase if player has lost all lives
                                                $ despawnOutOfBounds -- despawn all bullets and enemies which have gone out of bounds
                                                $ takeHitsPlayer     -- and check again if the player has been hit
                                                $ enemiesMove        -- then move the enemies
                                                $ takeHitsPlayer     -- and if the player has been hit
                                                $ killEnemies        -- after moving bullets, check if that killed any enemies
                                                $ moveBullets
                                                $ enemiesShoot
                                                $ lowerCooldowns t gs
                                       _ -> gs

class Despawnable a where
  despawn :: [a] -> GameStateTransform

-- Use list difference operator \\ to remove elements to despawn from each list
instance Despawnable BasicEnemy where
  despawn es gs = let (basics, bursts, cones, basicseekings, fastseekings) = enemies gs
                   in gs {enemies = (basics \\ es, bursts, cones, basicseekings, fastseekings)}

instance Despawnable BurstEnemy where
  despawn es gs = let (basics, bursts, cones, basicseekings, fastseekings) = enemies gs
                   in gs {enemies = (basics, bursts \\ es, cones, basicseekings, fastseekings)}

instance Despawnable ConeEnemy where
  despawn es gs = let (basics, bursts, cones, basicseekings, fastseekings) = enemies gs
                   in gs {enemies = (basics, bursts, cones \\ es, basicseekings, fastseekings)}

instance Despawnable BasicPlayerSeekingEnemy where
  despawn es gs = let (basics, bursts, cones, basicseekings, fastseekings) = enemies gs
                   in gs {enemies = (basics, bursts, cones, basicseekings \\ es, fastseekings)}

instance Despawnable FastPlayerSeekingEnemy where
  despawn es gs = let (basics, bursts, cones, basicseekings, fastseekings) = enemies gs
                   in gs {enemies = (basics, bursts, cones, basicseekings, fastseekings \\ es)}

instance Despawnable Bullet where
  despawn bs gs = gs {bullets = bullets gs \\ bs}
